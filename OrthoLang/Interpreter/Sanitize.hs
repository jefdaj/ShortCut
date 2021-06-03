
{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Interpreter.Sanitize
  -- ( hashIDsFile
  ( hashIDsFile
  -- , writeIDs
  , unhashIDs
  , unhashIDsFile
  , readIDs
  -- , lookupHash
  -- , lookupHashesFile
  , lookupID
  , lookupIDsFile
  )
  where

-- TODO space leak probably in here, right? pinpoint it!

{- This tries to prevent various programs from choking on badly formatted FASTA
 - sequence IDs. It replaces them with hashes before running anything, keeps a
 - hash -> ID map, and puts them back in the final output.
 -}

-- TODO this is almost fast enough to be usable, but tweak performance some more
--      (getting rid of debug helps, as does DList)
-- TODO would having it be a set from the beginning be faster still?
-- TODO would doing it in a separate script be better so it can run on other nodes?

import Prelude hiding (readList, error)
import OrthoLang.Debug
-- import qualified Data.DList as D
import qualified Data.Map.Strict   as M
-- import qualified Text.Regex as R

import Development.Shake
import OrthoLang.Types

import OrthoLang.Util    (digest, digestLength, headOrDie)
import OrthoLang.Locks   (withWriteLock')
import OrthoLang.Interpreter.Actions (trackWrite', readFileStrict', readList, writeCachedLines, runCmd, CmdDesc(..))
import OrthoLang.Interpreter.Paths   (toPath, fromPath, addDigest)
import Data.Char             (isSpace)
import Data.Maybe            (catMaybes, mapMaybe, fromJust)
import Data.List             (isPrefixOf, intersperse, nub)
import Data.List.Utils       (split, subIndex)
import System.FilePath (takeFileName, takeDirectory, (<.>))
import System.Directory           (createDirectoryIfMissing, copyFile)
import Data.IORef                  (readIORef)
import System.Exit                (ExitCode(..))
-- import qualified Data.Text.Lazy as T
-- import Text.Pretty.Simple (pShowNoColor)
import Control.Exception (SomeException)

-- type HashedIDList = D.DList (String, String)

-- if the line is a fasta sequence id we hash it, otherwise leave alone
-- TODO use the map itself as an accumulator instead
hashIDsLine :: String -> (String, Maybe (String, String))
hashIDsLine ('>':seqID) = ('>':idHash, Just (idHash, seqID)) -- TODO issue dropping newlines here?
  where
    loc = "ortholang.interpreter.sanitize.hashIDsLine"
    idHash = "seqid_" ++ digest loc seqID -- TODO does storing the extra seqid_ prefix slow it down?
hashIDsLine txt = (txt, Nothing)

-- return the FASTA content with hashed IDs, along with a map of hashes -> original IDs
hashIDsTxt :: String -> (String, M.Map String String)
hashIDsTxt txt = (unlines lines', M.fromList seqids)
  where
    hashed = map hashIDsLine $ lines txt
    lines' = map fst hashed
    seqids = mapMaybe snd hashed

-- copy a fasta file to another path, replacing sequence ids with their hashes
-- hashIDsFile :: Config -> LocksRef -> Path -> Path -> Action IDs
-- hashIDsFile cfg ref inPath outPath = do
--   let inPath'  = fromPath loc cfg inPath
--       outPath' = fromPath loc cfg outPath
--   -- txt <- withReadLock' inPath' $ readFile' $ fromPath loc cfg inPath
--   txt <- readFileStrict' inPath'
--   let (!fasta', !ids) = hashIDsTxt txt
--       (Path k) = outPath
--       v = takeFileName inPath'
--       -- ids' = trace ("k: \"" ++ k ++ "' v: \"" ++ v ++ "\"") $ M.insert k v ids
--       ids' = M.insert k v ids
--   withWriteLock' outPath' $ liftIO $ writeFile outPath' fasta' -- TODO be strict?
--   trackWrite' [outPath']
--   return ids'

-- rewrite of hashIDsFile as a python script; will it fix the space leak?
hashIDsFile :: Bool -> Path -> Path -> Action ()
hashIDsFile allowDuplicateIDs inFa outFa = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.sanitize.hashIDsFile"
      inFa'   = fromPath loc cfg inFa
      outFa'  = fromPath loc cfg outFa
      outIDs' = outFa' <.> "ids"
      outIDs  = toPath loc cfg outIDs'
      -- (Path k) = inFa
      -- v = takeFileName inPath'
      -- ids' = M.insert k v ids
  liftIO $ createDirectoryIfMissing True $ takeDirectory inFa'
  liftIO $ createDirectoryIfMissing True $ takeDirectory outFa'
  runCmd $ CmdDesc
    { cmdBinary = "hash_seqids.py"
    , cmdArguments = [outFa', inFa', show allowDuplicateIDs]
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdOptions = []
    , cmdInPatterns = [inFa']
    , cmdNoNeedDirs = []
    , cmdOutPath = outFa'
    , cmdExtraOutPaths = [outIDs']
    , cmdSanitizePaths = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outFa', outIDs']
    }

-- writeIDs :: Config -> LocksRef -> Path -> IDs -> Action ()
-- writeIDs cfg ref path ids = do
--   withWriteLock' path'
--     $ liftIO
--     $ writeFile path' -- TODO be strict?
--     $ unlines
--     $ map toLine
--     $ M.toList ids
--   trackWrite' [path']
--   where
--     path' = fromPath loc cfg path
--     toLine (h, i) = h ++ "\t" ++ i

-- see https://stackoverflow.com/q/48571481
-- unhashIDs :: IDs -> String -> String
-- unhashIDs ids txt = foldl (\acc (k, v) -> R.subRegex (R.mkRegex k) acc v) txt $ M.toList ids

-- sorry in advance! this is what you get when you build it in ghci
-- its hackier than the regex, but also faster
-- TODO add a config setting for long or short IDs
-- TODO does storing a bunch of seqid_ strings in the map slow it down?
unhashIDs :: Bool -> IDs -> String -> String
unhashIDs longIDs ids t = case findNext t of
                   Nothing -> t
                   Just i  -> let (before, after) = splitAt i t
                                  after' = replaceNextFold after
                              -- the take/drop thing is a hacky way to prevent endless cycles
                              in before ++ take 1 after' ++ unhashIDs longIDs ids (drop 1 after')
                              -- in before ++ unhashIDs longIDs ids after'
  where
    replaceNextFold txt = foldr replacePattern txt patterns
    replacePattern (ptn, fn) txt = if ptn `isPrefixOf` txt then replaceLen txt fn else txt
    replaceLen txt lFn = let (sid, rest) = splitAt (lFn txt) txt
                         in case lookupID ids sid of
                              Nothing -> txt -- happens to other $TMPDIR paths
                              Just v  -> if longIDs
                                           then v ++ rest
                                           else fst (splitSeqid' v) ++ rest
    findNext txt = case mapMaybe ((`subIndex` txt) . fst) patterns of
                     (x:xs) -> Just $ minimum (x:xs)
                     _ -> Nothing
    patterns =
      [ ("seqid_"  , \_   -> length "seqid_" + digestLength)
      , ("$TMPDIR" , length . takeWhile (not . isSpace))
      , ("$WORKDIR", length . takeWhile (not . isSpace)) -- TODO remove this one?
      ]

-- This sometimes operates internally, but also writes the final result of the cut to a file.
-- In that case the outpath might not be able to be a cutpath.
unhashIDsFile' :: Path -> FilePath -> Action ()
unhashIDsFile' inPath outPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.sanitize.unhashIDsFile"
      inPath'  = fromPath loc cfg inPath
  -- txt <- withReadLock' inPath' $ readFile' $ fromPath loc cfg inPath

  -- TODO if the file turns out to be binary, just copy rather than unhashing anything
  txt <- readFileStrict' inPath'

  -- liftIO $ putStrLn $ "txt: \"" ++ txt ++ "\""
  -- let txt' = unhashIDs ids txt
  idref <- fmap fromJust getShakeExtra
  ids <- liftIO $ readIORef idref
  -- liftIO $ putStrLn $ "ids: " ++ show ids
  let txt' = unhashIDs True ids txt
  -- liftIO $ putStrLn $ "txt': \"" ++ txt' ++ "\""
  withWriteLock' outPath $ liftIO $ writeFile outPath txt' -- TODO be strict?
  trackWrite' [outPath]

-- unhashIDsFile' fails on binary files. for now, the simplest fix seems to be
-- to skip it and copy instead
unhashIDsFile :: Path -> FilePath -> Action ()
unhashIDsFile inPath outPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.sanitize.unhashIDsFile'"
      in' = fromPath loc cfg inPath
  unhashIDsFile' inPath outPath `actionCatch` (\(_ :: SomeException) -> liftIO $ copyFile in' outPath)

-- from: https://stackoverflow.com/a/40297465
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

splitSeqid :: String -> (String, String)
splitSeqid = fmap (drop 1) . break (`elem` " \t;") -- TODO does this work for most cases?

splitSeqid' :: String -> (String, String)
splitSeqid' s = let r = splitSeqid s in trace "splitSeqid" ("splitSeqid '" ++ s ++ "' -> '" ++ show r ++ "'") r

-- TODO should this be IO or Action?
-- TODO any reason to keep the rest of the seqids as keys to the hashes? most ppl will use short ones
readIDs :: Path -> Action (M.Map String String)
readIDs path = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.sanitize.readIDs"
      path' = fromPath loc cfg path
  -- txt <- withReadLock' path' $ readFile' path'
  txt <- readFileStrict' path'
  -- let splitFn l = let ws = split "\t" l
  --                 in if length ws < 2
  --                      then error ("failed to split \"" ++ l ++ "\"")
  --                      else (head ws, concat $ intersperse "\t" $ tail ws)
  let ids = map (splitAtFirst '\t') (lines txt)
      -- hashes = map (\(h, i) -> (fst $ splitSeqid i, h)) ids
  -- liftIO $ putStrLn $ "loaded " ++ show (length ids) ++ " ids"
  -- liftIO $ putStrLn $ "ids'' with    seqid_: " ++ show ((take 10 $ filter (\(k,_) ->     ("seqid_" `isPrefixOf` k)) ids) :: [(String, String)])
  -- liftIO $ putStrLn $ "ids'' without seqid_: " ++ show ((take 10 $ filter (\(k,_) -> not ("seqid_" `isPrefixOf` k)) ids) :: [(String, String)])
  return $ M.fromList ids

-- TODO display error to user in a cleaner way than error!
-- TODO is this really needed? seems suuuper slow. replace with error or just by warning them?
-- lookupID :: IDs -> String -> [String]
-- lookupID ids s = map fst $ filter (\(k,v) -> s == k || s `isPrefixOf` v) (M.toList ids)

-- this works whether the ID is for a file or seqid. files are checked first
lookupID :: IDs -> String -> Maybe String
lookupID IDs {hFiles = f, hSeqIDs = s} i =
  if M.member i f then M.lookup i f                     -- i is a path; return the short version
  else case mapMaybe (M.lookup i) (M.elems s) of -- i is a seqid; look in all the maps for it
    []  -> Nothing
    [x] -> Just x
    xs  -> trace "interpreter.sanitize.lookupID" ("WARNING: duplicate seqids. using the first:\n" ++ show xs) (Just $ head xs)

-- TODO move to Actions? causes an import cycle so far
-- TODO is this one ever used, or just the reverse hashes one below? maybe remove
lookupIDsFile :: Path -> Path -> Action ()
lookupIDsFile inPath outPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.sanitize.lookupIDsFile"
  partialIDs <- readList loc $ fromPath loc cfg inPath
  ref <- fmap fromJust getShakeExtra
  ids <- liftIO $ readIORef ref
  let lookupFn v = case lookupID ids v of
                     Just i  -> return i
                     Nothing -> do
                       liftIO $ putStrLn ("warning: no ID found for \"" ++ v ++ "\"")
                       -- liftIO $ putStrLn $ "here are all the current ids:\n" ++ T.unpack (pShowNoColor ids)
                       return v
  idKeys <- mapM lookupFn partialIDs
  dRef <- fmap fromJust getShakeExtra
  liftIO $ addDigest dRef (ListOf str) outPath -- TODO make this an Action?
  let loc = "interpreter.sanitize.lookupIDsFile"
  writeCachedLines loc (fromPath loc cfg outPath) idKeys

-- lookupHash :: IDs -> String -> Maybe String
-- lookupHash (IDs {hSeqHashes = hs}) i =
--   case catMaybes (map (M.lookup i) (M.elems hs)) of -- i is a seqid; look in all the maps for it
--     []  -> Nothing
--     [x] -> Just x
--     xs  -> trace "interpreter.sanitize.lookupHash" ("WARNING: duplicate hashes. using the first:\n" ++ show xs) (Just $ head xs)
-- 
-- -- TODO move to Actions? causes an import cycle so far
-- lookupHashesFile :: Path -> Path -> Action ()
-- lookupHashesFile inPath outPath = do
--   cfg <- fmap fromJust getShakeExtra
--   let loc = "interpreter.sanitize.lookupHashesFile"
--   partialIDs <- readList loc $ fromPath loc cfg inPath
--   ref <- fmap fromJust getShakeExtra
--   ids <- liftIO $ readIORef ref
--   let lookupFn v = case lookupHash ids v of
--                      Just h  -> return h
--                      Nothing -> do
--                        liftIO $ putStrLn ("warning: no seqid_ hash found for \"" ++ v ++ "\"")
--                        return v
--   idKeys <- mapM lookupFn partialIDs
--   dRef <- fmap fromJust getShakeExtra
--   liftIO $ addDigest dRef (ListOf str) outPath -- TODO make this an Action?
--   writeCachedLines loc (fromPath loc cfg outPath) idKeys
