{-# LANGUAGE ScopedTypeVariables #-}

{-|
Some Shake Actions wrapped with OrthoLang-specific additions,
and some other stuff. Eventually, it would be nice if all IO happened here.

TODO rename to be shorter (no "safe", "wrapped", etc)
-}

module OrthoLang.Interpreter.Actions

  -- read files
  ( readLit
  , readLits
  , readPath
  , readPaths
  , absolutizePaths
  , readLitPath
  , readLitPaths
  , readString
  , readStrings
  , readList
  , countLines
  , readFileStrict
  , readFileStrict'

  -- write files
  , writeLit
  , writeLits
  , writePath
  , writePaths
  , writeString
  , writeStrings
  , writeCachedLines
  , writeCachedVersion
  , sanitizeFileInPlace
  , sanitizeFilesInPlace

  -- debugging
  -- TODO export different ones for the modules from Modules.hs?
  , need'
  , trackWrite' -- TODO should this not be needed?
  , traceA
  , debugA

  -- run system commands
  -- TODO remove all these except one, and have it take a record
  , CmdDesc(..)
  , runCmd

  -- misc
  -- , digestFile  -- TODO what's the difference with hashContent?
  , hashContent
  , withBinHash
  , symlink
  , cachedLinesPath

  )
  where

import Prelude hiding (readList, error)
import OrthoLang.Debug
import Development.Shake hiding (doesFileExist)
import OrthoLang.Types
-- import OrthoLang.Interpreter.Config (debug)

import Control.Monad              (when)
import Data.List                  (sort, nub, isPrefixOf, isInfixOf, isSuffixOf)
import Data.List.Split            (splitOneOf)
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators, makeRelative, splitPath)
-- import OrthoLang.Interpreter.Debug        (debug)
import OrthoLang.Interpreter.Paths        (Path, toPath, fromPath, checkLit, isGeneric, fromGeneric,
                                   checkLits, cacheDir, pathString, isURL,
                                   stringPath, toGeneric, sharedPath, addDigest)
import OrthoLang.Util         (digest, digestLength, rmAll, readFileStrict, absolutize, resolveSymlinks,
                                   ignoreExistsError, digest, globFiles, isEmpty, headOrDie)
import OrthoLang.Locks        (withReadLock', withReadLocks', markDone, withWriteOnce,
                                   withWriteLock', withWriteLocks')
import System.Directory           (doesFileExist, createDirectoryIfMissing, pathIsSymbolicLink, copyFile, renameFile)
import System.Exit                (ExitCode(..))
import System.FilePath            ((<.>), takeDirectory, takeExtension, takeBaseName)
import System.FilePath.Glob       (compile, globDir1)
-- import System.IO                  (IOMode(..), withFile)
import System.Posix.Files         (readSymbolicLink, createSymbolicLink, setFileMode)
import System.Posix.Escape         (escape)
-- import System.IO.Temp (emptyTempFile)
-- import Control.Concurrent.Thread.Delay (delay)
import Data.Maybe (isJust, fromJust)
-- import Network.HTTP (simpleHTTP, getRequest, Response(..))
import System.IO.Temp (writeTempFile)
import qualified Data.ByteString as BS
import Network.Download (openURIString)
-- import Control.Exception (try)
import Control.Exception.Safe (catchAny)
import Data.Scientific            (Scientific())
import Data.IORef (readIORef)
import qualified System.IO.Strict as Strict
-- import System.Environment (getEnv)
import Data.Set (toList, fromList, difference)

---------------
-- debugging --
---------------

-- TODO should this use one of Shake's put* functions instead and be in Action?
-- TODO oh, and shake has traced now too! consider using that, or rewriting with Logging
debugA :: String -> String -> Action ()
debugA name msg = liftIO $ debug name msg

-- debugA is for export; debugA' is specialized to this module
debugA' :: String -> String -> Action ()
debugA' name = debugA ("interpreter.actions." ++ name)

-- TODO use this for all (or most) of the current debugS calls if possible
-- TODO use Shake's traced function for this? rewrite it to work with Logging?
traceA :: (Show a, Show b) => DebugLocation -> a -> [b] -> a
traceA name out args = trace "interpreter.actions" msg out
  where
    msg = name ++ " creating " ++ show out ++ " from " ++ show args

-- TODO is this .need thing the best convention?
need' :: String -> [FilePath] -> Action ()
need' fnName paths = do
  cfg <- fmap fromJust getShakeExtra
  let loc = fnName ++ ".need'"
  mapM_ (needShared fnName . toPath loc cfg) paths -- TODO loc instead of fnName?

needDebug :: String -> [FilePath] -> Action ()
needDebug fnName paths = do
  debugA (fnName ++ ".need") (show paths)
  need paths

{-|
A version of 'Development.Shake.need' with both debugging and shared cache lookup.
There are a few extra cases to account for when doing shared lookup...

If the path is to reps, vars, or somewhere outside the TMPDIR, don't try to mess with it.
Take a list of FnTags, and skip shared lookup if they say it depends on the local filesystem.
(This can be accurately approximated with the specific load + glob exceptions below for now)
If the path already exists or is to a str, num, or list of those, skip shared lookup for speed.
Is the special case for downloaded paths needed? Maybe remove it.
If the path is available in the shared cache, copy it over + trackWrite. Otherwise needDebug as normal.

TODO If the path is to a list of paths, should we also recursively need those
     at the end (whether or not it already exists)?
-}
-- TODO replace these with FnTags saying not to fetch them
needShared :: String -> Path -> Action ()
needShared name path@(Path p) = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.actions.needShared" -- TODO pass from callers?
      path' = fromPath loc cfg path
  debugA' loc $ "called for '" ++ path' ++ "'"

  -- special cases to skip shared lookup...
  -- skip cache lookup if the file exists already
  done <- liftIO $ doesFileExist path'
  if done
    -- these are probably faster to recompute than fetch
    || ("$TMPDIR/exprs/str/" `isPrefixOf` p)
    || ("$TMPDIR/exprs/num/" `isPrefixOf` p)
    || ("$TMPDIR/reps/" `isPrefixOf` p)
    || ("$TMPDIR/vars/" `isPrefixOf` p)
    -- these depend on the local filesystem
    || not ("$TMPDIR" `isPrefixOf` p)
    || ("$TMPDIR/exprs/load" `isPrefixOf` p)
    || ("$TMPDIR/exprs/glob" `isPrefixOf` p)
    -- if any of those ^ special cases apply, skip shared lookup
    then do
      debugA' "needShared" $ "skip shared lookup and needDebug normally: '" ++ path' ++ "'"
      needDebug name [path']

    -- otherwise, attempt shared lookup
    else do
      debugA' "needShared" $ "checking for shared version: '" ++ show path ++ "'"
      shared <- lookupShared path
      case shared of
        -- path not in shared cache; need via the usual mechanism instead
        Nothing -> do
          debugA' "needShared" $ "no shared version found, so needDebug normally: '" ++ path' ++ "'"
          needDebug name [path']
        -- path is available!
        -- now the main task: copy and/or download the file
        Just sp -> do
          debugA' "needShared" $ "found shared path for '" ++ path' ++ "': '" ++ sp ++ "'"
          fetchShared sp path'

-- TODO figure out better criteria for download
-- TODO and do it via macro expansion rather than as a case in here
fetchShared :: FilePath -> FilePath -> Action ()
fetchShared sp path' = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path'
  liftIO $ if "download" `isInfixOf` sp
             then renameFile sp path'
             else copyFile   sp path'
  trackWrite' [path']

-- TODO and should the symlink also be created?
-- TODO this will get the abspath of the *cache* version, right? have to get the local one!
-- needLinkSrcIfAny :: String -> FilePath -> Action ()
-- needLinkSrcIfAny name link = do
--   isLink <- liftIO $ pathIsSymbolicLink link
--   when isLink $ do
--     relPath <- liftIO $ readSymbolicLink link
--     absPath <- liftIO $ absolutize $ takeDirectory link </> relPath
--     need' name [absPath]

{-|
This is also a little more complicated than it would seem, because:

1. The shared location might be a local folder or a URL
2. The file we're looking up might require others to go along with it

TODO are all the files needed always the prefix + "*"?
TODO move download from here to a macro expansion?
-}
lookupShared :: Path -> Action (Maybe FilePath)
lookupShared path = do
  cfg <- fmap fromJust getShakeExtra
  case sharedPath cfg path of
    Nothing -> return Nothing
    Just sp -> do
      if "http" `isPrefixOf` sp -- TODO come up with something more robust to detect urls!
      then liftIO $ download cfg sp
        else do
          exists <- liftIO $ doesFileExist sp
          return $ if exists
            then Just sp
            else Nothing

-- TODO where should this go?
download :: Config -> String -> IO (Maybe FilePath)
download cfg url = do
  es <- openURIString url
  case es of
    Left _ -> return Nothing
    Right s -> Just <$> writeTempFile (tmpdir cfg) "download.txt" s

----------------
-- read files --
----------------

-- Action version of readFileStrict. This is used for all reads during a cut;
-- the raw one is just for showing results, reading cmd files etc.
-- TODO this fails on binary files?
readFileStrict' :: FilePath -> Action String
readFileStrict' path = do
  ref <- fmap fromJust getShakeExtra
  need' "interpreter.actions.readFileStrict'" [path]
  -- withReadLock' path $ liftIO (readFileStrict ref path)
  liftIO (readFileStrict ref path)
-- {-# INLINE readFileStrict' #-}

{- OrthoLang requires empty strings to contain the text <<emptystr>> so we
 - can distinguish them from the empty files that might result from a cmd
 - failing, and from empty lists in case of an error in the typechecker.  This
 - also gives empty lists and strings distinct hashes.
 -}
readLit :: DebugLocation -> FilePath -> Action String
readLit loc path = do
  let loc = "ortholang.interpreter.actions.readLit"
  debugA' loc path
  need' loc [path] -- note: this does actually seem to be important for reps!
  empty <- isEmpty path
  if empty
    then return ""
    else checkLit loc . init <$> readFileStrict' path

readLits :: DebugLocation -> FilePath -> Action [String]
readLits loc path = checkLits loc <$> readList loc path

readPath :: DebugLocation -> FilePath -> Action Path
readPath loc path = headOrDie "readPath failed" <$> readPaths loc path

-- TODO should this have checkPaths?
-- TODO what happens when each is itself a list of paths?
readPaths :: DebugLocation -> FilePath -> Action [Path]
readPaths loc path = do
  map stringPath <$> readList loc path

-- makes a copy of a list of paths without ortholang funny business,
-- suitible for external scripts to read
-- TODO does this go here or somewhere else?
absolutizePaths :: DebugLocation -> FilePath -> FilePath -> Action ()
absolutizePaths loc inPath outPath = do
  let loc' = loc ++ ".absolutizePaths"
  paths  <- readPaths loc' inPath
  cfg <- fmap fromJust getShakeExtra
  paths' <- mapM (liftIO . absolutize. fromPath loc' cfg) paths
  need' loc' paths' -- because they will be read by the script next
  -- liftIO $ putStrLn $ "paths': " ++ show paths'
  withWriteLock' outPath $ writeFile' outPath $ unlines paths'

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
-- Note this does *not* imply that the paths are to literals
readLitPaths :: DebugLocation -> FilePath -> Action [Path]
readLitPaths loc path = do
  let loc' = loc ++ ".readLitPaths"
  cfg <- fmap fromJust getShakeExtra
  -- pth'' <- if isURL pth' then curl pth' else return $ toPath loc cfg pth'
  let toAbs line
        | isURL line = line
        | isGeneric line = fromGeneric loc' cfg line
        | isAbsolute line = line
        | otherwise = workdir cfg </> line
  ls <- readList loc' path
  let ls'  = map toAbs ls
      ls'' = map (toPath loc cfg) ls'
  -- Note: need' causes infinite recursion here, so we skip to needDebug
  -- needDebug loc' ls'
  return ls''

readLitPath :: DebugLocation -> FilePath -> Action Path
readLitPath loc path = headOrDie "readLitPaths failed" <$> readLitPaths loc path

-- TODO how should this relate to readLit and readStr?
readString :: DebugLocation -> Type -> FilePath -> Action String
readString loc etype path = headOrDie "readString failed" <$> readStrings loc etype path

{- Read a "list of whatever". Mostly for generic set operations. You include
 - the Type (of each element, not the list!) so it knows how to convert from
 - String, and then within the function you treat them as Strings.
 -}
readStrings :: DebugLocation -> Type -> FilePath -> Action [String]
readStrings loc etype p =
  let loc' = loc ++ ".readStrings"
  in if etype' `elem` [str, num]
       then readLits loc' p
       else do
         cfg <- fmap fromJust getShakeExtra
         (fmap . map) (fromPath loc' cfg) (readPaths loc' p)
       where
         etype' = trace "interpreter.actions.readStrings" ("readStrings (each " ++ ext etype ++ ") from " ++ p) etype

{- OrthoLang requires empty lists to contain the text <<emptylist>> so we can
 - distinguish them from the empty files that might result from a cmd
 - failing, and from empty strings in case of an error in the typechecker.
 - This also gives empty lists and strings distinct hashes.
 -
 - Note that strict reading is important to avoid "too many open files" on long lists.
 -}
readList :: DebugLocation -> FilePath -> Action [String]
readList loc path = do
  let loc' = loc ++ ".readList"
  debugA' loc' $ show path
  -- need' loc' [path] -- Note isEmpty also does this
  empty <- isEmpty path
  if empty
    then return []
    else do
      lRef <- fmap fromJust getShakeExtra
      fmap lines
        $ withReadLock' path
        $ liftIO
        $ readFileStrict lRef path

countLines :: FilePath -> Action Scientific
countLines path = count <$> readList "interpreter.actions.countLines" path
  where
    count ls = read (show $ length ls) :: Scientific

-----------------
-- write files --
-----------------

-- TODO move to Paths?
cachedLinesPath :: Config -> [String] -> FilePath
cachedLinesPath cfg content = cDir </> digest loc content <.> "txt"
  where
    loc = "interpreter.actions.cachedLinesPath"
    cDir = fromPath loc cfg $ cacheDir cfg "lines"

-- TODO move to Util?
first50 :: Show a => a -> String
first50 thing = if length shown > 50 then take 50 shown ++ "..." else shown
  where
    shown = show thing

-- TODO move to Util?
last50 :: Show a => a -> String
last50 thing = if length shown > 50 then "..." ++ reverse (take 50 $ reverse shown) else shown
  where
    shown = show thing

{-|
This ensures that when two lists have the same content, their expression
paths will be links to the same cached path. That causes them to get
properly deduplicated when used in a set operation. It also makes the @.tree@
test files much stricter, since they'll change if any list element changes.
The Maybe is in case you only need to write lists as part of a larger list
and don't want individual outpaths.

TODO switch to md5sum/hashContent?

TODO does it need to handle a race condition when writing to the cache?

TODO any reason to keep original extensions instead of all using .txt?
     oh, if we're testing extensions anywhere. lets not do that though
-}
writeCachedLines :: DebugLocation -> FilePath -> [String] -> Action ()
writeCachedLines loc outPath content = do
  cfg <- fmap fromJust getShakeExtra
  let cache = cachedLinesPath cfg content
  let loc' = loc ++ ".writeCachedLines"
  debugA' loc' $ first50 content ++ " -> " ++ last50 cache
  -- withWriteOnce cache $ do
  writeFile' cache $ unlines content -- TODO writeFile'?
    -- trackWrite' [cache]
  produces [cache]
  symlink (toPath loc' cfg outPath) (toPath loc' cfg cache)

-- like writeCachedLines but starts from a file written by a script
-- TODO remove in favor of anitizeFileInPlace?
writeCachedVersion :: DebugLocation -> FilePath -> FilePath -> Action ()
writeCachedVersion loc outPath inPath = do
  let loc' = loc ++ ".writeCachedVersion"
  content <- lines <$> readFileStrict' inPath
  cfg <- fmap fromJust getShakeExtra
  let content' = map (toGeneric cfg) content
  writeCachedLines loc' outPath content'

-- TODO take a Path for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
-- TODO explicit case for empty lists that isn't just an empty file!
writePaths :: DebugLocation -> FilePath -> [Path] -> Action ()
writePaths loc out cpaths = writeCachedLines loc' out paths -- >> trackWrite paths -- TODO trackwrite'?
  where
    loc' = loc ++ ".writePaths"
    paths = if null cpaths then ["<<emptylist>>"] else map pathString cpaths

writePath :: DebugLocation -> FilePath -> Path -> Action ()
writePath loc out path = do
  let loc' = loc ++ ".writePath"
  debugA' loc' (show path)
  writePaths loc' out [path]

writeLits :: DebugLocation -> FilePath -> [String] -> Action ()
writeLits loc path lits = do
  let loc' = loc ++ ".writeLits"
  debugA' loc' $ show lits ++ " -> writeCachedLines " ++ show lits'
  writeCachedLines loc' path lits'
  where
    lits' = if null lits then ["<<emptylist>>"] else checkLits loc lits

-- TODO any need to prevent writing <<emptystr>> in a .num?
--      (seems almost certain to be caught on reading later)
writeLit :: DebugLocation -> FilePath -> String -> Action ()
writeLit loc path lit = do
  -- debugS (pack "interpreter.actions.writeLit") (pack $ "writeLit lit: \"" ++ lit ++ "\"")
  -- debugS (pack "interpreter.actions.writeLit") (pack $ "writeLit lits: \"" ++ lits ++ "\"")
  debugA' loc' $ show lit ++ " -> writeLits " ++ show lits
  writeLits loc' path lits
  where
    loc' = loc ++ ".writeLit"
    lits = [if null lit then "<<emptystr>>" else lit]

{-|
Write a "list of whatever". Mostly for generic set operations. You include
the Type (of each element, not the list!) so it knows how to convert
to/from String, and then within the function you treat them as Strings.
-}
writeStrings :: DebugLocation -> Type -> FilePath -> [String] -> Action ()
writeStrings loc etype out whatevers = do
  let loc' = loc ++ ".writeStrings"
  debugA' loc' $ first50 (take 3 whatevers) ++ " (each " ++ ext etype ++ ") -> " ++ last50 out
  if etype `elem` [str, num]
    then writeLits loc' out whatevers
    else do
      cfg <- fmap fromJust getShakeExtra
      writePaths loc' out $ map (toPath loc' cfg) whatevers

writeString :: DebugLocation -> Type -> FilePath -> String -> Action ()
writeString loc etype out whatever = writeStrings loc' etype out [whatever]
  where
    loc' = loc ++ ".writeString"

{-|
Turns out there's a race condition during `repeat` calls, because the same
literals are being compiled in each thread at roughly the same time. The way
I solved it was 1) check if the file as written already, and 2) if there's a
conflict in the middle of the operation anyway, ignore the error. Whichever
thread got there first will be writing the same exact text anyway.

TODO rename like myReadFile, myReadLines?

TODO move to Util?
-}
trackWrite' :: [FilePath] -> Action ()
trackWrite' fs = do
  -- mapM_ (assertNonEmptyFile) fs
  -- also ensure it only gets written once:
  -- liftIO $ mapM_ (\f -> setFileMode f 444) fs -- TODO is 444 right? test it
  -- cfg <- fmap fromJust getShakeExtra
  -- liftIO $ mapM_ ((\f -> catchAny f (\_ -> return ())) . setReadOnly cfg) fs -- TODO is 444 right? test it
  lRef <- fmap fromJust getShakeExtra
  liftIO $ mapM_ (markDone lRef) fs
  trackWrite $ traceShow "interpreter.actions.trackWrite'" fs

setReadOnly :: Config -> FilePath -> IO ()
setReadOnly cfg path = do
  path' <- resolveSymlinks (Just [tmpdir cfg]) path
  -- TODO skip if path' outside tmpdir
  setFileMode path' 444

-------------------------
-- run system commands --
-------------------------

{-|
OrthoLang requires explicit empty files with contents like "<<emptylist>>" to
distinguish them from runtime errors. This function replaces those with
actual empty files before passing them to a cmd, so logic for that
doesn't have to be duplicated over and over.

TODO do that only once after a command runs, before tracking it written
TODO and merge with sanitizing the output files?
-}
-- fixEmptyText :: FilePath -> Action FilePath
-- fixEmptyText path = do
--   need' "interpreter.actions.fixEmptyText" [path] -- Note isEmpty does this too
--   empty <- isEmpty path
--   return $ if empty then "/dev/null" else path -- TODO will /dev/null work?

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- | Shake's command_ adapted to work with wrapperScript and wrapperLimit if used.
--   ptns is a list of patterns for files to delete in case the cmd fails.
--   TODO gather shake stuff into a Shake.hs module?
--        could have config, debug, wrappedCmd, eval...
--   TODO separate wrappedReadCmd with a shared lock?
--   TODO multiple out patterns too?
data CmdDesc = CmdDesc
  { cmdBinary        :: String
  , cmdArguments     :: [String] -- TODO auto-include outpath before these?
  , cmdFixEmpties    :: Bool -- TODO version for after too?
  , cmdInPatterns    :: [String]
  , cmdNoNeedDirs    :: [FilePath] -- for dirs to be removed from cmdInPatterns
  , cmdExtraOutPaths :: [FilePath]
  , cmdSanitizePaths :: [FilePath]
  , cmdRmPatterns    :: [String]
  , cmdOptions       :: [CmdOption] -- TODO remove?
  , cmdOutPath       :: FilePath -- TODO Maybe?
  , cmdParallel      :: Bool
  , cmdExitCode      :: ExitCode -- expected exit code (others are errors)
  }
  deriving (Show)

{-|
One wrappedCmd equivalent function to rule them all.
It's controlled by a CmdDesc record instead of a bunch of unnamed positional arguments,
and will help enforce consistency because all the patterns are enforced in one place.

TODO should it track the .out and .err files, or ignore them?

TODO take GlobalEnv instead of individual cfg + locks?

TODO if exit is wrong (usually non-zero), cat out stderr for user

TODO if stdout == outfile, put it there and skip the .out file altogether, or symlink it?
-}

-- TODO how to prevent this re-running on makeblastdb?
runCmd :: CmdDesc -> Action ()
-- runCmd d = withWriteOnce (cmdOutPath d) $ do
runCmd d = do
  -- liftIO $ randomDelay

  cfg <- fmap fromJust getShakeExtra
  let stdoutPath = takeDirectory (cmdOutPath d) </> "out" -- TODO what about when this is the outpath too?
      stderrPath = takeDirectory (cmdOutPath d) </> "err"
      dbg = debugA' "runCmd"
  -- liftIO $ delay 1000000

  dbg $ "running command " ++ show d

  -- TODO is this part causing the extra rebuilds?
  inPaths  <- fmap concat $ liftIO $ mapM globFiles $ cmdInPatterns d
  let inPaths' = toList $ difference (fromList inPaths) (fromList $ cmdNoNeedDirs d)
  -- inPaths' <- if cmdFixEmpties d
  --               then mapM (fixEmptyText) inPaths
  --               else need' "interpreter.actions.runCmd" inPaths >> return inPaths
  need' "interpreter.actions.runCmd" inPaths'

  -- liftIO $ createDirectoryIfMissing True $ takeDirectory stdoutPath
  dbg $ "wrappedCmd acquiring read locks on " ++ show inPaths'
  -- dbg $ pack $ "wrappedCmd cfg: " ++ show cfg
  (lRef :: LocksRef) <- fmap fromJust getShakeExtra
  (disk, par, _) <- liftIO $ readIORef lRef

  -- TODO need to have this whole operation run on the mapped elements too, right?
  let parLockFn = if cmdParallel d
                    then withResource par 1
                    else id

      -- TODO any problem locking the whole dir?
      -- TODO and if not, can the other locks inside that be removed?
      -- writeDir = takeDirectory $ cmdOutPath d
      -- withDirLock fn = if "exprs/" `elem` splitPath writeDir then do
      --                    dbg $ "runCmd acquired dir lock: " ++ writeDir
      --                    withWriteLock' writeDir fn
      --                  else id fn

      -- writeLockFn fn = withDirLock $ do
      writeLockFn fn = do
        -- dbg $ "runCmd acquired expr dir write lock: " ++ show writeDir
        -- withWriteOnce (cmdOutPath d) $ do
        dbg $ "runCmd acquired outpath write lock: " ++ show (cmdOutPath d)
        withWriteLocks' (cmdExtraOutPaths d) $ do
          dbg $ "runCmd acquired extra write locks: " ++ show (cmdExtraOutPaths d)
          parLockFn fn

  path <- fromJust <$> getEnv "PATH"
  let envDirs = Env [("PATH", path), ("TMPDIR", tmpdir cfg), ("WORKDIR", workdir cfg)]

  -- writeLockFn $ withReadLocks' inPaths' $ do
  writeLockFn $ do
    -- TODO remove opts?
    -- TODO always assume disk is 1?
    dbg "runCmd proper starting"
    Exit code <- withResource disk (length inPaths' + 1) $ case wrapper cfg of
      Nothing -> command (envDirs:cmdOptions d) (cmdBinary d) (cmdArguments d)
      Just w  -> command (envDirs:Shell:cmdOptions d) w [escape $ unwords (cmdBinary d:cmdArguments d)]
    -- Exit _ <- command [] "sync" [] -- TODO is this needed?
    -- This is disabled because it can make the logs really big
    -- dbg $ "wrappedCmd: " ++ bin ++ " " ++ show args ++ " -> " ++ show (out, err, code')
    -- return ()

    -- TODO use exitWith here?
    when (code /= cmdExitCode d) $
      let rmPatterns = (takeDirectory (cmdOutPath d) </> "*"):cmdRmPatterns d
      in handleCmdError (cmdBinary d) code stderrPath rmPatterns

    let sPaths = stdoutPath:stderrPath:cmdSanitizePaths d -- TODO main outpath too?
    sanitizeFilesInPlace sPaths
    dbg $ "runCmd sanitized these files: " ++ show sPaths

    -- TODO what about when stderr isn't written?
    let written = nub $ cmdOutPath d:stdoutPath:stderrPath:cmdExtraOutPaths d
    trackWrite' written
    dbg $ "runCmd tracked these written: " ++ show written

  -- return () -- TODO out, err, code here?

-- TODO does this do directories?
-- TODO does this work on absolute paths?
matchPattern :: Config -> String -> Action [FilePath]
matchPattern cfg ptn = liftIO $ globDir1 (compile ptn) (tmpdir cfg)

handleCmdError :: String -> ExitCode -> FilePath -> [String] -> Action a
handleCmdError bin n stderrPath rmPatterns = do
  hasErr <- liftIO $ doesFileExist stderrPath
  errMsg2 <- if hasErr
               then do
                 errTxt <- readFileStrict' stderrPath
                 return ["Stderr was:", errTxt]
               else return []
  cfg <- fmap fromJust getShakeExtra
  files' <- concat <$> mapM (matchPattern cfg) rmPatterns
  let files'' = sort $ nub files'
  -- liftIO $ rmAll files''
  let errMsg = [ bin ++ " failed with " ++ show n ++ "."
               , "The files it was working on have been deleted:"
               ] ++ files''

  -- TODO should this be a different type of failure?
  -- TODO does this get caught by recoverAll in eval? make sure it does!
  -- TODO also try adding a manual flush before each external command in case it's an io delay thing
  error "handleCmdError" $ unlines $ errMsg ++ errMsg2

----------
-- misc --
----------

-- | This is the only function that should access readFileStrict' directly;
--   all others go through readStr and readList. TODO no longer true?
--   TODO use a Path here?
-- digestFile :: FilePath -> Action String
-- digestFile path = readFileStrict' path >>= return . digest

-- TODO fixEmpties should be False here, but don't want to break existing tmpdir just yet
-- TODO take mod time into account to avoid re-hashing (see if Shake exports that code)
hashContent :: Path -> Action String
hashContent path = do
  -- alwaysRerun -- TODO does this help?
  cfg  <- fmap fromJust getShakeExtra
  (lRef :: LocksRef) <- fmap fromJust getShakeExtra
  (disk, _, _) <- liftIO $ readIORef lRef
  let loc = "interpreter.actions.hashContent"
      path' = fromPath loc cfg path
  need' loc [path']
  -- Stdout out <- withReadLock' path' $ command [] "md5sum" [path']
  -- out <- wrappedCmdOut False True [path'] [] [] "md5sum" [path'] -- TODO runCmd here
  Stdout out <- withReadLock' path' $ withResource disk 1 $ case wrapper cfg of
    Nothing -> command [] "md5sum" [path']
    Just w  -> command [Shell] w ["md5sum", path']
  -- liftIO $ putStrLn $ "out: " ++ out
  let md5 = take digestLength out
  -- liftIO $ putStrLn $ "md5: " ++ md5
  return md5

{-|
Hashing doesn't save any space here, but it puts the hashes in
@src\/tests\/plots\/*.tree@ so we can test that the plots don't change.

What it does:

1. make a random temporary path

2. pass that to actFn to make the actual output

3. hash the output to determine cache path

4. symlink hash path -> tmp path, actual outPath -> hash path

TODO remove if ggplot turns out to be nondeterministic

TODO use hash of expr + original ext instead so it looks nicer?
     this would also probably help certain programs recognize their filetypes

TODO remove the need for the expr and just use a random dir
-}
withBinHash :: Show a => a -> Path
            -> (Path -> Action ()) -> Action ()
withBinHash uniq outPath actFn = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.actions.withBinHash"
      binDir'  = fromPath loc cfg $ cacheDir cfg "bin"
      -- outPath' = fromPath loc cfg outPath
  liftIO $ createDirectoryIfMissing True binDir'
  let binTmp' = binDir' </> digest loc uniq -- <.> takeExtension outPath'
      binTmp  = toPath loc cfg binTmp'
  _ <- actFn binTmp
  md5 <- hashContent binTmp
  let binOut' = binDir' </> md5 -- <.> takeExtension outPath'
      binOut  = toPath loc cfg binOut'
  -- debugS $ "withBinHash binOut: "  ++ show binOut
  -- debugS $ "withBinHash binOut': " ++ show binOut'
  debugA' loc $ show binTmp ++ " -> " ++ show binOut
  symlink binOut  binTmp
  symlink outPath binOut

{-|
Takes source and destination paths in the tmpdir and makes a path between
them with the right number of dots.

TODO check that the Path is in TMPDIR, not WORKDIR!
-}
tmpLink :: Config -> FilePath -> FilePath -> FilePath
tmpLink cfg src dst = dots </> tmpRel dst
  where
    tmpRel  = makeRelative $ tmpdir cfg
    dots    = foldr1 (</>) $ replicate (nSeps - 1) ".."
    nSeps   = length $ splitOneOf pathSeparators $ tmpRel src

{-|
Note that src here means what's sometimes called the destination. The first arg
should be the symlink path and the second the file it points to. (it was going
to be kind of confusing either way)
-}
-- TODO prevent duplicate writes here?
symlink :: Path -> Path -> Action ()
symlink src dst = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "interpreter.actions.symlink"
      src' = fromPath loc cfg src
      dst' = fromPath loc cfg dst
      dstr = tmpLink cfg src' dst' -- TODO use cutpaths here too?
  -- TODO why does this break it?
  need' "interpreter.actions.symlink" [dst']
  -- withWriteOnce src' $ do
  liftIO $ createDirectoryIfMissing True $ takeDirectory src'
  liftIO $ ignoreExistsError $ createSymbolicLink dstr src'
  trackWrite' [src']

-- Apply toGeneric to sanitize the output(s) of a script
-- Should be done before trackWrite to avoid confusing Shake
sanitizeFileInPlace :: FilePath -> Action ()
sanitizeFileInPlace path = do
  -- txt <- readFileStrict' path
  exists <- liftIO $ doesFileExist path
  when exists $ do
    txt <- liftIO $ Strict.readFile path -- avoid using need or readFileStrict here for simplicity
    cfg <- fmap fromJust getShakeExtra
    let txt' = toGeneric cfg txt
    -- liftIO $ putStrLn $ "txt': \"" ++ txt' ++ "\""
    writeFile' path txt'
    trackWrite' [path]
    -- writeFile' path $ toGeneric cfg txt
    -- writeCachedLines path []

{-|
Apply toGeneric to sanitize the output(s) of a script.
Should be done before trackWrite to avoid confusing Shake
-}
sanitizeFilesInPlace :: [FilePath] -> Action ()
sanitizeFilesInPlace = mapM_ sanitizeFileInPlace
