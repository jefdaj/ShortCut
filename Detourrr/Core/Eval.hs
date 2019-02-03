{-# LANGUAGE FlexibleContexts #-}

{- Detourrr code is interpreted in phases, but client code shouldn't need to
 - care about that, so this module wraps them in a simplified interface. It
 - just holds whatever [i]nterpret functions the Repl and Detourrr modules use
 - for now rather than any comprehensive API.
 -}

-- TODO ghc --make MyBuildSystem -rtsopts -with-rtsopts=-I0
-- TODO -j (is that done already?)
-- TODO --flush=N

-- TODO should there be an iLine function that tries both expr and assign?
-- TODO create Eval.hs again and move [e]val functions there? might be clearer
--      but then again interpret and eval are kind of the same thing here right?
--      it's either interpret or compile + eval

module Detourrr.Core.Eval
  ( eval
  , evalFile
  , evalScript
  )
  where

import Development.Shake
import Text.PrettyPrint.HughesPJClass

import Detourrr.Core.Types
import Detourrr.Core.Pretty (renderIO)
import Detourrr.Core.Config (debug)

import Control.Retry
-- import qualified Data.Map as M

import Control.Exception.Enclosed     (catchAny)
import Data.Maybe                     (maybeToList)
import Detourrr.Core.Compile.Basic    (compileScript)
import Detourrr.Core.Parse            (parseFileIO)
import Detourrr.Core.Pretty           (prettyNum)
import Detourrr.Core.Paths            (RrrPath, toRrrPath, fromRrrPath)
-- import Detourrr.Core.Locks            (withReadLock')
import Detourrr.Core.Sanitize         (unhashIDs)
import Detourrr.Core.Actions          (readLits, readPaths)
import System.IO                      (Handle, hPutStrLn)
import System.FilePath                ((</>))
import Data.IORef                     (readIORef)

-- TODO use hashes + dates to decide which files to regenerate?
-- alternatives tells Shake to drop duplicate rules instead of throwing an error
myShake :: RrrConfig -> Rules () -> IO ()
myShake cfg rules = do
  (shake myOpts . alternatives) rules
  -- removeIfExists $ cfgTmpDir cfg </> ".shake.lock"
  where
    myOpts = shakeOptions
      { shakeFiles     = cfgTmpDir cfg
      , shakeVerbosity = if cfgDebug cfg then Chatty else Quiet
      , shakeThreads   = 0 -- TODO make customizable? increase?
      , shakeReport    = [cfgTmpDir cfg </> "profile.html"] ++ maybeToList (cfgReport cfg)
      , shakeAbbreviations = [(cfgTmpDir cfg, "$TMPDIR"), (cfgWorkDir cfg, "$WORKDIR")]
      -- , shakeChange    = ChangeModtimeAndDigest -- TODO test this
      -- , shakeCommandOptions = [EchoStdout True]
      -- , shakeProgress = progressSimple
      -- , shakeLineBuffering = False
      }

{- This seems to be separately required to show the final result of eval.
 - It can't be moved to Pretty.hs either because that causes an import cycle.
 -
 - TODO is there a way to get rid of it?
 - TODO rename prettyContents? prettyResult?
 - TODO should this actually open external programs
 - TODO idea for sets: if any element contains "\n", just add blank lines between them
 - TODO clean this up!
 -}
prettyResult :: RrrConfig -> Locks -> RrrType -> RrrPath -> Action Doc
prettyResult _ _ Empty  _ = return $ text "[]"
prettyResult cfg ref (ListOf t) f
  | t `elem` [str, num] = do
    lits <- readLits cfg ref $ fromRrrPath cfg f
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map prettyNum lits
    return $ text "[" <> sep ((punctuate (text ",") lits')) <> text "]"
  | otherwise = do
    paths <- readPaths cfg ref $ fromRrrPath cfg f
    pretties <- mapM (prettyResult cfg ref t) paths
    return $ text "[" <> sep ((punctuate (text ",") pretties)) <> text "]"
prettyResult cfg ref (ScoresOf _)  f = do
  s <- liftIO (defaultShow cfg ref $ fromRrrPath cfg f)
  return $ text s
prettyResult cfg ref t f = liftIO $ fmap showFn $ (tShow t cfg ref) f'
  where
    showFn = if t == num then prettyNum else text
    f' = fromRrrPath cfg f

-- run the result of any of the c* functions, and print it
-- (only compileScript is actually useful outside testing though)
-- TODO rename `runRules` or `runShake`?
-- TODO require a return type just for showing the result?
-- TODO take a variable instead?
-- TODO add a top-level retry here? seems like it would solve the read issues
eval :: Handle -> RrrConfig -> Locks -> HashedSeqIDsRef -> RrrType -> Rules ResPath -> IO ()

-- TODO put this back once done debugging (duplicates everything annoyingly)
-- eval hdl cfg ref rtype = retryIgnore . eval'

eval hdl cfg ref ids rtype = retryIgnore . eval'
  where
    -- This isn't as bad as it sounds. It just prints an error message instead
    -- of crashing the rest of the program but the error will be visible.
    ignoreErrors fn = catchAny fn (\e -> putStrLn $ "error! " ++ show e)

    -- This one is as bad as it sounds, so remove it when able! It's the only
    -- way I've managed to solve the occasional "openFile" lock conflicts.
    -- TODO at least log when a retry happens for debugging
    -- TODO ask Niel if individual actions can be retried instead
    -- TODO could always fork Shake to put it in if needed too
    retryIgnore fn = ignoreErrors $ recoverAll (limitRetries 4) $ report fn

    -- Reports how many failures so far and runs the main fn normally
    -- TODO debug rather than putStrLn?
    report fn status = case rsIterNumber status of
      0 -> fn
      n -> debug cfg ("error! eval failed " ++ show n ++ " times") fn

    eval' rpath = myShake cfg $ do
      (ResPath path) <- rpath
      want ["eval"]
      "eval" ~> do
        alwaysRerun
        need [path] -- TODO is this done automatically in the case of result?
        res  <- prettyResult cfg ref rtype $ toRrrPath cfg path
        ids' <- liftIO $ readIORef ids
        -- liftIO $ putStrLn $ show ids'
        -- liftIO $ putStrLn $ "rendering with unhashIDs (" ++ show (length $ M.keys ids') ++ " keys)..."

        -- TODO fix the bug that causes this to remove newlines after seqids:
        res' <- fmap (unhashIDs cfg ids') $ liftIO $ renderIO cfg res -- TODO why doesn't this handle a str.list?

        liftIO $ hPutStrLn hdl res'
        -- liftIO $ putStrLn $ "done rendering with unhashIDs"

-- TODO get the type of result and pass to eval
evalScript :: Handle -> RrrState -> IO ()
evalScript hdl s@(as, c, ref, ids) = case lookup (RrrVar "result") as of
  Nothing  -> putStrLn "no result variable. that's not right!"
  Just res -> eval hdl c ref ids (typeOf res) (compileScript s Nothing)

evalFile :: Handle -> RrrConfig -> Locks -> HashedSeqIDsRef -> IO ()
evalFile hdl cfg ref ids = case cfgScript cfg of
  Nothing  -> putStrLn "no script. that's not right!"
  Just scr -> do
    s <- parseFileIO cfg ref ids scr -- TODO just take a RrrState?
    evalScript hdl (s, cfg, ref, ids)