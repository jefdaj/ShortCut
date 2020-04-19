{-# LANGUAGE OverloadedStrings #-}

-- TODO show cfgShare

module OrthoLang.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import OrthoLang.Debug (debug)
import OrthoLang.Types (Config(..), Module(..))
import OrthoLang.Util  (absolutize, justOrDie)

import Control.Logging            (LogLevel(..), setLogLevel, setDebugSourceRegex)
import Control.Monad              (when)
import Data.List                  (isPrefixOf)
import Data.Maybe                 (isNothing)
import Data.Text                  (pack)
import Development.Shake          (newResourceIO)
import GHC.Conc                   (getNumProcessors)
import Paths_OrthoLang            (getDataFileName)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent, longOption, getAllArgs)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import System.Directory           (doesFileExist)
import System.FilePath            ((</>), (<.>))
import System.Info                (os)
import Text.Read.HT               (maybeRead)

{- The logging module keeps its own state in an IORef, so no need to include
 - this in the main OrthoLang config below.
 - TODO still put all the config stuff in Config though, and make it changable in the repl!
 -}
dispatch :: Arguments -> String -> IO () -> IO ()
dispatch args arg act = when (isPresent args $ longOption arg) $ do
  debug' $ "handling --" ++ arg
  act

{- The base debugging function used in other modules too. This is admittedly a
 - weird place to put it, but makes everything much easier as far as avoiding
 - import cycles.
 -
 - TODO remove this and rewrite with logging module
 -}
debug' :: String -> IO ()
debug' = debug "config"

loadField :: Arguments -> C.Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

defaultConfig :: FilePath -> FilePath -> IO Config
defaultConfig td wd = do
  par <- newResourceIO "parallel" 8 -- TODO set to number of nodes
  os' <- getOS
  cp <- getNumProcessors
  return Config
    { cfgScript      = Nothing
    , cfgInteractive = False
    , cfgTmpDir      = td
    , cfgWorkDir     = wd
    , cfgDebug       = Nothing
    , cfgModules     = [] -- TODO fix this
    , cfgWrapper     = Nothing
    , cfgOutFile     = Nothing
    , cfgShare       = Nothing
    , cfgReport      = Nothing
    , cfgTestPtn     = [] -- [] means run all tests
    , cfgWidth       = Nothing
    , cfgSecure      = False
    , cfgNoProg      = True
    , cfgParLock     = par
    , cfgOS          = os'
    , cfgThreads     = cp
    , cfgDevMode     = False
    }

loadConfig :: [Module] -> Arguments -> IO Config
loadConfig mods args = do
  debug' $ "docopt arguments: " ++ show args
  let path = justOrDie "parse --config arg failed!" $ getArg args $ longOption "config"
  cfg <- C.load [C.Optional path]
  csc <- loadField args cfg "script"
  csc' <- case csc of
            Nothing -> return Nothing
            Just s  -> absolutize s >>= return . Just
  dbg <- loadField args cfg "debug"
  ctd <- mapM absolutize =<< loadField args cfg "tmpdir"
  cwd <- mapM absolutize =<< loadField args cfg "workdir"
  let ctd' = justOrDie "parse --tmpdir arg failed!" ctd
      cwd' = justOrDie "parse --workdir arg failed!" cwd
  def <- defaultConfig ctd' cwd'
  rep <- mapM absolutize =<< loadField args cfg "report"
  cls <- mapM absolutize =<< loadField args cfg "wrapper"
  out <- mapM absolutize =<< loadField args cfg "output"
  shr <- mapM (\p -> if "http" `isPrefixOf` p then return p else absolutize p) =<< loadField args cfg "shared"
  let ctp = getAllArgs args (longOption "test")
  let int = isNothing csc' || (isPresent args $ longOption "interactive")
  let res = def
              { cfgScript  = csc'
              , cfgInteractive = int
              , cfgDebug   = dbg
              , cfgModules = mods
              , cfgWrapper = cls
              , cfgReport  = rep
              , cfgTestPtn = ctp
              , cfgWidth   = Nothing -- not used except in testing
              , cfgSecure  = isPresent args $ longOption "secure"
              , cfgNoProg  = isPresent args $ longOption "noprogress"
              , cfgOutFile = out
              , cfgShare   = shr
              }
  debug' $ show res
  updateDebug dbg
  return res

getOS :: IO String
getOS = return $ if os == "darwin" then "mac" else os

-- TODO any way to recover if missing? probably not
-- TODO use a safe read function with locks here?
getDoc :: [FilePath] -> IO String -- TODO IO (Maybe String)?
getDoc docPaths = do
  paths' <- mapM (\p -> getDataFileName ("docs" </> p <.> "txt") >>= absolutize) $ docPaths
  tests <- mapM doesFileExist paths'
  -- let path' = listToMaybe [p | (p, t) <- zip paths' tests, t] -- TODO remove head?
  let path' = head [p | (p, t) <- zip paths' tests, t] -- TODO remove head?
  -- putStrLn $ "path':" ++ path'
  -- this should only happen during development:
  -- written <- doesFileExist path'
  -- when (not written) $ writeFile path' $ "write " ++ docPath ++ " doc here"
  -- mapM readFile path'
  doc <- readFile path'
  return doc

getUsage :: IO Docopt
-- getUsage = getDoc ["usage"] >>= parseUsageOrExit . fromJust
getUsage = getDoc ["usage"] >>= parseUsageOrExit

-- hasArg :: Arguments -> String -> Bool
-- hasArg as a = isPresent as $ longOption a

-------------------------
-- getters and setters --
-------------------------

{- These are done the simple, repetitive way for now to avoid lenses.  That
 - might change in the future though, because turns out getters and setters are
 - horrible!
 -
 - Note that cfgSecure is purposely not avialable here.
 -}

-- This is mainly for use in the REPL so no need to return usable data
-- TODO just show the whole config; no need for fields
-- TODO oh and there's a display function for that!
-- showConfigField :: Config -> String -> String
-- showConfigField cfg key = case lookup key fields of
--   Nothing -> "no such config setting: " ++ key
--   Just (getter, _) -> getter cfg

setConfigField :: Config -> String -> String -> Either String (IO Config)
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just setter -> setter cfg val

-- TODO add modules? maybe not much need
-- TODO add interactive?
-- TODO these show* functions could be Pretty instances, or just directly showable
-- TODO remove anything that can't be shown
-- TODO remove show functions and show directly (possibly using Configurator.display)
fields :: [(String, (Config -> String -> Either String (IO Config)))]
fields =
  [ ("script" , setScript )
  , ("tmpdir" , setTmpdir )
  , ("workdir", setWorkdir)
  , ("debug"  , setDebug  )
  , ("wrapper", setWrapper)
  , ("report" , setReport )
  , ("width"  , setWidth  )
  , ("output" , setOutFile)
  , ("threads", setThreads)
  -- TODO add share?
  ]

-- showConfig :: Config -> String
-- showConfig cfg = unlines $ map showField fields
--   where
--     showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: Config -> String -> Either String (IO Config)
setDebug cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ do
    updateDebug $ Just v
    return $ cfg { cfgDebug = Just v }

-- this is run during setDebug, and once in loadConfig
updateDebug :: Maybe String -> IO ()
updateDebug regex = case regex of
  Nothing -> do
    debug' "turning off debugging"
    setLogLevel LevelWarn
  Just r -> do
    setLogLevel LevelDebug
    setDebugSourceRegex r
    debug' $ "set debug regex to " ++ show regex

-- TODO this seems ok, it just needs a generic "set everything" function
--      it's basically all files (strings) right? oh and booleans

setScript :: Config -> String -> Either String (IO Config)
setScript cfg "Nothing" = Right $ return $ cfg { cfgScript = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgScript = Just v }

setTmpdir :: Config -> String -> Either String (IO Config)
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgTmpDir = v }

setWorkdir :: Config -> String -> Either String (IO Config)
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWorkDir = v }

setWrapper :: Config -> String -> Either String (IO Config)
setWrapper cfg "Nothing" = Right $ return $ cfg { cfgWrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWrapper = Just v }

setReport :: Config -> String -> Either String (IO Config)
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ return $ cfg { cfgReport = v }

setThreads :: Config -> String -> Either String (IO Config)
setThreads cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgThreads = v }

setWidth :: Config -> String -> Either String (IO Config)
setWidth cfg "Nothing" = Right $ return $ cfg { cfgWidth = Nothing }
setWidth cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just n  -> Right $ return $ cfg { cfgWidth = Just n }

setOutFile :: Config -> String -> Either String (IO Config)
setOutFile cfg "Nothing" = Right $ return $ cfg { cfgOutFile = Nothing }
setOutFile cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgOutFile = Just v }
