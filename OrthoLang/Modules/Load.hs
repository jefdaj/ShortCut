module OrthoLang.Modules.Load
  (

  -- * Module with load_list, glob_files
    olModule

  -- * Functions for use in other modules
  , mkLoad
  , mkLoadPath
  , mkLoadEach
  , mkLoadPathEach
  , mkLoadGlob

  )
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import qualified Data.Map.Strict as M

import Control.Monad.IO.Class     (liftIO)
import Data.IORef                 (atomicModifyIORef')
import Data.List                  (sort)
import Data.Maybe                 (fromJust)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>), takeFileName, takeBaseName)
-- import OrthoLang.Modules.Curl     (curl)
import OrthoLang.Util             (absolutize, resolveSymlinks, unlessExists)
import System.FilePath.Glob       (glob)

olModule :: Module
olModule = Module
  { mName = "Load"
  , mDesc = "Load generic lists"
  , mTypes = [str]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =
    [ loadList
    , globFiles
    ]
  }

globFiles :: Function
globFiles = newFnA1 "glob_files" (Exactly str) (Exactly $ ListOf str) aGlobNew [ReadsDirs]

aGlobNew :: NewAction1
aGlobNew (ExprPath out) a1 = do
  let loc = "modules.load.aGlobNew"
  globptn  <- strip <$> readLit loc a1
  relpaths <- liftIO $ sort <$> glob globptn
  writeLits loc out relpaths

-- TODO does it ever read a URL?
mkLoadGlob :: String -> Function -> Function
mkLoadGlob name = compose1 name [ReadsDirs, ReadsFile, ReadsURL] globFiles

{-|
Takes a string with the filepath to load. Creates a trivial expression file
that's just a symlink to the given path. These should be the only absolute
links, and the only ones that point outside the temp dir.
-}

mkLoad :: Bool -> String -> TypeSig -> Function
mkLoad hashSeqIDs name oSig = newFnA1 name (Exactly str) oSig (aLoad hashSeqIDs) [ReadsFile]

-- | For loading directly from a path, as opposed to the normal "read a str,
--   then load that path" indirection. Used in macro expansions for
--   "re-loading" a file after processing it.
mkLoadPath :: Bool -> String -> TypeSig -> Function
mkLoadPath hashSeqIDs name oSig = hidden $ newFnA1 name (Exactly str) oSig (aLoadPath hashSeqIDs) [ReadsFile]

loadList :: Function
loadList = mkLoad False "load_list" (Exactly $ ListOf str)

{-|
Converts user-specified strs (which may represent relative or absolute paths)
to properly formatted OrthoLang Paths. Used to sanitize inputs to the load_* functions
Note the weird name keeps it from conflicting with 'OrthoLang.Interpreter.Paths.toPath'
-}

{-|
Like mkLoad, except it operates on a list of strings. Note that you can also
load lists using mkLoad, but it's not recommended because then you have to write
the list in a file, whereas this can handle literal lists in the source code.
-}

mkLoadEach :: Bool -> String -> TypeSig -> Function
mkLoadEach hashSeqIDs name elemSig = newFnA1 name (Exactly $ ListOf str) (ListSigs elemSig) (aLoadEach hashSeqIDs) [ReadsFile]

mkLoadPathEach :: Bool -> String -> TypeSig -> Function
mkLoadPathEach hashSeqIDs name elemSig = hidden $ newFnA1 name (Exactly $ ListOf str) (ListSigs elemSig) (aLoadListPaths hashSeqIDs) [ReadsFile]

{-|
The paths here are a little confusing: expr is a str of the path we want to
link to. So after compiling it we get a path to *that str*, and have to read
the file to access it. Then we want to `ln` to the file it points to.
-}

-- note: .faa ext turns out to be required for orthofinder to recognize fasta files
aLoadHash :: Bool -> Type -> Path -> Action Path
aLoadHash hashSeqIDs t src = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadHash"
      src' = fromPath loc cfg src
  -- need' loc [src']
  md5 <- hashContent src
  let tmpDir'   = fromPath loc cfg $ cacheDir cfg "load"
      hashPath' = tmpDir' </> md5 <.> ext t
      hashPath  = toPath loc cfg hashPath'
  if not hashSeqIDs
    then symlink hashPath src
    else do
      let idsPath' = hashPath' <.> "ids"
          idsPath  = toPath loc cfg idsPath'
      unlessExists idsPath' $ do
        hashIDsFile False src hashPath -- TODO remove unlessExists?
        aLoadIDs idsPath' -- TODO do it anyway? "reloadseqids" should have already
      dRef <- fmap fromJust getShakeExtra
      liftIO $ addDigest dRef t hashPath
  return hashPath


-- TODO problem when the str is a url? shouldn't `need` it then
-- TODO remove this "url as path" idea and use dated curl fn instead once it works
aLoad :: Bool -> NewAction1
aLoad hashSeqIDs o@(ExprPath out') strPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoad"
      out = toPath loc cfg out'
  dRef <- fmap fromJust getShakeExtra
  t    <- liftIO $ decodeNewRulesType cfg dRef o
  pth'  <- readLit loc strPath'
  let pth = toPath loc cfg pth'
  -- TODO is this the proper place to actually do the download?
  pth'' <- if isURL pth' then undefined pth else return pth -- TODO fix this with new curl!
  hashPath <- aLoadHash hashSeqIDs t pth''
  symlink out hashPath

aLoadPath :: Bool -> NewAction1
aLoadPath hashSeqIDs o@(ExprPath out') loadPath' = do
  alwaysRerun
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadPath"
      out = toPath loc cfg out'
      loadPath = toPath loc cfg loadPath'
  dRef <- fmap fromJust getShakeExtra
  t    <- liftIO $ decodeNewRulesType cfg dRef o
  hashPath <- aLoadHash hashSeqIDs t loadPath
  symlink out hashPath

-- TODO this should be able to be a list of URLs too. demo that!
aLoadEach :: Bool -> NewAction1
aLoadEach hashSeqIDs o lstPath = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  (ListOf et) <- liftIO $ decodeNewRulesType cfg dRef o
  let aFn = if et `elem` [str, num]
              then aLoadListLits
              else aLoadListPaths hashSeqIDs
  aFn o lstPath

aLoadListLits :: NewAction1
aLoadListLits (ExprPath outPath') litsPath' = do
  let loc = "modules.load.aLoadListLits"
      out       = traceA loc outPath' [outPath', litsPath']
  lits  <- readLits loc litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits loc out lits'

aLoadListPaths :: Bool -> NewAction1
aLoadListPaths hashSeqIDs o@(ExprPath out') linksPath' = do
  alwaysRerun
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListPaths"
  dRef <- fmap fromJust getShakeExtra
  (ListOf t) <- liftIO $ decodeNewRulesType cfg dRef o -- TODO does this make sense?
  links  <- readLitPaths loc linksPath'
  paths' <- liftIO $ mapM (resolveSymlinks (Just [tmpdir cfg </> "vars", tmpdir cfg </> "exprs"]) . fromPath loc cfg) links -- TODO remove?
  let paths = map (toPath loc cfg) paths'
  hashPaths <- mapM (aLoadHash hashSeqIDs t) paths
  let hashPaths' = map (fromPath loc cfg) hashPaths
  need' loc hashPaths' -- TODO remove?
  writePaths loc out' hashPaths
