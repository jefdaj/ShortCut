module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

-- import Control.Monad       (sequence)
import ShortCut.Core.Types (CutConfig(..), CutModule)
import ShortCut.Core.Util  (mkTestGroup)
import System.IO.Temp      (withSystemTempDirectory)
import Test.Tasty          (TestTree, defaultMain)
import System.Environment  (setEnv)
import Paths_ShortCut      (getDataFileName)
import System.FilePath.Posix ((</>))
import System.Process        (readCreateProcessWithExitCode, shell)
import System.Directory      (setCurrentDirectory)

import qualified ShortCut.Test.Deps    as D
import qualified ShortCut.Test.Parse   as P
import qualified ShortCut.Test.Repl    as R
import qualified ShortCut.Test.Scripts as S

-- TODO reorder these to start with the latest scripts?
mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "all tests"
  [ D.mkTests
  , P.mkTests
  , R.mkTests
  , S.mkTests
  ]

mkTestConfig :: [CutModule] -> FilePath -> CutConfig
mkTestConfig mods dir = CutConfig
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgWorkDir = dir
  , cfgDebug   = False
  , cfgModules = mods
  , cfgWrapper = Nothing -- TODO test this?
  , cfgReport  = Nothing
  }

runTests :: [CutModule] -> IO ()
runTests mods = withSystemTempDirectory "shortcut" $ \td -> do
  wd <- getDataFileName ""
  setCurrentDirectory wd
  -- TODO check exit code?
  (_,_,_) <- readCreateProcessWithExitCode
               (shell $ unwords ["ln -s", wd </> "data", (td </> "data")]) ""
  tests <- mkTests $ mkTestConfig mods td
  setEnv "LANG" "C"
  defaultMain tests
