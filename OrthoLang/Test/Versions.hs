-- Simple Golden tests to confirm versionScripts are found.
-- TODO should it be more flexible about the exact versions?

module OrthoLang.Test.Versions where

import OrthoLang.Debug
import OrthoLang.Types
import Paths_OrthoLang             (getDataFileName)

import Development.Shake.FilePath ((<.>), (</>))
import System.Process             (shell, readCreateProcessWithExitCode)
import Test.Tasty                 (TestTree, TestName, testGroup)
import Test.Tasty.Golden          (goldenVsString)
import qualified Data.ByteString.Lazy.Char8 as C8
import System.FilePath.Posix      (takeBaseName)
import OrthoLang.Interpreter.Config (os)

-- TODO blastdbget makeblastdb cut blast tar hmmsearch orthogroups.py?
--      greencut psiblast? seqiostuff sonicparanoid
versionScripts :: [(String, FilePath)]
versionScripts = map (\(a,b) -> (a, os ++ ":" ++ b))
  [ ("bash"         , "bash.sh")
  , ("curl"         , "curl.sh")
  , ("cut"          , "cut.sh")
  , ("tar"          , "tar.sh")
  , ("sed"          , "sed.sh")
  , ("tree"         , "tree.sh") -- TODO where should this go?
  , ("awk"          , "awk.sh") -- TODO where should this go?
  , ("file"         , "file.sh") -- TODO where should this go?
  , ("zip"          , "zip.sh")

  , ("blast"        , "blast.sh")
  , ("busco"        , "busco.sh")
  , ("crbblast"     , "crbblast.sh")
  , ("diamond"      , "diamond.sh")
  , ("hmmer"        , "hmmer.sh")
  , ("makeblastdb"  , "makeblastdb.sh")
  -- , ("mmseqs"       , "mmseqs.sh")
  , ("muscle"       , "muscle.sh")
  , ("orthofinder"  , "orthofinder.sh")
  , ("psiblast"     , "psiblast.sh") -- TODO remove?

  , ("python2"      , "python2.sh")
  , ("py2_numpy"    , "py2_numpy.py")
  , ("py2_scipy"    , "py2_scipy.py")
  , ("py2_BioPython", "py2_BioPython.py")

  , ("python3"      , "python3.sh")
  , ("py3_numpy"    , "py3_numpy.py")
  , ("py3_scipy"    , "py3_scipy.py")
  , ("py3_BioPython", "py3_BioPython.py")

  , ("r"            , "r.sh")
  , ("biomartr"     , "r_biomartr.R")
  , ("dplyr"        , "r_dplyr.R")
  , ("data.table"   , "r_data_table.R")
  , ("futile.logger", "r_futile_logger.R")
  , ("ggplot2"      , "r_ggplot2.R")
  , ("readr"        , "r_readr.R")
  , ("tidyr"        , "r_tidyr.R")
  , ("UpSetR"       , "r_UpSetR.R")
  , ("VennDiagram"  , "r_VennDiagram.R")

  -- , ("sonicparanoid", "sonicparanoid.sh")
  -- , ("treecl"       , "treecl.sh")
  ]

-- Unlike the other tests, these don't need access to the runtime config
mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg _ _ _ = do
  testDir <- getDataFileName "tests"
  debug "test.versions" $ "test dir is " ++ testDir
  return $ testGroup "check dependency versions"
         $ map (mkTestVersion cfg testDir) versionScripts

mkTestVersion :: Config -> FilePath -> (TestName, String) -> TestTree
mkTestVersion _ dir (name, cmd) = goldenVsString d gld act
  where
    d = "found expected version of " ++ name
    gld = dir </> "versions" </> takeBaseName cmd <.> "txt"
    msg = "tested " ++ name ++ " " ++ os ++ " version"
    act = do
      (_, out, err) <- time "test.versions" msg $ readCreateProcessWithExitCode (shell cmd) ""
      -- helpful for updating tests
      -- writeFile ("/tmp" </> takeBaseName gld <.> "txt") $ toGeneric cfg $ err ++ out
      return $ C8.pack $ err ++ out
