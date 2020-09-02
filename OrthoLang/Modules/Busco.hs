module OrthoLang.Modules.Busco
  where

-- TODO update to BUSCO v4.0.0 (after the rewrite, separately!)
-- TODO add old datasets? maybe no need
-- TODO for rewrite, need to follow the datasets.cfg link into the lineage cache dir?
-- TODO later, also add a function to fetch/load the hmms for use with hmmer? if they're compatible

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.Curl (curl)
import OrthoLang.Modules.Load (mkLoad)

import Control.Monad             (when)
import Data.List                 ((\\), isInfixOf)
import Data.Maybe                (isJust, fromJust)
import Data.Scientific           (Scientific)
import OrthoLang.Modules.BlastDB (aFilterList)
import OrthoLang.Modules.SeqIO   (fna, faa, mkConcat)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           (takeBaseName, takeDirectory, (<.>), (</>))
import System.FilePath.Glob      (glob)

olModule :: Module
olModule = Module
  { mName = "Busco"
  , mDesc = "Benchmarking Universal Single-Copy Orthologs"
  , mTypes = [blh, bsr, bst, faa]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
      [ loadLineage
      , buscoListLineages
      , buscoFetchLineage
      , buscoProteins       , buscoProteinsEach
      , buscoTranscriptome  , buscoTranscriptomeEach
      , buscoPercentComplete, buscoPercentCompleteEach
      , buscoScoresTable
      , buscoFilterCompleteness
      , mkConcat bst -- TODO Each too?
      ]
  }

blh :: Type
blh = Type
  { tExt  = "blh"
  , tDesc = "BUSCO lineage HMMs"
  , tShow = defaultShow
  }

bsr :: Type
bsr = Type
  { tExt  = "bsr"
  , tDesc = "BUSCO results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      let tail9 = unlines . filter (not . null) . reverse . take 9 . reverse . lines
      return $ init $ "BUSCO result:" ++ tail9 txt
  }

bst :: Type
bst = Type
  { tExt  = "bst"
  , tDesc = "BUSCO scores table"
  , tShow = defaultShow
  }

-- TODO when is this used?
loadLineage = mkLoad False "load_lineage" (Exactly blh)

buscoCache :: Config -> Path
buscoCache cfg = cacheDir cfg "busco"

-------------------------
-- busco_list_lineages --
-------------------------

-- TODO wait, this needs to also filter them right?

buscoListLineages :: Function
buscoListLineages = newFnA1
  "busco_list_lineages"
  (Exactly str)
  (Exactly $ ListOf str)
  aBuscoListLineages
  []

-- buscoListLineages = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly str]
--   , fOutput = Exactly (ListOf str)
--   , fTags = [] -- TODO make it read a URL?
--   , fNewRules = NewNotImplemented
--   , fOldRules = rBuscoListLineages
--   }
--   where
--     name = "busco_list_lineages"

-- rBuscoListLineages :: RulesFn
-- rBuscoListLineages scr e@(Fun _ _ _ _ [f]) = do
--   (ExprPath fPath) <- rExpr scr f
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let loc = "modules.busco.rBuscoListLineages"
--       oPath   = exprPath cfg dRef scr e
--       tmpDir  = buscoCache cfg
--       tmpDir' = fromPath loc cfg tmpDir
--       listTmp = tmpDir' </> "dblist" <.> "txt"
--       oPath'  = fromPath loc cfg oPath
--       lTmp'   = toPath loc cfg listTmp
--       fPath' = toPath loc cfg fPath
--   listTmp %> \_ -> aBuscoListLineages lTmp'
--   oPath'  %> \_ -> aFilterList oPath lTmp' fPath'
--   return (ExprPath oPath')
-- rBuscoListLineages _ _ = fail "bad argument to rBuscoListLineages"

aBuscoListLineages :: NewAction1
aBuscoListLineages (ExprPath oPath) fPath = do
  -- cfg <- fmap fromJust getShakeExtra
  -- let listTmp' = fromPath loc cfg listTmp
  -- let tmpDir   = takeDirectory $ listTmp
      -- oPath    = traceA loc listTmp [listTmp]
  -- liftIO $ createDirectoryIfMissing True tmpDir
  let loc = "modules.busco.rBuscoListLineages"
  fStr <- readLit loc fPath
  let matches = filter (fStr `isInfixOf`) allLineages
  writeLits loc oPath matches
  where
    -- These seem static, but may have to be updated later.
    -- The list is generated by "Download all datasets" on the homepage
    allLineages =
      -- Bacteria
      [ "v2/datasets/bacteria_odb9"
      , "v2/datasets/proteobacteria_odb9"
      , "v2/datasets/rhizobiales_odb9"
      , "v2/datasets/betaproteobacteria_odb9"
      , "v2/datasets/gammaproteobacteria_odb9"
      , "v2/datasets/enterobacteriales_odb9"
      , "v2/datasets/deltaepsilonsub_odb9"
      , "v2/datasets/actinobacteria_odb9"
      , "v2/datasets/cyanobacteria_odb9"
      , "v2/datasets/firmicutes_odb9"
      , "v2/datasets/clostridia_odb9"
      , "v2/datasets/lactobacillales_odb9"
      , "v2/datasets/bacillales_odb9"
      , "v2/datasets/bacteroidetes_odb9"
      , "v2/datasets/spirochaetes_odb9"
      , "v2/datasets/tenericutes_odb9"
      -- Eukaryota
      , "v2/datasets/eukaryota_odb9"
      , "v2/datasets/fungi_odb9"
      , "v2/datasets/microsporidia_odb9"
      , "v2/datasets/dikarya_odb9"
      , "v2/datasets/ascomycota_odb9"
      , "v2/datasets/pezizomycotina_odb9"
      , "v2/datasets/eurotiomycetes_odb9"
      , "v2/datasets/sordariomyceta_odb9"
      , "v2/datasets/saccharomyceta_odb9"
      , "v2/datasets/saccharomycetales_odb9"
      , "v2/datasets/basidiomycota_odb9"
      , "v2/datasets/metazoa_odb9"
      , "v2/datasets/nematoda_odb9"
      , "v2/datasets/arthropoda_odb9"
      , "v2/datasets/insecta_odb9"
      , "v2/datasets/endopterygota_odb9"
      , "v2/datasets/hymenoptera_odb9"
      , "v2/datasets/diptera_odb9"
      , "v2/datasets/vertebrata_odb9"
      , "v2/datasets/actinopterygii_odb9"
      , "v2/datasets/tetrapoda_odb9"
      , "v2/datasets/aves_odb9"
      , "v2/datasets/mammalia_odb9"
      , "v2/datasets/euarchontoglires_odb9"
      , "v2/datasets/laurasiatheria_odb9"
      , "v2/datasets/embryophyta_odb9"
      , "v2/datasets/protists_ensembl"
      , "v2/datasets/alveolata_stramenophiles_ensembl"
      -- prerelease
      , "datasets/prerelease/chlorophyta_odb10"
      , "datasets/prerelease/embryophyta_odb10"
      , "datasets/prerelease/eudicotyledons_odb10"
      , "datasets/prerelease/liliopsida_odb10"
      , "datasets/prerelease/solanaceae_odb10"
      , "datasets/prerelease/viridiplantae_odb10"

      ]

------------------------
-- busco_fetch_lineage --
------------------------

-- TODO consistent naming with similar functions
-- TODO busco_fetch_lineages? (the _each version)

buscoFetchLineage :: Function
buscoFetchLineage  = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly str]
  , fOutput = Exactly blh
  , fTags = [ReadsURL]
  , fNewRules = NewNotImplemented
  , fOldRules = rBuscoFetchLineage
  }
  where
    name = "busco_fetch_lineage"

-- TODO move to Util?
untar :: Path -> Path -> Action ()
untar from to = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.busco.untar"
      from' = fromPath loc cfg from
      to' = fromPath loc cfg to
  liftIO $ createDirectoryIfMissing True to'
  runCmd $ CmdDesc
    { cmdBinary = "tar"
    , cmdArguments = ["-xf", from', "-C", takeDirectory to']
    , cmdFixEmpties = False
    , cmdParallel   = False
    , cmdInPatterns = [from']
    , cmdNoNeedDirs = []
    , cmdOutPath    = to'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [to']
    }

rBuscoFetchLineage :: RulesFn
rBuscoFetchLineage scr expr@(Fun _ _ _ _ [nPath]) = do
  (ExprPath namePath) <- rExpr scr nPath
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let outPath  = exprPath cfg dRef scr expr
      outPath' = fromPath loc cfg outPath
      blhDir   = (fromPath loc cfg $ buscoCache cfg) </> "lineages"
      loc = "modules.busco.rBuscoFetchLineage"
  outPath' %> \_ -> do
    nameStr <- readLit loc namePath
    let untarPath = blhDir </> nameStr
        url       = toPath loc cfg $ "http://busco.ezlab.org/" ++ nameStr ++ ".tar.gz"
        datasetPath'  = untarPath </> "dataset.cfg" -- final output we link to
        datasetPath   = toPath loc cfg datasetPath'
    tarPath <- fmap (fromPath loc cfg) $ curl url
    unlessExists untarPath $ do
      untar (toPath loc cfg tarPath) (toPath loc cfg untarPath)
    symlink outPath datasetPath
  return $ ExprPath outPath'
rBuscoFetchLineage _ e = error $ "bad argument to rBuscoFetchLineage: " ++ show e

-------------------------------------------
-- busco_{genome,proteins,transcriptome} --
-------------------------------------------

mkBusco :: String -> String -> Type -> Function
mkBusco name mode inType = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly blh, Exactly inType]
  , fOutput = Exactly bsr
  , fTags = [Nondeterministic] -- TODO double check nondeterminism
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aBusco mode
  }

buscoProteins, buscoTranscriptome :: Function
buscoProteins      = mkBusco "busco_proteins"      "prot" faa
buscoTranscriptome = mkBusco "busco_transcriptome" "tran" fna
-- buscoGenome = mkBusco "busco_genome" "geno"

-- TODO looks like you have to follow the final result symlink to cd into the dir with dataset.cfg?
aBusco :: String -> ([Path] -> Action ())
aBusco mode [outPath, blhPath, faaPath] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.busco.aBusco"
      out' = fromPath loc cfg outPath
      blh' = takeDirectory $ fromPath loc cfg blhPath
      cDir = fromPath loc cfg $ buscoCache cfg
      rDir = cDir </> "runs" -- TODO need digest here?
      faa' = fromPath loc cfg faaPath
  blh'' <- liftIO $ resolveSymlinks (Just [tmpdir cfg]) blh'
  liftIO $ createDirectoryIfMissing True rDir
  runCmd $ CmdDesc
    { cmdBinary = "busco.sh"
    , cmdArguments = [out', faa', blh'', mode, cDir] -- TODO cfgtemplate, tdir
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO fix shake error and set to True
    , cmdInPatterns = [faa']
    , cmdNoNeedDirs = []
    , cmdOutPath = out'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
  -- This is rediculous but I haven't been able to shorten it...
  let oBasePtn = "*" ++ takeBaseName out' ++ "*"
      tmpOutPtn = rDir </> oBasePtn </> "short_summary*.txt"
  tmpOut <- liftIO $ fmap (headOrDie "failed to read BUSCO summary in aBusco") $ glob tmpOutPtn
  sanitizeFileInPlace tmpOut -- will this confuse shake?
  symlink outPath $ toPath loc cfg tmpOut
aBusco _ as = error $ "bad argument to aBusco: " ++ show as

------------------------------------------------
-- busco_{genome,proteins,transcriptome}_each --
------------------------------------------------

mkBuscoEach :: String -> String -> Type -> Function
mkBuscoEach name mode inType = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly blh, Exactly (ListOf inType)]
  , fOutput = Exactly (ListOf bsr)
  , fTags = [Nondeterministic] -- TODO double check nondeterminism
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 2 $ aBusco mode
  }

buscoProteinsEach, buscoTranscriptomeEach :: Function
buscoProteinsEach      = mkBuscoEach "busco_proteins_each"      "prot" faa
buscoTranscriptomeEach = mkBuscoEach "busco_transcriptome_each" "tran" fna
-- buscoGenomeEach = mkBusco "busco_genome_each" "geno"

-----------------------------
-- busco_percent_complete* --
-----------------------------

buscoPercentComplete :: Function
buscoPercentComplete  = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly bsr]
  , fOutput = Exactly num
  , fTags = [Nondeterministic] -- TODO double check nondeterminism
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleScript "busco_percent_complete.sh"
  }
  where
    name = "busco_percent_complete"

buscoPercentCompleteEach :: Function
buscoPercentCompleteEach  = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf bsr)]
  , fOutput =  Exactly (ListOf num)
  , fTags = [Nondeterministic] -- TODO double check nondeterminism
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "busco_percent_complete.sh"
  }
  where
    name = "busco_percent_complete_each"

------------------------
-- busco_scores_table --
------------------------

buscoScoresTable :: Function
buscoScoresTable  = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf bsr)]
  , fOutput = Exactly bst
  , fTags = [] -- TODO not deterministic, right?
  -- , fNewRules = NewNotImplemented, fOldRules = rSimpleScript $ name <.> "py"
  , fNewRules = NewNotImplemented
  , fOldRules = rBuscoScoresTable
  }
  where
    name = "busco_scores_table"

-- TODO variant of rSimpleScript that reads + passes in a list of input files?
rBuscoScoresTable :: RulesFn
rBuscoScoresTable scr e@(Fun _ _ _ _ [l]) = do
  (ExprPath lsPath) <- rExpr scr l
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let o  = exprPath cfg dRef scr e
      o' = fromPath loc cfg o
      loc = "modules.busco.rBuscoScoresTable"
  o' %> \_ -> do
    ins <- readPaths loc lsPath
    let ins' = map (fromPath loc cfg) ins
    runCmd $ CmdDesc
      { cmdBinary = "busco_scores_table.py"
      , cmdArguments = o':ins'
      , cmdFixEmpties = False
      , cmdParallel   = False
      , cmdInPatterns = ins'
      , cmdNoNeedDirs = []
      , cmdOutPath    = o'
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = [] -- TODO any?
      , cmdOptions = []
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [o']
      }
  return $ ExprPath o'
rBuscoScoresTable _ e = error $ "bad argument to rBuscoScoresTable: " ++ show e

-------------------------------
-- busco_filter_completeness --
-------------------------------

-- TODO this can filter proteomes/transcriptomes by which their completeness in a table
--      bst should it take the table as an explicit arg, or generate it from the inputs?
--      explicit is probably better! abort with error if the table doesn't contain all of them
-- TODO remove busco_percent_complete* afterward since the table will be more useful?
-- TODO make an _each version of this one

buscoFilterCompleteness :: Function
buscoFilterCompleteness  = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Exactly bst, Exactly (ListOf faa)]
  , fOutput = Exactly (ListOf faa)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rBuscoFilterCompleteness
  }
  where
    name = "busco_filter_completeness"

-- TODO how to get the hash? resolveSymlinks and read it from the filename?
--      that might fail if it was generated by a fn instead of loaded from an external file
--      maybe the solution is to add generated fastas to cached lines?
-- TODO try the same way it works for sets: one canonical full path!
-- TODO do it the simple way for now, then see if it breaks and if so fix it
rBuscoFilterCompleteness :: RulesFn
rBuscoFilterCompleteness scr e@(Fun _ _ _ _ [m, t, fs]) = do
  (ExprPath scorePath) <- rExpr scr m
  (ExprPath tablePath) <- rExpr scr t
  (ExprPath faasList ) <- rExpr scr fs
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out  = exprPath cfg dRef scr e
      out' = fromPath loc cfg out
      loc = "modules.busco.rBuscoFilterCompleteness"
  out' %> \_ -> do
    score <- fmap (read :: String -> Scientific) $ readLit loc scorePath
    table <- readFileStrict' tablePath -- TODO best read fn?
    faaPaths <- readPaths loc faasList
    let allScores = map parseWords $ map words $ lines table
        missing   = faaPaths \\ map fst allScores
        okPaths   = map fst $ filter (\(_, c) -> c >= score) allScores
    when (not $ null missing) $
      error $ "these paths are missing from the table: " ++ show missing
    writePaths loc out' okPaths
  return $ ExprPath out'
  where
    parseWords (p:c:_) = (Path p, read c :: Scientific)
    parseWords ws = error $ "bad argument to parseWords: " ++ show ws
rBuscoFilterCompleteness _ e = error $
  "bad argument to rBuscoFilterCompleteness: " ++ show e
