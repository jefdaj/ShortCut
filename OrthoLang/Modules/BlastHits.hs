module OrthoLang.Modules.BlastHits where

-- TODO are crb files *exactly* in blast format? if so, no need for separate types
-- TODO rename all extract_ functions to not have the extract part? or replace it with list_?

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Modules.Blast    (bht)
import OrthoLang.Modules.Load     (mkLoad, mkLoadPath, mkLoadEach, mkLoadPathEach, mkLoadGlob)
import OrthoLang.Modules.CRBBlast (crb)
import System.Exit                (ExitCode(..))
import System.FilePath            ((</>), takeDirectory)
import Data.Maybe (fromJust)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "BlastHits"
  , mDesc = "Work with BLAST hit tables"
  , mTypes = [bht, crb]
  , mGroups = [ht]
  , mEncodings = []
  , mRules = []
  , mFunctions =
    [ loadBht, loadBhtPath, loadBhtEach, loadBhtPathEach, loadBhtGlob
    , extractQueries, extractQueriesEach
    , extractTargets, extractTargetsEach

    -- filter hit tables by cutoff
    , mkFilterHits "evalue"  , mkFilterHitsEach "evalue"
    , mkFilterHits "bitscore", mkFilterHitsEach "bitscore"
    , mkFilterHits "pident"  , mkFilterHitsEach "pident"

    -- filter hit tables by best e-value per gene
    -- TODO mkFilterHits "rawscore", mkFilterHitsEach "rawscore"
    , bestHits, bestHitsEach

    ]
  }

-- TODO use this more!
ht :: TypeGroup
ht = TypeGroup
  { tgExt = "ht"
  , tgDesc  = "hit table"
  , tgMembers = [Exactly bht, Exactly crb] -- TODO mms too
  }


---------------
-- load_bht* --
---------------

-- this smells weird, but the whole group is needed
-- TODO use ht instead of bht?
-- TODO separate "_rawids" versions for when the table is already from ortholang?
loadBht         = mkLoad         True "load_bht"           (Exactly bht)
loadBhtPath     = mkLoadPath     True "load_bht_path"      (Exactly bht)
loadBhtEach     = mkLoadEach     True "load_bht_each"      (Exactly bht)
loadBhtPathEach = mkLoadPathEach True "load_bht_path_each" (Exactly bht)
loadBhtGlob     = mkLoadGlob          "load_bht_glob"       loadBhtEach


----------------------
-- extract_*(_each) --
----------------------

mkExtractCol :: String -> Bool -> Int -> Function
mkExtractCol colname b colnum = newFnA1
  ("extract_" ++ colname)
  (Some ht "a hit table")
  (Exactly $ ListOf str)
  (aCutCol b colnum)
  []

extractQueries :: Function
extractQueries = mkExtractCol "queries" True 1

extractTargets :: Function
extractTargets = mkExtractCol "targets" True 2

mkExtractColEach :: String -> Function
mkExtractColEach colname =
  let name = "extract_" ++ colname
  in newFnA1
       (name ++ "_each")
       (ListSigs $ Some ht "a hit table")
       (Exactly $ ListOf $ ListOf str)
       (newMap1of1 name)
       []

extractQueriesEach :: Function
extractQueriesEach = mkExtractColEach "queries"

extractTargetsEach :: Function
extractTargetsEach = mkExtractColEach "targets"

-- TODO remove uniq, unless it's used somewhere?
aCutCol :: Bool -> Int -> NewAction1
aCutCol _ n (ExprPath outPath') tsvPath' = do
  let loc = "modules.blasthits.aCutCol"
      outPath'' = traceA loc outPath' [show n, outPath', tsvPath']
      tmpPath'  = takeDirectory outPath'' </> "tmp" -- the non-deduped version
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = tmpPath'
    , cmdInPatterns = [tsvPath']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "cut_tsv.sh"
    , cmdArguments = [tmpPath', tsvPath', show n]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath']
    }
  trackWrite' [tmpPath']
  writeCachedVersion loc outPath'' tmpPath'

  -- TODO remove this? why does it need to be here at all?
  -- let outOut = outPath' <.> "out"
  -- unlessExists outOut $ do
  --   symlink outPath $ toPath loc cfg outOut

---------------------
-- filter_*(_each) --
---------------------

mkFilterHits :: String -> Function
mkFilterHits colname = newFnA2
  ("filter_" ++ colname)
  (Exactly num, Some ht "a hit table")
  (Some ht "a hit table")
  (aFilterHits colname)
  []

mkFilterHitsEach :: String -> Function
mkFilterHitsEach colname =
  let name = "filter_" ++ colname
  in newFnA2
      (name ++ "_each")
      (Exactly num, ListSigs $ Some ht "a hit table")
      (ListSigs $ Some ht "a hit table")
      (newMap2of2 name)
      []

aFilterHits :: String -> NewAction2
aFilterHits colname (ExprPath out') cutoff' hits' = do
  let loc = "modules.blasthits.aFilterHits"
      out''   = traceA "aFilterHits" out' [out', cutoff', hits']
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [cutoff', hits']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "filter_hits.R"
    , cmdArguments = [out', colname, cutoff', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out'']
    }

----------------
-- best_hits* --
----------------

-- TODO move to BlastRBH?
-- TODO should this return whatever hittable type it's given?
-- TODO split into best_hits_evalue and best_hits_bitscore?

bestHits :: Function
bestHits = newFnA1
  "best_hits"
  (Some ht "a hit table")
  (Some ht "a hit table")
  aBestHits
  []

bestHitsEach :: Function
bestHitsEach = newFnA1
  "best_hits_each"
  (ListSigs $ Some ht "a hit table")
  (ListSigs $ Some ht "a hit table")
  (newMap1of1 "best_hits")
  []

aBestHits :: NewAction1
aBestHits (ExprPath out') hits' = do
  let loc = "modules.blasthits.aBestHits"
      out'' = traceA "aBestHits" out' [out', hits']
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [hits']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdBinary = "best_hits.R"
    , cmdArguments = [out', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
