-- TODO rename something more general like SeqUtils?

module Detourrr.Modules.SeqIO where

import Development.Shake

import Detourrr.Core.Types
-- import Detourrr.Core.Config (debug)

import Detourrr.Core.Util          (digest)
import Detourrr.Core.Actions       (readPaths, writePaths, debugA, debugNeed,
                                    wrappedCmdOut, writeCachedLines)
import Detourrr.Core.Paths         (toDtrPath, fromDtrPath, DtrPath)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple, rSimpleScript, aSimpleScriptNoFix)
import Detourrr.Core.Compile.Map  (rMap, rMapSimpleScript)
import System.FilePath             ((</>), (<.>), takeDirectory)
import System.Directory            (createDirectoryIfMissing)
import Detourrr.Modules.Load       (mkLoaders)

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "SeqIO"
  , mDesc = "Sequence file manipulations using BioPython's SeqIO"
  , mTypes = [gbk, faa, fna]
  , mFunctions =
    [ gbkToFaa    , gbkToFaaEach
    , gbkToFna    , gbkToFnaEach
    , extractSeqs , extractSeqsEach
    , extractIds  , extractIdsEach
    , translate   , translateEach
    , mkConcat fna  , mkConcatEach fna
    , mkConcat faa  , mkConcatEach faa
    , splitFasta faa, splitFastaEach faa
    , splitFasta fna, splitFastaEach fna
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
    ++ mkLoaders True fna
    ++ mkLoaders True faa
    ++ mkLoaders False gbk
  }

gbk :: DtrType
gbk = DtrType
  { tExt  = "gbk"
  , tDesc = "genbank"
  , tShow = defaultShow
  }

faa :: DtrType
faa = DtrType
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tShow = defaultShow
  }

fna :: DtrType
fna = DtrType
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tShow = defaultShow
  }

-----------------------
-- gbk_to_f*a(_each) --
-----------------------

-- TODO need to hash IDs afterward!
gbkToFaa :: DtrFunction
gbkToFaa = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [gbk] faa
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_faa.py"
  }
  where
    name = "gbk_to_faa"

-- TODO need to hash IDs afterward!
gbkToFaaEach :: DtrFunction
gbkToFaaEach = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf faa)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf gbk] (ListOf faa)
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "gbk_to_faa.py"
  }
  where
    name = "gbk_to_faa_each"

-- TODO need to hash IDs afterward!
gbkToFna :: DtrFunction
gbkToFna = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [gbk] fna
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_fna.py"
  }
  where
    name = "gbk_to_fna"

-- TODO need to hash IDs afterward!
gbkToFnaEach :: DtrFunction
gbkToFnaEach = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf fna)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf gbk] (ListOf fna)
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "gbk_to_fna.py"
  }
  where
    name = "gbk_to_fna_each"

------------------------
-- extract_ids(_each) --
------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

extractIds :: DtrFunction
extractIds = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractIds
  , fDesc = Nothing, fTypeDesc  = name ++ " : fa -> str.list"
  , fRules     = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

extractIdsEach :: DtrFunction
extractIdsEach = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractIdsEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

tExtractIds :: [DtrType] -> Either String DtrType
tExtractIds [x] | elem x [faa, fna] = Right (ListOf str)
tExtractIds _ = Left "expected a fasta file"

tExtractIdsEach :: [DtrType] -> Either String DtrType
tExtractIdsEach [ListOf x] | elem x [faa, fna] = Right (ListOf $ ListOf str)
tExtractIdsEach _ = Left "expected a fasta file"

-------------------------
-- extract_seqs(_each) --
-------------------------

-- TODO also extract them from genbank files

extractSeqs :: DtrFunction
extractSeqs = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fDesc = Nothing, fTypeDesc  = name ++ " : fa -> str.list"
  , fRules     = rSimpleScript "extract_seqs.py"
  }
  where
    name = "extract_seqs"

extractSeqsEach :: DtrFunction
extractSeqsEach = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqsEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rMapSimpleScript 1 "extract_seqs.py"
  }
  where
    name = "extract_seqs_each"

tExtractSeqs  :: [DtrType] -> Either String DtrType
tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a fasta file and a list of strings"

tExtractSeqsEach  :: [DtrType] -> Either String DtrType
tExtractSeqsEach [x, ListOf (ListOf s)]
  | s == str && elem x [faa, fna] = Right $ ListOf x
tExtractSeqsEach _ = Left "expected a fasta file and a list of strings"

----------------------
-- translate(_each) --
----------------------

-- TODO name something else like fna_to_faa?
translate :: DtrFunction
translate = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [fna] faa
  , fRules     = rSimpleScript "translate.py"
  }
  where
    name = "translate"

translateEach :: DtrFunction
translateEach = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf fna] (ListOf faa)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf fna] (ListOf faa)
  , fRules     = rMapSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

--------------
-- concat_* --
--------------

-- TODO separate concat module?

mkConcat :: DtrType -> DtrFunction
mkConcat cType = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf cType] cType
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fRules     = rSimple $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext

mkConcatEach :: DtrType -> DtrFunction
mkConcatEach cType = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf $ ListOf cType] (ListOf cType)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fRules     = rMap 1 $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext ++ "_each"

{- This is just a fancy `cat`, with handling for a couple cases:
 - * some args are empty and their <<emptywhatever>> should be removed
 - * all args are empty and they should be collapsed to one <<emptywhatever>>
 -
 - TODO special case of error handling here, since cat errors are usually temporary?
 -}
-- aConcat :: DtrType -> DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
-- aConcat cType cfg ref ids [oPath, fsPath] = do
--   fPaths <- readPaths cfg ref fs'
--   let fPaths' = map (fromDtrPath cfg) fPaths
--   debugNeed cfg "aConcat" fPaths'
--   let out'    = fromDtrPath cfg oPath
--       out''   = debugA cfg "aConcat" out' [out', fs']
--       outTmp  = out'' <.> "tmp"
--       emptyStr = "<<empty" ++ extOf cType ++ ">>"
--       grepCmd = "egrep -v '^" ++ emptyStr ++ "$'"
--       catArgs = fPaths' ++ ["|", grepCmd, ">", outTmp]
--   wrappedCmdWrite cfg ref outTmp fPaths' [] [Shell] "cat"
--     (debug cfg ("catArgs: " ++ show catArgs) catArgs)
--   needsFix <- isReallyEmpty outTmp
--   if needsFix
--     then liftIO $ writeFile out'' emptyStr
--     else copyFile' outTmp out''
--   where
--     fs' = fromDtrPath cfg fsPath
-- aConcat _ _ _ _ = error "bad argument to aConcat"

-- TODO WHY DID THIS BREAK CREATING THE CACHE/PSIBLAST DIR? FIX THAT TODAY, QUICK!
aConcat :: DtrType -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aConcat cType cfg ref ids [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  let tmpDir'   = cfgTmpDir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ extOf cType) <.> "txt"
      emptyStr  = "<<empty" ++ extOf cType ++ ">>"
      inList'   = tmpDir' </> digest inList <.> "txt" -- TODO is that right?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fromDtrPath cfg outPath
  writeCachedLines cfg ref emptyPath [emptyStr]
  inPaths <- readPaths cfg ref $ fromDtrPath cfg inList
  let inPaths' = map (fromDtrPath cfg) inPaths
  debugNeed cfg "aConcat" inPaths'
  writeCachedLines cfg ref inList' inPaths'
  aSimpleScriptNoFix "cat.py" cfg ref ids [ outPath
                                      , toDtrPath cfg inList'
                                      , toDtrPath cfg emptyPath]
aConcat _ _ _ _ _ = error "bad argument to aConcat"

-- writeCachedLines cfg ref outPath content = do

-- TODO would it work to just directly creat a string and tack onto paths here?
-- aSimpleScript' :: Bool -> String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
-- aSimpleScript' fixEmpties script cfg ref (out:ins) = aSimple' cfg ref ids out actFn Nothing ins

------------------------
-- split_fasta(_each) --
------------------------

splitFasta :: DtrType -> DtrFunction
splitFasta faType = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [faType] (ListOf faType)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [faType] (ListOf faType)
  , fRules     = rSimple $ aSplit name ext
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext

splitFastaEach :: DtrType -> DtrFunction
splitFastaEach faType = DtrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf faType] (ListOf $ ListOf faType)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf faType] (ListOf $ ListOf faType)
  , fRules     = rMap 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> (DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ())
aSplit name ext cfg ref _ [outPath, faPath] = do
  let faPath'   = fromDtrPath cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "_"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromDtrPath cfg outPath
      outPath'' = debugA cfg "aSplit" outPath' [outPath', faPath']
      args      = [outDir', prefix', faPath']
  -- TODO make sure stderr doesn't come through?
  -- TODO any locking needed here?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True outDir'
  out <- wrappedCmdOut False True cfg ref [faPath'] [] [] "split_fasta.py" args
  let loadPaths = map (toDtrPath cfg) $ lines out
  writePaths cfg ref outPath'' loadPaths
aSplit _ _ _ _ _ paths = error $ "bad argument to aSplit: " ++ show paths
