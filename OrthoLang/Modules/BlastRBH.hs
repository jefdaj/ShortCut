module OrthoLang.Modules.BlastRBH where

import Development.Shake

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Interpreter.Compile as C
import OrthoLang.Modules.Blast   (bht, BlastDesc, blastDescs)
import OrthoLang.Modules.BlastDB (blastdb)
import OrthoLang.Modules.SeqIO   (fna, faa)

import Data.List.Utils           (replace)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           ((</>), (<.>))
import Data.Maybe (fromJust)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

-- for tracking down non-deduplicating blastp functions
debugNames :: String -> Expr -> Expr -> a -> a
debugNames fnName (Fun _ _ _ bname _) (Fun _ _ _ aname _) rtn = C.debugC fnName msg rtn
  where
    msg = "\"" ++ bname ++ "' -> \"" ++ aname ++ "\""
debugNames fnName _ _ _ = error $ "bad argument to debugNames from " ++ fnName

olModule :: Module
olModule = Module
  { mName = "BlastRBH"
  , mDesc = "Reciprocal BLAST+ best hits"
  , mTypes = [fna, faa, bht]
  , mGroups = []
  , mEncodings = [blastdb]
  , mRules = []
  , mFunctions =

    -- TODO also work with the non-symmetric ones that have an obvious way to do it?
    map mkBlastFromFaRev     blastDescsRev ++
    map mkBlastFromFaRevEach blastDescsRev ++

    [reciprocalBest, reciprocalBestAll] ++

    map mkBlastRbh     blastDescsRev ++
    map mkBlastRbhEach blastDescsRev

  }

-- note that this just filters. it doesn't "reverse" the descriptions
blastDescsRev :: [BlastDesc]
blastDescsRev = filter isReversible blastDescs
  where
    isReversible (_, qType, sType, _) = qType == sType

-----------------
-- *blast*_rev --
-----------------

-- TODO rewrite with newExprExpansion

mkBlastFromFaRev :: BlastDesc -> Function
mkBlastFromFaRev (bCmd, qType, sType, _) = newExprExpansion
  (bCmd ++ "_rev")
  [Exactly num, Exactly sType, Exactly qType]
  (Exactly bht)
  mBlastFromFaRev
  []

-- flips the query and subject arguments and reuses the regular compiler above
-- TODO can this be used in Diamond.hs too? make it more generic if possible
mBlastFromFaRev :: ExprExpansion
mBlastFromFaRev _ _ (Fun r ms ds n [e, q, s]) = Fun r ms ds (replace "_rev" "" n) [e, s, q]
mBlastFromFaRev _ _ e = error "ortholang.modules.blastrbh.mBlastFromFaRev" $ "bad argument: " ++ show e

----------------------
-- *blast*_rev_each --
----------------------

-- TODO rewrite with newMap (but do it last)

-- TODO fix expression paths!
mkBlastFromFaRevEach :: BlastDesc -> Function
mkBlastFromFaRevEach (bCmd, sType, qType, _) = newFnA3
  (bCmd ++ "_rev_each")
  (Exactly num, Exactly sType, Exactly $ ListOf qType)
  (Exactly $ ListOf bht)
  (newMap3of3 $ bCmd ++ "_rev")
  []

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

-- TODO rewrite with newFnS3 (do this one first!)

reciprocalBest :: Function
reciprocalBest = newFnA2
  "reciprocal_best"
  (Exactly bht, Exactly bht)
  (Exactly bht)
  aReciprocalBest
  []

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
-- TODO why is this the only one that fails, and only when called from repeat??
aReciprocalBest :: NewAction2
aReciprocalBest (ExprPath out') left' right' = do
  let loc   = "modules.blastrbh.aReciprocalBest"
      out'' = traceA loc out' [out', left', right']
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [left', right']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "reciprocal_best.R"
    , cmdArguments = [out', left', right']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }

-------------------------
-- reciprocal_best_all --
-------------------------

-- TODO rewrite with newFnS2

reciprocalBestAll :: Function
reciprocalBestAll = newFnA2
  "reciprocal_best_all"
  (Exactly $ ListOf bht, Exactly $ ListOf bht)
  (Exactly bht)
  aReciprocalBestAll
  []

aReciprocalBestAll :: NewAction2
aReciprocalBestAll (ExprPath out') lefts' rights' = do
  cfg <- fmap fromJust getShakeExtra
  let cDir = fromPath loc cfg $ cacheDir cfg "blastrbh"
      loc = "modules.blastrbh.aReciprocalBestAll"
      tmpPath p = cDir </> digest loc p <.> "bht"
      ins' = map (\p -> (p, tmpPath p)) [lefts', rights']
      ins  = map (toPath loc cfg . snd) ins'
      out  = toPath loc cfg out'
  liftIO $ createDirectoryIfMissing True cDir
  mapM_ (uncurry (absolutizePaths loc)) ins'
  aSimpleScriptNoFix "reciprocal_best_all.R" (out:ins)

-----------------
-- *blast*_rbh --
-----------------

-- TODO rewrite with newExprExpansion

mkBlastRbh :: BlastDesc -> Function
mkBlastRbh d@(bCmd, qType, sType, _) = newExprExpansion
  (bCmd ++ "_rbh")
  [Exactly num, Exactly qType, Exactly sType]
  (Exactly bht)
  (mBlastRbh d)
  []

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
mBlastRbh :: BlastDesc -> ExprExpansion
mBlastRbh (bCmd, _, _, _) _ _ (Fun _ seed deps _ [e, l, r]) = main
  where
    main  = Fun bht seed deps "reciprocal_best" [lHits, rHits]
    lHits = Fun bht seed deps  bCmd            [e, l, r]
    rHits = Fun bht seed deps (bCmd ++ "_rev") [e, l, r]
mBlastRbh _ _ _ e = error "ortholang.modules.blastrbh.mBlastRbh" $ "bad argument: " ++ show e

----------------------
-- *blast*_rbh_each --
----------------------

-- TODO rewrite with newExprExpansion

mkBlastRbhEach :: BlastDesc -> Function
mkBlastRbhEach d@(bCmd, qType, sType, _) = newExprExpansion
  (bCmd ++ "_rbh_each")
  [Exactly num, Exactly qType, Exactly sType]
  (Exactly bht)
  (mBlastRbhEach d)
  []

mBlastRbhEach :: BlastDesc -> ExprExpansion
mBlastRbhEach (bCmd, _, _, _) _ _ (Fun _ seed deps _ [e, l, rs]) = main
  where
    main  = Fun (ListOf bht) seed deps "reciprocal_best_each" [lHits, rHits]
    lHits = Fun (ListOf bht) seed deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = Fun (ListOf bht) seed deps (bCmd ++ "_rev_each")  [e, l, rs]
mBlastRbhEach _ _ _ e = error "ortholang.modules.blastrbh.mBlastRbhEach" $ "bad argument: " ++ show e
