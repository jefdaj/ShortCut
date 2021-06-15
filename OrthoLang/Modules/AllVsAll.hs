module OrthoLang.Modules.AllVsAll
  where

-- TODO this should be easily doable using the extractExprs trick for lists but not fn calls,
--      but should you bother since it also might not be needed for the greencut algorithm?

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "All-Vs-All"
  , mDesc = "Creates all-vs-all hit tables from any BLAST-like search for use in ortholog finding algorithms"
  , mTypes = [ava]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions = [] -- TODO put the functions here, or in their respective modules?
  }

-- TODO should this actually be an encoding rather than a file type?
--      maybe make the name more general, but use the combined extensions like bht.ava?
-- TODO but is the subject and query type also important? like faa.ava or whatever?
-- TODO alternatively, can you make all the hit table types compatible and avoid that?
ava :: Type
ava = Type
  { tExt  = "ava"
  , tDesc = "all-vs-all hit table listing"
  , tShow = defaultShow
  }

-- TODO any reason to take the name as a separate arg here?
-- rMkAva :: RulesFn
-- rMkAva st (Fun _ _ _ _ [_, faas]) = do
--   (ExprPath _) <- rExpr st faas
--   return undefined
-- rMkAva _ e = error $ "bad argument to rMkAva: " ++ show e

-- construct a BLAST-like search expression from compiled paths
-- mkSearchExpr :: Type -> Maybe Seed -> [Var] -> String -> Expr -> ExprPath -> ExprPath -> Expr
-- mkSearchExpr rtn _ deps name  evalueExpr  queryFaPath subjFaPath
--   =   Fun rtn Nothing deps name [evalueExpr, queryExpr,  subjExpr]
--   where
--     queryExpr = Map $ MappedExpr faa queryFaPath (return queryFaPath)
--     subjExpr  = Map $ MappedExpr faa subjFaPath  (return subjFaPath)

-- Warning: the fn has to be of type : num faa faa -> bht, but this is not enforced in the code yet
mkAva :: String -> Function -> Function
mkAva name fn = newFnA2
  (name ++ "_ava")
  (Exactly num, Exactly $ ListOf faa)
  (Exactly ava)
  (aAva fn)
  []

rowHashes ePath qPath sPaths = qHash : eHashes
  where
    qHash   = undefined
    eHashes = undefined
  -- TODO add the q path hash first as the row name
  -- TODO map over sPaths. for each:
  --        create the fn expr
  --        find its hash
  --        return the hash (to the row)

-- TODO is anything besides the name needed?
-- it seems like it has to be name : num faa.list -> bht basically
-- oh, except the result table type might be different?
-- TODO remove the other hit table types? check if they're needed at all
-- TODO would this be better to raise up to the old compiler type function level rather than NewAction?
aAva :: Function -> NewAction2
aAva fn (ExprPath oPath) ePath fasPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.allvsall.aAva"
      out = toPath loc cfg oPath
  faPaths <- readPaths loc fasPath
  need' loc $ map (fromPath loc cfg) faPaths
  let header = "" : (map undefined faPaths) -- TODO tab-separated list of input fa expr path hashes
      rows   = map (\q -> rowHashes ePath q faPaths) faPaths
      table  = header : rows
  undefined
