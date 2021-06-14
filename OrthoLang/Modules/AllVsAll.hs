module OrthoLang.Modules.AllVsAll
  where

-- TODO this should be easily doable using the extractExprs trick for lists but not fn calls,
--      but should you bother since it also might not be needed for the greencut algorithm?

-- import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa)

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

mkAva :: String -> Function
mkAva name = newExprExpansion
  (name ++ "_ava")
  [Exactly num, Exactly $ ListOf faa] -- TODO can this be more general?
  (Exactly ava)
  (mAva name)
  []

-- TODO is anything besides the name needed?
-- it seems like it has to be name : num faa.list -> bht basically
-- oh, except the result table type might be different?
-- TODO remove the other hit table types? check if they're needed at all
mAva :: String -> ExprExpansion
mAva = undefined
