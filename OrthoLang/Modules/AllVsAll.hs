module OrthoLang.Modules.AllVsAll
  ( olModule
  , ava
  , mkAva
  )
  where

-- TODO this should be easily doable using the extractExprs trick for lists but not fn calls,
--      but should you bother since it also might not be needed for the greencut algorithm?

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa)
import OrthoLang.Modules.Blast (bht)
import OrthoLang.Interpreter.Paths (pathDigest, getExprPathSeed, unsafeExprPathExplicit)
import Data.List  (intercalate)
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
mkAva :: String -> Function
mkAva name = newFnA2
  (name ++ "_ava") -- note that name must match the non-ava single version
  (Exactly num, Exactly $ ListOf faa)
  (Exactly ava)
  (aAva name)
  []

rowDigests :: Config -> DigestsRef -> Maybe Seed -> String -> Type
           -> PathDigest -> PathDigest -> [PathDigest]
           -> [(Path, PathDigest)]
rowDigests c d ms fnName t eDig qDig sDigs = map (cellDigest c d ms fnName t eDig qDig) sDigs

cellDigest :: Config -> DigestsRef-> Maybe Seed -> String -> Type
           -> PathDigest -> PathDigest -> PathDigest
           -> (Path, PathDigest)
cellDigest c d ms fnName t (PathDigest eHash) (PathDigest qHash) (PathDigest sHash)
  = (path, pathDigest path)
  where
    expr   = Fun t ms [] fnName []
    hashes = [eHash, qHash, sHash]
    path   = unsafeExprPathExplicit c d fnName t ms hashes

aAva :: String -> NewAction2
aAva fnName (ExprPath oPath) ePath fasPath = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  let loc = "modules.allvsall.aAva"
      ms  = getExprPathSeed oPath
  faPaths <- readPaths loc fasPath
  let eDig   = pathDigest $ toPath loc cfg ePath
      faDigs = map pathDigest faPaths
  need' loc $ ePath : map (fromPath loc cfg) faPaths
  let header  = intercalate "\t" $ "" : map (\(PathDigest d) -> d) faDigs
      cells   = map (\qDig -> rowDigests cfg dRef ms fnName bht eDig qDig faDigs) faDigs
      digests = (map . map) (\(_, PathDigest d) -> d) cells
      rows    = map (\(PathDigest a,b) -> intercalate "\t" (a:b)) $ zip faDigs digests
      paths   = map (fromPath loc cfg . fst) $ concat cells
      table   = header : undefined
  need' loc paths
  writeCachedLines loc oPath table
