module ShortCut.Modules.Sample where

-- TODO single sample works, but why doesn't the salt change when repeating??

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic  (rExpr)
import ShortCut.Core.Paths (exprPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions (readLit, readStrings, writeStrings, debugL)
import Data.Scientific
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

cutModule :: CutModule
cutModule = CutModule
  { mName = "Sample"
  , mDesc = "Random (but reproducable) sampling of list elements.\n\n\
            \WARNING: Because of the way ShortCut caches tempfiles, calling these\n\
            \more than once will give the same sublist each time! For different\n\
            \sublists, use in combination with the 'repeat' function"
  , mTypes = []
  , mFunctions = [sample]
  }

sample :: CutFunction
sample = CutFunction
  { fName      = name 
  , fFixity    = Prefix
  , fTypeCheck = tSample
  , fRules     = rSample
  , fTypeDesc  = name ++ " : num X.list -> X.list"
  , fDesc = Just "Take a random sample from a list. Can be used to test your\n\
                 \algorithm on a smaller set of genes/genomes, or as the 'permute'\n\
                 \step in the permute, repeat, summarize (PRS) pattern."
  }
  where
    name = "sample"

tSample :: [CutType] -> Either String CutType
tSample [n, ListOf x] | n == num = Right $ ListOf x
tSample _ = Left "sample requires a num and a list"

rSample :: RulesFn
rSample st@(_, cfg, ref) expr@(CutFun _ salt _ _ [n, lst]) = do
  (ExprPath nPath' ) <- rExpr st n
  (ExprPath inPath') <- rExpr st lst
  let nPath    = toCutPath cfg nPath'
      inPath   = toCutPath cfg inPath'
      outPath  = exprPath st expr
      outPath' = fromCutPath cfg outPath
      (ListOf t) = typeOf lst
  outPath' %> \_ -> aSample salt t cfg ref outPath nPath inPath
  return $ ExprPath outPath'
rSample _ _ = error "bad argument to rSample"

aSample :: Int -> CutType -> Action2
aSample salt t cfg ref outPath nPath lstPath = do
  let nPath'   = fromCutPath cfg nPath
      lstPath' = fromCutPath cfg lstPath
      outPath' = fromCutPath cfg outPath
  nStr <- readLit cfg ref nPath'
  lst  <- readStrings t cfg ref lstPath'
  debugL cfg $ "aSample salt: " ++ show salt
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = randomSample salt n lst
  writeStrings t cfg ref outPath' elements'

randomSample :: Int -> Int -> [String] -> [String]
randomSample seed n lst = take n $ shuffle lst gen
  where
    shuffle xs = shuffle' xs $ length xs
    gen = mkStdGen seed
