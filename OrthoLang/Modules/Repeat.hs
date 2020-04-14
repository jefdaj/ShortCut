module OrthoLang.Modules.Repeat where

{- They're named similarly, but repeat and replace mean different things. The
 - Replace module copies the script and replaces a variable in each version of
 - it without changing the salts, so many times no real work will need to be
 - redone to get the new answer. Repeat (this module) is used when you want to
 - redo the same work multiple times. It's implemented by duplicating a
 - specific expression, changing its salt in each version, and passing those to
 - replace_each. So far this is the only thing the salts are ever used for.
 -}

-- TODO which parts of this should go in Core/Repeat.hs?
-- TODO debug transformations too!

import OrthoLang.Core

import OrthoLang.Modules.Replace (rReplaceEach)
import Data.Scientific           (Scientific(), toBoundedInteger)

olModule :: Module
olModule = Module
  { mName = "Repeat"
  , mDesc = "Repeatdly re-calculate variables using different random salts"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions = [repeatN]
  }

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

repeatN :: Function
repeatN = Function
  { fOpChar = Nothing, fName = "repeat"
  ,fTags = []
  -- , fTypeCheck = tRepeatN
  -- , fTypeDesc  = "repeat : <outputvar> <inputvar> num -> <output>.list"
  , fInputs = [AnyType "the return type", AnyType "the input type", Exactly num]
  , fOutput =  ListSigs (AnyType "the return type")
  , fNewRules = NewNotImplemented
  , fOldRules = rRepeatN
  }

-- takes a result type, a starting type, and an int,
-- and returns a list of the result var type. start type can be whatever
-- (Some ot "any type", num) (ListOf (Some ot "any type"))
-- shown as "t num -> t.list, where t is any type"
tRepeatN :: [Type] -> Either String Type 
tRepeatN [rType, _, n] | n == num = Right $ ListOf rType
tRepeatN _ = Left "invalid args to repeatN"

readSciInt :: String -> Int
readSciInt s = case toBoundedInteger (read s :: Scientific) of
  Nothing -> error $ "Not possible to repeat something " ++ s ++ " times."
  Just n  -> n

-- TODO is the bug here? might need to convert string -> sci -> int
extractNum :: Script -> Expr -> Int
extractNum _   (Lit x n) | x == num = readSciInt n
extractNum scr (Ref _ _ _ v) = extractNum scr $ justOrDie "extractNum failed!" $ lookup v scr
extractNum _ _ = error "bad argument to extractNum"

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
-- TODO is this how the salts should work?
rRepeatN :: RulesFn
rRepeatN scr (Fun t mSalt deps name [resExpr, subVar@(Ref _ _ _ v), repsExpr]) =
  rReplaceEach scr (Fun t mSalt deps name [resExpr, subVar, subList])
  where
    subExpr = justOrDie "lookup of subExpr in rRepeatN failed!" $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = take nReps $ zipWith setSalt [0..] (repeat subExpr) -- TODO is always starting from 0 right?
    -- subs    = zipWith setSalt (unfoldRepID salt nReps) (repeat subExpr)
    -- subs'   = trace ("rRepeatN salts: " ++ show (map saltOf subs)) subs
    subList = Lst (typeOf subExpr) (depsOf subExpr) subs -- TODO salt right?
rRepeatN _ _ = fail "bad argument to rRepeatN"
