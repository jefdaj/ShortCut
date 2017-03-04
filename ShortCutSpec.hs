module ShortCutSpec where

{- Runs all the Hspec tests.
 - You can also run the nested Spec.hs files independently like:
 - `runhaskell ShortCut/ParseSpec.hs`
 - `ghc --make ShortCut/ParseSpec.hs -o parse-spec && ./parse-spec`.
 - See: https://github.com/hspec/hspec-example
 -}

-- TODO incorporate golden testing (it seems simple enough)
-- https://hackage.haskell.org/package/test-framework-golden-1.1.3.3

-- TODO test for freeze when running the actual binary on a minimal script

import Test.Hspec

import qualified ShortCut.TypesSpec           as T
-- import qualified ShortCut.Interpret.ParseSpec as P TODO update these!
import qualified ShortCut.ReplSpec            as R
import qualified ShortCut.InterpretSpec       as I
