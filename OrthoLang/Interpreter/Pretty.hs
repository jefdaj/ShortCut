{-# LANGUAGE FlexibleInstances #-}

module OrthoLang.Interpreter.Pretty
  ( prettyShow
  -- , prettyResult (moved to Eval)
  , writeScript
  , Expr(..)
  , prettyNum -- TODO get rid of this?

  -- re-export for convenience
  , Pretty()
  , render
  , renderIO
  , pPrint
  , pPrintHdl
  )
  where

import Prelude hiding ((<>))
import Data.Scientific            (Scientific(), toBoundedInteger)
-- import Development.Shake.FilePath ((</>))
import OrthoLang.Types
-- import OrthoLang.Interpreter.Config (showConfig)
import Text.PrettyPrint.HughesPJClass
import System.Console.Terminal.Size (Window(..), size)
import System.IO (Handle, hPutStrLn)
-- import Control.Monad.Trans (liftIO)
-- import Data.String.Utils          (replace)
import Test.Tasty.Golden (writeBinaryFile)

import qualified Data.Text.Lazy as T
import Text.Pretty.Simple (pShowNoColor)

getWidth :: IO Int
getWidth = do
  s <- size
  return $ case s of
    Nothing -> 120
    Just (Window {width = w}) -> w

-- Render with my custom style (just width so far)
-- Needs to have the optional constant width for the REPL tests
renderIO :: Config -> Doc -> IO String
renderIO cfg doc = do
  currentWidth <- getWidth
  let renderWidth = case termcolumns cfg of
                      Nothing -> currentWidth
                      Just w  -> w
  let s = style {lineLength = renderWidth, ribbonsPerLine = 0.2}
  -- let s = style {lineLength = renderWidth}
  return $ renderStyle s doc

-- Print something pretty to a handle, rendering with custom style from Pretty.hs
-- TODO move to Pretty.hs?
pPrintHdl :: Pretty a => Config -> Handle -> a -> IO ()
pPrintHdl cfg hdl thing = renderIO cfg (pPrint thing) >>= hPutStrLn hdl

instance Pretty Type where
  pPrint Empty           = error "should never need to print Empty"
  pPrint (ListOf  Empty) = text "empty list"
  pPrint (ListOf      t) = text "list of" <+> pPrint t <> text "s"
  pPrint (ScoresOf    t) = text "list of" <+> pPrint t <> text "s with scores"
  pPrint (EncodedAs e t) = pPrint t <+> text "encoded as" <+> text (enExt e)
  -- pPrint (Some (TypeGroup {tgExt = t, tgDesc = d}) s) = text t <+> parens (text d) <+> parens (text s) -- TODO refine this
  pPrint (Type            { tExt = t,  tDesc = d}   ) = text t <+> parens (text d)

instance Pretty Encoding where
  pPrint e = text $ enExt e

instance Pretty Var where
  pPrint (Var _ s) = text s -- TODO show the salt?

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (ListOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty Assign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

-- TODO is totally ignoring the sDigests part OK here?
instance {-# OVERLAPPING #-} Pretty Script where
  pPrint [] = empty
  pPrint as = vcat $ map pPrint as

-- TODO move to a "files/io" module along with debug fns?
-- TODO use safe write here?
writeScript :: Config -> Script -> FilePath -> IO ()
writeScript cfg scr path = do
  txt <- renderIO cfg $ pPrint scr
  writeBinaryFile path txt

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty Expr where
  pPrint e@(Lit _ s)
    | typeOf e == num = prettyNum s
    | otherwise = text $ show s
  pPrint (Ref _ _ _ v)    = pPrint v
  pPrint (Fun _ _ _ s es) = text s <+> sep (map pNested es)
  pPrint (Lst _ _ es)  = pList es
  pPrint (Com (CompiledExpr t (ExprPath p) _)) = text $ "Compiled " ++ ext t ++ " " ++ p

  -- this is almost right except it breaks lines too early (always nesting),
  -- which looks super weird for short bops:
  -- pPrint (Bop _ _ _ c e1 e2) = pPrint e1 $$ nest (-2) (text c) $$ pPrint e2

  -- this one is a little better: the first line is right and *then* it starts doing that
  -- TODO ask on stackoverflow if there's any better way, but later
  pPrint (Bop _ _ _ c e1 e2) = sep $ punctuate (text $ " " ++ c) [pPrint e1, pPrint e2]

pList :: (Pretty a) => [a] -> Doc
pList es = text "[" <> sep (punctuate (text ",") (map pPrint es)) <> text "]"

prettyNum :: String -> Doc
prettyNum s = text $
  case toBoundedInteger n of
    Just i  -> show (i :: Int)
    Nothing -> show n -- as decimal
  where
    n = read s :: Scientific

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: Expr -> Doc
pNested e@(Fun  _ _ _ _ _  ) = parens $ pPrint e
pNested e@(Bop  _ _ _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

-- TODO update this by mapping over the fields
instance Pretty Config where
  pPrint = text . T.unpack . pShowNoColor

-- TODO change this to something useful
instance Pretty Function where
  pPrint fn = text $ "Function \"" ++ fName fn ++ "\""

instance Show Function where
  show = prettyShow

-- TODO change this to something useful
instance Pretty Module where
  pPrint fn = text $ "Module \"" ++ mName fn ++ "\""

-- instance Show Module where
  -- show = prettyShow

instance Pretty DigestMap where
  pPrint m = text $ show m

instance Pretty TypeGroup where
  pPrint g = text $ tgExt g

instance Pretty TypeSig where
  pPrint (ListSigs s)     = pPrint s <> text ".list"
  pPrint (ScoresSigs s)   = pPrint s <> text ".scores"
  pPrint (EncodedSig e s) = pPrint s <> text ("." ++ enExt e)
  pPrint (AnyType _)      = text "anytype" -- TODO does this make sense? might have to do variables
  pPrint (Some g _)       = pPrint g
  pPrint (Exactly t)      = pPrint t