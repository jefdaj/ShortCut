-- TODO once the modules are done, will this mostly be gone?
--      all except the basic refs, symlinks, etc. i guess

-- Once text has been parsed into an abstract syntax tree (Parse.hs), this
-- module "compiles" it by translating it into a set of Shake build rules. To
-- actually run the rules, use `eval` in the Interpret module.

-- TODO add more descriptive runtime error for canonicalizePath failing b/c no file
-- TODO see if you can avoid making more than one absolute symlink per input file
-- TODO make systematically sure there's only one rule for each file
-- TODO pass tmpDir as a config option somehow, and verbosity

-- TODO why doesn't turning down the verbosity actually work?

module ShortCut.Core.Compile
  ( compileScript
  , cBop
  , cLoad
  , hashedTmp
  , hashedTmp'
  , cExpr
  , cList
  , cacheDir
  )
  where

import Development.Shake
import ShortCut.Core.Types

import Crypto.Hash                (hash, Digest, MD5)
import Data.ByteString.Char8      (pack)
import Data.List                  (find)
import Data.List.Utils            (delFromAL)
import Data.Maybe                 (fromJust)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((<.>), (</>))
import System.Directory           (canonicalizePath)
import System.FilePath            (makeRelative)


--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _) = e
mangleExpr fn (CutRef  t vs v      ) = CutRef  t (map fn vs)   (fn v)
mangleExpr fn (CutBop  t vs n e1 e2) = CutBop  t (map fn vs) n (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t vs n es   ) = CutFun  t (map fn vs) n (map (mangleExpr fn) es)
mangleExpr fn (CutList t vs   es   ) = CutList t (map fn vs)   (map (mangleExpr fn) es)
mangleExpr fn (CutSubs r v ss as) = CutSubs (mangleExpr fn r) (fn v) (mangleExpr fn ss) (mangleScript fn as)
-- CutSubs CutExpr CutExpr CutVar [CutAssign] -- dep, ind, ind', cxt

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: Int -> (CutVar -> CutVar)
addPrefix n (CutVar s) = CutVar $ s ++ "." ++ show n

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: Int -> CutScript -> CutScript
addPrefixes n = mangleScript (addPrefix n)


---------------------
-- determine paths --
---------------------

-- TODO move all this stuff to utils or a new config module or something...

-- TODO remove or put in Types
cacheDir :: CutConfig -> FilePath
cacheDir cfg = cfgTmpDir cfg </> "cache"

-- TODO what was this even for? remove it?
exprDir :: CutConfig -> FilePath
exprDir cfg = cacheDir cfg </> "shortcut"

-- Note that MD5 is no longer considered secure
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take 10 $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

-- TODO flip arguments for consistency with everything else There's a special
-- case for "result", which is like the "main" function of a ShortCut script,
-- and always goes to <tmpdir>/result.
namedTmp :: CutConfig -> CutVar -> CutExpr -> FilePath
namedTmp cfg (CutVar var) expr = cfgTmpDir cfg </> base
  where
    base  = if var == "result" then var else var <.> extOf (typeOf expr)

-- TODO extn can be found inside expr now; remove it
hashedTmp :: CutConfig -> CutExpr -> [FilePath] -> FilePath
hashedTmp cfg expr paths = exprDir cfg </> uniq <.> extOf (typeOf expr)
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'

-- overrides the expression's "natural" extension
-- TODO figure out how to remove!
hashedTmp' :: CutConfig -> CutType -> CutExpr -> [FilePath] -> FilePath
hashedTmp' cfg rtn expr paths = exprDir cfg </> uniq <.> extOf rtn
  where
    paths' = map (makeRelative $ cfgTmpDir cfg) paths
    uniq = digest $ unlines $ (show expr):paths'


------------------------------
-- compile the ShortCut AST --
------------------------------

-- TODO what happens to plain sets?
-- TODO WAIT ARE SETS REALLY NEEDED? OR CAN WE JUST REFER TO FILETYPES?
cExpr :: CutConfig -> CutExpr -> Rules FilePath
cExpr c e@(CutLit  _ _      ) = cLit c e
cExpr c e@(CutRef  _ _ _    ) = cRef c e
cExpr c e@(CutList _ _ _    ) = cList c e
cExpr c e@(CutSubs _ _ _ _  ) = cSubs c e
cExpr c e@(CutBop  _ _ n _ _) = compileByName c e n -- TODO turn into Fun?
cExpr c e@(CutFun  _ _ n _  ) = compileByName c e n

-- TODO remove once no longer needed (parser should find fns)
compileByName :: CutConfig -> CutExpr -> String -> Rules FilePath
compileByName cfg expr name = case findByName cfg name of
  Nothing -> error $ "no such function '" ++ name ++ "'"
  Just f  -> (fCompiler f) cfg expr

-- TODO remove once no longer needed (parser should find fns)
findByName :: CutConfig -> String -> Maybe CutFunction
findByName cfg name = find (\f -> fName f == name) fs
  where
    ms = cfgModules cfg
    fs = concat $ map mFunctions ms

-- TODO is the result thing going to mess everything up?
cSub :: CutConfig -> CutExpr -> CutVar -> CutScript -> Int
     -> CutExpr -> Rules FilePath
cSub cfg resExpr subVar script n subExpr = do
  let res    = (CutVar "result", resExpr)
      sub    = (subVar, subExpr)
      scr'   = delFromAL script subVar -- TODO need to remove result too?
      scr''  = res:sub:scr'
      scr''' = addPrefixes n scr''
  resPath <- compileScript cfg scr''' (Just n)
  return resPath

-- TODO this has to work with *Refs* to the things too! (no assuming CutList)
--      does that mean it has to be written to a file?
--      ... not possible :( requires the recursive script it holds too
--      maybe it's time to give up and pass the whole state?
--      then this could be a regular function in a Substitute module
--      yeah, better go with that for now!
cSubs :: CutConfig -> CutExpr -> Rules FilePath
cSubs cfg (CutSubs resExpr subVar (CutList _ _ subList) scr) = do
  -- subPaths <- cExpr cfg subList TODO is this not even needed? WIN :D
  resPaths <- mapM (\(n,e) -> cSub cfg resExpr subVar scr n e) (zip [1..] subList)
  let resPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
      outPath   = hashedTmp' cfg (ListOf $ typeOf resExpr) resExpr resPaths'
  outPath %> \out -> need resPaths >> writeFileLines out resPaths'
  return outPath
cSubs _ expr = error $ "bad argument to cSubs: " ++ show expr

cAssign :: CutConfig -> CutAssign -> Rules (CutVar, FilePath)
cAssign cfg (var, expr) = do
  path  <- cExpr cfg expr
  path' <- cVar cfg var expr path
  return (var, path')

-- TODO how to fail if the var doesn't exist??
--      (or, is that not possible for a typechecked AST?)
compileScript :: CutConfig -> CutScript -> Maybe Int -> Rules FilePath
compileScript cfg as n = do
  -- liftIO $ putStrLn "entering compileScript"
  rpaths <- mapM (cAssign cfg) as
  return $ fromJust $ lookup (CutVar res) rpaths
  where
    res = case n of
      Nothing -> "result"
      Just n' -> "result." ++ show n'

-- write a literal value from ShortCut source code to file
cLit :: CutConfig -> CutExpr -> Rules FilePath
cLit cfg expr = do
  -- liftIO $ putStrLn "entering cLit"
  let path = hashedTmp cfg expr []
  path %> \out -> do
    -- putQuiet $ unwords ["write", out]
    writeFileChanged out $ paths expr ++ "\n" -- TODO is writeFileChanged right?
  return path
  where
    paths :: CutExpr -> String
    paths (CutLit _ s) = s
    paths _ = error "bad argument to paths"

-- TODO how to show the list once it's created? not just as a list of paths!
-- TODO why are lists of lists not given .list.list ext? hides a more serious bug?
--      or possibly the bug is that we're making accidental lists of lists?
cList :: CutConfig -> CutExpr -> Rules FilePath
cList cfg e@(CutList EmptyList _ _) = do
  let link = hashedTmp cfg e []
  link %> \out -> quietly $ cmd "touch" [out]
  return link
cList cfg e@(CutList _ _ exprs) = do
  paths <- mapM (cExpr cfg) exprs
  let path   = hashedTmp cfg e paths
      paths' = map (makeRelative $ cfgTmpDir cfg) paths
  path %> \out -> need paths >> writeFileChanged out (unlines paths')
  return path
cList _ _ = error "bad arguemnts to cList"

-- return a link to an existing named variable
-- (assumes the var will be made by other rules)
cRef :: CutConfig -> CutExpr -> Rules FilePath
cRef cfg expr@(CutRef _ _ var) = do
  -- liftIO $ putStrLn "entering cRef"
  return $ namedTmp cfg var expr
cRef _ _ = error "bad argument to cRef"

-- creates a symlink from expression file to input file
-- these should be the only absolute ones,
-- and the only ones that point outside the temp dir
cLoad :: CutConfig -> CutExpr -> Rules FilePath
cLoad cfg e@(CutFun _ _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoad"
  path <- cExpr cfg f
  let link = hashedTmp cfg e [path]
  link %> \out -> do
    str'   <- fmap strip $ readFile' path
    path'' <- liftIO $ canonicalizePath str'
    -- putQuiet $ unwords ["link", str', out]
    quietly $ cmd "ln -fs" [path'', out]
  return link
cLoad _ _ = error "bad argument to cLoad"

-- Creates a symlink from varname to expression file.
-- TODO how should this handle file extensions? just not have them?
-- TODO or pick up the extension of the destination?
cVar :: CutConfig -> CutVar -> CutExpr -> FilePath -> Rules FilePath
cVar cfg var expr dest = do
  -- liftIO $ putStrLn "entering cVar"
  let link  = namedTmp cfg var expr
      dest' = makeRelative (cfgTmpDir cfg) dest
  link %> \out -> do
    alwaysRerun
    need [dest]
    -- putQuiet $ unwords ["link", (cfgTmpDir cfg) </> dest', out]
    quietly $ cmd "ln -fs" [dest', out]
  return link

-- handles the actual rule generation for all binary operators
-- basically the `paths` functions with pattern matching factored out
cBop :: CutConfig -> CutType -> CutExpr -> (CutExpr, CutExpr)
      -> Rules (FilePath, FilePath, FilePath)
cBop cfg t expr (n1, n2) = do
  -- liftIO $ putStrLn "entering cBop"
  p1 <- cExpr cfg n1
  p2 <- cExpr cfg n2
  return (p1, p2, hashedTmp' cfg t expr [p1, p2])
