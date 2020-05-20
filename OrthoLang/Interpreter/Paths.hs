{-# LANGUAGE OverloadedStrings #-}

-- TODO rename this module to TmpFiles?
-- TODO remove from Interpreter to main lib?

{-|
OrthoLang makes heavy use of tmpfiles, and this module controls where they go
inside the main tmpdir. The overall layout is:

@
TMPDIR
|-- cache: per-module indexes, temporary files, etc.
|   |-- biomartr
|   |-- blast
|   |-- crb-blast
|   |-- seqio
|   `-- ...
|-- exprs: hashed result of every expression, organized by fn + arg hashes + seed
|   |-- all
|   |-- any
|   |-- concat_fastas
|   |-- crb_blast
|   |-- crb_blast_each
|   `-- ...
|-- vars: symlinks from user variable names to hashed expressions
|   |-- green_hits.str.list
|   |-- greens.faa.list
|   |-- plantcut.str.list
|   |-- result
|   `-- ...
`-- reps: per-repeat vars separated by random hash prefixes
    |-- 00f6aa06e2
    |   |-- green_hits.str.list
    |   |-- greens.faa.list
    |   |-- plantcut.str.list
    |   |-- result
    |   `-- ...
    |-- 13ba15a45b
    `-- ...
@

Files in the cache are organized however seems best on a per-module basis
with help from 'cacheDir', 'cacheDirUniq', and 'cacheFile'.

Var links are determined by 'varPath' using the user-given name and 'Type'.

Expression paths merit some more explanation. They are determined by
'exprPath' or 'exprPathExplicit'. They get the base name by 'show'ing the
expression and 'digest'ing the resulting 'String', and the folder based on
constructor + function name if a function. Some made up examples:

@
TMPDIR\/exprs\/cut_list\/f987e9b98a.str.list
TMPDIR\/exprs\/cut_lit\/a09f8e8b9c.str
TMPDIR\/exprs\/crb_blast\/38978s9a79.crb
TMPDIR\/exprs\/gbk_to_fna\/289379af7a.fna
@

For most functions, the full path is determined by fn name + argument digests
+ repeat seed, like this:

@
TMPDIR\/exprs\/fn_name\/\<digest1\>\/\<digest2\>\/\<digest3\>\/\<seed\>\/result
@

The repeat seed is a number (0, 1, ...) that causes OrthoLang to re-generate
the result multiple times by changing the path when a user calls one of the
repeat functions. Note: deterministic functions will soon have their repeat
seeds removed.

The last directory with 'result' is a per-call tmpdir for executing scripts
and cleaning up anything they generate if they fail before trying again.
There may also be 'stdout' and 'stderr' logs, and lockfiles.

Digests are truncated md5sums of the corresponding expression path. Their
implementation doesn't really matter much. The important thing is that
whenever an expression is compiled to a path (TODO link to that), we also
store its digest (in the 'IDs' IORef for now) to look up later. Then we
can decode the dependencies of any function call (note: not every
expression!) from its path and tell Shake to 'need' them.

That works for fn calls, but not for literals or lists since they have no
depdendencies and an indeterminate number of dependencies respectively. So
their paths are chosen by content. There's also no need for seeds or
per-call tmpdirs:

@
TMPDIR\/exprs\/\<num or str\>\/\<digest of content\>
TMPDIR\/exprs\/list\/\<digest of element digests\>
@

Lists are especially tricky because we can't necessarily know their contents at
\"Rules-time\". Even lists of literals can be generated by functions. So we
treat them as having only one argument digest for their whole contents. In case
of explicit 'str' or 'num' literals from the source code we can fold over their
digests to generate it, and in case of function calls we use their digest
directly.

There are also a few special cases where we have to break up the fn call
tmpdirs further for performance reasons, because having more than ~1000
files per dir is really slow on Linux. So for example 'split_faa' has a
whole tree of dirs for all the tiny FASTA files it produces.

The @TMPDIR\/cache\/lines@ dir is also special. Any text file written anywhere
by 'writeCachedLines' actually goes there, and is symlinked to its
destination. That sounds complicated, but is necessary to make sure the same
file contents always have the same canonical path, which is necessary for
set deduplication to work.
-}

module OrthoLang.Interpreter.Paths
  (
  -- * Convert to\/from paths
    Path()
  , toPath
  , fromPath
  , sharedPath
  , pathString
  , stringPath
  , isGeneric
  , isURL
  , toGeneric
  , fromGeneric

  -- * Generate paths
  , cacheDir
  , exprPath
  , addDigest
  , exprPathExplicit -- watch out! this one does not add the digest
  , unsafeExprPathExplicit
  , varPath

  -- * Validate paths
  , checkLit
  , checkLits
  , checkPath
  , checkPaths

  -- * Generate path digests
  , pathDigest
  , exprDigest
  , exprDigests
  -- , scriptDigests
  , decodeNewRulesType
  , decodeNewRulesDeps

  -- * Internal utilities
  , argHashes
  , bop2fun
  , listDigestsInPath
  , listExprs
  -- , listScriptExprs
  , makeTmpdirRelative
  , upBy
  , prefixOf -- TODO remove

  )
  where

import OrthoLang.Errors
import OrthoLang.Types
import OrthoLang.Util (digest, digestLength)
import OrthoLang.Debug

import Data.Maybe                     (maybeToList)
import Data.List                      (intersperse, isInfixOf, isPrefixOf)
import Data.List.Split                (splitOn)
import Data.String.Utils              (replace)
import Development.Shake.FilePath     ((</>), (<.>), isAbsolute)
import Path                           (parseAbsFile, fromAbsFile)
-- import Text.PrettyPrint.HughesPJClass (Pretty)

import Prelude hiding (error, log)
import qualified Data.Map.Strict as M
-- import qualified OrthoLang.Util as U

import Control.Monad              (when)
import Data.Either                (partitionEithers)
import Development.Shake.FilePath (makeRelative, splitPath)
import Data.IORef (readIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (throw, throwIO)

-----------
-- paths --
-----------

{-|
Replace current absolute paths with generic placeholders that won't change
when the tmpDir is moved later or whatever.

TODO rewrite with a more elegant [(fn, string)] if there's time
-}
toGeneric :: Config -> String -> String
toGeneric cfg txt = replace (workdir cfg) "$WORKDIR"
                  $ replace (tmpdir  cfg) "$TMPDIR"
                  $ txt

-- | Replace generic path placeholders with current paths
-- TODO rewrite with a more elegant [(fn, string)] if there's time
fromGeneric :: DebugLocation -> Config -> String -> String
fromGeneric loc cfg txt = replace "$WORKDIR" (workdir cfg)
                    $ replace "$TMPDIR"  (tmpdir  cfg)
                    $ checkPath (loc ++ ".fromGeneric") txt -- TODO take loc arg?

isGeneric :: FilePath -> Bool
isGeneric path
  = path == "<<emptylist>>" -- TODO could this be <<emptystr>>?
  || "$TMPDIR"  `isPrefixOf` path
  || "$WORKDIR" `isPrefixOf` path

-- This is hacky, but should work with multiple protocols like http(s):// and ftp://
isURL :: String -> Bool
isURL s = "://" `isInfixOf` take 10 s

toPath :: DebugLocation -> Config -> FilePath -> Path
toPath loc cfg s = Path $ checkPath loc' $ toGeneric cfg $ normalize s
  where
    loc' = loc ++ ".toPath"
    normalize p = if isURL s then s
                  else case parseAbsFile p of
                    Just p' -> fromAbsFile p'
                    Nothing -> if isAbsolute p then p
                               else workdir cfg </> p

fromPath :: DebugLocation -> Config -> Path -> FilePath
fromPath loc cfg (Path path) = fromGeneric (loc ++ ".fromPath") cfg path

sharedPath :: Config -> Path -> Maybe FilePath
sharedPath cfg (Path path) = fmap (\sd -> replace "$TMPDIR" sd path) (shared cfg)

-- | weird, but needed for writing cutpaths to files in Actions.hs
pathString :: Path -> String
pathString (Path path) = path

-- TODO this is basically just exporting Path right? any better way?
stringPath :: String -> Path
stringPath = Path

----------------
-- cache dirs --
----------------

cacheDir :: Config -> String -> Path
cacheDir cfg modName = toPath loc cfg path
  where
    loc = "interpreter.paths.cacheDir"
    path = tmpdir cfg </> "cache" </> modName

-- TODO cacheDirUniq or Explicit?

--------------
-- tmpfiles --
--------------

{-|
This is just a convenience used in exprPath

TODO rename hSomething?

TODO does it need the config at all?
-}
argHashes :: Config -> DigestsRef -> Script -> Expr -> [String]
argHashes c d s (Ref _ _ _ (Var _ vName)) = case lookupExpr vName (sAssigns s) of
                                         Nothing -> error "argHashes" $ "no such var '" ++ vName ++ "'"
                                         Just e  -> argHashes c d s e
argHashes _ _ _ (Lit _     v      ) = [digest v]
argHashes c d s (Fun _ _ _ _ es   ) = map (digest . exprPath c d s) es
argHashes c d s (Bop _ _ _ _ e1 e2) = map (digest . exprPath c d s) [e1, e2] -- TODO remove?
argHashes c d s (Lst _ _ _   es   ) = [digest $ map (digest . exprPath c d s) es] -- TODO use seed here?

-- | Temporary hack to fix Bop expr paths
bop2fun :: Expr -> Expr
bop2fun e@(Bop t r ds _ e1 e2) = Fun t r ds (prefixOf e) [Lst t r ds [e1, e2]]
bop2fun e = error "bop2fun" $ "called with non-Bop: \"" ++ render (pPrint e) ++ "\""

-- TODO rename to tmpPath?
-- TODO remove the third parseenv arg (digestmap)?
exprPath :: Config -> DigestsRef -> Script -> Expr -> Path
exprPath c d s (Ref _ _ _ (Var _ vName)) = case lookupExpr vName (sAssigns s) of
                               Nothing -> error "exprPath" $ "no such var '" ++ vName ++ "'\n" ++ show (sAssigns s)
                               Just e  -> exprPath c d s e
exprPath c d s e@(Bop _ _ _ _ _ _) = exprPath c d s (bop2fun e)
exprPath c d s expr = traceP "exprPath" expr res
  where
    prefix = prefixOf expr
    rtype  = typeOf expr
    seed   = seedOf expr
    hashes = argHashes c d s expr
    res    = unsafeExprPathExplicit c d prefix rtype seed hashes

-- TODO add names to the Bops themselves... or associate with prefix versions?
-- TODO rewrite this to be the proper thing for bops, which is how you currently use it
prefixOf :: Expr -> String
prefixOf (Lit rtn _       ) = ext rtn
prefixOf (Fun _ _ _ name _) = name
prefixOf (Lst _ _ _ _     ) = "list"
prefixOf (Ref _ _ _ _     ) = error "prefixOf" "Refs don't need a prefix"
prefixOf (Bop _ _ _ n _ _ ) = case n of
                                   "+" -> "add"
                                   "-" -> "subtract"
                                   "*" -> "multiply"
                                   "/" -> "divide"
                                   "|" -> "any"
                                   "&" -> "all"
                                   "~" -> "diff"
                                   x   -> error "prefixOf" $ "unknown Bop: \"" ++ x ++ "\""


-- TODO remove repeat seed if fn is deterministic
-- note this is always used with its unsafe digest wrapper (below)
exprPathExplicit :: Config -> String -> Maybe Seed -> [String] -> Path
exprPathExplicit cfg prefix mSeed hashes = toPath loc cfg path
  where
    loc = "interpreter.paths.exprPathExplicit"
    dir  = tmpdir cfg </> "exprs" </> prefix
    base = concat $ intersperse "/" $ hashes ++ (maybeToList $ fmap (\(Seed n) -> show n) mSeed)
    path = dir </> base </> "result" -- <.> ext rtype

-- TODO remove VarPath, ExprPath types once Path works everywhere
-- TODO pass the Type directly rather than an Expr here?
varPath :: Config -> Var -> Expr -> Path
varPath cfg (Var (RepID rep) var) expr = toPath loc cfg $ tmpdir cfg </> repDir </> base
  where
    loc = "interpreter.paths.varPath"
    base = if var == "result" then var else var <.> ext (typeOf expr)
    repDir = case rep of
               Nothing -> "vars"
               Just r  -> "reps" </> r -- TODO digest other stuff too, like the expr?

---------------
-- io checks --
---------------

{-|
These are just to alert me of programming mistakes,
and can be removed once the rest of the IO stuff is solid.
-}
checkLit :: DebugLocation -> String -> String
checkLit loc l = if isGeneric l
                   then throw $ PathLitMixup loc $ "\"" ++  l ++ "\" looks like a path"
                   else l

checkLits :: DebugLocation -> [String] -> [String] -- (or error, but let's ignore that)
checkLits loc = map $ checkLit loc


checkPath :: DebugLocation -> FilePath -> FilePath
checkPath loc path = if isAbsolute path || isGeneric path || isURL path
                       then path
                       else throw $ PathLitMixup loc $ "\"" ++ path ++ "\" looks like a literal"

checkPaths :: DebugLocation -> [FilePath] -> [FilePath]
checkPaths loc = map $ checkPath loc

-----------
-- utils --
-----------

-- TODO move this somewhere else?

-- TODO there must be a standard function for this right?
-- TODO guard that the top level stays to prevent it being /
upBy :: Int -> Path -> Path
upBy n (Path path) = Path path'
  where
    components = splitOn  "/" path -- TODO allow other delims?
    components' = reverse $ drop n $ reverse components
    path' = concat $ intersperse "/" $ components'

{-|
For passing scripts paths that don't depend on the $TMPDIR location, but also
don't require any ortholang funny business to read. It relies on the assumption
that the script will be called from inside $TMPDIR. The level is how many ..s
to add to get back up to $TMPDIR from where you call it.

TODO any good way to simplify that?
-}
makeTmpdirRelative :: Int -> Path -> FilePath
makeTmpdirRelative level (Path path) = replace "$TMPDIR" dots path
  where
    dots = concat $ intersperse "/" $ take level $ repeat ".."


-------------
-- digests --
-------------

-- TODO err function

addDigest :: DigestsRef -> Type -> Path -> IO ()
addDigest dRef rtype path = atomicModifyIORef' dRef $ \ds ->
  (M.insert (pathDigest path) (rtype, path) ds, ())

-- TODO should the safe version still exist? should one be renamed?
unsafeExprPathExplicit :: Config -> DigestsRef -> String -> Type -> Maybe Seed -> [String] -> Path
unsafeExprPathExplicit cfg dRef prefix rtype mSeed hashes =
  let path = exprPathExplicit cfg prefix mSeed hashes
  in path `seq` unsafePerformIO $ addDigest dRef rtype path >> return path

pathDigest :: Path -> PathDigest
pathDigest = PathDigest . digest

exprDigest :: Config -> DigestsRef -> Script -> Expr -> DigestMap
exprDigest cfg dRef scr expr = traceShow "interpreter.paths.exprDigest" res
  where
    p = exprPath cfg dRef scr expr
    dKey = PathDigest $ digest p
    res = M.singleton dKey (typeOf expr, p)

exprDigests :: Config -> DigestsRef -> Script -> [Expr] -> DigestMap
exprDigests cfg dRef scr exprs = M.unions $ map (exprDigest cfg dRef scr) $ concatMap listExprs exprs

-- scriptDigests :: Config -> Script -> DigestMap
-- scriptDigests cfg scr = exprDigests cfg scr $ listScriptExprs scr

{-|
"Flatten" (or "unfold"?) an expression into a list of it + subexpressions.

TODO is there a better word for this, or a matching typeclass?
-}
listExprs :: Expr -> [Expr]
listExprs   (Ref _ _ _ _    ) = [] -- TODO or is it e?
listExprs e@(Lit _ _        ) = [e]
listExprs e@(Fun _ _ _ _  es) = e : concatMap listExprs es
listExprs e@(Lst _ _ _    es) = e : concatMap listExprs es
listExprs e@(Bop _ _ _ _ _ _) = listExprs $ bop2fun e

-- listScriptExprs :: Script -> [Expr]
-- listScriptExprs scr = concatMap listExprs $ map snd scr

-- insertNewRulesDigest :: GlobalEnv -> Expr -> IO ()
-- insertNewRulesDigest st@(_, cfg, _, idr) expr
--   = traceD "insertNewRulesDigest" st expr
--   $ atomicModifyIORef' idr
--   $ \h@(IDs {hExprs = ids}) -> (h {hExprs = M.insert eDigest (eType, ePath) ids}, ())
--   where
--     eType   = typeOf expr
--     ePath   = exprPath cfg dRef scr expr
--     eDigest = pathDigest ePath

decodeNewRulesType :: Config -> DigestsRef -> ExprPath -> IO Type
decodeNewRulesType cfg dRef (ExprPath out) = do
  dMap <- readIORef dRef
  let loc = "interpreter.paths.decodeNewRulesType"
      k = pathDigest $ toPath loc cfg out
  case M.lookup k dMap of
    Nothing -> throwIO $ MissingDigests out [show k]
    Just (t, _) -> return t

-- TODO take an ExprPath
decodeNewRulesDeps :: Config -> DigestsRef -> ExprPath -> IO (Type, [Type], [Path])
decodeNewRulesDeps cfg dRef (ExprPath out) = do
  dMap <- readIORef dRef
  -- look up the outpath digest, and throw an error if missing
  -- (could do this before or after dependencies)
  let loc = "interpreter.paths.decodeNewRulesDeps"
      oKey  = pathDigest $ toPath loc cfg out
      moDig = M.lookup oKey dMap
  case moDig of
    Nothing -> throwIO $ MissingDigests out [show oKey]
    -- then look up the others too
    -- TODO any reason to re-confirm that the path is right here too?
    Just (oType, _) -> do
      let findk k = case M.lookup k dMap of { Nothing -> Left k; Just d -> Right d }
          dKeys  = listDigestsInPath cfg out
          (dFails, dVals) = partitionEithers $ map findk $ trace loc ("dKeys: " ++ show dKeys) dKeys
          dTypes = map fst dVals
          dPaths = map snd dVals
      when (length dFails > 0) $ throwIO $ MissingDigests out $ map show dFails
      -- if everything looks good, return types + paths
      return (oType, dTypes, dPaths)

-- TODO hey, is it worth just looking up every path component to make it more robust?
listDigestsInPath :: Config -> FilePath -> [PathDigest]
listDigestsInPath cfg
  = map PathDigest
  . reverse
  . dropWhile (\s -> length s < digestLength) -- drop "result" or "<seed>/result"
  . reverse
  . drop 2 -- TODO drop "exprs", "<fnname>"
  . dropWhile (/= "exprs")
  . map (filter (/= '/'))
  . splitPath
  . makeRelative (tmpdir cfg)
