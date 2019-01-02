module Detourrr.Modules.SonicParanoid
  where

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Modules.SeqIO      (fna, faa)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple)
import System.FilePath             ((</>), takeBaseName)
import Detourrr.Core.Paths         (DtrPath, toDtrPath, fromDtrPath)
import Detourrr.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd)
import System.Directory            (createDirectoryIfMissing)
import Detourrr.Core.Util          (digest, unlessExists)

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "SonicParanoid"
  , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [ sonicparanoid
      ]
  }

spr :: DtrType
spr = DtrType
  { tExt  = "spr"
  , tDesc = "SonicParanoid results"
  , tShow = defaultShow
  -- , tShow = \_ ref path -> do
  --     txt <- readFileStrict ref path
  --     return $ unlines $ take 17 $ lines txt
  }

-------------------
-- sonicparanoid --
-------------------

sonicparanoid :: DtrFunction
sonicparanoid = let name = "sonicparanoid" in DtrFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] spr -- TODO or fna
  , fTypeCheck = defaultTypeCheck [ListOf faa] spr -- TODO or fna
  , fDesc      = Just "Run SonicParanoid on a list of genomes in FASTA format.\n\
                      \It produces lots of result files! Use the extract_* functions\n\
                      \or look in the TMPDIR to find the specific info you want."
  , fFixity    = Prefix
  , fRules     = rSimple aSonicParanoid
  }

-- TODO run mmseqs2 separately and put the results in tmpDir first, then use -mo
--      (or let sonicparanoid run it and link from here to the mmseqs2 tmpdir)
aSonicParanoid :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
aSonicParanoid cfg ref _ [out, faListPath] = do

  let cacheDir    = cfgTmpDir cfg </> "cache" </> "sonicparanoid"
      sharedDir   = cacheDir </> "shared"
      tmpDir      = cacheDir </> digest faListPath
      -- mmseqsDir   = sharedDir </> "mmseqs2_db"
      dbDir       = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- this is shared with the MMSeqs module TODO make explicit
      -- outDir      = tmpDir </> "result" -- TODO copy input files here?
      inDir       = tmpDir </> "input_links" -- TODO can you prevent it duplicating this to input?
      opPath'     = tmpDir </> "ortholog_relations" </> "ortholog_pairs.tsv"
      opPath      = toDtrPath cfg opPath'
      faListPath' = fromDtrPath cfg faListPath
      out'        = fromDtrPath cfg out
      out''       = debugA cfg "aSonicParanoid" out' [out', faListPath']

  unlessExists opPath' $ do
    liftIO $ createDirectoryIfMissing True inDir -- sonicparanoid will create the others

    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromDtrPath cfg) faPaths
    debugNeed cfg "aSonicParanoid" faPaths'
    let faLinks = map (\p -> toDtrPath cfg $ inDir </> (takeBaseName $ fromDtrPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks

    (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "sonicparanoid"
      [ "-sh", sharedDir
      , "-db", dbDir -- TODO share this with the mmseqs2 module
      , "-i", inDir
      , "-o", tmpDir
      , "-m", "fast" -- TODO set this based on fn name
      , "-noidx" -- TODO optional?
      , "-d" -- TODO set based on cfgDebug
      , "-op" -- write ortholog pairs
      -- , "-ka" -- TODO is this good?
      ]
    putNormal $ unlines [o, e] -- TODO remove

  symlink cfg ref out opPath

aSonicParanoid _ _ _ args = error $ "bad argument to aSonicParanoid: " ++ show args
