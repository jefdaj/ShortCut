module OrthoLang.Modules.CustomScript where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Debug (error)
import Prelude hiding (error)
import Data.Maybe (fromJust)
import OrthoLang.Modules.Load (mkLoad)
import OrthoLang.Modules.Plots (varNames)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "CustomScript"
  , mDesc = "Run your own script and OrthoLang will assume the types are valid"
  , mTypes = [bin]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [loadScript, runScriptExplicit, runScript]
  }

bin :: Type
bin = Type
  { tExt  = "bin"
  , tDesc = "custom scripts"
  , tShow = defaultShow -- TODO what if it's binary? maybe use file command to show?
  }

-----------------
-- load_script --
-----------------

loadScript :: Function
loadScript = mkLoad False "load_script" (Exactly bin)

-------------------------
-- run_script_explicit --
-------------------------

-- TODO have to replace the shorthand somewhere in here to get at the list element exprs
-- TODO maybe that could be another macro fn?
-- | Hidden version of `runScript` that takes an explicit pre-loaded script and varnames file.
runScriptExplicit :: Function
runScriptExplicit = hidden $ newFnA3
  "run_script_explicit"
  (Exactly bin, ListSigs (Exactly str), ListSigs (Exactly Untyped))
  (Exactly Untyped)
  aRunScriptExplicit
  [Hidden]

aRunScriptExplicit :: NewAction3
aRunScriptExplicit (ExprPath out) inScr inNames inList = do
  cfg <- fmap fromJust $ getShakeExtra
  let loc  = "modules.customscript.aRunScriptExplicit"
      out' = toPath loc cfg out
  withBinHash out out' $ \tmpPath -> do
    let tmp' = fromPath loc cfg tmpPath
    aNewRulesS2 inScr id (ExprPath tmp') inNames inList

----------------
-- run_script --
----------------

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
runScript :: Function
runScript = newExprExpansion
  "run_script"
  [Exactly str, ListSigs (Exactly Untyped)]
  (Exactly Untyped)
  mRunScript
  [ReadsFile]

-- TODO rewrite Plots.hs functions to use expr expansions with varNames, like this
mRunScript :: ExprExpansion
mRunScript _ _ (Fun r _ ds _ [bStr, iList]) =
  let b = Fun bin Nothing ds "load_script"    [bStr]
  in      Fun r   Nothing ds "run_script_explicit" [b, varNames iList, iList]
mRunScript _ _ e = error "modules.customscript.mRunScript" $ "bad argument: " ++ show e
