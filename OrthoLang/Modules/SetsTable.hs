module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Core
import OrthoLang.Core (defaultTypeCheck)
import OrthoLang.Modules.Plots (rPlotListOfLists)

olModule :: Module
olModule = Module
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mGroups = [lit]
  , mFunctions = [setsTable]
  }

-- TODO should this be more specific?
tsv :: Type
tsv = Type
  { tExt  = "tsv"
  , tDesc = "set membership table (spreadsheet)"
  , tShow = defaultShow
  }

setsTable :: Function
setsTable = let name = "sets_table" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf (ListOf (Some lit "some lit"))] tsv
  , fTypeDesc  = mkTypeDesc       name [ListOf (ListOf (Some lit "some lit"))] tsv
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rPlotListOfLists "sets_table.R"
  }
