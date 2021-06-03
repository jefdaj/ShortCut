module OrthoLang.Modules.JustOrthologs where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa)

-- TODO jof type "justorthologs format" (sorted fasta coding sequences with the asterisks)
-- TODO mapped justorthologs_format_each
-- TODO gff3 type?
-- TODO justorthologs_format : gff faa -> jff to extract the proper cds format from just one gff3 + reference genome pair
-- TODO set -t to match nproc
-- TODO search functions: justorthologs_related, justorthologs_distant, justorthologs (combined)
-- TODO what's the correlation value, and should the search fns expose it?
-- TODO orthogroups function to extract them from the output (or is it just pairs here?)
-- TODO express the optional compression of the input fasta files

olModule :: Module
olModule = Module
  { mName = "JustOrthologs"
  , mDesc = "A Fast, Accurate, and User-Friendly Ortholog-Identification Algorithm"
  , mTypes = [gff3, jof, jor]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =
    [ justOrthologsFormat
    , justOrthologsFormatEach
    , justOrthologs
    , justOrthologsRelated
    , justOrthologsDistant
    ]
  }

jof :: Type
jof = Type
  { tExt  = "jof"
  , tDesc = "JustOrthologs format Fasta"
  , tShow = defaultShow
  }

jor :: Type
jor = Type
  { tExt = "jor"
  , tDesc = "JustOrthologs results"
  , tShow = defaultShow
  }

gff3 :: Type
gff3 = Type
  { tExt = "gff3"
  , tDesc = "NCBI genome annotation file"
  , tShow = defaultShow
  }

--------------------
-- format genomes --
--------------------

justOrthologsFormat :: Function
justOrthologsFormat = newFnA2
  "justorthologs_format"
  (Exactly gff3, Exactly faa)
  (Exactly jof)
  aJustOrthologsFormat
  []

aJustOrthologsFormat :: NewAction2
aJustOrthologsFormat = undefined

-- TODO is it a problem that users have to manually match list elements up?
justOrthologsFormatEach :: Function
justOrthologsFormatEach = newFnA2
  "justorthologs_format_each"
  (Exactly $ ListOf gff3, Exactly $ ListOf faa)
  (Exactly $ ListOf jof)
  undefined -- TODO new fn to zip/map over both together?
  []

--------------------------
-- search for orthologs --
--------------------------

mkJustOrthologs :: String -> (CmdDesc -> CmdDesc) -> Function
mkJustOrthologs name descFn = newFnA2
  name
  (Exactly jof, Exactly jof)
  (Exactly jor)
  (undefined descFn)
  []

justOrthologs :: Function
justOrthologs = mkJustOrthologs "justorthologs" id

justOrthologsDistant :: Function
justOrthologsDistant = mkJustOrthologs "justorthologs_distant" undefined

justOrthologsRelated :: Function
justOrthologsRelated = mkJustOrthologs "justorthologs_related" undefined
