module OrthoLang.Modules.JustOrthologs where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa, mkConcat)

-- TODO set -t to match nproc
-- TODO what's the correlation value, and should the search fns expose it?
-- TODO orthogroups function to extract them from the output (or is it just pairs here?)
-- TODO express the optional compression of the input fasta files

olModule :: Module
olModule = Module
  { mName = "JustOrthologs"
  , mDesc = "A Fast, Accurate, and User-Friendly Ortholog-Identification Algorithm"
  , mTypes = [faa, gff3, jof, jor]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =
    [ justOrthologsFormat , justOrthologsFormatEach
    , justOrthologs       , justOrthologsEach       , justOrthologsAll
    , justOrthologsRelated, justOrthologsRelatedEach, justOrthologsRelatedAll
    , justOrthologsDistant, justOrthologsDistantEach, justOrthologsDistantAll
    , concatJor
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

-- TODO add standard load fns for these (see weird details in SeqIO)
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
-- TODO at least warn that set operations will mess it up
-- TODO and have an error for when a pair doesn't match
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

-- TODO use a list of args rather than the whole cmddesc here?
mkSearch :: String -> [String] -> Function
mkSearch name extraArgs = newFnA2
  name
  (Exactly jof, Exactly jof)
  (Exactly jor)
  (aJustOrthologs extraArgs)
  []

aJustOrthologs :: [String] -> NewAction2
aJustOrthologs extraArgs = undefined

justOrthologs :: Function
justOrthologs = mkSearch "justorthologs" ["-c"]

justOrthologsDistant :: Function
justOrthologsDistant = mkSearch "justorthologs_distant" ["-d"]

justOrthologsRelated :: Function
justOrthologsRelated = mkSearch "justorthologs_related" []

--------------------
-- _each variants --
--------------------

mkEach :: String -> Function
mkEach singleName = newFnA2
  (singleName ++ "_each")
  (Exactly jof, Exactly $ ListOf jof)
  (Exactly $ ListOf jor)
  (newMap2of2 singleName)
  []

justOrthologsEach :: Function
justOrthologsEach = mkEach "justorthologs"

justOrthologsDistantEach :: Function
justOrthologsDistantEach = mkEach "justorthologs_distant"

justOrthologsRelatedEach :: Function
justOrthologsRelatedEach = mkEach "justorthologs_related"

-------------------
-- _all variants --
-------------------

-- TODO this makes sense, right? check that they can be concatenated
concatJor :: Function
concatJor = mkConcat jor

mkAll :: String -> Function -> Function
mkAll name eachFn = compose1 name [] eachFn concatJor

justOrthologsAll :: Function
justOrthologsAll = mkAll "justorthologs_all" justOrthologsEach

justOrthologsDistantAll :: Function
justOrthologsDistantAll = mkAll "justorthologs_distant_all" justOrthologsDistantEach

justOrthologsRelatedAll :: Function
justOrthologsRelatedAll = mkAll "justorthologs_related_all" justOrthologsRelatedEach
