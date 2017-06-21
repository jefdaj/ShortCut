module ShortCut.Modules where

import ShortCut.Core.Types (CutModule(..))

import qualified ShortCut.Modules.Load      as Load
import qualified ShortCut.Modules.BioMartR  as BioMartR
-- import qualified ShortCut.Modules.Blast     as Blast -- TODO replace with shmlast
import qualified ShortCut.Modules.BlastCRB  as BlastCRB
import qualified ShortCut.Modules.Cheat     as Cheat -- TODO write this
import qualified ShortCut.Modules.Fasta     as Fasta
import qualified ShortCut.Modules.Math      as Math
import qualified ShortCut.Modules.Permute   as Permute
import qualified ShortCut.Modules.Repeat    as Repeat
import qualified ShortCut.Modules.Sets      as Sets
import qualified ShortCut.Modules.Summarize as Summarize

modules :: [CutModule]
modules =
  [ Load.cutModule
  , BioMartR.cutModule
  -- , Blast.cutModule
  , BlastCRB.cutModule
  , Cheat.cutModule
  , Fasta.cutModule
  , Math.cutModule
  , Permute.cutModule
  , Repeat.cutModule
  , Sets.cutModule
  , Summarize.cutModule
  ]
