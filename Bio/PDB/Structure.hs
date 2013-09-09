{-# LANGUAGE DisambiguateRecordFields #-}
module Bio.PDB.Structure(String,
                         vdot, vnorm, vproj, vperpend, vperpends, vdihedral, (*|), (|*),
                         Structure(..), Model(..), Chain(..), Residue(..), Atom(..))

where

import Prelude hiding(String)
import Bio.PDB.EventParser.PDBEvents(String, Vector3(..)) -- extract to a separate module?
import Control.DeepSeq
--import Data.Derive.NFData
import Bio.PDB.Structure.List as L
import Bio.PDB.Structure.Vector

-- | Structure holds all data parsed from a single PDB entry
data Structure = Structure { -- remarks :: [Map String String]
                             models    :: L.List Model
                           } deriving (Eq, Show)

instance NFData Structure where
  rnf m = rnf (models m) `seq` ()

{-- Using DERIVE would be so much simpler, but also more difficult to get working on GHC-7.4 (for now)
deriving instance NFData Structure
deriving instance NFData Model
deriving instance NFData Chain
deriving instance NFData Residue
deriving instance NFData Atom
--}

-- | PDB entry may contain multiple models, with slight differences in coordinates etc.
data Model     = Model     { modelId   :: !Int,
                             chains    :: L.List Chain
                           } deriving (Eq, Show)

instance NFData Model where
  rnf m = modelId m `seq` rnf (chains m) `seq` ()

-- | Single linear polymer chain of protein, or nucleic acids
data Chain     = Chain     { chainId   :: !Char,
                             residues  :: L.List Residue
                           } deriving (Eq, Show)

instance NFData Chain where
  rnf m = chainId m `seq` rnf (residues m) `seq` ()

-- | Residue groups all atoms assigned to the same aminoacid or nucleic acid base within a polymer chain.
data Residue   = Residue   { resName   :: !String,
                             resSeq    :: !Int,
                             atoms     :: L.List Atom,

                             insCode   :: !Char
                             -- ss :: SSType 
                           } deriving (Eq, Show)

instance NFData Residue where
  rnf r = rnf (atoms r) `seq` ()
{- TODO:
data SSType = SSHelix  HelixType |
              SSStrand StrandType StrandSense
 -}
-- TODO: LINKs (default and added)

-- | Single atom position
-- | NOTE: disordered atoms are now reported as multiplicates
data Atom      = Atom      { atName    :: !String,
                             atSerial  :: !Int,
                             coord     :: !Vector3,

                             bFactor   :: !Double,
                             occupancy :: !Double,
                             element   :: !String,
                             segid     :: !String,
                             charge    :: !String,
                             hetatm    :: !Bool
                           } deriving (Eq, Show)

-- constructor is strict in all arguments...
instance NFData Atom where
