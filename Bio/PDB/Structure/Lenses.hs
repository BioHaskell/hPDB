module Bio.PDB.Structure.Lenses(LAtom(..), LResidue(..), LChain(..), LModel(..), LStructure(..)) where

import Data.Lens

class Path a b where
  pathUp :: a -> b
  
  

class Iterable a b => LIterable a b
  
  
