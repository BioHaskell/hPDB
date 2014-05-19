{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-- | Convenience functions using specific Iterable instances.
module Bio.PDB.Iterable.Utils(firstModel,
                              numAtoms,
                              numResidues,
                              numChains,
                              numModels)
where

import Data.Proxy(Proxy)
import Bio.PDB.Structure
import Bio.PDB.Iterable.Instances

-- | Takes a first model.
firstModel :: (Iterable a Model) => a -> Maybe Model
firstModel = itfoldr (\m _ -> Just m) Nothing

-- GHC BUG: I see no reason, why such a function has to have explicit type decl

-- | Number of all atoms within the structure.
numAtoms :: Iterable a Atom => a -> Int
numAtoms = itlength (undefined :: Proxy Atom)

-- | Number of all residues within the structure.
numResidues :: Iterable a Residue => a -> Int
numResidues = itlength (undefined :: Proxy Residue)

-- | Number of all chains within the structure.
numChains :: Iterable a Chain => a -> Int
numChains = itlength (undefined :: Proxy Chain)

-- | Number of all models within the structure.
numModels :: Iterable a Model => a -> Int
numModels = itlength (undefined :: Proxy Model)

