{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Bio.PDB.Iterable.Utils(firstModel,
                              numAtoms,
                              numResidues,
                              numChains,
                              numModels)
where

import Bio.PDB.Structure
import Bio.PDB.Iterable

firstModel :: (Iterable a Model) => a -> Maybe Model
firstModel = ifoldr (\m _ -> Just m) Nothing

-- GHC BUG: I see no reason, why such a function has to have explicit type decl
numAtoms :: Iterable a Atom => a -> Int
numAtoms = ilength (undefined :: Atom)

numResidues :: Iterable a Residue => a -> Int
numResidues = ilength (undefined :: Residue)

numChains :: Iterable a Chain => a -> Int
numChains = ilength (undefined :: Chain)

numModels :: Iterable a Model => a -> Int
numModels = ilength (undefined :: Model)

