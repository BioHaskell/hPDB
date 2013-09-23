-- | Iterable instances and convenience functions.
module Bio.PDB.Iterable(Iterable(..),
                        firstModel,
                        numAtoms,
                        numResidues,
                        numChains,
                        numModels)
where

import Bio.PDB.Iterable.Instances
import Bio.PDB.Iterable.Utils
