{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- | Searching for neighbouring atoms in a 3D space using `Octree`.
module Bio.PDB.Structure.Neighbours(makeOctree,
                                    findInRadius, findNearest,
                                    AtomOctree(..))
where

import qualified Data.Octree                as Oct
import           Bio.PDB.Structure
import           Bio.PDB.Iterable
import           Linear

-- | Octree of `Atom`s.
type AtomOctree = Oct.Octree Atom

-- | Preparing atom to be inserted into `Octree`.
extract      :: Atom -> (Oct.Vector3, Atom)
extract (at@(Atom { coord    = cvec,
                    atSerial = ser ,
                    element  = elt })) = (cvec, at)

-- | Make an Octree of `Atom`s
makeOctree   :: Iterable a Atom => a -> AtomOctree
makeOctree   = Oct.fromList . Prelude.map extract . itfoldr (:) []

-- | Find all `Atom`s within a given radius from a point.
findInRadius :: AtomOctree -> Double -> V3 Double -> [(V3 Double, Atom)]
findInRadius = Oct.withinRange

-- | Find an `Atom`s closest to a point.
findNearest  :: AtomOctree -> V3 Double -> Maybe (V3 Double, Atom)
findNearest  = Oct.nearest
