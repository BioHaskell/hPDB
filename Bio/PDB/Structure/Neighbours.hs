{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-- | Searching for neighbouring atoms in a 3D space using `Octree`.
module Bio.PDB.Structure.Neighbours(makeOctree, findInRadius, findNearest)
where

import qualified Data.Octree                as Oct
import           Bio.PDB.Structure
import           Bio.PDB.Iterable
import           Data.Vector.V3

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
findInRadius :: AtomOctree -> Double -> Vector3 ->      [(Vector3, Atom)]
findInRadius = Oct.withinRange

-- | Find an `Atom`s closest to a point.
findNearest  :: AtomOctree ->           Vector3 -> Maybe (Vector3, Atom)
findNearest  = Oct.nearest
