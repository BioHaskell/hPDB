{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}
module Bio.PDB.Structure.Vector(Vec3D(..),
                                unpackVec3D,
                                vnormalise, vdot, (*|), (|*),
                                vzip, vmap,
                                vnorm, vproj, vperpend, vperpends, vdihedral) where

import qualified Data.Vector.Class as C
import Data.Vector.V3
import Data.List(foldl')
import Test.QuickCheck

-- ^ This module wraps 3D vector operations, and adds missing ones. Also hides a "Vector" class

-- | Defines type alias for position and translation vectors in PDB structures.
type Vec3D = Vector3

-- | Unpacks an abstract 3D vector into a triple of Doubles.
unpackVec3D :: Vec3D -> (Double, Double, Double)
unpackVec3D (Vector3 x y z) = (x, y, z)

-- | Maps an operation that modifies a Double onto a 3D vector.
{-# INLINE vmap #-}
vmap :: (Double -> Double) -> Vec3D -> Vec3D
vmap = C.vmap

-- | Maps an operation on a pair of Doubles onto a pair of 3D vectors
--   coordinatewise.
vzip :: (Double -> Double -> Double) -> Vec3D -> Vec3D -> Vec3D
vzip = C.vzip

-- | Normalises to a unit vector in the same direction as input.
{-# INLINE vnormalise #-}
vnormalise :: Vec3D -> Vec3D
vnormalise = C.vnormalise 

-- | Computes a dot product of two 3D vectors.
{-# INLINE vdot #-}
vdot :: Vec3D -> Vec3D -> Double
vdot = C.vdot

{-# INLINE vnorm #-}
-- | 2-norm of a vector (also called a magnitude or length.)
vnorm :: Vec3D -> Double
vnorm = C.vmag

{-# INLINE vdihedral #-}
-- | Compute dihedral between three bond vectors using spherical angle formula.
vdihedral :: Vec3D -> Vec3D -> Vec3D -> Double
vdihedral !a !b !c = (atan2 (vnorm b * (a `vdot`  (b `vcross` c)))
                            ((a `vcross` b) `vdot` (b `vcross` c)) )

-- | Scalar product. (`*` indicates side on which one can put a scalar.)
{-# INLINE (*|) #-}
(*|) :: Double -> Vec3D  -> Vec3D
(*|) = (C.*|)

-- | Scalar product. (`*` indicates side on which one can put a scalar.)
{-# INLINE (|*) #-}
(|*) :: Vec3D  -> Double -> Vec3D
(|*) = (C.|*)

{-# INLINE vproj #-}
-- | Finds a vector component of the first vector that is a projection onto direction of second vector.
vproj    v w = vmap (*scale) uw
  where
    uw    = vnormalise w
    scale = v `vdot` uw

{-# INLINE vperpend #-}
-- | Returns a component of the vector v that is perpendicular to w.
vperpend v w = v - (v `vproj` w)


-- | Finds a component of the vector v that is perpendicular to all vectors in a list.
vperpends v ws = foldl' vperpend v ws

instance Arbitrary Vector3 where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Vector3 a b c



