{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}
module Bio.PDB.Structure.Vector(Vector3(..),
                                unpackVector3,
                                vnormalise, vdot, (*|), (|*),
                                vzip, vmap,
                                vnorm, vproj, vperpend, vperpends, vdihedral) where

import qualified Data.Vector.Class as C
import Data.Vector.V3
import Data.List(foldl')
import Test.QuickCheck

-- ^ This module wraps 3D vector operations, and adds missing ones.

-- | Unpacks an abstract 3D vector into a triple of 'Double's.
unpackVector3 :: Vector3 -> (Double, Double, Double)
unpackVector3 (Vector3 x y z) = (x, y, z)

-- | Maps an operation that modifies a 'Double' onto a 3D vector.
{-# INLINE vmap #-}
vmap :: (Double -> Double) -> Vector3 -> Vector3
vmap = C.vmap

-- | Maps an operation on a pair of 'Double's onto a pair of 3D vectors
--   coordinatewise.
vzip :: (Double -> Double -> Double) -> Vector3 -> Vector3 -> Vector3
vzip = C.vzip

-- | Normalises to a unit vector in the same direction as input.
{-# INLINE vnormalise #-}
vnormalise :: Vector3 -> Vector3
vnormalise = C.vnormalise 

-- | Computes a dot product of two 3D vectors.
{-# INLINE vdot #-}
vdot :: Vector3 -> Vector3 -> Double
vdot = C.vdot

{-# INLINE vnorm #-}
-- | 2-norm of a vector (also called a magnitude or length.)
vnorm :: Vector3 -> Double
vnorm = C.vmag

{-# INLINE vdihedral #-}
-- | Compute dihedral between three bond vectors using spherical angle formula.
vdihedral :: Vector3 -> Vector3 -> Vector3 -> Double
vdihedral !a !b !c = atan2 (vnorm b * (a `vdot`  (b `vcross` c)))
                           ((a `vcross` b) `vdot` (b `vcross` c))

-- | Scalar product. (asterisk - "*" - indicates side on which one can put a scalar.)
{-# INLINE (*|) #-}
(*|) :: Double -> Vector3  -> Vector3
(*|) = (C.*|)

-- | Scalar product. (asterisk - "*" - indicates side on which one can put a scalar.)
{-# INLINE (|*) #-}
(|*) :: Vector3  -> Double -> Vector3
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



