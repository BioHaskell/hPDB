{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}
-- | This module wraps 3D vector operations, and adds missing ones.
module Bio.PDB.Structure.Vector(V3(..),
                                unpackV3,
                                vnormalise, vdot, (*|), (|*),
                                vzip, vmap,
                                vnorm, vproj, vperpend, vperpends, vdihedral) where

import Linear
import Data.List(foldl')
import Test.QuickCheck

-- | Unpacks an abstract 3D vector into a triple of 'Double's.
unpackV3 :: V3 Double -> (Double, Double, Double)
unpackV3 (V3 x y z) = (x, y, z)

-- | Maps an operation that modifies a 'Double' onto a 3D vector.
{-# INLINE vmap #-}
vmap :: (Double -> Double) -> V3 Double -> V3 Double
vmap = fmap

-- | Maps an operation on a pair of 'Double's onto a pair of 3D vectors
--   coordinatewise.
vzip :: (Double -> Double -> Double) -> V3 Double -> V3 Double -> V3 Double
vzip = liftI2

-- | Normalises to a unit vector in the same direction as input.
{-# INLINE vnormalise #-}
vnormalise :: V3 Double -> V3 Double
vnormalise = normalize 

{-# INLINE vdot #-}
-- | Computes a dot product of two 3D vectors.
vdot :: V3 Double -> V3 Double -> Double
vdot = dot

{-# INLINE vnorm #-}
-- | 2-norm of a vector (also called a magnitude or length.)
vnorm :: V3 Double -> Double
vnorm = norm

{-# INLINE vdihedral #-}
-- | Compute dihedral between three bond vectors using spherical angle formula.
vdihedral :: V3 Double -> V3 Double -> V3 Double -> Double
vdihedral !a !b !c = atan2 (vnorm b * (a `vdot`  (b `cross` c)))
                           ((a `cross` b) `vdot` (b `cross` c))

{-# INLINE (*|) #-}
-- | Scalar product. (asterisk - "*" - indicates side on which one can put a scalar.)
(*|) :: Double -> V3 Double -> V3 Double
(*|) = (*^)

{-# INLINE (|*) #-}
-- | Scalar product. (asterisk - "*" - indicates side on which one can put a scalar.)
(|*) :: V3 Double -> Double -> V3 Double
(|*) = (^*)

{-# INLINE vproj #-}
-- | Finds a vector component of the first vector that is a projection onto direction of second vector.
vproj    v w = vmap (*scale) uw
  where
    uw    = vnormalise w
    scale = v `vdot` uw

{-# INLINE vperpend #-}
-- | Returns a component of the vector v that is perpendicular to w.
vperpend v w = v - (v `vproj` w)


{-# INLINE vperpends #-}
-- | Finds a component of the vector v that is perpendicular to all vectors in a list.
vperpends v ws = foldl' vperpend v ws

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ V3 a b c


