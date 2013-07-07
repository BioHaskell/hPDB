{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Bio.PDB.Structure.Vector
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck

tol = 10e-9

class AEq a where
  (~==) :: a -> a -> Bool
  infix 4 ~==

instance AEq Double where
  a ~== b = abs (a - b) < tol

instance AEq Vector3 where
  v ~== w = vnorm (v - w) < tol

prop_vproj v      = vproj v v ~== v

prop_vperpend v w = vproj v w + vperpend v w ~== v

-- | Compute _unsigned_ dihedral between four positions using plane normals formula.
vdihedral2 :: Vec3D -> Vec3D -> Vec3D -> Vec3D -> Double
vdihedral2 a b c d = if abs dot < 1e-9
                      then asin cross
                      else acos dot
  where
    dot    =         uv `vdot`   uw
    cross  = vnorm $ uv `vcross` uw
    uv = vnormalise $ (b-a) `vcross` (c-a)
    uw = vnormalise $ (b-d) `vcross` (c-d)

prop_vdihedrals a b c d = abs (vdihedral a b c d) ~== abs (vdihedral2 u v w)
  where u = a-b
        v = b-c
        w = c-d

prop_vdihedral_90_degree = vdihedral2 u1 u2 u3 ~== pi/2
  where u1 = Vector3 1.0 0.0 0.0
        u2 = Vector3 0.0 1.0 0.0
        u3 = Vector3 0.0 0.0 1.0

prop_vdihedral_0_degree v = vdihedral2 v v v ~== 0.0 

prop_vdihedral_scaling u v w s = abs (vdihedral2 u v w) ~== abs (vdihedral2 (s*|u) (s*|v) (s*|w))

prop_vdihedral_scaling2 u v w p r s = abs (vdihedral2 u v w) ~== abs (vdihedral2 (abs p*|u) (abs r*|v) (abs s*|w))


main = $quickCheckAll
