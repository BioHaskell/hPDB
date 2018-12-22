{-# LANGUAGE OverloadedStrings #-}
module Bio.PDB.StructureTest where

import Linear
import Test.Tasty
import Test.Tasty.HUnit

import Bio.PDB.Structure

tests :: TestTree
tests = testGroup "Bio.PDB.StructureTest"
  [ testCase "atom1" unit_atom1
  ]

atom1 :: Atom
atom1 = Atom {
    atName = "nm1"
  , atSerial = 1
  , coord = V3 1 2 3
  , bFactor = 2.0
  , occupancy = 3.0
  , element = "elem"
  , segid = "sid1"
  , charge = "chrg"
  , hetatm = True
  }

unit_atom1 :: IO ()
unit_atom1 = atom1 @?= atom1
