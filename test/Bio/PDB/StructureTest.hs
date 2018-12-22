{-# LANGUAGE OverloadedStrings #-}
module Bio.PDB.StructureTest where

import qualified Data.Vector as V
import Linear
import Test.Tasty
import Test.Tasty.HUnit

import Bio.PDB.Structure

tests :: TestTree
tests = testGroup "Bio.PDB.StructureTest"
  [ testCase "atom1" unit_atom1
  ]

structure1 :: Structure
structure1 = Structure {
    models = V.fromList [ model1 ]
  }

model1 :: Model
model1 = Model {
    modelId = 1
  , chains = V.fromList [ chain1 ]
  }

chain1 :: Chain
chain1 = Chain {
    chainId = 'C'
  , residues = V.fromList [ residue1 ]
  }

residue1 :: Residue
residue1 = Residue {
    resName = "resname1"
  , resSeq = 1
  , atoms = V.fromList [ atom1 ]
  , insCode = 'I'
  }

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
