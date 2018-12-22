module Main where

import Test.Tasty

import qualified Bio.PDB.StructureTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit"
  [ Bio.PDB.StructureTest.tests
  ]
