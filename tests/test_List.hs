{-# LANGUAGE TemplateHaskell #-}
module TestList(main) where

import Bio.PDB.Structure.List as L
{- (List(..), TempList,
                              initialNew, new, add, finalize, tempLength, empty,
                              map, mapM, fold, foldM, filter, length,
                              defaultSize, residueVectorSize, chainVectorSize
                             ) 
 -}

import Control.Monad.ST
import Test.QuickCheck.All

aha = L.foldr (:) []

{-
prop_ createList l = L.foldr (:) [] fl == l
  where fl = runST $ do tl <- initialNew 10
                        mapM_ (L.add tl :: (a -> TempList ) l
                        finalize tl
-}                      

prop_lengthNew i = r == i
  where
    r = runST $ do tl <- initialNew i
                   L.length tl

main = $quickCheckAll
