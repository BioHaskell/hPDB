{-# LANGUAGE TemplateHaskell #-}
-- | Basic parallel folding utility.
module Bio.PDB.Util.ParFold(parFold1) where

import Data.Tree
import Data.List
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary
import Control.Parallel.Strategies(parList, rseq, using)

-- | Parallel folding like fold1, but assuming associativity and using O(n*lg)
parFold1 f [ ] = error "parFold of empty list!"
parFold1 f [a] = a
parFold1 f l   = parFold1 f . (`using` parList rseq) $ aList
  where
    aList             = foldLevel l
    -- | reduction by one level
    foldLevel [ ]     = [ ]
    foldLevel [a]     = [a]
    foldLevel (a:b:l) = (a `f` b):foldLevel l
    

-- NOTE: join function must be associative
-- | Checks parFold1 with a given associative function
--check_join f l = foldBin f l == foldl1 f l

--prop_join_plus l = check_join (+) l

--main = $quickCheckAll
