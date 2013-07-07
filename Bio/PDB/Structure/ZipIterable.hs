{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies #-}
module Bio.PDB.Structure.ZipIterable(Zipper   (..)   ,
                                     Zippable (..)   ,
                                     Container(..)   ,
                                     ZipperIndexError)
  where

import Data.Typeable
import Control.Exception

import Bio.PDB.Iterable
import Bio.PDB.Structure
import Bio.PDB.Structure.List as L

{--
class Zipper z where
  next    :: z -> Maybe z
  prev    :: z -> Maybe z

-- | Isn't this class a derivative of Zipper and Zippable?
class (Zipper z, Zippable z a) => ZipMapper z a | z -> a where
  mapNext :: (a -> a) -> z -> z
  mapPrev :: (a -> a) -> z -> z

class Zippable z a | z -> a where
  zget    :: z -> a
  zupdate :: (a -> a) -> (z -> z)

class Container z x i where
  up       :: z -> x
  find     :: x -> i -> z -- NOTE: *i* may be multilevel selector.
  selector :: z -> i      -- Returns *own* identifier.
--}

data Zipper c = Zipper { current    :: c       ,
                         preceding  :: [c]     ,
                         following  :: [c]     ,
                         containing :: ZipUp c }

znext zipper = case following zipper of
                 (c:cs) -> Just $ Zipper { current    = c                  ,
                                           preceding  = c:preceding zipper ,
                                           following  = cs                 ,
                                           containing = containing zipper  }
                 []     -> Nothing

zprev zipper = case preceding zipper of
                 (c:cs) -> Just $ Zipper { current    = c                  ,
                                           preceding  = cs                 ,
                                           following  = c:following zipper ,
                                           containing = containing zipper  }
                 []     -> Nothing

-- | Moves zipper up one level.
zup zipper = containing zipper { current = zclose zipper }

-- | Closes a zipper.
zclose :: Zipper a -> a
zclose zipper = zsetter (current $ containing zipper) (L.fromList contents)
  where
    contents = reverse (preceding zipper) ++ following zipper

-- TODO: check 
zdown :: Zipper a -> Int -> ZipDown a
zdown z i = Zipper { current    = curr          ,
                     preceding  = L.toList prec ,
                     following  = L.toList succ ,
                     containing = zipper        }
  where
    n                  = L.length contentsVector
    contentsVector     = zgetter $ current c
    (prec, curr, succ) = vectorSplit errMsg i contentsVector
    errMsg             = show $ current $ zipper

vectorSplit errMsg i n v = if i >= n
                             then throw $ ZipperIndexError errMsg i n -- TODO: here one should raise a nice exception!
                             else assert $ L.length contentsVector == L.length prec + L.length succ + 1
                                         $ (prec, curr, succ)
  where
    n              = L.length contentsVector
    prec           = L.slice 0     i           contentsVector
    succ           = L.slice (i+1) (n-(i+1)-1) contentsVector
    curr           = contentsVector L.!! i

data ZipperIndexError = ZipperIndexError Int Int a
  deriving (Show, Typeable)

instance Show ZipperInde
instance (Show a) => Exception ZipperIndexError

data family ZipUp :: * -> *
data instance ZipUp Structure = Zipper Structure -- loops
data instance ZipUp Model     = Zipper Structure
data instance ZipUp Chain     = Zipper Model
data instance ZipUp Residue   = Zipper Chain
data instance ZipUp Atom      = Zipper Residue

data family ZipDown :: * -> *
data instance ZipDown Structure = Zipper Model
data instance ZipDown Model     = Zipper Chain
data instance ZipDown Chain     = Zipper Residue
data instance ZipDown Residue   = Zipper Atom
data instance ZipDown Atom      = Zipper Atom    -- loops

type ZModel = Zipper Model Structure

data ZModel = ZModel { current   :: Model   ,
                       preceding :: [Model] ,
                       following :: [Model] }
data ZChain = ZChain { current   :: Chain   ,
                       preceding :: [Chain] ,
                       following :: [Chain] }
data ZResidue = ZChain { current   :: Chain   ,
                         preceding :: [Chain] ,
                         following :: [Chain] }

newtype ZStructure = ZStructure Structure
data ZModel        = ZModel     Structure !Int                Model   -- where Int is an index of ZModel
data ZChain        = ZChain     Structure !Int !Int           Chain   -- first Int to Model, second to Chain
data ZResidue      = ZResidue   Structure !Int !Int !Int      Residue -- first Int to Model, second to Chain, third to Residue
data ZAtom         = ZAtom      Structure !Int !Int !Int !Int Atom    -- first Int to Model, second to Chain, third to Residue, fourth to Atom

instance Zipper ZStructure where
  next _ = Nothing
  prev _ = Nothing
  mapNext zs = zs
  mapPrev zs = zs

instance Zippable ZStructure Structure where
  zget      (ZStructure s) = s
  zupdate f (ZStructure s) = ZStructure $ f s

instance Zipper ZModel where
  next (ZModel s a _) = let m = models s in if L.length m > a + 1
                                              then let b = a+1 in b `seq` Just (ZModel s b (m L.! b))
                                              else Nothing
  prev (ZModel s a _) = let m = models s in if a - 1 >= 0
                                              then let b = a-1 in b `seq` Just (ZModel s b (m L.! b))
                                              else Nothing
  mapNext f (ZModel s a _) = let m  = models s
                                 m' = L.imap (\i e -> if i <= a
                                                        then e
                                                        else f e) m
                                 s' = s { models = m' }
                             in ZModel s' a (m' !! a)
  mapPrev f (ZModel s a _) = let m  = models s
                                 m' = L.imap (\i e -> if i >= a
                                                        then e
                                                        else f e) m
                                 s' = s { models = m' }
                             in ZModel s' a (m' !! a)

instance Zippable ZModel Model where
  zget      (ZModel s a) = models s !! a
  zupdate f (ZModel s a) = ZModel s' a
    where
      s' = undefined -- TODO

  
