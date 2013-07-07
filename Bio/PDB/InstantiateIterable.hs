{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Bio.PDB.InstantiateIterable(Iterable(..)  ,
                                   gen_iterable  ,
                                   self_iterable ,
                                   trans_iterable) where

import qualified Bio.PDB.Structure.List as L
import Language.Haskell.TH.Syntax
import Control.Monad.Identity(runIdentity,foldM)
import Data.List(foldl')

class Iterable a b where
  imapM   :: (Monad m) => (b -> m b) -> a -> m a
  imap    ::              (b ->   b) -> a ->   a
  imap f e = runIdentity $ imapM (\b -> return $ f b) e
  ifoldM  :: (Monad m) => (c -> b -> m c) -> c -> a -> m c
  ifoldr  ::              (b -> c ->   c) -> c -> a ->   c
  ifoldl  ::              (c -> b ->   c) -> c -> a ->   c
  ifoldl' ::              (c -> b ->   c) -> c -> a ->   c
  ilength :: b -> a -> Int -- NOTE: b is 'dummy' type argument to satisfy Iterable a b constraint

-- | Generates a direct instance of iterable between $typA and $typB with
--   given names of getter and setter, so that:
--   $getter :: $typA -> $typB 
--   $setter :: $typB -> $typA -> $typA
gen_iterable typA typB getter setter = 
  [d| instance Iterable $(typA) $(typB) where
        imapM f a =
          do b' <- L.mapM f ( $(getter) a)
             return $ $(setter) a b'
        ifoldM  f e a  = do r <- L.foldM f e ( $(getter) a)
                            return r
        ifoldr  f e a = L.foldr  f e ( $(getter) a)
        ifoldl  f e a = L.foldl  f e ( $(getter) a)
        ifoldl' f e a = L.foldl' f e ( $(getter) a) 
        ilength   d a = L.length ( $(getter) a)
    |]

-- | Generates convenience function for iterating over a single object.
self_iterable typA = 
  [d| instance Iterable $(typA) $(typA) where
        imapM f a     = f a 
        ifoldM  f e a = f e a
        ifoldr  f e a = f a e
        ifoldl  f e a = f e a 
        ifoldl' f e a = f e a
        ilength   d a = 1
    |]
--self_iterable typA = gen_iterable typA typA [e| id |] [e| L.singleton |]

-- This works:
--   ilength     a = L.length     ( $(getter) a)

-- | Generates a transitive instance of `Iterable` between $typA and $typC,
--   assuming existence of `Iterable` $typA $typB, and `Iterable` $typB $typC.
trans_iterable typA typB typC = 
  [d| instance Iterable $(typA) $(typC) where
        imapM   f a   = (imapM   :: (Monad m) => ( $(typB) -> m $(typB) ) -> $(typA)   -> m $(typA) ) (imapM f) a 
        imap    f a   = (imap    ::              ( $(typB) ->   $(typB) ) -> $(typA)   ->   $(typA) ) (imap  f) a 
        ifoldM  f e a = (ifoldM  :: (Monad m) => (c -> $(typB)   -> m c) -> c   -> $(typA)   -> m c       ) (ifoldM  f) e a 
        ifoldr  f e a = (ifoldr  ::              ($(typB) -> c   ->   c) -> c   -> $(typA)   ->   c       ) (\bb cc -> ifoldr  f cc bb) e a
        ifoldl  f e a = (ifoldl  ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (ifoldl  f) e a
        ifoldl' f e a = (ifoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (ifoldl' f) e a
        ilength _   a = (ifoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (\a b-> a + ilength (undefined :: $(typC)) b) 0 a
    |]
-- How to make this work:
--       ilength     a = (ifoldl' ::              (Int -> $(typB) -> Int) -> Int -> $(typA)   ->   Int     ) (\i b -> i+(ilength :: Iterable $(typB) $(typC) => $(typB) -> Int) b) 0 a

