{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module ClassesFD(FunctorFD, FoldableFD) where

class MapFD c a | c -> a where
  mapfd :: (a -> a) -> (c -> c)

-- NOTE: is it necessary, given we have ifold yet?
class FoldableFD c a | c -> a where
  foldfd :: (a -> b) -> (b -> b) -> (c -> b)
