{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | hPDB PARALLEL radius of gyration via Bio.PDB.StructureBuilder.Parallel
-- (parseParallel) — the SAME chunk-parse-merge scheme our PDBML parallel scan
-- imitates, run on hPDB's own full structure parse. Scales with +RTS -Nk.
--
-- This is the head-to-head for SCALING: hPDB parses every field of every
-- fixed-width record (compute-heavy per byte), so the question is whether it
-- stays compute-bound (near-linear) where our thin PDBML coordinate scanner is
-- bandwidth-bound (sub-linear ~4x at N8).
module Main (main) where

import           Bio.PDB                              (Atom(..), Iterable(..))
import qualified Bio.PDB.Structure                    as S
import           Bio.PDB.StructureBuilder             (parseParallel)
import           Bio.PDB.IO.OpenAnyFile               (readFile)
import           Prelude hiding                       (readFile)
import           Control.DeepSeq                      (force)
import           Control.Exception                    (evaluate)
import           Linear                               (V3(..))
import           System.Environment                   (getArgs)
import           GHC.Conc                             (numCapabilities)
import           Text.Printf                          (printf)

data Sum3 = Sum3 !Double !Double !Double !Int

addCoord :: Sum3 -> Atom -> Sum3
addCoord (Sum3 !sx !sy !sz !n) a = let V3 x y z = coord a
                                   in Sum3 (sx+x) (sy+y) (sz+z) (n+1)

addDev :: V3 Double -> Double -> Atom -> Double
addDev (V3 cx cy cz) !acc a = let V3 x y z = coord a
                                  dx=x-cx; dy=y-cy; dz=z-cz
                              in acc + dx*dx + dy*dy + dz*dz

main :: IO ()
main = do
  [fname] <- getArgs
  contents <- readFile fname
  -- parseParallel uses numCapabilities chunks; force the structure so the
  -- parse work actually happens (and is what -Nk parallelises).
  let (st :: S.Structure, _errs) = parseParallel fname contents
      Sum3 sx sy sz n = itfoldl' addCoord (Sum3 0 0 0 0) st
      !c     = V3 (sx/fromIntegral n) (sy/fromIntegral n) (sz/fromIntegral n)
      sumsq = itfoldl' (addDev c) 0 st
      rg    = sqrt (sumsq / fromIntegral n)
  _ <- evaluate (force (n, rg))
  printf "hPDB-parallel  caps=%d  atoms=%d  Rg=%.6f\n" numCapabilities (n :: Int) (rg :: Double)
