{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Unweighted all-atom radius of gyration via hPDB, to compare head-to-head
-- with the xml-typelift PDBML reader's Rg on the SAME structure.
--
-- Rg = sqrt( (1/n) * Σ_i |r_i - r_cm|^2 ),  r_cm = (1/n) Σ_i r_i
-- (geometric centroid, every atom weight 1) -- identical definition to
-- bench/cross-validate/pdbml_reference.py / ParAtomScan, so the two numbers
-- are directly comparable.
module Main (main) where

import           Bio.PDB              (parse, Atom(..), Iterable(..))
import qualified Bio.PDB.Structure    as S
import           Data.Proxy           (Proxy(..))
import           Linear               (V3(..))
import           System.Environment   (getArgs)
import           Text.Printf          (printf)

-- Accumulate Σx, Σy, Σz, n in one strict pass.
data Sum3 = Sum3 !Double !Double !Double !Int

addCoord :: Sum3 -> Atom -> Sum3
addCoord (Sum3 !sx !sy !sz !n) a =
  let V3 x y z = coord a
  in Sum3 (sx + x) (sy + y) (sz + z) (n + 1)

-- Accumulate Σ|r - c|^2 given a fixed centroid c.
addDev :: V3 Double -> Double -> Atom -> Double
addDev (V3 cx cy cz) !acc a =
  let V3 x y z = coord a
      dx = x - cx; dy = y - cy; dz = z - cz
  in acc + dx*dx + dy*dy + dz*dz

main :: IO ()
main = do
  [fname] <- getArgs
  mstruct <- parse fname
  case mstruct of
    Nothing -> putStrLn "parse failed" >> error "no structure"
    Just (st :: S.Structure) -> do
      let Sum3 sx sy sz n = itfoldl' addCoord (Sum3 0 0 0 0) st
          c     = V3 (sx / fromIntegral n) (sy / fromIntegral n) (sz / fromIntegral n)
          sumsq = itfoldl' (addDev c) 0 st
          rg    = sqrt (sumsq / fromIntegral n)
      printf "hPDB  atoms=%d  Rg=%.6f\n" (n :: Int) (rg :: Double)
