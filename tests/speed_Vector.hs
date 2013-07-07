module Main(main) where

import Bio.PDB.Structure.Vector

import qualified Criterion.Main  as C
import qualified Test.QuickCheck     as QC
import qualified Test.QuickCheck.Gen as QCG
import System.Random

makeVectors :: Int -> IO [Vector3]
makeVectors n = do randomGen <- getStdGen
                   return $ map (QCG.unGen QC.arbitrary randomGen) [1..n]

splitTriples    (a:b:c  :ls) = (a,b,c)   : splitTriples    ls
splitTriples    []           = []

splitQuadruples (a:b:c:d:ls) = (a,b,c,d) : splitQuadruples ls
splitQuadruples []           = []

makeVectorQuadruples n = do l <- makeVectors $ 4*n
                            return $ splitQuadruples l

makeVectorTriples n = do l <- makeVectors $ 3*n
                         return $ splitTriples l

main = do testVectorTriples    <- makeVectorTriples    1000
          testVectorQuadruples <- makeVectorQuadruples 1000
          (C.defaultMain [
             C.bgroup "dihedral"  $ [C.bench "1000" $ C.nf (map dihe)  testVectorQuadruples],
             C.bgroup "dihedral2" $ [C.bench "1000" $ C.nf (map dihe2) testVectorQuadruples]
           ])
  where
    dihe2 (v, w, u, q) = vdihedral2 v w u q
    dihe  (v, w, u, q) = vdihedral  (v-w) (w-u) (u-q)
