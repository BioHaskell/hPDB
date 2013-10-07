-- | Experiments in parallel parsing using hPDB.
-- To be included in the Bio.PDB.StructureBuilder.Parallel?
module Main(main) where

import           Prelude hiding(readFile)
import           Bio.PDB.StructureBuilder.Parallel
import           Bio.PDB.StructureBuilder(defaultModelId)
import           Bio.PDB.Iterable
import           Bio.PDB.IO
import           Bio.PDB.Iterable.Utils
import           System.Environment(getArgs)
import           Control.Monad(forM_)
import           Control.Arrow((&&&))
import           GHC.Conc(numCapabilities)
import           Bio.PDB.IO.OpenAnyFile(readFile)

import qualified Data.Vector as V
import           Bio.PDB.EventParser.PDBEvents(PDBEvent(PDBParseError))
import           Bio.PDB.Structure

-- | Intermediate result from parsing of PDB chunk.
type ParseResult = (Structure, V.Vector PDBEvent, Int)

-- | Joins 'ParseResult' from two different chunks and returns a single 'ParseResult'.
joinResult :: ParseResult -> ParseResult -> ParseResult
joinResult (struct1, errs1, ln1) (struct2, errs2, ln2) = (resultStruct, resultErrs, ln2)
  where
    resultStruct = struct1 `joinStructure` struct2
    resultErrs   = errs1 V.++ V.map (updateErrorLine ln1) errs2


joinStructure ::  Structure -> Structure -> Structure
joinStructure = joiner models (\s m -> s { models = m }) modelId matchModelId joinModel
  where
    modelId1 `matchModelId` modelId2 | modelId2 == defaultModelId = True
    modelId1 `matchModelId` modelId2 | modelId1 == modelId2       = True
    modelId1 `matchModelId` modelId2                              = False

joinModel :: Model -> Model -> Model
joinModel = joiner chains (\m c -> m { chains = c }) chainId (==) joinChain

joinChain :: Chain -> Chain -> Chain
joinChain = joiner residues (\c r -> c { residues = r }) resId (==) joinResidue
  where
    resId = resName &&& resSeq &&& insCode

joinResidue = joiner atoms (\r a -> r { atoms = a }) (const ()) (/=) (error "Never happens")

{-# INLINE joiner #-}
joiner :: (a -> V.Vector a1)-> (a -> V.Vector a1 -> t)-> (a1 -> t1)-> (t1 -> t1 -> Bool)-> (a1 -> a1 -> a1)-> a-> a-> t
joiner getter setter idGetter matcher subjoiner = join
  where
    s1 `join` s2 | len s1 == 0 || len s2 == 0 = s1 `setter` (getter s1 V.++ getter s2)
      where
        len = V.length . getter
    s1 `join` s2 | id1 `matcher` id2          = s1 `setter` V.concat [V.init      (getter s1)        ,
                                                                      V.singleton (m1 `subjoiner` m2),
                                                                      V.tail      (getter s2)        ]
      where
        id1 = idGetter m1
        id2 = idGetter m2
        m1       = V.last $ getter s1
        m2       = V.head $ getter s2
    s1 `join` s2                              = s1 `setter` (getter s1 V.++ getter s2)

-- | Increments line numbers in 'PDBParseError' records by a given value.
updateErrorLine ::  Int -> PDBEvent -> PDBEvent
updateErrorLine startingLineNo (PDBParseError lineNo colNo line) = PDBParseError (lineNo + startingLineNo) colNo line
updateErrorLine startingLineNo evt                               = evt

main = do fnames <- getArgs
          forM_ fnames $ \fname -> do contents <- readFile fname
                                      putStr $ "Parsing with " ++ show numCapabilities ++ " capabilities."
                                      let results = parseParallel fname contents
                                          (struct, errs, ln)  = foldl joinResult (head results) (tail results)
                                      print $ numAtoms struct

