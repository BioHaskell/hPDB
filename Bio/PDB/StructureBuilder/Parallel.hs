{-# LANGUAGE BangPatterns, DisambiguateRecordFields, MultiParamTypeClasses, NamedFieldPuns, FlexibleContexts, OverloadedStrings, RankNTypes, PatternGuards #-}
module Bio.PDB.StructureBuilder.Parallel(parseParallel, parseWithNParallel, joinStructure, joinResult)
where

import           Prelude hiding(String)
import           Bio.PDB.StructureBuilder.Internals
import           Bio.PDB.Structure
import           Bio.PDB.EventParser.PDBEvents(PDBEvent(PDBParseError))
import           GHC.Conc(numCapabilities)
import           Control.Parallel.Strategies
import           Control.Arrow((&&&))
import qualified Bio.PDB.Structure.List     as L
import qualified Data.ByteString.Char8      as BS
import qualified Control.Monad.ST           as ST
import           Control.Monad.State.Strict as State 
import           Data.STRef                 as STRef
import qualified Data.Vector                as V
import qualified Bio.PDB.Structure.List     as L

-- | Parse a fragment of PDB file, returning final line number (within the chunk.)
partialParse :: FilePath -> String -> (Structure, L.List PDBEvent, Int)
partialParse fname contents = ST.runST $ do initial <- initializeState
                                            (s, e, l)  <- State.evalStateT parsing initial
                                            return (s :: Structure, e :: L.List PDBEvent, l :: Int)
  where parsing = do parsePDBRec (BS.pack fname) contents (\() !ev -> parseStep ev) ()
                     closeStructure
                     s     <- State.gets currentStructure
                     e     <- State.gets errors
                     lnref <- State.gets lineNo
                     ln    <- lift $ STRef.readSTRef lnref
                     e'    <- L.finalize e
                     return (s, e', ln)

-- | Parse file in parallel with as many threads as we have capabilities.
parseParallel = parseWithNParallel numCapabilities
-- TODO: merging
-- | Intermediate result from parsing of PDB chunk.
type ParseResult = (Structure, V.Vector PDBEvent, Int)

-- | Joins 'ParseResult' from two different chunks and returns a single 'ParseResult'.
joinResult :: ParseResult -> ParseResult -> ParseResult
joinResult (struct1, errs1, ln1) (struct2, errs2, ln2) = (resultStruct, resultErrs, ln2)
  where
    resultStruct = struct1 `joinStructure` struct2
    resultErrs   = errs1 V.++ V.map (updateErrorLine ln1) errs2

-- | Joins 'Structure's resulting from partial parses.
joinStructure ::  Structure -> Structure -> Structure
joinStructure = joiner models (\s m -> s { models = m }) modelId matchModelId joinModel
  where
    modelId1 `matchModelId` modelId2 | modelId2 == defaultModelId = True
    modelId1 `matchModelId` modelId2 | modelId1 == modelId2       = True
    modelId1 `matchModelId` modelId2                              = False

-- | Joins 'Model's resulting from partial parses.
joinModel :: Model -> Model -> Model
joinModel = joiner chains (\m c -> m { chains = c }) chainId (==) joinChain

-- | Joins 'Chain's resulting from partial parses.
joinChain :: Chain -> Chain -> Chain
joinChain = joiner residues (\c r -> c { residues = r }) resId (==) joinResidue
  where
    resId = resName &&& resSeq &&& insCode

-- | Joins 'Residue's resulting from partial parses.
joinResidue = joiner atoms (\r a -> r { atoms = a }) (const ()) (/=) (error "Never happens")

{-# INLINE joiner #-}
-- | Produce joinX function, given:
--   * getter for subordinate component vector,
--   * setter for subordinate component vector,
--   * getter for the id of a subordinate component,
--   * matcher for ids of subordinate components that decides whether they have to be joined,
--   * and joining function for subordinate objects (if they share the same id.)
--
-- This joining function merges two data structures.
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

--parseParallelWithNSparks :: Int -> FilePath -> String -> [(Structure, List PDBEvent)]
-- | Parse input file with N parallel threads.
parseWithNParallel sparks fname input = (struct, errs)
  where
    chunkLen = ceiling (fromIntegral (BS.length input) / fromIntegral sparks)
    chunks = chunkString chunkLen input
    pList = map (partialParse fname) chunks
    partialResults  = pList `using` parList (evalTuple3 rdeepseq r0 r0)
    (struct, errs, ln)  = foldl joinResult (head partialResults) (tail partialResults)
-- TODO: correct line numbers! partial parse should return Structure + line number

-- | Splits a ByteString into chunks of given size, and ending at end of line.
chunkString ::  Int -> String -> [String]
chunkString l s | BS.length s <= l                          = [s]
chunkString l s | Just n <- BS.elemIndex '\n' (BS.drop l s) = BS.take (l+n+1) s:chunkString l (BS.drop (l+n+1) s)
chunkString l s                                             = [s]

