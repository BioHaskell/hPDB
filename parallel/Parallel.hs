{-# LANGUAGE BangPatterns, DisambiguateRecordFields, MultiParamTypeClasses, NamedFieldPuns, FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Bio.PDB.StructureBuilder.Parallel(parseParallel, parseWithNParallel)
where

import           Prelude hiding(String)
import           Bio.PDB.StructureBuilder
import           Bio.PDB.Structure
import           Bio.PDB.EventParser.PDBEvents
import           GHC.Conc(numCapabilities)
import           Control.Parallel.Strategies
import qualified Bio.PDB.Structure.List     as L
import qualified Data.ByteString.Char8      as BS
import qualified Control.Monad.ST           as ST
import           Control.Monad.State.Strict as State 
import           Data.STRef                 as STRef

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

--parseParallel :: FilePath -> String -> ([Structure], [L.List PDBEvent])
parseParallel = parseWithNParallel numCapabilities
-- TODO: merging

--parseParallelWithNSparks :: Int -> FilePath -> String -> [(Structure, List PDBEvent)]
parseWithNParallel sparks fname input = pList `using` parList (evalTuple3 rdeepseq r0 r0)
  where
    chunkLen = ceiling (fromIntegral (BS.length input) / fromIntegral sparks)
    chunks = chunkString chunkLen input
    pList = map (partialParse fname) chunks
-- TODO: correct line numbers! partial parse should return Structure + line number

-- | Splits a ByteString into chunks of given size, and ending at end of line.
chunkString ::  Int -> String -> [String]
chunkString l s | BS.length s <= l                          = [s]
chunkString l s | Just n <- BS.elemIndex '\n' (BS.drop l s) = BS.take (l+n+1) s:chunkString l (BS.drop (l+n+1) s)
chunkString l s                                             = [s]

-- | Record holding a current state of the structure record builder.
{-
data BState s = BState { currentResidue    :: Maybe Residue,
                         currentModel      :: Maybe Model,
                         currentChain      :: Maybe Chain,
                         currentStructure  :: Structure,
                         residueContents   :: L.TempList  s Atom,
                         chainContents     :: L.TempList  s Residue,
                         modelContents     :: L.TempList  s Chain,
                         structureContents :: L.TempList  s Model,
                         errors            :: L.TempList  s PDBEvent,
                         lineNo           :: STRef.STRef s Int
                       }
-}
