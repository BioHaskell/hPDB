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

-- | Test method
main = do fnames <- getArgs
          forM_ fnames $ \fname -> do contents <- readFile fname
                                      putStr $ "Parsing with " ++ show numCapabilities ++ " capabilities."
                                      let (struct, errs) = parseParallel' fname contents
                                      print $ numAtoms struct

