{-# LANGUAGE OverloadedStrings  #-}
-- | Simple input/output wrappers taking filenames, and handling compression.
module Bio.PDB.IO(parse, write) where

import qualified Control.Exception(catch)
import Control.Exception.Base(SomeException)
import System.IO hiding(writeFile, FilePath)
import Prelude hiding(String,writeFile)
import Bio.PDB.EventParser.PDBParsingAbstractions
import Bio.PDB.Structure.List as L
import qualified Bio.PDB.StructurePrinter as PDBSP
import Control.Monad(when)

import Bio.PDB.EventParser.PDBEvents(PDBEvent(PDBParseError, PDBIgnoredLine))
--import qualified Bio.PDB.EventParser.PDBEventPrinter as PDBEventPrinter
import qualified Bio.PDB.StructureBuilder(parse)
import qualified Bio.PDB.Structure

import qualified Data.ByteString.Char8 as BS
import Bio.PDB.IO.OpenAnyFile
import Control.DeepSeq

-- | Type alias.
type String = BS.ByteString

-- Until I get a newer version of Control.DeepSeq:
-- | Alias to a function present in newer versions of 'deepseq' library.
force x = x `deepseq` x

-- | Parse a .pdb file and return `Bio.PDB.Structure.Structure`.
parse :: FilePath -> IO (Maybe Bio.PDB.Structure.Structure)
parse filename = do input <- Bio.PDB.IO.OpenAnyFile.readFile filename
                    do (structure, errs) <- return $ Bio.PDB.StructureBuilder.parse filename input
                       mapM_ (showError filename) (L.toList errs)
                       structure `deepseq` return $ Just structure
                     `Control.Exception.catch`
                     exceptionHandler filename

-- | Default exception handler that for `IO (Maybe a)` just prints nice error message to stderr, and returns Nothing
exceptionHandler :: FilePath -> SomeException -> IO (Maybe a)
exceptionHandler filename e = do printError [BS.pack filename, ":", BS.pack $ show e]
                                 return Nothing

-- | Prints a catenated list of ByteStrings to stderr. (Convenience function.)
printError msg = BS.hPutStrLn System.IO.stderr $ BS.concat msg

-- | Show error message from `PDBParser`.
showError filename (PDBParseError line_no col_no msg) =
  printError [BS.pack filename, ":", BS.pack $ show line_no, ":", BS.pack $ show col_no, "\t", msg]
showError filename (PDBIgnoredLine line)              =
  printError [BS.pack filename, ": IGNORED ", line]

-- | Write structure to a .pdb file.
write :: Bio.PDB.Structure.Structure -> FilePath -> IO ()
write structure fname = writeFile fname $ \h -> PDBSP.write h structure

