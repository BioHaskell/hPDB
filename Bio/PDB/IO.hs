{-# LANGUAGE OverloadedStrings  #-}

module Bio.PDB.IO(parse, write) where

import qualified Control.Exception(catch)
import Control.Exception.Base(SomeException)
import System.IO hiding(writeFile)
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

type String = BS.ByteString

-- Until I get a newer version of Control.DeepSeq:
force x = x `deepseq` x

parse :: String -> IO (Maybe Bio.PDB.Structure.Structure)
parse filename = do input <- Bio.PDB.IO.OpenAnyFile.readFile $ BS.unpack filename
                    (do (structure, errs) <- return $ Bio.PDB.StructureBuilder.parse filename input
                        mapM_ (showError filename) (L.toList errs)
                        structure `deepseq` return $ Just $ structure
                      `Control.Exception.catch`
                     (exceptionHandler filename))

exceptionHandler :: String -> SomeException -> IO (Maybe a)
exceptionHandler filename e = do printError $ [filename, ":", BS.pack $ show e]
                                 return Nothing

printError msg = BS.hPutStrLn System.IO.stderr $ BS.concat msg

showError filename (PDBParseError line_no col_no msg) =
  printError $ [filename, ":", BS.pack $ show line_no, ":", BS.pack $ show col_no, "\t", msg]
showError filename (PDBIgnoredLine line)              =
  printError $ [filename, ": IGNORED ", line]

-- | Write structure to a .pdb file. (NOT YET IMPLEMENTED)
write :: Bio.PDB.Structure.Structure -> String -> IO ()
write structure fname = writeFile (BS.unpack fname) $ \h -> PDBSP.write h structure

