{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | Parsing a SEQRES record.
module Bio.PDB.EventParser.ParseSEQRES(parseSEQRES)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SEQRES records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
COLUMNS        DATA TYPE      FIELD        DEFINITION
-------------------------------------------------------------------------------------
 1 -  6        Record name    "SEQRES"
 8 - 10        Int        serNum       Serial number of the SEQRES record for  the
                                           current  chain. Starts at 1 and increments
                                           by one  each line. Reset to 1 for each chain.
WARNING: As seen in 3JYV - serial number spans from 7th column!!!
12             Character      chainID      Chain identifier. This may be any single
                                           legal  character, including a blank which is
                                           is  used if there is only one chain.
14 - 17        Int        numRes       Number of residues in the chain.
                                           This  value is repeated on every record.
20 - 22        Residue name   resName      Residue name.
24 - 26        Residue name   resName      Residue name.
28 - 30        Residue name   resName      Residue name.
32 - 34        Residue name   resName      Residue name.
36 - 38        Residue name   resName      Residue name.
40 - 42        Residue name   resName      Residue name.
44 - 46        Residue name   resName      Residue name.
48 - 50        Residue name   resName      Residue name.
52 - 54        Residue name   resName      Residue name.
56 - 58        Residue name   resName      Residue name.
60 - 62        Residue name   resName      Residue name.
64 - 66        Residue name   resName      Residue name.
68 - 70        Residue name   resName      Residue name.
--}

seqresFields = [(6,  mKeyword "record header" "SEQRES"),
                (7,  mSpc                     1),
                (10, mInt     "serial number"),
                (11, mSpc                     1),
                (12, mChr     "chain id"),
                (13, mSpc                     1),
                (17, mInt     "number of residues"),
                (70, pStr     "residues")]

splitResidues resStr = if BS.null resStr
                         then []
                         else res:splitResidues rest2
  where
    (res, rest)  = BS.splitAt 3 resStr
    (" ", rest2) = BS.splitAt 1 rest
    

-- | Parses a SEQRES record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSEQRES :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseSEQRES line line_no = return $ if null errs
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, errs) = parseFields seqresFields line line_no
    [fRec, fSpc1, fSerial, fSpc2, fChain, fSpc3, fNum, fRes] = fields
    -- unpack fields
    IFInt  serial   = fSerial
    IFChar chain    = fChain
    IFInt  num      = fNum
    IFStr  residues = fRes
    resList         = filter (not . BS.null) (BS.split ' ' residues)
    -- assuming residue name won't contain spaces...
    result = SEQRES serial chain num resList

-- NOTE: consecutive "SEQRES" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} SEQRES records

