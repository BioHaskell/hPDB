{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing of HET records.
module Bio.PDB.EventParser.ParseHET(parseHET)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ HET/HETSYN records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
---------------------------------------------------------------------------------
 1 -  6       Record name   "HET   "
 8 - 10       LString(3)    hetID          Het identifier, right-justified.
13            Character     ChainID        Chain  identifier.
14 - 17       Integer       seqNum         Sequence  number.
18            AChar         iCode          Insertion  code.
21 - 25       Integer       numHetAtoms    Number of HETATM records for the group
                                           present in the entry.
31 - 70       String        text           Text describing Het group.
--}

{-# INLINE hetFields #-}
hetFields = [(6,  mKeyword "record header"     "HET   "            ),
             (7,  mSpc     1                                       ),
             (10, mStr     "hetero group identifier"               ),
             (12, mSpc     2                                       ),
             (13, mChr     "chain identifier"                      ),
             (17, mInt     "sequence number"                       ),
             (18, mChr     "insertion code"                        ),
             (20, mSpc     2                                       ),
             (25, mInt     "number of HETATM records per group"    ),
             (30, pSpc                                             ),
             (70, pStr     "text describing HET group"             )]

-- | Parses a HET record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseHET ::  (Monad m) => String -> Int -> m [PDBEvent]
parseHET line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    (fields, errs) = parseFields hetFields line line_no
    [fRec, _, fHetId, _, fChain, fSeqNum, fInsCode, _, fAtmNum, _, fText] = fields
    IFStr  hetId   = fHetId
    IFChar chain   = fChain
    IFInt  seqNum  = fSeqNum
    IFChar insCode = fInsCode
    IFInt  atmNum  = fAtmNum
    IFStr  text    = fText

    -- unpack fields
    result = HET hetId chain seqNum insCode atmNum text

--------------- }}} HET/HETSYN records

