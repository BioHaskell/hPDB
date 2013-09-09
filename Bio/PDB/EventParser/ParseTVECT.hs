{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- | Parses TVECT records.
module Bio.PDB.EventParser.ParseTVECT(parseTVECT)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ TVECT records
{--
As described on:
http://deposit.rcsb.org/adit/docs/pdb_atom_format.html#TVECT
[This record is not in official PDB file format description v3.20!]

COLUMNS       DATA TYPE      CONTENTS
--------------------------------------------------------------------------------
 1 -  6       Record name    "TVECT "                                    
 8 - 10       Integer        Serial number
11 - 20       Real(10.5)     t[1]
21 - 30       Real(10.5)     t[2]
31 - 40       Real(10.5)     t[3]
41 - 70       String         Text comment                        
--}

titleFields = [( 6,  mKeyword "record header" "TVECT "),
               ( 7,  mSpc     1                       ),
               (10,  dInt     "serial number" 0       ),
               (20,  mDouble   "t[n][1]"               ),
               (30,  mDouble   "t[n][2]"               ),
               (40,  mDouble   "t[n][3]"               ),
               (70,  pStr     "comment"               )]

-- | Parses a TVECT record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseTVECT ::  (Monad m) => String -> Int -> m [PDBEvent]
parseTVECT line line_no = return $ if null errs
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, errs) = parseFields titleFields line line_no
    [fRec, _, fSerial, ft1, ft2, ft3, fComment] = fields
    -- unpack fields
    IFInt   serial = fSerial
    IFDouble t1     = ft1
    IFDouble t2     = ft2
    IFDouble t3     = ft3
    result = TVECT serial $ Vector3 t1 t2 t3

-- NOTE: consecutive "TVECT" records should be merged into a single multiline entry with SUCH method
--mergeTVECTRecords :: [PDBEvent] -> m [PDBEvent]

--------------- }}} TVECT records

