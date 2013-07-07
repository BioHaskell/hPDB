{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Parsing information about obsoleted entries: OBSLTE and SPRSDE.
module Bio.PDB.EventParser.ParseObsoleting(parseOBSLTE,parseSPRSDE)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ Obsoleting records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
---------------------------------------------------------------------------------------
 1 -  6       Record name   "OBSLTE" or "SPRSDE"
 9 - 10       Continuation  continuation  Allows concatenation of multiple records
12 - 20       Date          repDate       Date that this entry was replaced.
22 - 25       IDcode        idCode        ID code of this entry.
32 - 35       IDcode        rIdCode       ID code of entry that replaced this one.
37 - 40       IDcode        rIdCode       ID code of entry that replaced this one.
42 - 45       IDcode        rIdCode       ID code of entry  that replaced this one.
47 - 50       IDcode        rIdCode       ID code of entry that replaced this one.
52 - 55       IDcode        rIdCode       ID code of entry that replaced this one.
57 - 60       IDcode        rIdCode       ID code of entry that replaced this one.
62 - 65       IDcode        rIdCode       ID code of entry that replaced this one.
67 - 70       IDcode        rIdCode       ID code of entry that replaced this one.
72 - 75       IDcode        rIdCode       ID code of entry that replaced this one.
--}

{-# INLINE obsoletingFields #-}
obsoletingFields = [(6,  mKeywords "record header"     ["OBSLTE", "SPRSDE"]),
                    (8,  mSpc      2                                       ),
                    (10, dInt      "continuation"      0                   ),
                    (11, mSpc      1                                       ),
                    (20, mStr      "date"                                  ),
                    (21, mSpc      1                                       ),
                    (25, mStr      "PDB code of this entry"                ),
                    (31, mSpc      6                                       ),
                    (75, mStr      "PDB codes of relevant entries"         )]


-- | Parses an OBSLTE or SPRSDE record:
--
-- Arguments:
--
-- (1) a PDBEvent constructor:
--
--    * OBSLTE or
--
--    * SPRSDE
--
-- (2) input line
--
-- (3) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseObsoleting cons line line_no = return $ if errs == []
                                               then [result]
                                               else errs
  where
    -- parse
    (fields, errs) = parseFields obsoletingFields line line_no
    [fRec, _, fCont, _, fDate, _, fThisEntry, _, fOtherEntries] = fields
    IFInt cont         = fCont
    IFStr date         = fDate
    IFStr thisEntry    = fThisEntry
    IFStr otherEntries = fOtherEntries
    pdbids = filter (/="") $ BS.split ' ' otherEntries

    -- unpack fields
    result = cons cont date thisEntry pdbids

-- | Parses a OBSLTE record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseOBSLTE ::  (Monad m) => String -> Int -> m [PDBEvent]
parseOBSLTE = parseObsoleting OBSLTE

-- | Parses a SPRSDE record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSPRSDE ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSPRSDE = parseObsoleting SPRSDE

--------------- }}} Obsoleting records

