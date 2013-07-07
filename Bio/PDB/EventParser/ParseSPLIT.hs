{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parsing SPLIT records.
module Bio.PDB.EventParser.ParseSPLIT(parseSPLIT)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SPLIT records
{--
COLUMNS        DATA TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6        Record  name  "SPLIT "
 9 - 10        Continuation  continuation  Allows concatenation of multiple records.
12 - 15        IDcode        idCode        ID code of related entry.
17 - 20        IDcode        idCode        ID code of related entry.
22 - 25        IDcode        idCode        ID code of related entry.
27 – 30        IDcode        idCode        ID code of related entry.
32 - 35        IDcode        idCode        ID code of related entry.
37 - 40        IDcode        idCode        ID code of related entry.
42 - 45        IDcode        idCode        ID code of related entry.
47 - 50        IDcode        idCode        ID code of related entry.
52 - 55        IDcode        idCode        ID code of related entry.
57 - 60        IDcode        idCode        ID code of related entry.
62 - 65        IDcode        idCode        ID code of related entry.
67 - 70        IDcode        idCode        ID code of related entry.
72 - 75        IDcode        idCode        ID code of related entry.
77 - 80        IDcode        idCode        ID code of related entry.
--}

titleFields = [(6,  mKeyword "record header" "SPLIT "),
               (8,  mSpc                     2),
               (10, dInt     "continuation"  0),
               (80, pStr     "PDB id codes needed for whole complex")]

{-# SPECIALIZE parseSPLIT :: BS.ByteString -> Int -> IO [PDBEvent] #-}
-- | Parses a SPLIT record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSPLIT :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseSPLIT line line_no = return $ if errs == []
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, errs) = parseFields titleFields line line_no
    [fRec, fSpc, fCont, fCodeString] = fields
    -- unpack fields
    IFInt cont       = fCont
    IFStr codeString = fCodeString
    codes = filter (/="") $ BS.split ' ' codeString
    
    result = SPLIT cont codes 

-- NOTE: consecutive "SPLIT" records should be merged into a single multiline entry with SUCH method

--------------- }}} SPLIT records

