{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parse HETNAM and HETSYN records
module Bio.PDB.EventParser.ParseHETNAM(parseHETNAM)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ HETNAM/HETSYN records
{--
COLUMNS       DATA  TYPE    FIELD           DEFINITION
----------------------------------------------------------------------------
 1 -  6       Record name   "HETNAM" or "HETSYN"
 9 - 10       Continuation  continuation    Allows concatenation of multiple records.
12 - 14       LString(3)    hetID           Het identifier, right-justified.
16 - 70       String        text            Chemical name.
--}

{-# INLINE hetnamFields #-}
hetnamFields = [(6,  mKeywords "record header"     ["HETNAM", "HETSYN"]),
                (8,  mSpc      2                                       ),
                (10, dInt      "continuation"      0                   ),
                (11, mSpc      1                                       ),
                (14, mStr      "Het identifier, right-justified"       ),
                (15, mSpc      1                                       ),
                (70, mStr      "chemical name"                         )]

-- | Parses a HETNAM or HETSYN record.
--
-- Arguments:
--
-- (1) boolean indicating, if it is HETNAM ('True') or HETSYN ('False') record
--
-- (2) input line
--
-- (3) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseHETNAM :: (Monad m) => Bool -> String -> Int -> m [PDBEvent]
parseHETNAM isNameNotSynonym line line_no = return $ if errs == []
                                                       then [result]
                                                       else errs
  where
    -- parse
    (fields, errs) = parseFields hetnamFields line line_no
    [fRec, _, fCont, _, fHetId, _, fName] = fields
    IFInt cont    = fCont
    IFStr hetId   = fHetId
    IFStr name    = fName

    -- unpack fields
    result = HETNAM cont hetId name isNameNotSynonym

--------------- }}} HETNAM/HETSYN records

