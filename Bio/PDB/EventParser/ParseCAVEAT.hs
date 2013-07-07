{---# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings   #-}

-- | Parsing CAVEAT records.
module Bio.PDB.EventParser.ParseCAVEAT(parseCAVEAT)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ CAVEAT records
{--
COLUMNS       DATA  TYPE    FIELD          DEFINITION
---------------------------------------------------------------------------------------
  1 - 6       Record name   "CAVEAT"
 9 - 10       Continuation  continuation   Allows concatenation of multiple records.
12 - 15       IDcode        idCode         PDB ID code of this entry.
20 - 79       String        comment        Free text giving the reason for the  CAVEAT.
--}

{-# INLINE caveatFields #-}
caveatFields = [(6,  mKeyword "record header"     "CAVEAT"),
                (8,  mSpc     2                           ),
                (10, dInt     "continuation"      0       ),
                (11, mSpc     1                           ),
                (15, mStr     "PDB entry id code"         ),
                (19, mSpc     4                           ),
                (79, mStr     "comment"                   )]

-- | Parses a CAVEAT record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
parseCAVEAT ::  (Monad m) => String -> Int -> m [PDBEvent]
-- Result is a monad action returning a list of 'PDBEvent's.
parseCAVEAT line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    (fields, errs) = parseFields caveatFields line line_no
    [fRec, _, fCont, _, fPDBId, _, fComment] = fields
    IFInt cont    = fCont
    IFStr pdbid   = fPDBId
    IFStr comment = fComment

    -- unpack fields
    result = CAVEAT cont pdbid comment

--------------- }}} CAVEAT records

