{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parse a generic REMARK record.
module Bio.PDB.EventParser.ParseREMARK(parseREMARK)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions
import Bio.PDB.EventParser.ParseJRNL(parseREMARK1) -- supplementary references follow JRNL record format!


--------------- {{{ REMARK records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
COLUMNS       DATA TYPE     FIELD         DEFINITION
--------------------------------------------------------------------------------------
 1 -  6       Record name   "REMARK"
 8 - 10       Int       remarkNum     Remark  number. It is not an error for
                                          remark n to exist in an entry when
                                          remark n-1 does not.
12 - 70       LString       empty         Left  as white space in first line
                                          of each  new remark.
--}

remarkFields = [(6,  mKeyword "record header" "REMARK"),
                (7,  mSpc                     1       ),
                (10, dInt     "remark number" 0       ),
                (11, pSpc                             ),
                (80, pStr     "text"                  )]

-- | Parses a _generic_ REMARK record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseREMARK :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseREMARK line line_no = 
    if errs == []
      then (if (num == 1)                             && 
               not (BS.all (==' ') text)              &&
               not ("REFERENCE" `BS.isPrefixOf` text)
              then parseREMARK1 line line_no
              else return [result])
      else return errs
  where
    -- parse
    (fields, errs) = parseFields remarkFields line line_no
    [fRec, fSpc1, fNum, fSpc2, fText] = fields
    -- unpack fields
    IFInt num  = fNum
    IFStr text = fText
    result = REMARK { num  = num,
                      text = [text] }

-- NOTE: consecutive "REMARK" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} REMARK records

