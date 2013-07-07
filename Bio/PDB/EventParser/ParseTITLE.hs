{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Parse a TITLE record.
module Bio.PDB.EventParser.ParseTITLE(parseTITLE)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ TITLE records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6       Record name    "TITLE "
 9 - 10       Continuation   continuation  Allows concatenation of multiple records.
11 - 80       String         title         Title of the  experiment.
--}

titleFields = [(6,  mKeyword "record header" "TITLE "),
               (8,  mSpc                     2),
               (10, dInt     "continuation" 0),
--               (10, pInt     "continuation"),
               (80, pStr     "title")]

{-# SPECIALIZE parseTITLE :: BS.ByteString -> Int -> IO [PDBEvent] #-}
-- | Parses a TITLE record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseTITLE :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseTITLE line line_no = return $ if errs == []
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, errs) = parseFields titleFields line line_no
    [fRec, fSpc, fCont, fTitle] = fields
    -- unpack fields
    IFInt cont  = fCont
    IFStr title = fTitle
    result = TITLE { continuation = cont,
                     title        = title }

-- NOTE: consecutive "TITLE" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} TITLE records

