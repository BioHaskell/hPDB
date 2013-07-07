{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parsing of records with journal references.
module Bio.PDB.EventParser.ParseJRNL(parseJRNL, parseREMARK1)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ JRNL records
{--
COLUMNS       DATA  TYPE     FIELD               DEFINITION                         
-------------------------------------------------------------------------------
 1 -  6       Record name    "REMARK"                                          
10            LString(1)     "1"                                               
13 - 16       LString(4)     "AUTH"              
17 - 18       Continuation   continuation        Allows  a long list of authors.     
20 - 79       List           authorList          List of the authors.               
--}

titleFields = [(10, mKeywords "record header"    ["JRNL      ",
                                                  "REMARK   1"]  ),
               (12, mSpc                         2               ),
               (16, mKeywords "subrecord header" ["AUTH", "TITL",
                                                  "REF ", "REFN",
                                                  "PUBL", "PMID",
                                                  "DOI ", "EDIT"]), -- EDIT is from old (2.2) file format description
               (18, dInt      "continuation"     0               ),
               (19, mSpc                         1               ),
               (80, mStr      "content"                          )]

{-# SPECIALIZE parseJRNL :: BS.ByteString -> Int -> IO [PDBEvent] #-}
-- | Parses a JRNL or REMARK 1 record that contains a journal reference.
--
-- Arguments:
--
-- (1) boolean indicating, if it is first reference for a structure (JRNL)
-- or any other (REMARK 1.)
--
-- (2) input line
--
-- (3) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseJournalRef :: (Monad m) => Bool -> BS.ByteString -> Int -> m [PDBEvent]
parseJournalRef isFirst line line_no = return $ if errs == []
                                                  then [result]
                                                  else errs
  where
    -- parse
    (fields, errs) = parseFields titleFields line line_no
    [fRec, _, fSubrec, fCont, _, fContent] = fields
    -- unpack fields
    IFStr subrec  = fSubrec
    IFInt cont    = fCont
    IFStr content = fContent
    result = JRNL cont [(subrec, content)] isFirst

-- | Parses a JRNL record that contains a journal reference.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseJRNL :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseJRNL = parseJournalRef True

-- | Parses a REMARK 1 record that contains a journal reference.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseREMARK1 :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseREMARK1 = parseJournalRef False

-- NOTE: consecutive "JRNL" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} JRNL records

