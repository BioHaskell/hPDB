{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings  #-}

-- Parsing of records that contain simple list of values: KEYWDS, AUTHOR, MDLTYP, EXPDTA.
module Bio.PDB.EventParser.ParseListRecord(parseKEYWDS,parseAUTHOR,parseMDLTYP,parseEXPDTA)
where

import Prelude hiding (String)

import qualified Data.ByteString.Char8 as BS

-- Output data structure
import Bio.PDB.EventParser.PDBEvents hiding (String)
import qualified Bio.PDB.EventParser.ExperimentalMethods as ExperimentalMethods

-- Helper methods
import Bio.PDB.EventParser.PDBParsingAbstractions


-- | String type used all over the library.
type String = BS.ByteString

--------------- {{{ List containing records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6       Record name    "AUTHOR", "KEYWDS", "MDLTYP" or "EXPDTA"
 9 - 10       Continuation   continuation  Allows concatenation of multiple records.
11 - 80       String         title         Title of the  experiment.
--}

-- | Fields of the KEYWDS, AUTHOR, EXPDTA and MDLTYP records.
listFields = [(6,  mKeywords "record header" ["KEYWDS",
                                              "AUTHOR",
                                              "EXPDTA",
                                              "MDLTYP"]),
              (8,  mSpc                      2    ),
              (10, dInt      "continuation"  0    ),
              (80, pStr      "list"               )]

-- | Parses a record that contains a list of values.
--
-- Arguments:
--
-- (1) a matrix constructor
--
-- (2) a separator characteristic for this type of record
--
-- (3) input line
--
-- (4) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseListRecord :: (Monad m) =>(Int -> [String] -> PDBEvent)-> Char-> String-> Int-> m [PDBEvent]
parseListRecord cons sep line line_no = return $ if errs == []
                                                   then [result]
                                                   else errs
  where
    -- parse
    (fields, errs) = parseFields listFields line line_no
    [fRec, fSpc, fCont, fList] = fields
    -- unpack fields
    IFInt cont  = fCont
    IFStr aList = fList
    words = map trim $ BS.split sep aList
    result :: PDBEvent = cons cont words

-- | Parses a KEYWDS record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseKEYWDS :: (Monad m) => String -> Int -> m [PDBEvent]
parseKEYWDS = parseListRecord KEYWDS ','

-- | Parses a AUTHOR record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseAUTHOR :: (Monad m) => String -> Int -> m [PDBEvent]
parseAUTHOR = parseListRecord AUTHOR ','

-- | Parses a EXPDTA record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseEXPDTA :: (Monad m) => String -> Int -> m [PDBEvent]
parseEXPDTA = parseListRecord mkEXPDTA ';'
  where mkEXPDTA cont aList = EXPDTA cont $ map (ExperimentalMethods.mkExpMethod .
                                                 BS.words) aList
-- | Parses a MDLTYP record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseMDLTYP :: (Monad m) => String -> Int -> m [PDBEvent]
parseMDLTYP = parseListRecord MDLTYP ';'

-- NOTE: Consecutive AUTHOR and KEYWDS records should be merged.
--------------- }}} List containing records

