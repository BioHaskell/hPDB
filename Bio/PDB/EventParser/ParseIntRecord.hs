{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Parsing of records with a single integer value: NUMMDL and MODEL.
module Bio.PDB.EventParser.ParseIntRecord(parseNUMMDL,parseMODEL)
where

import Prelude hiding (String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ NUMMDL or MODEL records
{--
COLUMNS      DATA TYPE      FIELD         DEFINITION                           
------------------------------------------------------------------------------------
 1 -  6      Record name    "NUMMDL" or "MODEL "
11 - 14      Int            modelNumber   Number of models.   
--}

nummdlFields = [(6,  mKeywords "record header" ["NUMMDL", "MODEL "]),
                (10, mSpc                      4                   ),
                (80, mInt     "number"                             )]

-- | Parses a record with a single integer.
--
-- Arguments:
--
-- (1) constructor
--
-- (2) input line
--
-- (3) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseIntRecord :: (Monad m) => (Int -> PDBEvent) ->  String -> Int -> m [PDBEvent]
parseIntRecord cons line line_no = return $ if errs == []
                                              then [result]
                                              else errs
  where
    -- parse
    (fields, errs) = parseFields nummdlFields line line_no
    [fRec, fSpc, fNumber] = fields
    -- unpack fields
    IFInt num = fNumber
    result = cons num

-- | Parses a NUMMDL record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseNUMMDL ::  (Monad m) => String -> Int -> m [PDBEvent]
parseNUMMDL = parseIntRecord NUMMDL

-- | Parses a MODEL record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseMODEL ::  (Monad m) => String -> Int -> m [PDBEvent]
parseMODEL  = parseIntRecord MODEL

--------------- }}} NUMMDL or MODEL records

