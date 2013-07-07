{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

-- | Parsing of <<speclist>> type of records: COMPND and SOURCE.
module Bio.PDB.EventParser.ParseSpecListRecord(parseCOMPND, parseSOURCE)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SpecListRecord records
{--
COLUMNS       DATA TYPE       FIELD         DEFINITION 
----------------------------------------------------------------------------------
 1 -  6       Record name     "COMPND"   
 8 - 10       Continuation    continuation  Allows concatenation of multiple records.
11 - 80       Specification   compound      Description of the molecular components.
              list 
--}
-- TODO: Parse value types in COMPND records

{-# INLINE specListRecordFields #-}
specListRecordFields = [
                (6,  mKeywords "record header"      ["SOURCE", "COMPND"]),
                (7,  mSpc      1                                        ),
                (10, dInt      "continuation"       0                   ),
                (80, mStr      "specification list"                     )]

{-# INLINE parseSpecListRecord #-}
-- NOTE: SpecLists need to be parsed after merging!!!
--{-# SPECIALIZE parseSpecListRecord :: String -> Int -> IO [PDBEvent] #-}
parseSpecListRecord cons line line_no = return $
  if errs /= []
    then errs
    else [result]
  where
    -- parse
    errs :: [PDBEvent]
    errs = if fErrs /= [] then fErrs else pErrs
    (fields, fErrs)             = parseFields specListRecordFields line line_no
    [fRec, _, fCont, fSpecList] = fields
    IFInt cont                  = fCont
    IFStr specListString        = fSpecList
    
    -- unpack fields
    result = cons cont entries
    entries = map entry preEntries
    preEntries = map (BS.split ':') .
                 filter (/= "") $ ';' `BS.split` specListString
    entry [a, b] = (a, b)
    entry [   b] = ("", b) -- NOTE: continuation of previous entry
    pErrs :: [PDBEvent]
    pErrs  = concatMap checkEntry preEntries
    checkEntry :: [String] -> [PDBEvent]
    checkEntry [_, _] = []
    checkEntry [   _] = []
    checkEntry entry  = [PDBParseError line_no 11 {- column where speclist begins -} $
                         ("Cannot parse specification list fragment: " `BS.append`
                          ( BS.pack . show $ entry))]

-- | Parses a COMPND record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseCOMPND ::  (Monad m) => String -> Int -> m [PDBEvent]
parseCOMPND = parseSpecListRecord COMPND

-- | Parses a SOURCE record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
--
-- NOTE: SOURCE record values are always Strings
parseSOURCE ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSOURCE = parseSpecListRecord SOURCE

--------------- }}} SpecListRecord records
