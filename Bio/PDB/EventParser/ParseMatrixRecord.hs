{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

-- | Parsing of records on scale (SCALEn), origin (ORIGXn) and transformation (MTRIXn.)
module Bio.PDB.EventParser.ParseMatrixRecord(parseSCALEn, parseORIGXn, parseMTRIXn)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS
import Control.Exception(assert)

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ Matrix records (SCALEn, ORIGXn, MTRIXn)
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6       Record name    "SCALE[123]", "ORIGX[123]" or "MTRIX[123]"
11 - 20       Real(10.6)      o[n][1]  
21 - 30       Real(10.6)      o[n][2]  
31 - 40       Real(10.6)      o[n][3]  
46 - 55       Real(10.5)      t[n]     
60            Integer                      1 if coordinates for the related molecule are present;
                                           otherwise, blank.
                                           <MTRIXn> only
--}

titleFields = [(5,  mKeywords "record header" ["SCALE", "ORIGX", "TVECT", "MTRIX"]),
               (6,   mInt     "recno"                       ),
               (7,   mSpc     1                             ),
               (10,  dInt     "serial number"             0 ),
               (20,  mDouble   "o[n][1]"                     ),
               (30,  mDouble   "o[n][2]"                     ),
               (40,  mDouble   "o[n][3]"                     ),
               (45,  mSpc                                 5 ),
               (55,  mDouble   "t[n]"                        ),
               (59,  pSpc                                   ),
               (60,  dInt     "related molecule present?" 0 )]

-- | Parses a single line with matrix or vector information.
--
-- Arguments:
--
-- (1) record constructor: SCALEn, ORIGXn, MTRIXn
--
-- (2) input line
--
-- (3) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseMatrixRecord :: (Monad m) =>(Int -> Bool -> Int -> [V3 Double] -> [Double] -> PDBEvent)-> String-> Int-> m [PDBEvent]
parseMatrixRecord cons line line_no = return $ if null errs
                                                 then [result]
                                                 else errs
  where
    -- parse
    (fields, errs) = parseFields titleFields line line_no
    [fRec, fRecNo, _, fSerial, fo1, fo2, fo3, fSpc2, ft, fSpc3, fRelMol] = fields
    -- unpack fields
    IFInt   serial = fSerial
    IFInt   n      = fRecNo
    IFDouble o1     = fo1
    IFDouble o2     = fo2
    IFDouble o3     = fo3
    IFDouble t      = ft
    IFInt   relMol = fRelMol
    result = cons serial (relMol==1) n [V3 o1 o2 o3] [t]

-- | Parses a SCALEn record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSCALEn ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSCALEn = parseMatrixRecord (\s _ -> assert (s==0) SCALEn)

-- | Parses a ORIGXn record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseORIGXn ::  (Monad m) => String -> Int -> m [PDBEvent]
parseORIGXn = parseMatrixRecord (\s _ -> assert (s==0) ORIGXn)

-- | Parses a MTRIXn record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseMTRIXn ::  (Monad m) => String -> Int -> m [PDBEvent]
parseMTRIXn = parseMatrixRecord MTRIXn

-- NOTE: consecutive "TITLE" records should be merged into a single multiline entry with SUCH method
--mergeMatrixRecords :: [PDBEvent] -> m [PDBEvent]

--------------- }}} Matrix records

