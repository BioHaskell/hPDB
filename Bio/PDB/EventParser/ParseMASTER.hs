{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Parsing MASTER records.
module Bio.PDB.EventParser.ParseMASTER(parseMASTER)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ MASTER records
{--
COLUMNS         DATA TYPE     FIELD          DEFINITION
----------------------------------------------------------------------------------
 1 -  6         Record name   "MASTER"
11 - 15         Integer       numRemark      Number of REMARK records
16 - 20         Integer       "0"
21 - 25         Integer       numHet         Number of HET records
26 - 30         Integer       numHelix       Number of HELIX records
31 - 35         Integer       numSheet       Number of SHEET records
36 - 40         Integer       numTurn        deprecated
41 - 45         Integer       numSite        Number of SITE records
46 - 50         Integer       numXform       Number of coordinate transformation
                                             records  (ORIGX+SCALE+MTRIX)
51 - 55         Integer       numCoord       Number of atomic coordinate records
                                             records (ATOM+HETATM)
56 - 60         Integer       numTer         Number of MASTER records
61 - 65         Integer       numConect      Number of CONECT records
66 - 70         Integer       numSeq         Number of SEQRES records
--}


{-# INLINE masterFields #-}
-- | Fields of the MASTER record
masterFields ::  [(Int, String -> ParsedField)]
masterFields = [(6,  mKeyword "record header" "MASTER"   ),
                (10, mSpc     4                          ),
                (15, mInt     "number of REMARK records" ),
                (20, mInt     "<zero>"                   ),
                (25, mInt     "number of HET records"    ),
                (30, mInt     "number of HELIX  records" ),
                (35, mInt     "number of SHEET  records" ),
                (40, pInt     "number of TURN records"   ), -- deprecated
                (45, mInt     "number of SITE   records" ),
                (50, mInt     "number of coordinate transform records (ORIGX, SCALE,MTRIX)"),
                (55, mInt     "number of atomic coordinate records (ATOM, HETATM)"),
                (60, mInt     "number of MASTER records" ),
                (65, mInt     "number of CONECT records" ),
                (70, mInt     "number of SEQRES records" )]


-- | Parses a MASTER record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a mionad action returning a list of 'PDBEvent's.
parseMASTER line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    (fields, errs) = parseFields masterFields line line_no
    [fRec, _, fNumRemark, fZero, fNumHet, fNumHelix, fNumSheet, fNumTurn,
     fNumSite, fNumXform, fNumAts, fNumMaster, fNumConect, fNumSeqres] = fields
    IFInt  numRemark = fNumRemark
    IFInt  0         = fZero
    IFInt  numHet    = fNumHet
    IFInt  numHelix  = fNumHelix
    IFInt  numSheet  = fNumSheet
    IFInt  numTurn   = fNumTurn
    IFInt  numSite   = fNumSite
    IFInt  numXform  = fNumXform
    IFInt  numAts    = fNumAts
    IFInt  numMaster = fNumMaster
    IFInt  numConect = fNumConect
    IFInt  numSeqres = fNumSeqres

    -- unpack fields
    result = MASTER numRemark numHet numHelix numSheet numTurn
                    numSite numXform numAts numMaster numConect
                    numSeqres

--------------- }}} MASTER records

