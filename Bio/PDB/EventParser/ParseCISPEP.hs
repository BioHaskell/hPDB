{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns #-}

-- | Parsing a CISPEP record.
module Bio.PDB.EventParser.ParseCISPEP(parseCISPEP)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ CISPEP records
{--
COLUMNS       DATA  TYPE    FIELD         DEFINITION
-------------------------------------------------------------------------
 1 -  6       Record name   "CISPEP"
 8 - 10       Integer       serNum        Record serial number.
12 - 14       LString(3)    pep1          Residue name.
16            Character     chainID1      Chain identifier.
18 - 21       Integer       seqNum1       Residue sequence number.
22            AChar         icode1        Insertion code.
26 - 28       LString(3)    pep2          Residue name.
30            Character     chainID2      Chain identifier.
32 - 35       Integer       seqNum2       Residue sequence number.
36            AChar         icode2        Insertion code.
44 - 46       Integer       modNum        Identifies the specific model.
54 - 59       Real(6.2)     measure       Angle measurement in degrees.
--}

{-# INLINE cispepFields #-}
cispepFields = [(6,  mKeyword "record header"     "CISPEP"            ),
                (7,  mSpc     1                                       ),
                (10, mInt     "record serial number"                  ),
                (11, mSpc     1                                       ),
                (14, mStr     "residue name in first peptide"         ),
                (15, mSpc     1                                       ),
                (16, mChr     "chain id"                              ),
                (17, mSpc     1                                       ),
                (21, mInt     "residue sequence number"               ),
                (22, mChr     "insertion code"                        ),
                (25, mSpc     3                                       ),
                (28, mStr     "residue name in second peptide"        ),
                (29, mSpc     1                                       ),
                (30, mChr     "chain id"                              ),
                (31, mSpc     1                                       ),
                (35, mInt     "residue sequence number"               ),
                (36, mChr     "insertion code"                        ),
                (43, pSpc                                             ),
                (46, dInt     "model identifier"  1                   ),
                (53, pSpc                                             ),
                (59, pDouble   "angle measurement"                     )]

-- | Parses a CISPEP record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseCISPEP ::  (Monad m) => String -> Int -> m [PDBEvent]
parseCISPEP line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    errs = fErrs ++ fgErrs
    (fields, fErrs) = parseFields cispepFields line line_no
    [fRec, _, fSerial, _,
     fResname1, _, fChain1, _, fResnum1, fInsCode1, _,
     fResname2, _, fChain2, _, fResnum2, fInsCode2, _,
     fModNum,   _, fAngle] = fields
    IFInt   serial   = fSerial
    IFInt   modnum   = fModNum
    angle            = case fAngle of
                         IFDouble !f -> Just f
                         IFNone     -> Nothing

    fgRes1 = fgResidue False "residue 1" 14 fResname1 fChain1 fResnum1 fInsCode1
    fgRes2 = fgResidue False "residue 2" 28 fResname2 fChain2 fResnum2 fInsCode2
    fgErrs = liftFgErrs line_no [fgRes1, fgRes2]
    [res1, res2] = rights [fgRes1, fgRes2]

    -- unpack fields
    result = CISPEP serial res1 res2 modnum angle

--------------- }}} CISPEP/CISPEPSYN records

