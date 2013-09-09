{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- | Parsing SSBOND records.
module Bio.PDB.EventParser.ParseSSBOND(parseSSBOND)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SSBOND records
{--
COLUMNS        DATA  TYPE     FIELD            DEFINITION
--------------------------------------------------------------------------------
 1 -  6        Record name    "SSBOND"
 8 - 10        Integer        serNum           Serial number.
12 - 14        LString(3)     "CYS"            Residue name.
16             Character      chainID1         Chain identifier.
18 - 21        Integer        seqNum1          Residue sequence number.
22             AChar          icode1           Insertion code.
26 - 28        LString(3)     "CYS"            Residue name.
30             Character      chainID2         Chain identifier.
32 - 35        Integer        seqNum2          Residue sequence number.
36             AChar          icode2           Insertion code.
60 - 65        SymOP          sym1             Symmetry operator for residue 1.
67 - 72        SymOP          sym2             Symmetry operator for residue 2.
74 – 78        Real(5.2)      Length           Disulfide bond distance
--}

{-# INLINE ssbondFields #-}
ssbondFields = [(6,  mKeyword "record header"     "SSBOND"            ),
                (7,  mSpc     1                                       ),
                (10, mInt     "record serial number"                  ),
                (11, mSpc     1                                       ),
                (14, mKeyword "residue name 1"    "CYS"               ),
                (15, mSpc     1                                       ),
                (16, mChr     "chain id 1"                            ),
                (17, mSpc     1                                       ),
                (21, mInt     "residue sequence number 1"             ),
                (22, mChr     "insertion code"                        ),
                (25, mSpc     3                                       ),
                (28, mKeyword "residue name 2"    "CYS"               ),
                (29, mSpc     1                                       ),
                (30, mChr     "chain id 2"                            ),
                (31, mSpc     1                                       ),
                (35, mInt     "residue sequence number 2"             ),
                (36, mChr     "insertion code 2"                      ),
                (59, pSpc                                             ),
                (65, pStr     "symmetry operator for residue 1"       ),
                (66, pSpc                                             ),
                (72, pStr     "symmetry operator for residue 2"       ),
                (78, dDouble   "disulfide bond length"  2.05           )]

-- | Parses a SSBOND record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSSBOND ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSSBOND line line_no = return $ if null errs
                                   then [result]
                                   else errs
  where
    -- parse
    errs = fErrs ++ fgErrs
    (fields, fErrs) = parseFields ssbondFields line line_no
    [fRec, _, fSerial, _,
     fResname1, _, fChain1, _, fResnum1, fInsCode1, _,
     fResname2, _, fChain2, _, fResnum2, fInsCode2, _,
     fSymOp1,   _, fSymOp2, fBondLen] = fields
    IFInt   serial   = fSerial
    IFStr   symOp1   = fSymOp1
    IFStr   symOp2   = fSymOp2
    IFDouble bondLen  = fBondLen
    
    fgErrs       = liftFgErrs line_no [fgRes1, fgRes2]
    fgRes1       = fgResidue False "first residue of SSBOND"  14 (IFStr "CYS") fChain1 fResnum1 fInsCode1
    fgRes2       = fgResidue False "second residue of SSBOND" 28 (IFStr "CYS") fChain2 fResnum2 fInsCode2
    [res1, res2] = rights [fgRes1, fgRes2]

    -- unpack fields
    result = SSBOND serial res1 res2 symOp1 symOp2 bondLen

--------------- }}} SSBOND records

