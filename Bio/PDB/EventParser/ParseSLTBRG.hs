{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing of SLTBRG records.
module Bio.PDB.EventParser.ParseSLTBRG(parseSLTBRG)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SLTBRG records
{--
COLUMNS       DATA TYPE       FIELD         DEFINITION
---------------------------------------------------------------------------------
 1 -  6       Record name     "SLTBRG"
13 - 16       Atom            atom1         First atom name.
17            Character       altLoc1       Alternate location indicator.
18 - 20       Residue name    resName1      Residue name.
22            Character       chainID1      Chain identifier.
23 - 26       Integer         resSeq1       Residue sequence number.
27            AChar           iCode1        Insertion code.
43 - 46       Atom            atom2         Second atom name.
47            Character       altLoc2       Alternate location indicator.
48 - 50       Residue name    resName2      Residue name.
52            Character       chainID2      Chain identifier.
53 - 56       Integer         resSeq2       Residue sequence number.
57            AChar           iCode2        Insertion code.
60 - 65       SymOP           sym1          Symmetry operator for 1st atom.
67 - 72       SymOP           sym2          Symmetry operator for 2nd atom.
--}

{-# INLINE sltbrgFields #-}
sltbrgFields = [(6,  mKeyword "record header"     "SLTBRG"            ),
                (12, mSpc     6                                       ),
                (16, mStr     "first atom name"                       ),
                (17, mChr     "alternate location indicator 1"        ),
                (20, mStr     "residue name 1"                        ),
                (21, mSpc     1                                       ),
                (22, mChr     "chain identifier 1"                    ),
                (26, mInt     "residue 1 sequence number"             ),
                (27, mChr     "insertion code"                        ),
                (42, mSpc     15                                      ),
                (46, mStr     "second atom name"                      ),
                (47, mChr     "alternate location indicator 2"        ),
                (50, mStr     "residue name 2"                        ),
                (51, mSpc     1                                       ),
                (52, mChr     "chain identifier 2"                    ),
                (56, mInt     "residue sequence number 2"             ),
                (57, mChr     "insertion code 2"                      ),
                (59, pSpc                                             ),
                (65, pStr     "symmetry operator for residue 1"       ),
                (66, pSpc                                             ),
                (72, pStr     "symmetry operator for residue 2"       )]

-- | Parses a SLTBRG record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSLTBRG ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSLTBRG line line_no = return $ if null errs
                                   then [result]
                                   else errs
  where
    -- parse
    errs = fErrs ++ fgErrs
    (fields, fErrs) = parseFields sltbrgFields line line_no
    [fRec, _,
     fAtomName1, fAltLoc1, fResname1, _, fChain1, fResnum1, fInsCode1, _,
     fAtomName2, fAltLoc2, fResname2, _, fChain2, fResnum2, fInsCode2, _,
     fSymOp1,   _, fSymOp2] = fields
    IFChar  altloc1  = fAltLoc1
    IFChar  altloc2  = fAltLoc2
    IFStr   symOp1   = fSymOp1
    IFStr   symOp2   = fSymOp2
    
    fgErrs         = liftFgErrs line_no [fgAtom1, fgAtom2]
    fgAtom1        = fgAtom "first residue of SLTBRG"  16 fAtomName1 fResname1 fChain1 fResnum1 fInsCode1
    fgAtom2        = fgAtom "second residue of SLTBRG" 46 fAtomName1 fResname1 fChain2 fResnum2 fInsCode2
    [atom1, atom2] = rights [fgAtom1, fgAtom2]

    -- unpack fields
    result = SLTBRG atom1 altloc1 atom2 altloc2 symOp1 symOp2

--------------- }}} SLTBRG records

