{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing hydrogen bond records.
module Bio.PDB.EventParser.ParseHYDBND(parseHYDBND)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ HYDBND records
{--
COLUMNS       DATA TYPE       FIELD         DEFINITION
---------------------------------------------------------------------------------
 1 -  6        Record name     "HYDBND"
13 - 16        Atom            name1          Atom name.
17             Character       altLoc1        Alternate location indicator.
18 - 20        Residue name    resName1       Residue name.
22             Character       Chain1         Chain identifier.
23 - 27        Integer         resSeq1        Residue sequence number.
28             AChar           ICode1         Insertion code.
30 - 33        Atom            nameH          Hydrogen atom name.
34             Character       altLocH        Alternate location indicator.
36             Character       ChainH         Chain identifier.
37 - 41        Integer         resSeqH        Residue sequence number.
42             AChar           iCodeH         Insertion code.
44 - 47        Atom            name2          Atom name.
48             Character       altLoc2        Alternate location indicator.
49 - 51        Residue name    resName2       Residue name.
53             Character       chainID2       Chain identifier.
54 - 58        Integer         resSeq2        Residue sequence number.
59             AChar           iCode2         Insertion code.
60 - 65        SymOP           sym1           Symmetry operator for 1st
67 - 72        SymOP           sym2           Symmetry operator for 2nd
                                              non-hydrogen atom.
--}

{-# INLINE hydbndFields #-}
hydbndFields = [(6,  mKeyword "record header"     "HYDBND"            ),
                (12, mSpc     6                                       ),
                (16, mStr     "first atom name"                       ),
                (17, mChr     "alternate location indicator 1"        ),
                (20, mStr     "residue name 1"                        ),
                (21, mSpc     1                                       ),
                (22, mChr     "chain identifier 1"                    ),
                (27, mInt     "residue 1 sequence number"             ),
                (28, mChr     "insertion code"                        ),
                (29, mSpc     1                                       ),
                (33, mStr     "hydrogen atom name"                    ),
                (34, mChr     "hydrogen atom alternate location indicator"),
                (35, mSpc     1                                       ),
                (36, mChr     "hydrogen atom chain identifier"        ),
                (41, dInt     "hydrogen atom residue sequence number" (-1)),
                (42, mChr     "hydrogen atom insertion code"          ),
                (43, mSpc     1                                       ),
                (47, mStr     "second atom name"                      ),
                (48, mChr     "alternate location indicator 2"        ),
                (51, mStr     "residue name 2"                        ),
                (52, mSpc     1                                       ),
                (53, mChr     "chain identifier 2"                    ),
                (58, mInt     "residue sequence number 2"             ),
                (59, dChr     "insertion code 2" ' '                  ),
                (65, pStr     "symmetry operator for residue 1"       ),
                (66, pSpc                                             ),
                (72, pStr     "symmetry operator for residue 2"       )]

-- | Parses a HYDBND record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseHYDBND :: (Monad m) => String -> Int -> m [PDBEvent]
parseHYDBND line line_no = return $ if null errs
                                   then [result]
                                   else errs -- return $ [PDBParseError 0 0 $ BS.pack $ show $ Prelude.length fields]
  where
    -- parse
    errs = fErrs ++ fgErrs
    (fields, fErrs) = parseFields hydbndFields line line_no
    [fRec, _,
     fAtomName1, fAltLoc1, fResname1, _, fChain1, fResnum1, fInsCode1, _,
     fAtomNameH, fAltLocH,            _, fChainH, fResnumH, fInsCodeH, _,
     fAtomName2, fAltLoc2, fResname2, _, fChain2, fResnum2, fInsCode2,
     fSymOp1, _, fSymOp2] = fields
    IFChar  altloc1  = fAltLoc1
    IFChar  altlocH  = fAltLocH
    IFChar  altloc2  = fAltLoc2
    IFStr   symOp1   = fSymOp1
    IFStr   symOp2   = fSymOp2
    
    fgErrs         = liftFgErrs line_no [fgAtom1, fgAtomH, fgAtom2]
    fgAtom1        = fgAtom "first atom of HYDBND"  16 fAtomName1 fResname1 fChain1 fResnum1 fInsCode1
    fgAtomH        = fgAtom "hydrogen atom of HYDBND" 33 fAtomName1 fResname1 fChainH fResnumH fInsCodeH
    fgAtom2        = fgAtom "second atom of HYDBND" 47 fAtomName1 fResname1 fChain2 fResnum2 fInsCode2
    [atom1, atomH, atom2] = rights [fgAtom1, fgAtomH, fgAtom2]

    -- unpack fields
    result = HYDBND atom1 altloc1 atomH altlocH atom2 altloc2 symOp1 symOp2

--------------- }}} HYDBND records

