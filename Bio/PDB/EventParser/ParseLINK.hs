{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing LINK records.
module Bio.PDB.EventParser.ParseLINK(parseLINK)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ LINK records
{--
COLUMNS         DATA TYPE      FIELD           DEFINITION
------------------------------------------------------------------------------------
 1 -  6         Record name    "LINK  "
13 - 16         Atom           name1           Atom name.
17              Character      altLoc1         Alternate location indicator.
18 - 20         Residue name   resName1        Residue  name.
22              Character      chainID1        Chain identifier.
23 - 26         Integer        resSeq1         Residue sequence number.
27              AChar          iCode1          Insertion code.
43 - 46         Atom           name2           Atom name.
47              Character      altLoc2         Alternate location indicator.
48 - 50         Residue name   resName2        Residue name.
52              Character      chainID2        Chain identifier.
53 - 56         Integer        resSeq2         Residue sequence number.
57              AChar          iCode2          Insertion code.
60 - 65         SymOP          sym1            Symmetry operator atom 1.
67 - 72         SymOP          sym2            Symmetry operator atom 2.
74 – 78         Real(5.2)      Length          Link distance
--}

{-# INLINE linkFields #-}
linkFields = [(6,  mKeyword "record header"     "LINK  "            ),
	      (12, mSpc     6                                       ),
	      (16, mStr     "atom name"                             ),
	      (17, mChr     "alternate location indicator 1"        ),
	      (20, mStr     "residue name 1"                        ),
	      (21, mSpc     1                                       ),
	      (22, mChr     "chain id 1"                            ),
	      (26, mInt     "residue sequence number 1"             ),
	      (27, mChr     "insertion code 1"                      ),
	      (42, mSpc     15                                      ),
	      (46, mStr     "atom name 1"                           ),
	      (47, mChr     "alternate location indicator 2"        ),
	      (50, mStr     "residue name 2"                        ),
	      (51, mSpc     1                                       ),
	      (52, mChr     "chain id 2"                            ),
	      (56, mInt     "residue sequence number 2"             ),
	      (57, mChr     "insertion code 2"                      ),
	      (59, mSpc     2                                       ),
	      (65, mStr     "symmetry operator for atom 1"          ),
	      (66, mSpc     1                                       ),
	      (72, mStr     "symmetry operator for atom 2"          ),
              (73, mSpc     1                                       ),
              (78, pDouble   "link distance"                         )]

-- | Parses a LINK record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseLINK ::  (Monad m) => String -> Int -> m [PDBEvent]
parseLINK line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    errs = if fErrs == [] then fgErrs else fErrs
    (fields, fErrs) = parseFields linkFields line line_no
    [fRec, _,
     fAtName1, fAltLoc1, fResname1, _, fChain1, fResnum1, fInsCode1, _,
     fAtName2, fAltLoc2, fResname2, _, fChain2, fResnum2, fInsCode2, _,
     fSymOp1, _, fSymOp2, _, fLinkDist] = fields
    IFChar  altloc1  = fAltLoc1
    IFChar  altloc2  = fAltLoc2
    IFStr   symop1   = fSymOp1 
    IFStr   symop2   = fSymOp2 
    linkdist = case fLinkDist of
                 IFDouble ld -> Just ld
                 IFNone     -> Nothing

    fgAt1  = fgAtom "atom 1" 16 fAtName1 fResname1 fChain1 fResnum1 fInsCode1
    fgAt2  = fgAtom "atom 2" 26 fAtName2 fResname2 fChain2 fResnum2 fInsCode2
    fgErrs = liftFgErrs line_no [fgAt1, fgAt2]
    [at1, at2] = rights [fgAt1, fgAt2]

    -- unpack fields
    result = LINK at1 altloc1 at2 altloc2 symop1 symop2 linkdist 

--------------- }}} LINK/LINKSYN records

