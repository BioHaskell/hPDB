{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing a SITE record.
module Bio.PDB.EventParser.ParseSITE(parseSITE)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SITE records
{--
COLUMNS        DATA  TYPE    FIELD         DEFINITION
---------------------------------------------------------------------------------
 1 -  6        Record name   "SITE  "
 8 - 10        Integer       seqNum        Sequence number.
12 - 14        LString(3)    siteID        Site name.
16 - 17        Integer       numRes        Number of residues that compose the site.
19 - 21        Residue name  resName1      Residue name for first residue that 
                                           creates the site.
23             Character     chainID1      Chain identifier for first residue of site.
24 - 27        Integer       seq1          Residue sequence number for first residue
                                           of the  site.
28             AChar         iCode1        Insertion code for first residue of the site.
30 - 32        Residue name  resName2      Residue name for second residue that 
                                           creates the site.
34             Character     chainID2      Chain identifier for second residue of
                                           the  site.
35 - 38        Integer       seq2          Residue sequence number for second
                                           residue of the site.
39             AChar         iCode2        Insertion code for second residue
                                           of the  site.
41 - 43        Residue name  resName3      Residue name for third residue that 
                                           creates  the site.
45             Character     chainID3      Chain identifier for third residue
                                           of the site.
46 - 49        Integer       seq3          Residue sequence number for third
                                           residue of the site.
50             AChar         iCode3        Insertion code for third residue
                                           of the site.
52 - 54        Residue name  resName4      Residue name for fourth residue that 
                                           creates  the site.
56             Character     chainID4      Chain identifier for fourth residue
                                           of the site.
57 - 60        Integer       seq4          Residue sequence number for fourth
                                           residue of the site.
61             AChar         iCode4        Insertion code for fourth residue
                                           of the site.
--}

{-# INLINE siteFields #-}
siteFields = [(6,  mKeyword "record header"     "SITE  "            ),
              (7,  mSpc     1                                       ),
              (10, mInt     "record serial number"                  ),
              (11, mSpc     1                                       ),
              (14, mStr     "site id"                               ),
              (15, mSpc     1                                       ),
              (17, mInt     "number of residues"                    ),
              (18, mSpc     1                                       ),
              (21, mStr     "residue name 1"                        ),
              (22, mSpc     1                                       ),
              (23, mChr     "chain id 1"                            ),
              (27, mInt     "residue sequence number 1"             ),
              (28, mChr     "insertion code 1"                      ),
              (29, pSpc                                             ),
              (32, pStr     "residue name 2"                        ),
              (33, pSpc                                             ),
              (34, pChr     "chain id 2"                            ),
              (38, pInt     "residue sequence number 2"             ),
              (39, pChr     "insertion code 2"                      ),
              (40, pSpc                                             ),
              (43, pStr     "residue name 3"                        ),
              (44, pSpc                                             ),
              (45, pChr     "chain id 3"                            ),
              (49, pInt     "residue sequence number 3"             ),
              (50, pChr     "insertion code 3"                      ),
              (51, pSpc                                             ),
              (54, pStr     "residue name 4"                        ),
              (55, pSpc                                             ),
              (56, pChr     "chain id 4"                            ),
              (60, pInt     "residue sequence number 4"             ),
              (61, pChr     "insertion code 4"                      )]

-- | Parses a SITE record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSITE ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSITE line line_no = return $ if null errs
                                   then [result]
                                   else errs
  where
    -- parse
    errs = if null fErrs then fgErrs else fErrs
    (fields, fErrs) = parseFields siteFields line line_no
    [fRec, _, fSerial, _, fSiteId, _, fNumRes, _,
     fResname1, _, fChain1, fResnum1, fInsCode1, _,
     fResname2, _, fChain2, fResnum2, fInsCode2, _,
     fResname3, _, fChain3, fResnum3, fInsCode3, _,
     fResname4, _, fChain4, fResnum4, fInsCode4] = fields
    IFInt   serial   = fSerial
    IFStr   siteid   = fSiteId
    IFInt   numres   = fNumRes
    fgRes1 = fgResidue      False "residue 1" 21 fResname1 fChain1 fResnum1 fInsCode1
    fgRes2 = maybeFgResidue False "residue 2" 32 fResname2 fChain2 fResnum2 fInsCode2
    fgRes3 = maybeFgResidue False "residue 3" 43 fResname3 fChain3 fResnum3 fInsCode3
    fgRes4 = maybeFgResidue False "residue 4" 54 fResname4 fChain4 fResnum4 fInsCode4
    
    residues = rights [fgRes1] ++ maybeList (rights [fgRes2, fgRes3, fgRes4])

    fgErrs = liftFgErrs line_no [fgRes1] ++
             liftFgErrs line_no [fgRes2, fgRes3, fgRes4]

    -- unpack fields
    result = SITE serial siteid numres residues 

--------------- }}} SITE records

