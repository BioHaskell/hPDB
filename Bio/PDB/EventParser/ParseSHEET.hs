{-# LANGUAGE ScopedTypeVariables, OverloadedStrings    #-}

-- | Parsing a SHEET record.
module Bio.PDB.EventParser.ParseSHEET(parseSHEET)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.StrandSense
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SHEET records
{--
COLUMNS        DATA  TYPE    FIELD          DEFINITION
-------------------------------------------------------------------------------------
 1 -  6        Record name   "SHEET "
 8 - 10        Integer       strand         Strand  number which starts at 1 for each
                                            strand within a sheet and increases by one.
12 - 14        LString(3)    sheetID        Sheet  identifier.
15 - 16        Integer       numStrands     Number  of strands in sheet.
18 - 20        Residue name  initResName    Residue  name of initial residue.
22             Character     initChainID    Chain identifier of initial residue 
                                            in strand. 
23 - 26        Integer       initSeqNum     Sequence number of initial residue
                                            in strand.
27             AChar         initICode      Insertion code of initial residue
                                            in  strand.
29 - 31        Residue name  endResName     Residue name of terminal residue.
33             Character     endChainID     Chain identifier of terminal residue.
34 - 37        Integer       endSeqNum      Sequence number of terminal residue.
38             AChar         endICode       Insertion code of terminal residue.
39 - 40        Integer       sense          Sense of strand with respect to previous
                                            strand in the sheet. 0 if first strand,
                                            1 if  parallel,and -1 if anti-parallel.
42 - 45        Atom          curAtom        Registration.  Atom name in current strand.
46 - 48        Residue name  curResName     Registration.  Residue name in current strand
50             Character     curChainId     Registration. Chain identifier in
                                            current strand.
51 - 54        Integer       curResSeq      Registration.  Residue sequence number
                                            in current strand.
55             AChar         curICode       Registration. Insertion code in
                                            current strand.
57 - 60        Atom          prevAtom       Registration.  Atom name in previous strand.
61 - 63        Residue name  prevResName    Registration.  Residue name in
                                            previous strand.
65             Character     prevChainId    Registration.  Chain identifier in
                                            previous  strand.
66 - 69        Integer       prevResSeq     Registration. Residue sequence number
                                            in previous strand.
70             AChar         prevICode      Registration.  Insertion code in
                                            previous strand.

--}

helixFields = [(6,  mKeyword "record header" "SHEET "                  ),
               (7,  mSpc                     1                         ),
               (10, mInt     "strand"                                  ),
               (11, mSpc                     1                         ),
               (14, mStr     "sheet id"                                ),
               (16, mInt     "number of strands"                       ),
               (17, mSpc                     1                         ),
               (20, mStr     "initial residue name"                    ),
               (21, mSpc                     1                         ),
               (22, mChr     "initial residue chain id"                ),
               (26, mInt     "initial residue serial number"           ),
               (27, mChr     "initial insertion code"                  ),
               (28, mSpc                     1                         ),
               (31, mStr     "end residue name"                        ),
               (32, mSpc                     1                         ),
               (33, mChr     "end residue chain id"                    ),
               (37, mInt     "end residue serial number"               ),
               (38, mChr     "end insertion code"                      ),
               (40, mInt     "sense with respect to previous strand"   ),
               (41, pSpc                                               ),
               (45, pStr     "current atom name"                       ),
               (48, pStr     "current residue name"                    ),
               (49, pSpc                                               ),
               (50, pChr     "current chain id"                        ),
               (54, pInt     "current residue serial number"           ),
               (55, pChr     "current residue insertion code"          ),
               (56, pSpc                                               ),
               (60, pStr     "atom name in previous strand"            ),
               (63, pStr     "residue name in previous strand"         ),
               (64, pSpc                                               ),
               (65, pChr     "chain id in previous strand"             ),
               (69, pInt     "residue serial number in previous strand"),
               (70, pChr     "insertion code in previous strand"       )]

-- | Parses a SHEET record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSHEET ::  (Monad m) => String -> Int -> m [PDBEvent]
parseSHEET line line_no = --return [Test $ BS.pack $ show $ ((length helixFields)::Int)]
  return $ if errs == []
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, fErrs) = parseFields helixFields line line_no
    [fRec, _, fStrandId, _, fSheetId, fNumStrands, _,
     fIniResName, _, fIniChain, fIniResId, fIniResIns, _,
     fEndResName, _, fEndChain, fEndResId, fEndResIns,
     fSense, _,
     fCurAtomName,  fCurResName,  _, fCurChain,  fCurResId, fCurResIns, _,
     fPrevAtomName, fPrevResName, _, fPrevChain, fPrevResId, fPrevResIns] = fields
    -- unpack fields
    IFInt  strandId    = fStrandId
    IFStr  sheetId     = fSheetId
    
    IFInt  numStrands  = fNumStrands
    
    IFStr  iniResName  = fIniResName
    IFChar iniChain    = fIniChain
    IFInt  iniResId    = fIniResId
    IFChar iniResIns   = fIniResIns
    fgIniRes           = fgResidue False "initial" 20 fIniResName fIniChain fIniResId fIniResIns
    Right iniRes       = fgIniRes
    
    IFStr  endResName  = fEndResName
    IFChar endChain    = fEndChain
    IFInt  endResId    = fEndResId
    IFChar endResIns   = fEndResIns
    fgEndRes           = fgResidue False "end" 31 fEndResName fEndChain fEndResId fEndResIns  
    Right endRes       = fgEndRes

    IFInt  iSense      = fSense
    sense              = case iSense of
                            ( 0) -> Nothing     
                            ( 1) -> Just Parallel
                            (-1) -> Just Antiparallel
    
    fgCurAtom  = maybeFgAtom "current"  45 fCurAtomName  fCurResName  fCurChain  fCurResId  fCurResIns
    fgPrevAtom = maybeFgAtom "previous" 60 fPrevAtomName fPrevResName fPrevChain fPrevResId fPrevResIns
        
    fgErrs = liftFgErrs line_no [fgCurAtom, fgPrevAtom] ++
             liftFgErrs line_no [fgIniRes,  fgEndRes  ]

    Right curAtom  = fgCurAtom
    Right prevAtom = fgPrevAtom

    errs = fErrs ++ fgErrs

    -- assuming residue name won't contain spaces...
    result = SHEET strandId    sheetId   numStrands sense
                   iniRes
                   endRes
                   curAtom
                   prevAtom

--------------- }}} SHEET records

