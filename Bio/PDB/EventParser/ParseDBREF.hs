{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns  #-}

-- Parsing cross-references to other databases.
module Bio.PDB.EventParser.ParseDBREF(parseDBREF, parseDBREF12)
where

import qualified Data.ByteString.Char8 as BS
import Data.Char(ord,chr)
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ DBREF records
{--
COLUMNS       DATA TYPE     FIELD              DEFINITION
-----------------------------------------------------------------------------------
 1 -  6       Record name   "DBREF "
 8 - 11       IDcode        idCode             ID code of this entry.
13            Character     chainID            Chain  identifier.
15 - 18       Integer       seqBegin           Initial sequence number of the
                                               PDB sequence segment.
19            AChar         insertBegin        Initial  insertion code of the 
                                               PDB  sequence segment.
21 - 24       Integer       seqEnd             Ending sequence number of the
                                               PDB  sequence segment.
25            AChar         insertEnd          Ending insertion code of the
                                               PDB  sequence segment.
27 - 32       LString       database           Sequence database name. 
34 - 41       LString       dbAccession        Sequence database accession code.
43 - 54       LString       dbIdCode           Sequence  database identification code.
56 - 60       Integer       dbseqBegin         Initial sequence number of the
                                               database seqment.
61            AChar         idbnsBeg           Insertion code of initial residue of the
                                               segment, if PDB is the reference.
63 - 67       Integer       dbseqEnd           Ending sequence number of the
                                               database segment.
68            AChar         dbinsEnd           Insertion code of the ending residue of
                                               the segment, if PDB is the reference.

Database  name                     (columns 27 –  32)      
----------------------------------------------------------------------
GenBank                                   GB
Protein Data Bank                         PDB
UNIPROT                                   UNP
Norine                                    NORINE

--}

-- | Fields of a DBREF record.
dbrefFields = [( 6, mKeyword "record id" "DBREF "),
               ( 7, mSpc 1),
               (11, mStr "id code"),
               (12, mSpc 1),
               (13, mChr "chain id"),
               (14, mSpc 1),
               (18, mInt "initial sequence number of PDB sequence segment"),
               (19, mChr "initial insertion code of the PDB sequence segment"),
               (20, mSpc 1),
               (24, mInt "ending sequence number of the PDB sequence segment"),
               (25, mChr "ending insertion code of the PDB sequence segment"),
               (26, mSpc 1),
               (32, mStr "sequence database name"),
               (33, mSpc 1),
               (41, mStr "sequence database accession code"),
               (42, mSpc 1),
               (54, mStr "sequence database identification code"),
               (55, mSpc 1),
               (60, mInt "initial sequence number of the database segment"),
               (61, dChr "insertion code of initial residue of the sequence, if PDB is the reference" ' '),
               (62, mSpc 1),
               (67, mInt "ending sequence number of the database segment"),
               (68, dChr "insertion code of the ending residue of the segment, if PDB is the reference" ' ')]

-- | Parses a DBREF record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
{-# SPECIALIZE parseDBREF :: BS.ByteString -> Int -> IO [PDBEvent] #-}
parseDBREF :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseDBREF line line_no = return $ if errs == []
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, errs) = parseFields dbrefFields line line_no
    [fRec, _, fIdCode, _, fChain, _, fIniSeqNumPDB, fIniInsCodePDB, _,
     fEndSeqNumPDB, fEndInsCodePDB, _,
     fSeqDbName, _, fSeqDbAccCode, _, fSeqDbIdCode, _, fIniSeqNumInDb,
     fIniInsCodeInPDBRef, _, fEndSeqNumInDb, fEndInsCodeInPDBRef] = fields
    -- unpack fields
    IFStr  idCode           = fIdCode
    IFChar chain              = fChain
    IFInt  iniSeqNumPDB       = fIniSeqNumPDB
    IFChar iniInsCodePDB      = fIniInsCodePDB
    IFInt  endSeqNumPDB       = fEndSeqNumPDB
    IFChar endInsCodePDB      = fEndInsCodePDB
    IFStr  seqDbName          = fSeqDbName
    IFStr  seqDbAccCode       = fSeqDbAccCode
    IFStr  seqDbIdCode        = fSeqDbIdCode
    IFInt  iniSeqNumInDb      = fIniSeqNumInDb
    IFChar iniInsCodeInPDBRef = fIniInsCodeInPDBRef
    IFInt  endSeqNumInDb      = fEndSeqNumInDb
    IFChar endInsCodeInPDBRef = fEndInsCodeInPDBRef
    
    result = DBREF idCode chain iniSeqNumPDB iniInsCodePDB endSeqNumPDB endInsCodePDB seqDbName seqDbAccCode seqDbIdCode iniSeqNumInDb iniInsCodeInPDBRef endSeqNumInDb endInsCodeInPDBRef


-- DBREF1/2 is just DBREF split into two lines, when accession number is too long
{-
COLUMNS        DATA  TYPE    FIELD         DEFINITION
-----------------------------------------------------------------------------------
 1 -  6        Record name   "DBREF1"
 8 - 11        IDcode        idCode        ID code of this entry.
13             Character     chainID       Chain identifier.
15 - 18        Integer       seqBegin      Initial sequence number of the
                                           PDB sequence segment, right justified.
19             AChar         insertBegin   Initial insertion code of the 
                                           PDB sequence segment.
21 - 24        Integer       seqEnd        Ending sequence number of the
                                           PDB sequence segment, right justified.
25             AChar         insertEnd     Ending insertion code of the
                                           PDB sequence  segment.
27 - 32        LString       database      Sequence database name. 
48 - 67        LString       dbIdCode      Sequence database identification code, 
                                           left justified.

DBREF2

COLUMNS       DATA  TYPE    FIELD         DEFINITION
-----------------------------------------------------------------------------------
 1 -  6       Record name   "DBREF2"
 8 - 11       IDcode        idCode        ID code of this entry.
13            Character     chainID       Chain identifier.
19 - 40       LString       dbAccession   Sequence database accession code,
                                          left justified.
46 - 55       Integer       seqBegin      Initial sequence number of the
                                          Database segment, right justified.
58 - 67       Integer       seqEnd        Ending sequence number of the
                                          Database segment, right justified.

We assume that they occur in consecutive lines.
-}
-- | List of fields in DBREF1 line
dbref1Fields = [( 6, mKeyword "record id" "DBREF1"),
                ( 7, mSpc 1),
                (11, mStr "id code"),
                (12, mSpc 1),
                (13, mChr "chain id"),
                (14, mSpc 1),
                (18, mInt "initial sequence number of PDB sequence segment"),
                (19, mChr "initial insertion code of the PDB sequence segment"),
                (20, mSpc 1),
                (24, mInt "ending sequence number of the PDB sequence segment"),
                (25, mChr "ending insertion code of the PDB sequence segment"),
                (26, mSpc 1),
                (32, mStr "sequence database name"),
                (47, mSpc 15),
                (67, mStr "sequence database identification code")]

-- | List of fields in DBREF2 line.
dbref2Fields = [( 6, mKeyword "record id" "DBREF2"),
                ( 7, mSpc 1),
                (11, mStr "id code"),
                (12, mSpc 1),
                (13, mChr "chain id"),
                (18, mSpc 5),
                (40, mStr "sequence database accession code"),
                (45, mSpc 5),
                (55, mInt "initial sequence number of the database segment"),
                (58, mSpc 3),
                (67, mInt "ending sequence number of the database segment")]

-- | Checks agreement between DBREF1 and DBREF2.
checkEqs line_no []                  = []
checkEqs line_no ((a, b, col_no):eqs) = if trim a /= trim b
                                          then
                                            PDBParseError line_no col_no 
                                              (BS.intercalate ""
                                                 ["Fields of consecutive ",
                                                  "DBREF1/DBREF2 records don't agree: '",
                                                  a, "' /= '", b, "'."]): rest
                                          else
                                            rest
  where rest = checkEqs line_no eqs
                      

-- | Parses a pair of DBREF1 and DBREF2 records.
--
-- Arguments:
--
-- (1) two input lines as a tuple
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
{-# SPECIALIZE parseDBREF12 :: (BS.ByteString, BS.ByteString) -> Int -> IO [PDBEvent] #-}
parseDBREF12 :: (Monad m) => (BS.ByteString, BS.ByteString) -> Int -> m [PDBEvent]
parseDBREF12 (!line1, !line2) !line_no = return $ if errs == []
                                                    then [result]
                                                    else errs
  where
    -- parse
    (fields1, errs1) = parseFields dbref1Fields line1  line_no
    (fields2, errs2) = parseFields dbref2Fields line2 (line_no + 1)
    errs = errs1 ++ errs2 ++ mergeErrs
    [fRec1, _, fIdCode1, _, fChain1, _, fIniSeqNumPDB, fIniInsCodePDB, _,
     fEndSeqNumPDB, fEndInsCodePDB, _,
     fSeqDbName, _, fSeqDbIdCode] = fields1
    [fRec2, _, fIdCode2, _, fChain2, _, fSeqDbAccCode, _, fIniSeqNumInDb,
     _, fEndSeqNumInDb] = fields2
    -- unpack fields
    IFStr  idCode1            = fIdCode1
    IFStr  idCode2            = fIdCode1
    IFChar chain1             = fChain1
    IFChar chain2             = fChain2
    IFInt  iniSeqNumPDB       = fIniSeqNumPDB
    IFChar iniInsCodePDB      = fIniInsCodePDB
    IFInt  endSeqNumPDB       = fEndSeqNumPDB
    IFChar endInsCodePDB      = fEndInsCodePDB
    IFStr  seqDbName          = fSeqDbName
    IFStr  seqDbAccCode       = fSeqDbAccCode
    IFStr  seqDbIdCode        = fSeqDbIdCode
    IFInt  iniSeqNumInDb      = fIniSeqNumInDb
    IFInt  endSeqNumInDb      = fEndSeqNumInDb
    mergeErrs                 = checkEqs (line_no + 1) [(idCode1,                         idCode2, 11),
                                                        (BS.singleton chain1, BS.singleton chain2, 13)]
    
    result = DBREF idCode1 chain1 iniSeqNumPDB iniInsCodePDB endSeqNumPDB endInsCodePDB
                   seqDbName seqDbAccCode seqDbIdCode iniSeqNumInDb ' ' endSeqNumInDb ' '

--------------- }}} DBREF records

