{-# LANGUAGE ScopedTypeVariables, OverloadedStrings  #-}

-- | Parsing a SEQADV record.
module Bio.PDB.EventParser.ParseSEQADV(parseSEQADV)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ SEQADV records
{--
COLUMNS        DATA TYPE     FIELD         DEFINITION
-----------------------------------------------------------------
 1 -  6        Record name   "SEQADV"
 8 - 11        IDcode        idCode        ID  code of this entry.
13 - 15        Residue name  resName       Name of the PDB residue in conflict.
17             Character     chainID       PDB  chain identifier.
19 - 22        Integer       seqNum        PDB  sequence number.
23             AChar         iCode         PDB insertion code.
25 - 28        LString       database
30 - 38        LString       dbIdCode      Sequence  database accession number.
40 - 42        Residue name  dbRes         Sequence database residue name.
44 - 48        Integer       dbSeq         Sequence database sequence number.
50 - 70        LString       conflict      Conflict comment.
--}

titleFields = [(6,  mKeyword "record header"  "SEQADV"               ),
               (7,  mSpc                                            1),
               (11, mStr     "PDB id"                                ),
               (12, mSpc                                            1),
               (15, mStr     "residue name"                          ),
               (16, mSpc                                            1),
               (17, mChr     "chain id"                              ),
               (18, mSpc                                            1),
               (22, dInt     "sequence number"                   (-1)),
               (23, mChr     "insertion code"                        ),
               (24, mSpc                                            1),
               (28, mStr     "database"                              ),
               (29, mSpc                                            1),
               (38, mStr     "sequence database accession code"      ),
               (39, mSpc                                            1),
               (42, mStr     "sequence database residue name"        ),
               (43, mSpc                                            1),
               (48, dInt     "sequence database sequence number" (-1)),
               (49, mSpc                                            1),
               (70, mStr     "conflict comment"                      )]

{-# SPECIALIZE parseSEQADV :: BS.ByteString -> Int -> IO [PDBEvent] #-}
-- | Parses a SEQADV record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseSEQADV :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseSEQADV line line_no = return $ if null errs
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, fErrs) = parseFields titleFields line line_no
    [fRec, _, fPdbid, _, fResname, _, fChain, _, fSeqNum, fInsCode, _, fDb,
     _, fAccCode, _, fDbResname, _, fDbSeqNum, _, fComment] = fields
    -- unpack fields
    IFStr  pdbid     = fPdbid
    IFStr  db        = fDb
    IFStr  acccode   = fAccCode
    IFStr  dbresname = fDbResname
    mDbseqnum  = case fSeqNum of 
                   IFInt seqnum -> Just seqnum
                   IFNone       -> Nothing
    IFStr  comment   = fComment

    errs      = if null fErrs then fgErrs else fErrs
    fgRes     = maybeFgResidue False "modified residue" 15 fResname fChain fSeqNum fInsCode
    fgErrs    = liftFgErrs line_no [fgRes]
    Right res = fgRes

    result = SEQADV pdbid res
                    db acccode dbresname mDbseqnum
                    comment

--------------- }}} SEQADV records

