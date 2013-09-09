{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parsing of residue modification records (MODRES).
module Bio.PDB.EventParser.ParseMODRES(parseMODRES)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ MODRES records
{--
COLUMNS        DATA TYPE     FIELD       DEFINITION
--------------------------------------------------------------------------------
 1 -  6        Record name   "MODRES"
 8 - 11        IDcode        idCode      ID code of this entry.
13 - 15        Residue name  resName     Residue name used in this entry.
17             Character     chainID     Chain identifier.
19 - 22        Integer       seqNum      Sequence number.
23             AChar         iCode       Insertion code.
25 - 27        Residue name  stdRes      Standard residue name.
30 - 70        String        comment     Description of the residue modification.
--}

modresFields = [(6,  mKeyword "record header" "MODRES"     ),
                (7,  mSpc                     1            ),
                (11, mStr     "PDB code"                   ),
                (12, mSpc                     1            ),
                (15, dStr     "modified residue name" "   "),
                (16, mSpc                     1            ),
                (17, mChr     "chain id"                   ),
                (18, mSpc                     1            ),
                (22, mInt     "sequence number"            ),
                (23, mChr     "insertion code"             ),
                (24, mSpc                     1            ),
                (27, dStr     "standard residue name" "   "),
                (70, mStr     "comment"                    )]

-- | Parses a MODRES record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
{-# SPECIALIZE parseMODRES :: BS.ByteString -> Int -> IO [PDBEvent] #-}
parseMODRES :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseMODRES line line_no = return $ if null errs
                                     then [result]
                                     else errs
  where
    -- parse
    (fields, fErrs) = parseFields modresFields line line_no
    [fRec, _, fPdbCode, _, fModRes, _, fChain, _, fSeqNum, fInsCode, _, fStdRes, fComment] = fields
    -- unpack fields
    IFStr  pdbcode = fPdbCode
    IFStr  stdres  = fStdRes
    IFStr  comment = fComment

    errs = if null fErrs then fgErrs else fErrs
    fgRes     = fgResidue True "modified" 15 fModRes fChain fSeqNum fInsCode
    fgErrs    = liftFgErrs line_no [fgRes]
    Right res = fgRes

    --result = MODRES pdbcode res stdres comment
    result = PDBIgnoredLine $ BS.pack $ show (fModRes, fSeqNum, fChain, fInsCode)


-- NOTE: consecutive "MODRES" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} MODRES records

