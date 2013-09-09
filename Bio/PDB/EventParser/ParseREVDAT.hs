{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parse a REVDAT record (structure revision data.)
module Bio.PDB.EventParser.ParseREVDAT(parseREVDAT)
where

import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ REVDAT records
{--
OLUMNS       DATA  TYPE     FIELD         DEFINITION                             
-------------------------------------------------------------------------------------
 1 -  6       Record name    "REVDAT"                                             
 8 - 10       Int        modNum        Modification number.                   
11 - 12       Continuation   continuation  Allows concatenation of multiple records.
14 - 22       Date           modDate       Date of modification (or release  for   
                                           new entries)  in DD-MMM-YY format. This is
                                           not repeated on continued lines.
24 - 27       IDCode         modId         ID code of this entry. This is not repeated on 
                                           continuation lines.    
32            Int        modType       An integer identifying the type of    
                                           modification. For all  revisions, the
                                           modification type is listed as 1 
40 - 45       LString(6)     record        Modification detail. 
47 - 52       LString(6)     record        Modification detail. 
54 - 59       LString(6)     record        Modification detail. 
61 - 66       LString(6)     record        Modification detail.

--}

revdatFields = [(6,  mKeyword "record header"         "REVDAT" ),
                (7,  mSpc                             1        ),
                (10, mInt     "modification number"            ),
                (12, dInt     "continuation"          0        ),
                (13, mSpc                             1        ),
                (22, mStr     "modification date"              ),
                (23, mSpc                             1        ),
                (27, pStr     "modification id"                ),
                (31, mSpc                             4        ),
                (32, dInt     "modification type"     0        ),
                (39, pSpc                                      ),
                (45, pStr     "modification detail 1"          ),
                (46, pSpc                                      ),
                (52, pStr     "modification detail 2"          ),
                (53, pSpc                                      ),
                (59, pStr     "modification detail 3"          ),
                (60, pSpc                                      ),
                (66, pStr     "modification detail 4"          )]

{-# INLINE pStrList #-}
pStrList []            = []
pStrList (IFStr "":ls) = pStrList ls
pStrList (IFStr s :ls) = s:pStrList ls

{-# INLINE strListErrs #-}
strListErrs line (_:cols) (IFStr _:ls) = strListErrs line cols ls
strListErrs line (_:cols) (IFNone :ls) = strListErrs line cols ls
strListErrs line []       []           = []
strListErrs line (c:cols) (x      :ls) = PDBParseError line c
                                         (BS.concat ["Expecting list of strings, got ",
                                           BS.pack (show x)]) : strListErrs line cols ls

-- | Parses a REVDAT record:
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseREVDAT :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseREVDAT line line_no = return $ if null errs
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, errs1) = parseFields revdatFields line line_no
    errs = errs1 ++ strListErrs line_no [45, 52, 59, 66] strModList
    [fRec, fSpc1, fModNum, fCont, fSpc2, fModDat, fSpc3, fModId, fSpc4, fModTyp,
     fSpc5, fModDet1, fSpc6, fModDet2, fSpc7, fModDet3, fSpc8, fModDet4] = fields
    -- unpack fields
    IFInt modNum  = fModNum
    IFInt cont    = fCont
    IFStr modDat  = fModDat
    IFStr modId   = fModId
    IFInt modTyp  = fModTyp
    strModList    = [fModDet1, fModDet2, fModDet3, fModDet4]
    modList       = filter (not . BS.null) $ pStrList strModList
    result = REVDAT { modNum  = modNum,
                      cont    = cont,
                      modDat  = modDat,
                      modId   = modId,
                      modTyp  = modTyp,
                      details = modList
                    }

-- NOTE: consecutive "REVDAT" records should be merged into a single multiline entry with SUCH method
--mergeTitle :: [PDBEvent] -> m [PDBEvent]

--------------- }}} REVDAT records

