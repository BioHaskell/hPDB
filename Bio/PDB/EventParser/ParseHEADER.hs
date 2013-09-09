{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Parsing HEADER records.
module Bio.PDB.EventParser.ParseHEADER(parseHEADER)
where

import qualified Data.ByteString.Char8 as BS
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions

--------------- {{{ HEADER records
{--
 1 -  6       Record name    "HEADER"
<break>
11 - 50       String(40)     classification    Classifies the molecule(s).
51 - 59       Date           depDate           Deposition date. This is the date the
                                               coordinates  were received at the PDB.
<break>
63 - 66       IDcode         idCode            This identifier is unique within the PDB.
--}

headerFields = [(6,  mKeyword "header"         "HEADER"),
                (10, mSpc                      4       ),
                (50, dStr     "classification" ""      ),
                (59, dStr     "depDate"        ""      ),
                (62, mSpc                      3       ),
                (66, dStr     "idCode"         ""      )]
  
-- | Parses a HEADER record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseHEADER :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseHEADER line line_no = return $ if null errs then [result] else errs
  where
    -- parse
    (fields, errs) = parseFields headerFields line line_no
    [fRec, fSpc1, fClass, fDepDate, fSpc2, fIdCode] = fields
    -- unpack fields
    IFStr   clas    = fClass
    IFStr   depDate = fDepDate
    IFStr   idCode  = fIdCode
    result = HEADER { classification = clas,
                      depDate        = depDate,
                      idCode         = idCode }
--------------- }}} HEADER records

