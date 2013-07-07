{-# LANGUAGE ScopedTypeVariables, OverloadedStrings   #-}

-- | Parsing CONECT records.
module Bio.PDB.EventParser.ParseCONECT(parseCONECT)
where

import qualified Data.ByteString.Char8 as BS
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ CONECT records
{--
COLUMNS       DATA  TYPE     FIELD         DEFINITION
----------------------------------------------------------------------------------
 1 -  6        Record name    "CONECT"
 7 - 11        Int        serial       Atom  serial number
12 - 16        Int        serial       Serial number of bonded atom
17 - 21        Int        serial       Serial  number of bonded atom
22 - 26        Int        serial       Serial number of bonded atom
27 - 31        Int        serial       Serial number of bonded atom
--}

{-# INLINE conectFields #-}
conectFields = [(6,  mKeyword "record header" "CONECT"),
                (11, mInt     "atom serial number"    ),
                (16, mInt     "atom serial number"    ),
                (21, pInt     "atom serial number"    ),
                (26, pInt     "atom serial number"    ),
                (31, pInt     "atom serial number"    )]

{-# INLINE intList #-}
intList (IFInt i:ls) = i:intList ls
intList (IFNone :ls) = intList ls
intList []           = []

-- | Parses a CONECT record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseCONECT :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseCONECT line line_no = return $ if errs == []
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, errs) = parseFields conectFields line line_no
    [fRec, fS1, fS2, fS3, fS4, fS5] = fields
    serials = intList [fS1, fS2, fS3, fS4, fS5]
    -- unpack fields
    result = CONECT { atoms = serials }

--------------- }}} CONECT records

