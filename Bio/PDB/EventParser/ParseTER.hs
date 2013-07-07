{---# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Parsing of TER records.
module Bio.PDB.EventParser.ParseTER(parseTER)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ TER records
{--
COLUMNS        DATA  TYPE    FIELD           DEFINITION
-------------------------------------------------------------------------
 1 -  6        Record name   "TER   "
 7 - 11        Integer       serial          Serial number.
18 - 20        Residue name  resName         Residue name.
22             Character     chainID         Chain identifier.
23 - 26        Integer       resSeq          Residue sequence number.
27             AChar         iCode           Insertion code.

--}

{-# INLINE terFields #-}
terFields = [(6,  mKeyword "record header" "TER   "),
             (11, mInt     "atom serial number"    ),
             (17, mSpc     6                       ),
             (20, mStr     "residue name"          ),
             (21, mSpc     1                       ),
             (22, mChr     "chain identifier"      ),
             (26, mInt     "residue number"        ),
             (27, dChr     "insertion code"     ' ')] -- omitted if empty by some programs

-- | Parses a TER record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseTER ::  (Monad m) => String -> Int -> m [PDBEvent]
parseTER line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    (fields, errs) = parseFields terFields line line_no
    [fRec, fNum, _, fResName, _, fChain, fResId, fInsCode] = fields
    IFInt  num     = fNum
    IFStr  resname = fResName
    IFChar chain  = fChain
    IFInt  resid   = fResId
    IFChar insCode = fInsCode

    -- unpack fields
    result = TER num resname chain resid insCode

--------------- }}} TER records

