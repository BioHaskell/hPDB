{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Parsing FORMUL records.
module Bio.PDB.EventParser.ParseFORMUL(parseFORMUL)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ FORMUL records
{--
COLUMNS        DATA TYPE     FIELD         DEFINITION
-----------------------------------------------------------------------
 1 -  6        Record name   "FORMUL"
 9 - 10        Integer       compNum       Component  number.
13 - 15        LString(3)    formulID         Het identifier.
17 - 18        Integer       continuation  Continuation number.
19             Character     asterisk      "*" for water.
20 - 70        String        text          Chemical formula.
--}

{-# INLINE formulFields #-}
formulFields = [(6,  mKeyword "record header"     "FORMUL"),
                (8,  mSpc     2                           ),
                (10, mInt     "component number"          ),
                (12, mSpc     2                           ),
                (15, mStr     "hetero group identifier"   ),
                (16, mSpc     1                           ),
                (18, dInt     "continuation"      0       ),
                (19, mChr     "asterisk for water"        ),
                (70, mStr     "chemical formula"          )]

-- | Parses a FORMUL record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseFORMUL ::  (Monad m) => String -> Int -> m [PDBEvent]
parseFORMUL line line_no = return $ if errs == []
                                   then [result]
                                   else errs
  where
    -- parse
    errs            = fErrs ++ watErr
    (fields, fErrs) = parseFields formulFields line line_no
    [fRec, _, fCompNum, _, fHetId, _, fCont, fAsterisk, fFormula] = fields
    IFInt  compNum  = fCompNum
    IFStr  hetId    = fHetId
    IFInt  cont     = fCont
    IFChar asterisk = fAsterisk
    IFStr  formula  = fFormula
    
    watErr = if asterisk `elem` " *"
               then []
               else [PDBParseError line_no 19 $
                     BS.concat ["Expecting asterisk for water or space, but found: '",
                                BS.pack [asterisk], "'."]]
    isWater = asterisk == '*'

    -- unpack fields
    result = FORMUL compNum hetId cont isWater [formula]

--------------- }}} FORMUL/FORMULSYN records

