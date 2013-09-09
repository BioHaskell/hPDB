{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Parsing CRYST1 records.
module Bio.PDB.EventParser.ParseCRYST1(parseCRYST1)
where

import qualified Data.ByteString.Char8 as BS
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ CRYST1 records
{--
COLUMNS       DATA TYPE      CONTENTS
--------------------------------------------------------------------------------
 1 -  6       Record name    "CRYST1"
 7 - 15       Real(9.3)      a (Angstroms)
16 - 24       Real(9.3)      b (Angstroms)     
25 - 33       Real(9.3)      c (Angstroms)     
34 - 40       Real(7.2)      alpha (degrees)   
41 - 47       Real(7.2)      beta (degrees)    
48 - 54       Real(7.2)      gamma (degrees)   
56 - 66       LString        Space group       
67 - 70       Integer        Z value           
--}

crystFields = [(6,  mKeyword "record header" "CRYST1"),
               (7,  mSpc                     1),
               (15, mDouble   "a"              ),
               (24, mDouble   "b"              ),
               (33, mDouble   "c"              ),
               (40, mDouble   "alpha"          ),
               (47, mDouble   "beta"           ),
               (54, mDouble   "gamma"          ),
               (55, mSpc                     1),
               (66, mStr     "space group"    ),
               (70, mInt     "Z value"        )]

-- | Parses a CRYST1 record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
{-# SPECIALIZE parseCRYST1 :: BS.ByteString -> Int -> IO [PDBEvent] #-}
parseCRYST1 :: (Monad m) => BS.ByteString -> Int -> m [PDBEvent]
parseCRYST1 line line_no = return $ if null errs
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, errs) = parseFields crystFields line line_no
    [fRec, fSpc1, fA, fB, fC, fAlpha, fBeta, fGamma, fSpc2, fSpcGrp, fZValue] = fields
    -- unpack fields
    IFDouble a      = fA
    IFDouble b      = fB
    IFDouble c      = fC
    IFDouble alpha  = fAlpha
    IFDouble beta   = fBeta
    IFDouble gamma  = fGamma
    IFStr   spcGrp = fSpcGrp
    IFInt   zValue = fZValue
    result = CRYST1 a b c alpha beta gamma spcGrp zValue

--------------- }}} CRYST1 records

