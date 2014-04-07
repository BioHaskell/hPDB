{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-spec-constr #-} -- workaround for GHC 7.8.0 bug #8960
-- | Low-level event-based parser interface.
module Bio.PDB.EventParser.PDBEventParser(parsePDBRecords)
where

-- Parses PDB file format version 3.20 (dated Sept 15, 2008)

import qualified Data.ByteString.Char8 as BS
import Control.Monad(unless, foldM)

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions
-- Methods parsing individual records:
import Bio.PDB.EventParser.ParseATOM
import Bio.PDB.EventParser.ParseHEADER 
import Bio.PDB.EventParser.ParseTITLE
import Bio.PDB.EventParser.ParseREMARK
import Bio.PDB.EventParser.ParseIntRecord
import Bio.PDB.EventParser.ParseREVDAT
import Bio.PDB.EventParser.ParseCONECT
import Bio.PDB.EventParser.ParseSEQRES
import Bio.PDB.EventParser.ParseCRYST1
import Bio.PDB.EventParser.ParseHELIX
import Bio.PDB.EventParser.ParseSHEET
import Bio.PDB.EventParser.ParseTER
import Bio.PDB.EventParser.ParseMASTER
import Bio.PDB.EventParser.ParseMODRES
import Bio.PDB.EventParser.ParseSEQADV
import Bio.PDB.EventParser.ParseCAVEAT
import Bio.PDB.EventParser.ParseSPLIT
import Bio.PDB.EventParser.ParseJRNL
import Bio.PDB.EventParser.ParseDBREF
import Bio.PDB.EventParser.ParseHETNAM
import Bio.PDB.EventParser.ParseHET
import Bio.PDB.EventParser.ParseFORMUL
import Bio.PDB.EventParser.ParseCISPEP
import Bio.PDB.EventParser.ParseSSBOND
import Bio.PDB.EventParser.ParseLINK
import Bio.PDB.EventParser.ParseSLTBRG
import Bio.PDB.EventParser.ParseHYDBND
import Bio.PDB.EventParser.ParseSITE
import Bio.PDB.EventParser.ParseObsoleting
import Bio.PDB.EventParser.ParseSpecListRecord
import Bio.PDB.EventParser.ParseListRecord
import Bio.PDB.EventParser.ParseMatrixRecord
import Bio.PDB.EventParser.ParseTVECT

import System.IO.Unsafe --debug

--------------- {{{ Record parsers

--------------- }}} Record parsers
--------------- {{{ Main parser: putting it together
--parsePDBLines ::  (Monad m) => BS.ByteString -> BS.ByteString -> Int -> m [PDBEvent]
{- | Parses an input stream 'input' with name 'fname' at line 'line_no', and
uses parsed input 'evts' to perform an 'action' on them and accumulator
'acc'. 

Returns the ultimate value of the accumulated results in 'acc'
after all actions are performed in an order consistent with input.
-}
parsePDBLines !fname !input !line_no action acc = 
  if BS.null input
    then return acc
    else (
    case line of 
      -- Most frequent records
      a | "ATOM  " `BS.isPrefixOf` a -> cont1 $! parseATOM   line line_no
      a | "HETATM" `BS.isPrefixOf` a -> cont1 $! parseATOM   line line_no
      a | "ANISOU" `BS.isPrefixOf` a -> cont1 $! parseANISOU line line_no 
      a | "REMARK" `BS.isPrefixOf` a -> cont1 $! parseREMARK line line_no 
      a | "SEQRES" `BS.isPrefixOf` a -> cont1 $! parseSEQRES line line_no 
      a | "CONECT" `BS.isPrefixOf` a -> cont1 $! parseCONECT line line_no 
      a | "SIGATM" `BS.isPrefixOf` a -> cont1 $! parseATOM   line line_no
      a | "SIGUIJ" `BS.isPrefixOf` a -> cont1 $! parseANISOU line line_no 
      -- Delimiters
      a | "ENDMDL" `BS.isPrefixOf` a -> cont1 $! return [ENDMDL] 
      a | "END"    `BS.isPrefixOf` a -> cont1 $! return [END]
      -- common error in treatment of TER - omitting rest of the record
      "TER"                          -> cont1 $! return [TER { num = -1, resname = "", chain = ' ', resid = -1, insCode = ' ' }]
      -- proper TER
      a | "TER"    `BS.isPrefixOf` a -> cont1 $! parseTER    line line_no
      a | "MASTER" `BS.isPrefixOf` a -> cont1 $! parseMASTER line line_no
      -- Secondary structure declarations
      a | "HELIX"  `BS.isPrefixOf` a -> cont1 $! parseHELIX  line line_no
      a | "SHEET"  `BS.isPrefixOf` a -> cont1 $! parseSHEET  line line_no
      -- Crystallographic information
      a | "SCALE"  `BS.isPrefixOf` a -> cont1 $! parseSCALEn line line_no
      a | "ORIGX"  `BS.isPrefixOf` a -> cont1 $! parseORIGXn line line_no
      a | "MTRIX"  `BS.isPrefixOf` a -> cont1 $! parseMTRIXn line line_no
      a | "CRYST1" `BS.isPrefixOf` a -> cont1 $! parseCRYST1 line line_no
      a | "TVECT " `BS.isPrefixOf` a -> cont1 $! parseTVECT  line line_no
      -- Singular metarecords
      a | "DBREF " `BS.isPrefixOf` a -> cont1 $! parseDBREF  line line_no
      a | "DBREF1" `BS.isPrefixOf` a -> cont2 $! parseDBREF12 (line, line2) line_no
      a | "HETNAM" `BS.isPrefixOf` a -> cont1 $! parseHETNAM True  line line_no
      a | "HETSYN" `BS.isPrefixOf` a -> cont1 $! parseHETNAM False line line_no
      a | "HET   " `BS.isPrefixOf` a -> cont1 $! parseHET    line line_no
      a | "FORMUL" `BS.isPrefixOf` a -> cont1 $! parseFORMUL line line_no
      a | "CISPEP" `BS.isPrefixOf` a -> cont1 $! parseCISPEP line line_no
      a | "SSBOND" `BS.isPrefixOf` a -> cont1 $! parseSSBOND line line_no
      a | "LINK  " `BS.isPrefixOf` a -> cont1 $! parseLINK   line line_no
      a | "SLTBRG" `BS.isPrefixOf` a -> cont1 $! parseSLTBRG line line_no
      a | "HYDBND" `BS.isPrefixOf` a -> cont1 $! parseHYDBND line line_no
      a | "SITE  " `BS.isPrefixOf` a -> cont1 $! parseSITE   line line_no
      a | "MODRES" `BS.isPrefixOf` a -> cont1 $! parseMODRES line line_no
      a | "SEQADV" `BS.isPrefixOf` a -> cont1 $! parseSEQADV line line_no
      a | "MDLTYP" `BS.isPrefixOf` a -> cont1 $! parseMDLTYP line line_no
      a | "EXPDTA" `BS.isPrefixOf` a -> cont1 $! parseEXPDTA line line_no
      a | "SOURCE" `BS.isPrefixOf` a -> cont1 $! parseSOURCE line line_no
      a | "COMPND" `BS.isPrefixOf` a -> cont1 $! parseCOMPND line line_no
      a | "NUMMDL" `BS.isPrefixOf` a -> cont1 $! parseNUMMDL line line_no
      a | "MODEL " `BS.isPrefixOf` a -> cont1 $! parseMODEL  line line_no
      a | "REVDAT" `BS.isPrefixOf` a -> cont1 $! parseREVDAT line line_no
      a | "HEADER" `BS.isPrefixOf` a -> cont1 $! parseHEADER line line_no
      a | "TITLE " `BS.isPrefixOf` a -> cont1 $! parseTITLE  line line_no
      a | "AUTHOR" `BS.isPrefixOf` a -> cont1 $! parseAUTHOR line line_no
      a | "KEYWDS" `BS.isPrefixOf` a -> cont1 $! parseKEYWDS line line_no
      a | "CAVEAT" `BS.isPrefixOf` a -> cont1 $! parseCAVEAT line line_no
      a | "OBSLTE" `BS.isPrefixOf` a -> cont1 $! parseOBSLTE line line_no
      a | "SPRSDE" `BS.isPrefixOf` a -> cont1 $! parseSPRSDE line line_no
      a | "SPLIT " `BS.isPrefixOf` a -> cont1 $! parseSPLIT  line line_no
      a | "JRNL  " `BS.isPrefixOf` a -> cont1 $! parseJRNL   line line_no
      _                            -> cont1 $! return [PDBIgnoredLine line])
  where
    cont1 !a = do !evts    <- a
                  !new_acc <- foldM action acc evts
                  --(nextLine1 `seq` line_no1 `seq` new_acc `seq`
                  parsePDBLines fname nextLine1 line_no1 action new_acc
    cont2 a = do !evts   <- a
                 !new_acc <- foldM action acc evts
                 parsePDBLines fname nextLine2 line_no2 action acc
    (!line, !rest1) = BS.break (=='\n') input
    nextLine1 = BS.drop 1 rest1
    (line2,  rest2) = BS.break (=='\n') nextLine1
    nextLine2 = BS.drop 1 rest2
    !line_no1 = line_no  + 1
    line_no2 = line_no1 + 1

--parsePDBRecords :: (Monad m) =>String -> BS.ByteString -> (a -> PDBEvent -> m a) -> a -> m a
-- | Parses a strict ByteString 'contents' named 'fname' and performs 'action'
-- on events given by parsing chunks, returning accumulated results. Accumulator
-- is primed by 'acc'.
parsePDBRecords fname contents = parsePDBLines fname contents 0

-- | Checks whether line was ignored as unknown record type
ignoreLine (PDBIgnoredLine _) = False
ignoreLine _                  = True

--------------- }}} Main parser: putting it together
