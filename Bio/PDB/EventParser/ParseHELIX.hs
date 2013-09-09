{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | Parsing HELIX records.
module Bio.PDB.EventParser.ParseHELIX(parseHELIX)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BS

import Bio.PDB.EventParser.HelixTypes
import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions


--------------- {{{ HELIX records
{--
COLUMNS        DATA  TYPE     FIELD         DEFINITION
-----------------------------------------------------------------------------------
 1 -  6        Record name    "HELIX "
 8 - 10        Integer        serNum        Serial number of the helix. This starts
                                            at 1  and increases incrementally.
12 - 14        LString(3)     helixID       Helix  identifier. In addition to a serial
                                            number, each helix is given an 
                                            alphanumeric character helix identifier.
16 - 18        Residue name   initResName   Name of the initial residue.
20             Character      initChainID   Chain identifier for the chain containing
                                            this  helix.
22 - 25        Integer        initSeqNum    Sequence number of the initial residue.
26             AChar          initICode     Insertion code of the initial residue.
28 - 30        Residue  name  endResName    Name of the terminal residue of the helix.
32             Character      endChainID    Chain identifier for the chain containing
                                            this  helix.
34 - 37        Integer        endSeqNum     Sequence number of the terminal residue.
38             AChar          endICode      Insertion code of the terminal residue.
39 - 40        Integer        helixClass    Helix class (see below).
41 - 70        String         comment       Comment about this helix.
72 - 76        Integer        length        Length of this helix.
--}

helixFields = [(6,  mKeyword "record header" "HELIX "       ),
               (7,  mSpc                     1              ),
               (10, mInt     "serial number"                ),
               (11, mSpc                     1              ),
               (14, mStr     "helix id"                     ),
               (15, mSpc                     1              ),
               (18, mStr     "initial residue name"         ),
               (19, mSpc                     1              ),
               (20, mChr     "initial chain id"             ),
               (21, mSpc                     1              ),
               (25, mInt     "initial residue serial number"),
               (26, mChr     "initial insertion code"       ),
               (27, mSpc                     1              ),
               (30, mStr     "end residue name"             ),
               (31, mSpc                     1              ),
               (32, mChr     "end chain id"                 ),
               (33, mSpc                     1              ),
               (37, mInt     "end residue serial number"    ),
               (38, mChr     "end residue insertion code"   ),
               (40, mInt     "helix class"                  ),
               (70, mStr     "comment"                      ),
               (72, mSpc                     2              ),
               (76, mInt     "length"                       )]
  
-- | Parses a HET record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseHELIX :: (Monad m) => String -> Int -> m [PDBEvent]
parseHELIX line line_no = --return [Test $ BS.pack $ show $ ((length helixFields)::Int)]
  return $ if null errs
                                      then [result]
                                      else errs
  where
    -- parse
    (fields, fErrs) = parseFields helixFields line line_no
    [fRec, _, fSerial, _, fHelixId, _,
     fIniResName, _, fIniChain, _, fIniResId, fIniResIns, _,
     fEndResName, _, fEndChain, _, fEndResId, fEndResIns,
     fHelixClass, fComment, _, fLength] = fields
    -- unpack fields
    IFInt  serial     = fSerial
    IFStr  helixId    = fHelixId

    IFStr  iniResName = fIniResName
    IFChar iniChain   = fIniChain
    IFInt  iniResId   = fIniResId
    IFChar iniResIns  = fIniResIns
    fgIniRes          = fgResidue False "initial" line_no fIniResName fIniChain fIniResId fIniResIns
    Right iniRes      = fgIniRes
    
    IFStr  endResName = fEndResName
    IFChar endChain   = fEndChain
    IFInt  endResId   = fEndResId
    IFChar endResIns  = fEndResIns
    fgEndRes          = fgResidue False "ending" line_no fEndResName fEndChain fEndResId fEndResIns
    Right endRes      = fgEndRes

    errs = fErrs ++ liftFgErrs line_no [fgEndRes, fgIniRes]

    IFInt  helixClass = fHelixClass
    IFStr  comment    = fComment
    IFInt  len        = fLength
    -- assuming residue name won't contain spaces...
    result = HELIX serial iniRes
                          endRes
                   (code2helix helixClass) comment len

--------------- }}} HELIX records

