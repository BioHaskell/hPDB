{-# LANGUAGE ScopedTypeVariables, OverloadedStrings   #-}

-- | Contains function that parse ATOM and ANISOU records
module Bio.PDB.EventParser.ParseATOM(
  parseATOM,
  parseANISOU
) where

import qualified Data.ByteString.Char8 as BS
import Text.Printf(hPrintf)
import Prelude hiding(String)

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.PDBParsingAbstractions
--------------- {{{ ATOM/HETATM records
{-- 
From http://deposit.rcsb.org/adit/docs/pdb_atom_format.html:
 1 -  6        Record name     "ATOM  "                                            
 7 - 11        Int         Atom serial number.                   
13 - 16        Atom            Atom name.                            
17             Character       Alternate location indicator.         
18 - 20        Residue name    Residue name.                         
22             Character       Chain identifier.                     
23 - 26        Int         Residue sequence number.              
27             AChar           Code for insertion of residues.       
31 - 38        Real(8.3)       Orthogonal coordinates for X in Angstroms.                       
39 - 46        Real(8.3)       Orthogonal coordinates for Y in Angstroms.                            
47 - 54        Real(8.3)       Orthogonal coordinates for Z in Angstroms.                            

Following is optional for my parser:
55 - 60        Real(6.2)       Occupancy.                            
61 - 66        Real(6.2)       Temperature factor (Default = 0.0).                   

Following is optional in PDB format?
73 - 76        LString(4)      Segment identifier, left-justified.   
77 - 78        LString(2)      Element symbol, right-justified.      
79 - 80        LString(2)      Charge on the atom.       

Some programs treat 67-80 as generic "notes"

Example line
ATOM    282  SG  CYS A  40      17.199  10.929  10.237  1.00  7.30           S
--}
-- SHOULD BE ABSTRACTED - but this breaks optimization, and makes code 4x-5x slower
-- List of pairs (ending column, field parser)
--{-# INLINE commonFields #-}
-- fields common to ATOM/HETATM and ANISOU records
{-commonFields keywords middle = 
       [( 6, mKeywords "Record declaration" keywords),
        
        -- Atom description
        (11, mInt     "atom id"),
        (16, mStr     "atom name"),
        (17, mChr     "altloc"),

        -- Residue and chain description
        (20, mStr     "residue name"), -- three letter code
        (21, mSpc     1),  -- spacing
        (22, mChr     "chain id"),
        (26, mInt     "residue number"),
        (27, mChr     "residue insertion code")] ++

       middle ++

        -- Trailing fields optional according to PDB
       [(76, pStr     "SegId"),
        (78, pStr     "element"), 
        (80, pStr     "charge")        -- useful for MD
       ]-}

--{-# INLINE atomFields #-}
{-atomFields = commonFields ["ATOM  ", "HETATM"] [
        -- Coordinates
        (38, mDouble   "X coordinate"),
        (46, mDouble   "Y coordinate"),
        (54, mDouble   "Z coordinate"),

        -- Fields below must be present in a proper PDB entry, 
        -- but may be absent in theoretical models:
        (60, dDouble   "occupancy"  1.00),
        (66, dDouble   "B-factor"  99.99),
        (73, pSpc)]     -- spacing -}

{-# INLINE atomFields #-}
atomFields = [
        ( 6, mKeywords "Record declaration" ["ATOM  ", "HETATM", "SIGATM"]),

        -- Atom description
        (11, mInt     "atom id"),
        (16, mStr     "atom name"),
        (17, mChr     "altloc"),

        -- Residue and chain description
        (20, mStr     "residue name"), -- three letter code
        (21, mSpc     1),  -- spacing
        (22, mChr     "chain id"),
        (26, mInt     "residue number"),
        (27, mChr     "residue insertion code"),

        (38, mDouble   "X coordinate"),
        (46, mDouble   "Y coordinate"),
        (54, mDouble   "Z coordinate"),

        -- Fields below must be present in a proper PDB entry, 
        -- but may be absent in theoretical models:
        (60, dDouble   "occupancy"  1.00),
        (66, dDouble   "B-factor"  99.99),
        --(73, pStr     "<spaces>"), -- Use with GHC 6.12 and earlier to avoid broken optimization: should be pSpc 7
        --(73, pSpc      ), -- non-standard, but used by some programs, so we catenate it with SegId field

        (76, pStr     "SegId"),
        (78, pStr     "element"),
        (80, pStr     "charge")]

--{- ### INLINE parseATOM #-} -- strangely makes code 7x slower!!!
-- | Parses an ATOM record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
--{-# SPECIALIZE parseATOM :: Bool -> String -> Int -> IO [PDBEvent] #-}
parseATOM :: (Monad m) => String -> Int -> m [PDBEvent]
parseATOM line line_no = return $ if null errs
                                    then [result]
                                    else errs
  where
    -- parse
    (fields, errs) = parseFields atomFields line line_no
    [fRecTag, fAtId, fAtNam, fAltLoc, fResNam, _fSpace, fChain, fResId, fInsid,
     fX, fY, fZ, fOcc, fBFact, fSegid, fElt, fCharge] = fields
    -- unpack fields
    IFStr   rectag = fRecTag
    IFInt   atid   = fAtId
    IFStr   atnam  = fAtNam
    IFChar  altloc = fAltLoc
    IFStr   resnam = fResNam
    IFChar  chain  = fChain
    IFInt   resid  = fResId
    IFChar  insid  = fInsid
    IFDouble x      = fX
    IFDouble y      = fY
    IFDouble z      = fZ
    IFDouble occ    = fOcc
    IFDouble bFact  = fBFact
    segid          = unstr fSegid
    elt            = unstr fElt
    charge         = unstr fCharge
    -- assemble record
    coords         = Vector3 x y z
    result         = case rectag of 
                       "ATOM  " -> ATOM   atid atnam resnam chain resid insid altloc coords occ bFact segid elt charge False
                       "HETATM" -> ATOM   atid atnam resnam chain resid insid altloc coords occ bFact segid elt charge True
                       "SIGATM" -> SIGATM atid atnam resnam chain resid insid altloc coords occ bFact segid elt charge

--------------- }}} ATOM/HETATM records

--------------- {{{ ANISOU records
{-
COLUMNS       DATA  TYPE    FIELD          DEFINITION
-----------------------------------------------------------------
 1 - 6        Record name   "ANISOU"
 7 - 11       Integer       serial         Atom serial number.
13 - 16       Atom          name           Atom name.
17            Character     altLoc         Alternate location indicator
18 - 20       Residue name  resName        Residue name.
22            Character     chainID        Chain identifier.
23 - 26       Integer       resSeq         Residue sequence number.
27            AChar         iCode          Insertion code.

29 - 35       Integer       u[0][0]        U(1,1)
36 - 42       Integer       u[1][1]        U(2,2)
43 - 49       Integer       u[2][2]        U(3,3)
50 - 56       Integer       u[0][1]        U(1,2)
57 - 63       Integer       u[0][2]        U(1,3)
64 - 70       Integer       u[1][2]        U(2,3)

77 - 78       LString(2)    element        Element symbol, right-justified.
79 - 80       LString(2)    charge         Charge on the atom.
-}

--{-# INLINE anisouFields #-}
{-anisouFields = commonFields ["ANISOU"] [
  (29, mSpc 2),
  (35, mInt "U(1,1)"),
  (42, mInt "U(2,2)"),
  (49, mInt "U(3,3)"),
  (56, mInt "U(1,2)"),
  (63, mInt "U(1,3)"),
  (70, mInt "U(2,3)"),
  (76, mSpc 6)]-}

{-# INLINE anisouFields #-}
anisouFields = [
        ( 6, mKeywords "Record declaration" ["ANISOU", "SIGUIJ"]),

        -- Atom description
        (11, mInt     "atom id"),
        (16, mStr     "atom name"),
        (17, mChr     "altloc"),

        -- Residue and chain description
        (20, mStr     "residue name"), -- three letter code
        (21, mSpc     1),  -- spacing
        (22, mChr     "chain id"),
        (26, mInt     "residue number"),
        (27, mChr     "residue insertion code"),

        (29, mSpc 2),
        (35, mInt "U(1,1)"),
        (42, mInt "U(2,2)"),
        (49, mInt "U(3,3)"),
        (56, mInt "U(1,2)"),
        (63, mInt "U(1,3)"),
        (70, mInt "U(2,3)"),
        (75, pSpc ), -- use pStr "<spaces>" with GHC<=6.12 to avoid broken optimization

        (76, pStr     "SegId"),
        (78, pStr     "element"),
        (80, pStr     "charge")]


{-# INLINE parseANISOU #-}
-- | Parses an ANISOU record.
--
-- Arguments:
--
-- (1) input line
--
-- (2) input line number
--
-- Result is a monad action returning a list of 'PDBEvent's.
parseANISOU :: (Monad m) => String -> Int -> m [PDBEvent]
parseANISOU line line_no = return $ if null errs then result `seq` [result] else errs
  where
    -- parse
    (fields, errs) = parseFields anisouFields line line_no
    [fRecTag, fAtId, fAtNam, fAltLoc, fResNam, _fSpace, fChain, fResId, fInsid,
     _, fU_1_1, fU_2_2, fU_3_3, fU_1_2, fU_1_3, fU_2_3, _,
     fSegid, fElt, fCharge] = fields
    -- unpack fields
    IFStr   rectag = fRecTag
    IFInt   atid   = fAtId
    IFStr   atnam  = fAtNam
    IFChar  altloc = fAltLoc
    IFStr   resnam = fResNam
    IFChar  chain  = fChain
    IFInt   resid  = fResId
    IFChar  insid  = fInsid
    IFInt u_1_1    = fU_1_1
    IFInt u_2_2    = fU_2_2
    IFInt u_3_3    = fU_3_3
    IFInt u_1_2    = fU_1_2
    IFInt u_1_3    = fU_1_3
    IFInt u_2_3    = fU_2_3
    segid          = unstr fSegid
    elt            = unstr fElt
    charge         = unstr fCharge
    -- assemble record
    cons           = case rectag of
                       "ANISOU" -> ANISOU
                       "SIGUIJ" -> SIGUIJ
    result         = cons atid atnam
                          resnam chain resid insid altloc
                          u_1_1 u_2_2 u_3_3 u_1_2 u_1_3 u_2_3
                          segid elt charge

--------------- }}} ANISOU records
