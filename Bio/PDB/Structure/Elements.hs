{-# LANGUAGE OverloadedStrings #-}
-- | Basic properties of chemical 'Element's as suggested by Cambridge Structural Database.
--
-- NOTE: This module contains functions that could use hash tables for the lookup instead.
-- See https://ghc.haskell.org/trac/ghc/ticket/10565
module Bio.PDB.Structure.Elements(Element(..),
                                  -- Finding element for a given atom
                                  assignElement,
                                  -- Guessing element name from atom name (for standard residues only.)
                                  guessElement,
                                  -- properties of elements
                                  atomicNumber, atomicMass,
                                  covalentRadius,    maxCovalentRadius,
                                  vanDerWaalsRadius, maxVanDerWaalsRadius) where

import Prelude hiding (error, String)
import qualified Prelude     as S (String)
import Data.ByteString.Char8 as BS
import System.IO.Unsafe(unsafePerformIO)
import System.IO(stderr)
import Bio.PDB.Structure(Atom(..))
import Data.HashMap.Strict as M
import Data.Maybe

-- | Type alias for 'Element' names.
type Element = BS.ByteString
-- TODO: May be better as a newtype, and make sure that other modules use this declaration

-- | Internal method that reports error to stderr, and return given default value.
defaultingLookup :: BS.ByteString -> v -> HashMap S.String v -> Element -> v
defaultingLookup msg defaultValue dict e =
      fromMaybe withWarning
    $ M.lookup (BS.unpack e) dict
  where
    withWarning = unsafePerformIO $ do BS.hPutStrLn stderr $ BS.concat [msg, BS.pack . show . BS.unpack $ e]
                                       return defaultValue

-- | Atomic number of a given element
atomicNumber :: Element -> Int
atomicNumber = defaultingLookup "Unknown atomic number for element:"
                                0 dict 
  where
    dict = M.fromList [
              ( "C",   6)
             ,( "N",   7)
             ,( "O",   8)
             ,( "P",  15)
             ,( "S",  16)
             ,( "H",   1)
             ,("AC",  89)
             ,("AG",  47)
             ,("AL",  13)
             ,("AM",  95)
             ,("AR",  18)
             ,("AS",  33)
             ,("AT",  85)
             ,("AU",  79)
             ,( "B",   5)
             ,("BA",  56)
             ,("BE",   4)
             ,("BH", 107)
             ,("BI",  83)
             ,("BK",  97)
             ,("BR",  35)
             ,("CA",  20)
             ,("CD",  48)
             ,("CE",  58)
             ,("CF",  98)
             ,("CL",  17)
             ,("CM",  96)
             ,("CO",  27)
             ,("CR",  24)
             ,("CS",  55)
             ,("CU",  29)
             ,("DB", 105)
             ,("DS", 110)
             ,("DY",  66)
             ,("ER",  68)
             ,("ES",  99)
             ,("EU",  63)
             ,( "F",   9)
             ,("FE",  26)
             ,("FM", 100)
             ,("FR",  87)
             ,("GA",  31)
             ,("GD",  64)
             ,("GE",  32)
             ,("HE",   2)
             ,("HF",  72)
             ,("HG",  80)
             ,("HO",  67)
             ,("HS", 108)
             ,( "I",  53)
             ,("IN",  49)
             ,("IR",  77)
             ,( "K",  19)
             ,("KR",  36)
             ,("LA",  57)
             ,("LI",   3)
             ,("LR", 103)
             ,("LU",  71)
             ,("MD", 101)
             ,("MG",  12)
             ,("MN",  25)
             ,("MO",  42)
             ,("MT", 109)
             ,("NA",  11)
             ,("NB",  41)
             ,("ND",  60)
             ,("NE",  10)
             ,("NI",  28)
             ,("NO", 102)
             ,("NP",  93)
             ,("OS",  76)
             ,("PA",  91)
             ,("PB",  82)
             ,("PD",  46)
             ,("PM",  61)
             ,("PO",  84)
             ,("PR",  59)
             ,("PT",  78)
             ,("PU",  94)
             ,("RA",  88)
             ,("RB",  37)
             ,("RE",  75)
             ,("RF", 104)
             ,("RH",  45)
             ,("RN",  86)
             ,("RU",  44)
             ,("SB",  51)
             ,("SC",  21)
             ,("SE",  34)
             ,("SG", 106)
             ,("SI",  14)
             ,("SM",  62)
             ,("SN",  50)
             ,("SR",  38)
             ,("TA",  73)
             ,("TB",  65)
             ,("TC",  43)
             ,("TE",  52)
             ,("TH",  90)
             ,("TI",  22)
             ,("TL",  81)
             ,("TM",  69)
             ,( "U",  92)
             ,( "V",  23)
             ,( "W",  74)
             ,("XE",  54)
             ,( "Y",  39)
             ,("YB",  70)
             ,("ZN",  30)
             ,("ZR",  40)
             ]

-- | Covalent radius of an element with a given name.
covalentRadius = defaultingLookup "Unknown covalent radius for element:"
                                  0.0 dict 
  where
    dict = M.fromList [
               ("AC", 2.15)
             , ("AG", 1.45)
             , ("AL", 1.21)
             , ("AM", 1.80)
             , ("AR", 1.51)
             , ("AS", 1.21)
             , ("AT", 1.21)
             , ("AU", 1.36)
             , ( "B", 0.83)
             , ("BA", 2.15)
             , ("BE", 0.96)
             , ("BH", 1.50)
             , ("BI", 1.48)
             , ("BK", 1.54)
             , ("BR", 1.21)
             , ( "C", 0.68)
             , ("CA", 1.76)
             , ("CD", 1.44)
             , ("CE", 2.04)
             , ("CF", 1.83)
             , ("CL", 0.99)
             , ("CM", 1.69)
             , ("CO", 1.26)
             , ("CR", 1.39)
             , ("CS", 2.44)
             , ("CU", 1.32)
             , ("DB", 1.50)
             , ("DS", 1.50)
             , ("DY", 1.92)
             , ("ER", 1.89)
             , ("ES", 1.50)
             , ("EU", 1.98)
             , ( "F", 0.64)
             , ("FE", 1.52)
             , ("FM", 1.50)
             , ("FR", 2.60)
             , ("GA", 1.22)
             , ("GD", 1.96)
             , ("GE", 1.17)
             , ( "H", 0.23)
             , ("HE", 1.50)
             , ("HF", 1.75)
             , ("HG", 1.32)
             , ("HO", 1.92)
             , ("HS", 1.50)
             , ( "I", 1.40)
             , ("IN", 1.42)
             , ("IR", 1.41)
             , ( "K", 2.03)
             , ("KR", 1.50)
             , ("LA", 2.07)
             , ("LI", 1.28)
             , ("LR", 1.50)
             , ("LU", 1.87)
             , ("MD", 1.50)
             , ("MG", 1.41)
             , ("MN", 1.61)
             , ("MO", 1.54)
             , ("MT", 1.50)
             , ( "N", 0.68)
             , ("NA", 1.66)
             , ("NB", 1.64)
             , ("ND", 2.01)
             , ("NE", 1.50)
             , ("NI", 1.24)
             , ("NO", 1.50)
             , ("NP", 1.90)
             , ( "O", 0.68)
             , ("OS", 1.44)
             , ( "P", 1.05)
             , ("PA", 2.00)
             , ("PB", 1.46)
             , ("PD", 1.39)
             , ("PM", 1.99)
             , ("PO", 1.40)
             , ("PR", 2.03)
             , ("PT", 1.36)
             , ("PU", 1.87)
             , ("RA", 2.21)
             , ("RB", 2.20)
             , ("RE", 1.51)
             , ("RF", 1.50)
             , ("RH", 1.42)
             , ("RN", 1.50)
             , ("RU", 1.46)
             , ( "S", 1.02)
             , ("SB", 1.39)
             , ("SC", 1.70)
             , ("SE", 1.22)
             , ("SG", 1.50)
             , ("SI", 1.20)
             , ("SM", 1.98)
             , ("SN", 1.39)
             , ("SR", 1.95)
             , ("TA", 1.70)
             , ("TB", 1.94)
             , ("TC", 1.47)
             , ("TE", 1.47)
             , ("TH", 2.06)
             , ("TI", 1.60)
             , ("TL", 1.45)
             , ("TM", 1.90)
             , ( "U", 1.96)
             , ( "V", 1.53)
             , ( "W", 1.62)
             , ("XE", 1.50)
             , ( "Y", 1.90)
             , ("YB", 1.87)
             , ("ZN", 1.22)
             , ("ZR", 1.75)
             ]

{-# INLINE maxCovalentRadius #-}
-- | Upper bound of @covalentRadius@.
maxCovalentRadius :: Double
maxCovalentRadius  = covalentRadius "FR"

-- | Atomic mass of a given element in g/mol
atomicMass :: Element -> Double
atomicMass = defaultingLookup "Unknown atomic mass for element:"
                              0.0 dict 
  where
    dict = M.fromList [
              ( "C",  12.011)
             ,( "N",  14.007)
             ,( "O",  15.999)
             ,( "P",  30.974)
             ,( "S",  32.066)
             ,( "H",   1.008)
             ,("AC", 227.000)
             ,("AG", 107.868)
             ,("AL",  26.982)
             ,("AM", 243.000)
             ,("AR",  39.948)
             ,("AS",  74.922)
             ,("AT", 210.000)
             ,("AU", 196.967)
             ,( "B",  10.811)
             ,("BA", 137.327)
             ,("BE",   9.012)
             ,("BH", 264.000)
             ,("BI", 208.980)
             ,("BK", 247.000)
             ,("BR",  79.904)
             ,("CA",  40.078)
             ,("CD", 112.411)
             ,("CE", 140.116)
             ,("CF", 251.000)
             ,("CL",  35.453)
             ,("CM", 247.000)
             ,("CO",  58.933)
             ,("CR",  51.996)
             ,("CS", 132.905)
             ,("CU",  63.546)
             ,("DB", 262.000)
             ,("DS", 271.000)
             ,("DY", 162.500)
             ,("ER", 167.260)
             ,("ES", 252.000)
             ,("EU", 151.964)
             ,( "F",  18.998)
             ,("FE",  55.845)
             ,("FM", 257.000)
             ,("FR", 223.000)
             ,("GA",  69.723)
             ,("GD", 157.250)
             ,("GE",  72.610)
             ,("HE",   4.003)
             ,("HF", 178.490)
             ,("HG", 200.590)
             ,("HO", 164.930)
             ,("HS", 269.000)
             ,( "I", 126.904)
             ,("IN", 114.818)
             ,("IR", 192.217)
             ,( "K",  39.098)
             ,("KR",  83.800)
             ,("LA", 138.906)
             ,("LI",   6.941)
             ,("LR", 262.000)
             ,("LU", 174.967)
             ,("MD", 258.000)
             ,("MG",  24.305)
             ,("MN",  54.938)
             ,("MO",  95.940)
             ,("MT", 268.000)
             ,("NA",  22.991)
             ,("NB",  92.906)
             ,("ND", 144.240)
             ,("NE",  20.180)
             ,("NI",  58.693)
             ,("NO", 259.000)
             ,("NP", 237.000)
             ,("OS", 190.230)
             ,("PA", 231.036)
             ,("PB", 207.200)
             ,("PD", 106.420)
             ,("PM", 145.000)
             ,("PO", 210.000)
             ,("PR", 140.908)
             ,("PT", 195.078)
             ,("PU", 244.000)
             ,("RA", 226.000)
             ,("RB",  85.468)
             ,("RE", 186.207)
             ,("RF", 261.000)
             ,("RH", 102.906)
             ,("RN", 222.000)
             ,("RU", 101.070)
             ,("SB", 121.760)
             ,("SC",  44.956)
             ,("SE",  78.960)
             ,("SG", 266.000)
             ,("SI",  28.086)
             ,("SM", 150.360)
             ,("SN", 118.710)
             ,("SR",  87.620)
             ,("TA", 180.948)
             ,("TB", 158.925)
             ,("TC",  98.000)
             ,("TE", 127.600)
             ,("TH", 232.038)
             ,("TI",  47.867)
             ,("TL", 204.383)
             ,("TM", 168.934)
             ,( "U", 238.029)
             ,( "V",  50.942)
             ,( "W", 183.840)
             ,("XE", 131.290)
             ,( "Y",  88.906)
             ,("YB", 173.040)
             ,("ZN",  65.390)
             ,("ZR",  91.224)
             ]

-- | Van der Waals radius of the given element
vanDerWaalsRadius :: Element -> Double
vanDerWaalsRadius = defaultingLookup "Do not know van der Waals radius of "
                                     0.0 dict 
  where
    dict = M.fromList [
             ( "C", 1.70)
            ,( "N", 1.55)
            ,( "O", 1.52)
            ,( "P", 1.80)
            ,( "S", 1.80)
            ,("AC", 2.00)
            ,("AG", 1.72)
            ,("AL", 2.00)
            ,("AM", 2.00)
            ,("AR", 1.88)
            ,("AS", 1.85)
            ,("AT", 2.00)
            ,("AU", 1.66)
            , ("B", 2.00)
            ,("BA", 2.00)
            ,("BE", 2.00)
            ,("BH", 2.00)
            ,("BI", 2.00)
            ,("BK", 2.00)
            ,("BR", 1.85)
            ,("CA", 2.00)
            ,("CD", 1.58)
            ,("CE", 2.00)
            ,("CF", 2.00)
            ,("CL", 1.75)
            ,("CM", 2.00)
            ,("CO", 2.00)
            ,("CR", 2.00)
            ,("CS", 2.00)
            ,("CU", 1.40)
            ,("DB", 2.00)
            ,("DS", 2.00)
            ,("DY", 2.00)
            ,("ER", 2.00)
            ,("ES", 2.00)
            ,("EU", 2.00)
            ,( "F", 1.47)
            ,("FE", 2.00)
            ,("FM", 2.00)
            ,("FR", 2.00)
            ,("GA", 1.87)
            ,("GD", 2.00)
            ,("GE", 2.00)
            ,( "H", 1.09)
            ,("HE", 1.40)
            ,("HF", 2.00)
            ,("HG", 1.55)
            ,("HO", 2.00)
            ,("HS", 2.00)
            ,( "I", 1.98)
            ,("IN", 1.93)
            ,("IR", 2.00)
            ,( "K", 2.75)
            ,("KR", 2.02)
            ,("LA", 2.00)
            ,("LI", 1.82)
            ,("LR", 2.00)
            ,("LU", 2.00)
            ,("MD", 2.00)
            ,("MG", 1.73)
            ,("MN", 2.00)
            ,("MO", 2.00)
            ,("MT", 2.00)
            ,("NA", 2.27)
            ,("NB", 2.00)
            ,("ND", 2.00)
            ,("NE", 1.54)
            ,("NI", 1.63)
            ,("NO", 2.00)
            ,("NP", 2.00)
            ,("OS", 2.00)
            ,("PA", 2.00)
            ,("PB", 2.02)
            ,("PD", 1.63)
            ,("PM", 2.00)
            ,("PO", 2.00)
            ,("PR", 2.00)
            ,("PT", 1.72)
            ,("PU", 2.00)
            ,("RA", 2.00)
            ,("RB", 2.00)
            ,("RE", 2.00)
            ,("RF", 2.00)
            ,("RH", 2.00)
            ,("RN", 2.00)
            ,("RU", 2.00)
            ,("SB", 2.00)
            ,("SC", 2.00)
            ,("SE", 1.90)
            ,("SG", 2.00)
            ,("SI", 2.10)
            ,("SM", 2.00)
            ,("SN", 2.17)
            ,("SR", 2.00)
            ,("TA", 2.00)
            ,("TB", 2.00)
            ,("TC", 2.00)
            ,("TE", 2.06)
            ,("TH", 2.00)
            ,("TI", 2.00)
            ,("TL", 1.96)
            ,("TM", 2.00)
            ,( "U", 1.86)
            ,( "V", 2.00)
            ,( "W", 2.00)
            ,("XE", 2.16)
            ,( "Y", 2.00)
            ,("YB", 2.00)
            ,("ZN", 1.39)
            ,("ZR", 2.00)
            ]

{-# INLINE maxVanDerWaalsRadius #-}
-- | Upper bound of @vanDerWaalsRadius@.
maxVanDerWaalsRadius :: Double
maxVanDerWaalsRadius  = vanDerWaalsRadius "K"

-- | Given a PDB 'Atom' extract or guess its 'Element' name.
assignElement :: Atom -> Element
assignElement at = case element at of
                     ""   -> guessElement . atName $ at
                     code -> code

-- | Guessing an 'Element' name from PDB 'Atom' name. 
-- Returns empty string, if 'Element' can't be guessed.
guessElement :: BS.ByteString -> BS.ByteString
guessElement = \e -> fromMaybe "" $ M.lookup (BS.unpack e) els
  where
    els = M.fromList
        [ ("C"   , "C"),
          ("C1'" , "C"),
          ("C2"  , "C"),
          ("C2'" , "C"),
          ("C3'" , "C"),
          ("C4"  , "C"),
          ("C4'" , "C"),
          ("C5"  , "C"),
          ("C5'" , "C"),
          ("C6"  , "C"),
          ("C8"  , "C"),
          ("CA"  , "C"),
          ("CB"  , "C"),
          ("CD"  , "C"),
          ("CD1" , "C"),
          ("CD2" , "C"),
          ("CE"  , "C"),
          ("CE1" , "C"),
          ("CE2" , "C"),
          ("CE3" , "C"),
          ("CG"  , "C"),
          ("CG1" , "C"),
          ("CG2" , "C"),
          ("CH2" , "C"),
          ("CZ"  , "C"),
          ("CZ2" , "C"),
          ("CZ3" , "C"),
          ("N"   , "N"),
          ("N1"  , "N"),
          ("N2"  , "N"),
          ("N3"  , "N"),
          ("N4"  , "N"),
          ("N6"  , "N"),
          ("N7"  , "N"),
          ("N9"  , "N"),
          ("ND1" , "N"),
          ("ND2" , "N"),
          ("NE"  , "N"),
          ("NE1" , "N"),
          ("NE2" , "N"),
          ("NH1" , "N"),
          ("NH2" , "N"),
          ("NZ"  , "N"),
          ("O"   , "O"),
          ("O2"  , "O"),
          ("O2'" , "O"),
          ("O3'" , "O"),
          ("O4"  , "O"),
          ("O4'" , "O"),
          ("O5'" , "O"),
          ("O6"  , "O"),
          ("OD1" , "O"),
          ("OD2" , "O"),
          ("OE1" , "O"),
          ("OE2" , "O"),
          ("OG"  , "O"),
          ("OG1" , "O"),
          ("OH"  , "O"),
          ("OP1" , "O"),
          ("OP2" , "O"),
          ("OXT" , "O"),
          ("P"   , "P"),
          ("SD"  , "S"),
          ("SG"  , "S")]

