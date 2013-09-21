-- | Module contains enumeration of helix types, and auxiliary functions
-- for converting these into PDB CLASS codes.
module Bio.PDB.EventParser.HelixTypes(HelixT, helix2code, code2helix)
where
{-| Enumeration of helix types

PDB Class number in columns 39-40 for each type of helix in HELIX record:

 (1) Right-handed alpha (default, most common)

 (2) Right-handed omega

 (3) Right-handed pi

 (4) Right-handed gamma

 (5) Right-handed 3 - 10

 (6) Left-handed alpha

 (7) Left-handed omega

 (8) Left-handed gamma

 (9) 2 - 7 ribbon/helix

(10) Polyproline

-}

data HelixT = RightAlpha  |
              RightOmega  |
              RightPi     |
              RightGamma  |
              Right3_10   |
              LeftAlpha   |
              LeftOmega   |
              LeftGamma   |
              Ribbon2_7   |
              Polyproline
  deriving (Eq, Ord, Show, Read)

-- | helix2code converts a 'HelixT' enumeration into an PDB CLASS code.
helix2code RightAlpha  =  1
helix2code RightOmega  =  2
helix2code RightPi     =  3
helix2code RightGamma  =  4
helix2code Right3_10   =  5
helix2code LeftAlpha   =  6
helix2code LeftOmega   =  7
helix2code LeftGamma   =  8
helix2code Ribbon2_7   =  9
helix2code Polyproline = 10

-- | helix2code converts an PDB CLASS code into a 'HelixT' enumeration.
code2helix  1 = RightAlpha
code2helix  2 = RightOmega
code2helix  3 = RightPi
code2helix  4 = RightGamma
code2helix  5 = Right3_10
code2helix  6 = LeftAlpha
code2helix  7 = LeftOmega
code2helix  8 = LeftGamma
code2helix  9 = Ribbon2_7
code2helix 10 = Polyproline

