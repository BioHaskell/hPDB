{-# LANGUAGE BangPatterns, PatternGuards, ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-| Standard building blocks for PDB parser.
-}
module Bio.PDB.EventParser.PDBParsingAbstractions(
                              -- * Stage 1 parsing - separating columns
                              trim, parseFields,
                              unstr,
                              -- * Stage 2 parsing - parsing typed values from strings
                              ParsedField(..),
                              -- ** Optional fields
                              pSpc,
                              pInt, pStr, pChr, pDouble,
                              -- ** Optional fields with default values
                              dInt, dStr, dChr, dDouble,
                              -- ** Mandatory fields
                              mKeyword,
                              mKeywords,
                              mSpc, mInt, mStr, mChr, mDouble,
                              -- * Stage 2.5 parsing - grouping fields into compound values
                              -- ** Compound types
                              fgAtom,    maybeFgAtom,
                              fgResidue, maybeFgResidue,
                              -- ** Extracting lists of typed values or error events
                              lefts, rights,
                              liftFgErrs,
                              maybeList
                              -- * Stage 3 is making valid 'PDBEvent's (not included in this module.)
                              )
where

import Prelude hiding (String)
import qualified Data.ByteString.Char8    as BS
import Data.Char (isSpace)

import Bio.PDB.EventParser.PDBEvents
import Bio.PDB.EventParser.FastParse(strtof, trim, trimFront)

--------------- {{{ Parsing abstraction
-- * Parsing abstraction utilities 

--------------- {{{ Stage 1 parsing - dissection by columns
-- ** Stage 1 parsing - dissection by columns 
{-# INLINE splitByColumns #-}
-- | Splits a string into substrings by column numbers.
splitByColumns :: String -> -- -- ^ An input String
                  [Int]  -> -- -- ^ An input list of first column numbers for each record entry
                  [String]  -- -- ^ Output list of record entries as strings
splitByColumns !s !cols = split s cols 0
  where
    split !s []      _ = [s] -- leftover
    split !s (c:cs) !i = let (a, s2) = BS.splitAt (c-i) s in
                             a:split s2 cs c

{-# INLINE unstr #-}
-- | Converts a 'ParsedField' containing 'String' into this string or an empty string if nothing was parsed.
unstr :: ParsedField -> String
unstr (IFStr !s) = s
unstr  IFNone   = ""
--------------- }}} Stage 1 parsing - dissection by columns


--------------- {{{ Stage 2 parsing - type conversions
-- ** Stage 2 parsing - type conversions
-- | A common type for shuttling parsed and typed record values.
data ParsedField = IFInt   {-# UNPACK #-} !Int |
                   IFStr   {-# UNPACK #-} !String  |
                   IFChar  {-# UNPACK #-} !Char    |
                   IFDouble {-# UNPACK #-} !Double   |
                   IFError {-# UNPACK #-} !String  |
                   IFNone
  deriving (Eq, Ord, Show, Read)

-- | Construct an error message by concatenating list of 'Bytestring's
ifError msg = IFError $ BS.concat msg

-- *** Parsers for optional fields
{-# INLINE pChr #-}
-- | Parser for an optional single-character field, first argument is a name of a field, second is an input.
pChr :: String -> String -> ParsedField
pChr fname s | BS.length s == 1 = IFChar (BS.head s)
pChr fname ""  = IFNone
pChr fname s   = ifError ["Char field '", fname, "' should have length of 1 - is '", s, "'."]

{-# INLINE pSpc #-}
-- | Parser for an optional spacing, an argument is an input.
pSpc :: String -> ParsedField
pSpc s | BS.null $ trimFront s = IFNone
pSpc s = ifError ["Spacing expected, but '", s, "' found."]

-- | Old version of pSpc
--   NOTE: also slower than using new trimFront
pSpcO s | BS.dropWhile (==' ') s == "" = IFNone
pSpcO s = ifError ["Spacing expected, but '", s, "' found."]

{-# INLINE pInt #-}
-- | Parser for an optional integer field, first argument is a field name, a second argument is an input.
pInt :: String -> String -> ParsedField
pInt fname s | Just (a, r) <- BS.readInt $ trimFront s = if BS.null $ trimFront r
                                                           then IFInt a
                                                           else pIntErr fname s
pInt fname (trimFront -> "")                           = IFNone
pInt fname s                                           = pIntErr fname s

-- | Reports error from pInt.
pIntErr fname s = ifError ["Int field '", fname,
                           "' cannot be parsed: '", s, "'."]
{-# INLINE pStr #-}
-- | Parser for an optional string field, first argument is a field name, a second argument is an input.
pStr :: String -> String -> ParsedField
pStr fname s = IFStr $ trim s

{-# INLINE pDouble #-}
-- | Parser for an optional floating-point field, first argument is a field name, a second argument is an input.
pDouble :: String -> String -> ParsedField
pDouble fname (strtof -> Just f) = IFDouble f
pDouble fname (trimFront -> "")  = IFNone
pDouble fname s                  = ifError ["Double cannot be parsed: '", s, "'."]

-- *** Parsers with default field values
-- | A helper method for converting a parser of an optional field
-- into a parser of an optional field with a default value.
{-# INLINE dConv #-}
dConv :: (t -> t1 -> ParsedField) -> ParsedField -> t -> t1 -> ParsedField
dConv conv def fname s = case conv fname s of
                              IFNone -> def
                              other  -> other

{-# INLINE dChr #-}
-- | Parser for an optional single character field with a default value, arguments are:
--
-- (1) field name
--
-- (2) default value
--
-- (3) input
dChr   ::  String -> Char    -> String -> ParsedField
dChr   fname def = dConv pChr   (IFChar  def) fname

{-# INLINE dInt #-}
-- | Parser for an optional integer field with a default value, arguments are:
--
-- (1) field name
--
-- (2) default value
--
-- (3) input
dInt   ::  String -> Int -> String -> ParsedField
dInt   fname def = dConv pInt   (IFInt   def) fname

{-# INLINE dStr #-}
-- | Parser for an optional string field with a default value, arguments are:
--
-- (1) field name
--
-- (2) default value
--
-- (3) input
dStr   ::  String -> String  -> String -> ParsedField
dStr   fname def = dConv pStr   (IFStr   def) fname

{-# INLINE dDouble #-}
-- | Parser for an optional floating-point field with a default value, arguments are:
--
-- (1) field name
--
-- (2) default value
--
-- (3) input
dDouble ::  String -> Double   -> String -> ParsedField
dDouble fname def = dConv pDouble (IFDouble def) fname

-- *** Parsers for mandatory fields
{-# INLINE mandField #-}
-- | Converts an optional field parser into a mandatory field parser that bails on missing input.
--
-- Arguments are:
--
-- (1) name of the field type
--
-- (2) a parser function that takes field name, and input and returns 'ParsedField'
--
-- A results is a function that takes:
--
-- (1) field name
--
-- (2) input
--
-- and returns a 'ParsedField'.
mandField ::  String -> (String -> String -> ParsedField) -> String -> String -> ParsedField
mandField typename ftype fname "" = mandFieldErrMsg typename fname
mandField typename ftype fname s  = case ftype fname s of
  IFNone -> mandFieldErrMsg typename fname
  other  -> other 

{-# INLINE mandFieldErrMsg #-}
-- | Return error message when mandatory field is missing.
mandFieldErrMsg typename fname = ifError [typename, " field '", fname, "' is empty or missing!"]

{-# INLINE mChr #-}
-- | Parser for a mandatory character field with a default value, arguments are:
--
-- (1) field name
--
-- (2) input
mChr ::  String -> String -> ParsedField
mChr   = mandField "Char"    pChr
{-# INLINE mDouble #-}
-- | Parser for a mandatory floating-point field with a default value, arguments are:
--
-- (1) field name
--
-- (2) input
mDouble ::  String -> String -> ParsedField
mDouble = mandField "Double"   pDouble
{-# INLINE mStr #-}
-- | Parser for a mandatory string field with a default value, arguments are:
--
-- (1) field name
--
-- (2) input
mStr ::  String -> String -> ParsedField
mStr   = mandField "String"  pStr
{-# INLINE mInt #-}
-- | Parser for a mandatory integer field with a default value, arguments are:
--
-- (1) field name
--
mInt ::  String -> String -> ParsedField
-- (2) input
mInt   = mandField "Int" pInt
{-# INLINE mSpc #-}
-- | Parser for a mandatory spacing field, arguments are:
--
-- (1) number of columns for spacing
--
-- (2) input
mSpc :: Int -> String -> ParsedField
mSpc l s = if BS.length s == l
             then pSpc s
             else ifError ["Spacing has different length ",
                           BS.pack $ show $ BS.length s,
                           " than expected ", BS.pack $ show l, "."]

{-# INLINE mKeywords #-}
-- | Parser for a fixed field that can be filled with one of many keywords, arguments are:
--
-- (1) field name
--
-- (2) a list of valid keywords
--
-- (3) input
mKeywords ::  String -> [String] -> String -> ParsedField
mKeywords fname kwds s | s `elem` kwds = IFStr s
mKeywords fname kwds s                 = ifError ["Keyword field '", fname,
                                                  "' should contain one of strings: '",
                                                  BS.intercalate "', '" kwds,
                                                  "' not '", s, "'."]

{-# INLINE mKeyword #-}
-- | Parser for a fixed single keyword field, arguments are:
--
-- (1) field name
--
-- (2) keyword
--
-- (3) input
mKeyword ::  String -> String -> String -> ParsedField
mKeyword fname kwd = mKeywords fname [kwd] 

-- Dissects columns (stage 1) and applies converters (stage 2)
{-# INLINE convertColumns #-}
-- | Dissects columns from stage 1 parsing, and applies converters from stage 2 parsing
--
-- (1) list of string parsers that return typed 'ParsedField' values
--
-- (2) list of column numbers that indicate a beginning of each field
--
-- (3) input
convertColumns :: [String -> ParsedField] -> [Int] -> String -> [ParsedField]
convertColumns convs cols s = map convert (zip convs content)
  where
    convert (conv, s) = conv s
    content = splitByColumns s cols

{-# INLINE findColumnErrors #-}
-- | Finds IFError values in a list of 'ParsedField' values, and returns
-- a list of error events in case there are any.
--
-- (1) list of string parsers that return typed 'ParsedField' values
--
-- (2) list of column numbers that indicate a beginning of each field
--
-- (3) line number to be injected into error events
--
-- (4) input
findColumnErrors :: [ParsedField] -> [Int] -> Int -> [PDBEvent]
findColumnErrors fields cols line_no = concatMap findError (zip fields cols)
  where
    findError (IFError e, c) = [PDBParseError line_no c e]
    findError _              = []

{-# INLINE parseFields #-}
-- | Uses field declarations that are list of (column number, parser to 'ParsedField', ...)
-- tuples and applies it to a given line of input.
--
-- Arguments are:
--
-- (1) field declarations list
--
-- (2) input line
--
-- (3) ordinal number of an input line
parseFields fieldDecls line line_no = (fields, errs)
  where
    fieldTypes  = map snd fieldDecls
    fieldBounds = map fst fieldDecls
    fields :: [ParsedField] = convertColumns fieldTypes fieldBounds line
    errs = findColumnErrors fields fieldBounds line_no

--------------- }}} Stage 2 parsing - type conversions

--------------- {{{ Stage 2.5 parsing - field groups

-- ** Stage 2.5 parsing - field grouping

{-# INLINE nonEmptyIF #-}
-- | Returns if a given 'ParsedField' value _certainly_ represents a missing value.
nonEmptyIF ::  ParsedField -> Bool
nonEmptyIF  IFNone      = False
nonEmptyIF (IFStr  "" ) = False
nonEmptyIF  _           = True

{-# INLINE fullIF #-}
-- | Returns if a given 'ParsedField' value _certainly_ represents a present value.
fullIF IFNone                    = False
fullIF (IFStr s) | BS.all isSpace s = False
fullIF (IFChar ' ')              = False
fullIF _                         = True

-- residue description field group
{-# INLINE fgResidue #-}
-- | Merges a set of values that corresponds to a residue description. 
--
-- Arguments are:
--
-- (1) boolean indicating, if the field group may omit a residue number
--
-- (2) field group name (description)
--
-- (3) column number beginning the residue description entries
--
-- (4) 'ParsedField' containing a three letter residue identifier
--
-- (5) 'ParsedField' containing a single letter chain identifier
--
-- (6) 'ParsedField' containing a residue number
--
-- (7) 'ParsedField' containing a residue insertion code
--
-- A result is a 'Either' of pair with column number and error message,
-- or 'RESID' value with a residue description.
fgResidue :: Bool -> BS.ByteString -> Int -> ParsedField -> ParsedField -> ParsedField -> ParsedField -> Either (Int, BS.ByteString) RESID
fgResidue delible fname col r c d i = case maybeFgResidue delible fname col r c d i of
                                Right Nothing   -> Left (col, BS.concat [fname, " residue description missing!"])
                                Right (Just at) -> Right at
                                Left (col, s)   -> Left (col, s)

{-# INLINE maybeFgResidue #-}
-- | Merges a set of values that corresponds to an optional residue description.
--
-- Arguments are:
--
-- (1) boolean indicating, if the field group may omit a residue number
--
-- (2) field group name (description)
--
-- (3) column number beginning the residue description entries
--
-- (4) 'ParsedField' containing a three letter residue identifier
--
-- (5) 'ParsedField' containing a single letter chain identifier
--
-- (6) 'ParsedField' containing a residue number
--
-- (7) 'ParsedField' containing a residue insertion code
--
-- A result is a 'Either' of pair with column number and error message,
-- or 'Maybe' 'RESID' value that may contain a residue description.
maybeFgResidue :: Bool -> BS.ByteString -> Int -> ParsedField -> ParsedField -> ParsedField -> ParsedField -> Either (Int, BS.ByteString) (Maybe RESID)
maybeFgResidue delible fname col r c d i
  | all nonEmptyIF obligatoryFields =
    Right $ Just $ RESID (unr, unc, und, uni)
  | any fullIF [r, c, d, i] =
    Left
      (col,
       BS.concat
	 [fname, " residue descriptions contains fields: ",
	  BS.pack $ show [r, c, d, i]])
  | otherwise = Right Nothing
  where obligatoryFields = if delible then [c, i] else [c, d, i]
	IFStr unr = r
	IFChar unc = c
	IFInt und = d
	IFChar uni = i

{-# INLINE fgAtom #-}
-- | Merges a set of values that correspond to a mandatory atom description.
--
-- Arguments are:
--
-- (1) field group name (description)
--
-- (2) column number beginning the residue description entries
--
-- (3) 'ParsedField' containing a three letter atom identifier
--
-- (4) 'ParsedField' containing a three letter residue identifier
--
-- (5) 'ParsedField' containing a single letter chain identifier
--
-- (6) 'ParsedField' containing a residue number
--
-- (7) 'ParsedField' containing a residue insertion code
--
-- A result is a 'Either' of pair with column number and error message,
-- or 'ATID' value that may contain an atom description.
fgAtom :: BS.ByteString-> Int -> ParsedField-> ParsedField-> ParsedField-> ParsedField-> ParsedField-> Either (Int, BS.ByteString) ATID
fgAtom fname col a r c d i = case maybeFgAtom fname col a r c d i of
                               Right Nothing   -> Left (col, BS.concat [fname, " atom description missing!"])
                               Right (Just at) -> Right at
                               Left  (col, s)  -> Left (col, s)

{-# INLINE maybeFgAtom #-}
-- | Merges a set of values that correspond to an optional atom description.
--
-- Arguments are:
--
-- (1) field group name (description)
--
-- (2) column number beginning the residue description entries
--
-- (3) 'ParsedField' containing a three letter atom identifier
--
-- (4) 'ParsedField' containing a three letter residue identifier
--
-- (5) 'ParsedField' containing a single letter chain identifier
--
-- (6) 'ParsedField' containing a residue number
--
-- (7) 'ParsedField' containing a residue insertion code
--
-- A result is a 'Either' of pair with column number and error message,
-- or 'Maybe' 'ATID' value that may contain an atom description.
maybeFgAtom :: BS.ByteString-> Int -> ParsedField-> ParsedField-> ParsedField-> ParsedField-> ParsedField-> Either (Int, BS.ByteString) (Maybe ATID)
maybeFgAtom fname col a r c d i
  | all nonEmptyIF [a, r, c, d, i] =
    Right $ Just $ ATID (una, unr, unc, und, uni)
  | any fullIF [a, r, c, d, i] =
    Left
      (col,
       BS.concat
	 [fname, " atom descriptions contains fields: ",
	  BS.pack $ show [a, r, c, d, i]])
  | otherwise = Right Nothing
  where IFStr una = a
	IFStr unr = r
	IFChar unc = c
	IFInt und = d
	IFChar uni = i

-- Stage 3 is generation of events - code is separated for each kind of event.

{-# INLINE lefts #-}
-- | Changes a list of 'Either' values, into a list of all values in 'Left' entries.
lefts ::  [Either a b] -> [a]
lefts (Left  s:ls) = s:lefts ls
lefts (Right _:ls) =   lefts ls
lefts []           = []

{-# INLINE liftFgErrs #-}
-- | Extracts Left (column_number, error_message) values from a list of results in a given line,
-- to form 'PDBParseError' events with a given line number, column number and error message.
--
-- Arguments:
--
-- (1) line number
--
-- (2) list of 'Either' (column_number, error_message_string) result values,
-- where 'Left' entries are used to generate error messages.
--
-- Result is a list of 'PDBEvent' entries that contain 'PDBParseError's (if any.)
liftFgErrs ::  Int -> [Either (Int, String) b] -> [PDBEvent]
liftFgErrs line_no errs = map (uncurry $ PDBParseError line_no) (lefts errs)

{-# INLINE rights #-}
-- | Changes a list of 'Either' values, into a list of all values in 'Right' entries.
rights ::  [Either a b] -> [b]
rights (Left  _:ls) =   rights ls
rights (Right s:ls) = s:rights ls
rights []           = []
--------------- }}} Stage 2.5 parsing - field groups

-- | Utility: Changes a list of 'Maybe's to a list of values hidden in 'Just' _ records.
maybeList :: [Maybe a] -> [a]
maybeList []           = []
maybeList (Nothing:as) = maybeList as
maybeList (Just a :as) = a:maybeList as

--------------- }}} Parsing abstractions
