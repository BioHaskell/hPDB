{-# LANGUAGE ScopedTypeVariables, MagicHash, BangPatterns #-}
-- | Utility functions for faster parsing using 'Data.ByteString.Internal'.
module Bio.PDB.EventParser.FastParse(strtof, trim, trimFront)
where

import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU

--import Char(ord)
import GHC.Base(Char(..), Int(..))
import GHC.Prim

-- | Alias for 'ord' to make sure it is inlined.
-- Why on earth GHC 7.4 doesn't inline functions like ord?! They should normally compile to no-ops!!!
{-# INLINE ord #-}
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

-- | Faster 'String' to 'Float' conversion.
-- Top level function dealing with negation.
{-# INLINE strtof #-}
strtof :: BS.ByteString -> Maybe Double
strtof bs | BS.null bs = Nothing
strtof bs              = case chr of 
                               '-'                      -> strtof0 True 0 rest -- this allows for "-" == 0.0
                               _   | dv >= 0 && dv <= 9 -> strtof0 False dv rest
                               ' '                      -> strtof rest
                               _                        -> Nothing
  where chr  = BSI.w2c $ BSU.unsafeHead bs
        dv   = digitValue chr
        rest = BSU.unsafeTail bs

-- | Value of a decimal digit.
{-# INLINE digitValue #-}
digitValue :: Char -> Int
digitValue !c = ord c - ord '0'

-- | Strict negation.
{-# INLINE final #-}
{-# ANN final ("HLint: ignore Too strict if" :: [Char]) #-}
final :: Bool -> Double -> Maybe Double
final !sign !f = if sign then Just (-f) else Just f

-- | First helper function for fast 'ByteString' to 'Float' conversion
-- This function deals with things before the comma.
{-# INLINE strtof0 #-}
strtof0 :: Bool -> Int -> BS.ByteString -> Maybe Double
strtof0 !sign !f !bs | BS.null bs = final sign (fromIntegral f :: Double)
strtof0  sign  f  bs = case chr of _   | dv >= 0 && dv <= 9 -> strtof0 sign (10 * f + dv) rest
                                   '.'                      -> strtof1 sign 0 f rest
                                   _                        -> Nothing
  where chr  = BSI.w2c $ BSU.unsafeHead bs
        rest = BSU.unsafeTail bs
        dv   = digitValue chr

-- | Second helper function for fast 'ByteString' to 'Float' conversion
-- This function deals with things after the comma.
{-# INLINE strtof1 #-}
strtof1 :: Bool -> Int -> Int -> BS.ByteString -> Maybe Double
strtof1 !sign !e !f !bs | BS.null bs = makeDouble sign e f
strtof1  sign  e  f  bs              = case chr of
                                         _   | dv >= 0 && dv <= 9 -> strtof1 sign (e+1) (f*10 + dv) rest
                                         ' '                      -> fs `seq` checkSpaces fs rest
                                         _                        -> Nothing
  where chr  = BSI.w2c $ BSU.unsafeHead bs
        dv   = digitValue chr
        fs   = makeDouble sign e f
        rest = BSU.unsafeTail bs

-- | Finalize parsing of 'Double' and construct result.
{-# INLINE makeDouble #-}
makeDouble !sign !e !f = final sign (fromIntegral f * 0.1 ** fromIntegral e)

-- | Checks that remaining part of 'ByteString' is pure spaces,
-- and return 'Nothing' if there is something else (for error handling.)
{-# INLINE checkSpaces #-}
checkSpaces ::  Maybe a -> BS.ByteString -> Maybe a
checkSpaces !result !blanks = if BS.all (==' ') blanks then result else Nothing

-- | Trim spaces in front of the 'ByteString'.
{-# INLINE trimFront #-}
trimFront !s | BS.null s = s
trimFront !s = if BSU.unsafeHead s == 32 -- space
                 then trimFront $ BSU.unsafeTail s
                 else s
-- No idea why it is faster than BS.span version?

-- | Trim spaces at the end of the 'ByteString'.
{-# INLINE trimRear #-}
trimRear !s | BS.null s = s
trimRear !s = if BSU.unsafeIndex s (BS.length s - 1) == 32
                then trimRear $ butlast s
                else s

-- | Discard last character within 'ByteString' without checking. 
butlast (BSI.PS fp o l) = BSI.PS fp o (l-1)

-- | Trim spaces in the front and at the end of 'ByteString'.
trim !s = trimRear $ trimFront s

