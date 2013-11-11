{-# LANGUAGE CPP #-}

-- | This module contains instances of `NFData` for Data.ByteString (if necessary).
module Bio.PDB.Util.MissingInstances() where

import Prelude()
import qualified Data.ByteString as BS
import Control.DeepSeq

#if MIN_VERSION_bytestring >= (0, 10, 0)
-- NFData is already defined in the library.
#else
-- I use strict version of BS.ByteString so default implementation should do
-- | Nothing needs to be done in NFData instance for stricty `BS.ByteString`.
instance NFData BS.ByteString where
#endif

