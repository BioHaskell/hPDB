{-# LANGUAGE CPP #-}

-- | This module contains instances of `NFData` for Data.ByteString.
module Bio.PDB.Util.MissingInstances() where

import Prelude()
import qualified Data.ByteString as BS
import Control.DeepSeq

#ifdef DEFINE_NFDATA_INSTANCE
-- I use strict version of BS.ByteString so default implementation should do
-- | Nothing needs to be done in NFData instance for stricty `BS.ByteString`.
instance NFData BS.ByteString where
#endif

