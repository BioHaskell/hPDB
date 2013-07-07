{-# LANGUAGE CPP #-}

-- | This module contains instances of `NFData` for Data.ByteString
--   and `Params` before text-format 0.3.0.8.
module Bio.PDB.Util.MissingInstances() where

import Prelude()
import qualified Data.ByteString as BS
import Control.DeepSeq

#ifdef DEFINE_PARAMS_INSTANCES
import Data.Text.Buildable
import Data.Text.Format.Types
import Data.Text.Lazy.Builder
import Data.Text.Format.Params(Params(..))
#endif


#ifdef DEFINE_NFDATA_INSTANCE
-- I use strict version of BS.ByteString so default implementation should do
-- | Nothing needs to be done in NFData instance for stricty `BS.ByteString`.
instance NFData BS.ByteString where
#endif

#ifdef DEFINE_PARAMS_INSTANCES

-- | Adds instances of Params for tuples of more than ten Buildables.
instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k)
    => Params (a,b,c,d,e,f,g,h,i,j,k) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o,
         build p]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o,
         build p, build r]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o,
         build p, build r, build s]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s, Buildable t)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o,
         build p, build r, build s, build t]

instance (Buildable a, Buildable b, Buildable c, Buildable d, Buildable e,
          Buildable f, Buildable g, Buildable h, Buildable i, Buildable j,
          Buildable k, Buildable l, Buildable m, Buildable n, Buildable o,
          Buildable p, Buildable r, Buildable s, Buildable t, Buildable u)
    => Params (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u) where
    buildParams (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u) =
        [build a, build b, build c, build d, build e,
         build f, build g, build h, build i, build j,
         build k, build l, build m, build n, build o,
         build p, build r, build s, build t, build u]

#endif
