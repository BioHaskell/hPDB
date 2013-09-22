{-# LANGUAGE MagicHash, NoMonomorphismRestriction, CPP #-}
-- | Collections used within the Structure, currently aliases for the 'Vector' and 'MVector' types.
module Bio.PDB.Structure.List(List(..), TempList,
                              initialNew, new, add, finalize, tempLength, empty, last,
                              singleton,
                              map, mapM, foldl, foldl', foldr, foldM, filter, length,
                              defaultSize, residueVectorSize, chainVectorSize,
                              toList, vimap, (!)
                             ) where

--import Prelude(Int,Num(..),Monad(..))
import Prelude hiding (length, filter, drop, take, init, tail, mapM, splitAt, map, mapM, foldl, foldr, last)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad(when)
import Control.DeepSeq(NFData(..))
import Control.Monad.State.Strict(lift)
import Data.STRef as ST

-- | Type alias for a immutable sequence of elements.
type List a       = V.Vector a

-- | Type alias for a mutable sequence of elements.
data TempList m a = TempList {-# Unpack #-} !(ST.STRef m Int) !(ST.STRef m (M.MVector m a))

--instance Show (TempList m a) where
--  showsPrec p (TempList i v) s = Prelude.concat ["TempList ", show i, " ..."]

-- | Empty vector.
empty = V.empty

-- | Vector with a single element
singleton= V.singleton

#ifdef DEFINE_NFDATA_VECTOR
-- It is defined in newer versions of vector package.
instance NFData (V.Vector a)   where
#endif

instance NFData (TempList m a) where
  rnf t@(TempList i v) = i `seq` v `seq` ()

-- Defined in Data.Vector
--instance Show (M.MVector a) where
--  showsPrec v = shows $ V.fromList v

-- | Create a new mutable vector.
new i = lift $ initialNew i

-- | Allocate initial space for a new mutable vector.
initialNew i = do v  <- M.new i
                  s  <- ST.newSTRef 0
                  vs <- ST.newSTRef v
                  return $! TempList s vs

-- | Length of mutable vector.
tempLength (TempList s v) = lift $ readSTRef s

-- | Default initial size of a mutable vector for residue contents.
residueVectorSize =  23 -- most PDB atoms per residue seem to be in RNA

-- | Default initial size of a mutable vector for chain contents.
chainVectorSize   = 150 -- we don't really expect it to be much shorter..., average is 300, but PDB contains more small proteins of course

-- | Default initial size of a mutable vector for structure contents.
defaultSize       =   4 -- Not much point in allocating less than this...

-- | Appends an element to a mutable vector.
add (TempList s vs) a = lift $ do i <- readSTRef s
                                  v <- readSTRef vs
                                  when (i < 0) $ fail "Negative STRef"
                                  let j = i + 1
                                      l = M.length v
                                  v' <- if j > l
                                          then do nV <- M.grow v j
                                                  writeSTRef vs nV
                                                  return nV
                                          else return v
                                  M.write v' i a
                                  ST.modifySTRef s (+1)
                                  return ()

-- | Finalizes a mutable vector, and returns immutable vector.
--   [Does it shrink allocated space?]
finalize (TempList s vs) = lift $ do i  <- readSTRef s
                                     when (i < 0) $ fail "Negative STRef"
                                     v  <- readSTRef vs
                                     v' <- V.unsafeFreeze v
                                     writeSTRef s (-1)
                                     -- Can I? || writeSTRef vs undefined ||
                                     return $! V.slice 0 i v'


-- | `foldl` on immutable vectors.
foldl  = V.foldl

-- | `foldl'` on immutable vectors.
foldl' = V.foldl'

-- | `foldr` on immutable vectors.
foldr  = V.foldr 

-- | `map` on immutable vectors.
map    = V.map

-- | `filter` on immutable vectors.
filter = V.filter

-- | `mapM` on immutable vectors.
mapM   = V.mapM

-- | `foldM` on immutable vectors.
foldM  = V.foldM

-- | `length` on immutable vectors.
length = V.length

-- | `last` on immutable vectors.
last   = V.last  

-- | Conversion of an immutable vector to list.
toList = V.toList

-- | `map` on immutable vectors.
vimap  = V.imap

-- | Indexing of an immutable vector.
(!)    = (V.!)

