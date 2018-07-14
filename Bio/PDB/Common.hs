-- | Common datatype aliases.
module Bio.PDB.Common(String(..), V3(..))

where

import Linear

import Prelude hiding(String)

import qualified Data.ByteString.Char8 as BS
import Control.DeepSeq(NFData(..))
import Bio.PDB.Util.MissingInstances()

-- | We use only strict 'ByteString' as strings in PDB parser.
type String = BS.ByteString

-- -- | Datatype for 3D locations (numbers are in ångströms.)
--instance NFData V3 where
--  rnf (V3 (x, y, z)) = x `seq` y `seq` z `seq` ()

