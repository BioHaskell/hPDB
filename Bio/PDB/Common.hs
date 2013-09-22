-- | Common datatype aliases.
module Bio.PDB.Common(String(..), Vector3(..))

where

import Data.Vector.V3

import Prelude hiding(String)

import qualified Data.ByteString.Char8 as BS
import Control.DeepSeq(NFData(..))
import Bio.PDB.Util.MissingInstances()

-- | We use only strict 'ByteString' as strings in PDB parser.
type String = BS.ByteString

-- -- | Datatype for 3D locations (numbers are in ångströms.)
--instance NFData Vector3 where
--  rnf (Vector3 (x, y, z)) = x `seq` y `seq` z `seq` ()

