{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, OverlappingInstances, TemplateHaskell, FlexibleContexts #-}
-- | Instances of Iterable class.
module Bio.PDB.Iterable.Instances(Iterable(..)) where

import Data.Iterable
import Data.Iterable.Instantiate
import Data.Iterable.Instantiate.Vector
import Data.Vector
import Bio.PDB.Structure.List as L
import Bio.PDB.Structure 
import Control.Monad.Identity
import Control.Monad(foldM)

$(gen_vector_iterable [t| Structure |] [t| Model   |] [e| models   |] [e| (\s m -> s { models   = m }) |] )

$(gen_vector_iterable [t| Model     |] [t| Chain   |] [e| chains   |] [e| (\s m -> s { chains   = m }) |] )

$(gen_vector_iterable [t| Chain     |] [t| Residue |] [e| residues |] [e| (\s m -> s { residues = m }) |] )

$(gen_vector_iterable [t| Residue   |] [t| Atom    |] [e| atoms    |] [e| (\s m -> s { atoms    = m }) |] )

$(self_iterable [t| Structure |] )
$(self_iterable [t| Model     |] )
$(self_iterable [t| Chain     |] )
$(self_iterable [t| Residue   |] )
$(self_iterable [t| Atom      |] )

$(trans_iterable [t| Structure |] [t| Model   |] [t| Chain   |] )
$(trans_iterable [t| Structure |] [t| Model   |] [t| Residue |] )
$(trans_iterable [t| Structure |] [t| Model   |] [t| Atom    |] )

$(trans_iterable [t| Model     |] [t| Chain   |] [t| Residue |] )
$(trans_iterable [t| Model     |] [t| Chain   |] [t| Atom    |] )

$(trans_iterable [t| Chain     |] [t| Residue |] [t| Atom    |] )

