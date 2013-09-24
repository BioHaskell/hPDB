{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | High-level output routines for 'Structure'.
module Bio.PDB.StructurePrinter(write) where

import Prelude hiding(print)
import Data.ByteString.Char8 as BS
import Bio.PDB.Structure
import Bio.PDB.Iterable
import Bio.PDB.Structure.List as L
import Bio.PDB.EventParser.PDBEventPrinter as PR
import Control.Monad(mapM_)
import Bio.PDB.EventParser.PDBEvents 

-- | ShowS like type for a list of `PDBEvent`s.
type PDBEventS = [PDBEvent] -> [PDBEvent]

-- | Writes a structure in a PDB format to a filehandle.
write handle structure = mapM_ (PR.print handle) (structureEvents structure)
                            
-- | Generates list of `PDBEvent`s from a given Structure.
structureEvents :: Structure -> [PDBEvent]
structureEvents s = itfoldr modelEvents [END] s

-- | Generates list of `PDBEvent`s from a given Model.
modelEvents :: Model -> PDBEventS
modelEvents m cont = start:main (ENDMDL : cont)
  where
    start   = MODEL $ modelId m
    main  c = itfoldr chainEvents c m

-- | Generates list of `PDBEvent`s from a given Chain.
chainEvents :: Chain -> PDBEventS
chainEvents ch c = itfoldr (residueEvents ch) (ter:c) ch
  where
    ter = TER { num     = atSer + 1      , -- FIXME: should be lastAtom ch + 1
                resname = lastResName    ,
                chain   = chainId ch     ,
                resid   = lastResSeq     ,
                insCode = lastInsCode    }
    Atom    { atSerial = atSer } = L.last ats
    Residue { resName = lastResName,
              resSeq  = lastResSeq ,
              insCode = lastInsCode,
              atoms   = ats
            } = L.last . Bio.PDB.Structure.residues $ ch

-- | Generates list of `PDBEvent`s from a given Residue and its Chain.
residueEvents :: Chain -> Residue -> PDBEventS
residueEvents ch r c = itfoldr (atomEvents ch r) c r

-- | Generates list of `PDBEvent`s from a given Atom, its Residue, and its Chain.
atomEvents :: Chain -> Residue -> Atom -> PDBEventS
atomEvents   (Chain { chainId = chid }
           ) (Residue { resName = rtype,
                        resSeq  = rid,
                        insCode = rins
                      }
           ) (Atom { atName    = atName,
                     atSerial  = atSer,
                     coord     = coord,
                     bFactor   = bf,
                     occupancy = occ,
                     element   = e,
                     segid     = sid,
                     charge    = ch,
                     hetatm    = isHet
                   }) c = ATOM { no        = atSer, -- TODO: assign atom serial numbers
                                 atomtype  = atName,
                                 restype   = rtype,
                                 chain     = chid,
                                 resid     = rid,
                                 resins    = rins,
                                 altloc    = ' ',
                                 coords    = coord,
                                 occupancy = occ,
                                 bfactor   = bf,
                                 segid     = sid,
                                 elt       = e,
                                 charge    = ch,
                                 hetatm    = isHet
                               } : c
