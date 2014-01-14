{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
-- | High-level output routines for 'Structure'.
module Bio.PDB.StructurePrinter(write, PDBWritable()) where

import Prelude hiding(print)
import Control.Monad(mapM_)
import System.IO(Handle)
import Data.ByteString.Char8 as BS

import Bio.PDB.Structure
import Bio.PDB.Iterable
import Bio.PDB.Structure.List as L
import Bio.PDB.EventParser.PDBEventPrinter as PR
import Bio.PDB.EventParser.PDBEvents 

-- | ShowS like type for a list of `PDBEvent`s.
type PDBEventS = [PDBEvent] -> [PDBEvent]

-- * Class-based interface for generating PDB events from structure fragments.
-- | Writes a structure or its part in a PDB format to a filehandle.
write :: PDBWritable a => Handle -> a -> IO ()
write handle structure = mapM_ (PR.print handle) (pdbEvents structure)

-- | Class generating events for PDB structure fragments.
class PDBWritable a where
    pdbEvents :: a -> [PDBEvent]
    pdbEvents = flip pdbEventS []
    pdbEventS :: a -> PDBEventS

instance PDBWritable Structure
  where
    pdbEvents = structureEvents
    pdbEventS = error "Structure is closed by definition cannot have continuation!"

instance PDBWritable Model
  where
    pdbEventS = modelEvents

instance PDBWritable Chain
  where
    pdbEventS = chainEvents

instance PDBWritable Residue
  where
    pdbEventS = residueEvents blankChain

instance PDBWritable Atom
  where
    pdbEventS = atomEvents blankChain blankResidue

-- | Helper: blank chain in case we don't know which chain residue belongs to.
blankChain   :: Chain
blankChain   = Chain { chainId  = ' '
                     , residues = L.empty }

-- | Helper blank residue in case we don't know which residue the atom belongs to.
blankResidue :: Residue
blankResidue = Residue { resName = "UNK"
                       , resSeq  = 0
                       , insCode = ' '
                       , atoms   = L.empty }

-- * Routines for writing event list for fragments of the structure.
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

