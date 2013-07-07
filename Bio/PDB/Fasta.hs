{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Bio.PDB.Fasta(resname2fastacode, fastacode2resname  ,
                     defaultResname,    defaultFastaCode   ,
                     fastaSequence,     fastaGappedSequence,
                     fastaRecord,       fastaGappedRecord  ) where

import Bio.PDB.Iterable  as Iter
import Bio.PDB.Structure as PDB
import Data.Map          as Map

-- | Standard nucleic acid codes
codebook_nucleic_acids = [
  -- RNA codes
  ("A",  'A'),
  ("C",  'C'),
  ("G",  'G'),
  ("U",  'U'),
  ("I",  'I'),
  -- DNA codes
  ("DA", 'A'),
  ("DG", 'G'),
  ("DC", 'C'),
  ("DT", 'T'),
  ("DI", 'I'),
  -- incorrect name for thymine (instead of DT)
  ("T",  'T')
  ]

-- | List of all correspondences between FASTA 1-letter codes, and PDB 3-letter codes.
codebook = codebook_nucleic_acids ++ codebook_protein

-- | Standard protein codes
codebook_standard_protein = [
  ("ALA", 'A'),
  ("CYS", 'C'),
  ("ASP", 'D'),
  ("GLU", 'E'),
  ("PHE", 'F'),

  ("GLY", 'G'),
  ("HIS", 'H'),
  ("ILE", 'I'),
  ("LYS", 'K'),
  ("LEU", 'L'),

  ("MET", 'M'),
  ("ASN", 'N'),
  ("PRO", 'P'),
  ("GLN", 'Q'),
  ("ARG", 'R'),

  ("SER", 'S'),
  ("THR", 'T'),
  ("VAL", 'V'),
  ("TRP", 'W'),
  ("TYR", 'Y')]

-- | List of both standard and non-standard protein codes.
codebook_protein = codebook_standard_protein ++ [
  -- Protein codes (common variants)
  ("MSE", 'M')] -- selenomethionine

-- | Dictionary of translations from all 3-letter PDB codes into 1-letter FASTA aminoacid codes.
resname2fastacodeDictionary = Map.fromList codebook

-- | Dictionary of translations from 1-letter FASTA aminoacid (standard
--   protein) codes into 3-letter PDB codes.
fastacode2resnameDictionary = Map.fromList . Prelude.map (\(a, b) -> (b, a)) $ codebook_standard_protein

-- | Three-letter PDB code for an unknown type of residue.
defaultResname   = "UNK"

-- | One-letter aminoacid code for an unknown type of residue.
defaultFastaCode = 'X'

-- | Dictionary mapping three-letter PDB residue code to a single-letter FASTA code.
resname2fastacode resname = Map.findWithDefault defaultFastaCode resname resname2fastacodeDictionary

-- | Dictionary mapping single-letter FASTA standard aminoacid code to a PDB residue name
fastacode2resname code    = Map.findWithDefault defaultResname   code    fastacode2resnameDictionary

-- | Converts a `Bio.PDB.Structure.Residue` into a one character aminoacid code.
res2code :: Residue -> Char
res2code r = resname2fastacode . resName $ r

-- | Converts an `Iterable` yielding `Residue`s into a list of aminoacid one-character codes.
fastaSequence :: (Iterable a Residue) => a -> [Char]
fastaSequence = Iter.ifoldr (\a b -> res2code a : b) []

-- | Converts an `Iterable` yielding `Residue`s into a list of aminoacid one-character codes.
fastaGappedSequence :: (Iterable a Residue) => a -> [Char]
fastaGappedSequence = concat . scan2 insertGaps projectAA . Iter.ifoldr (\a b -> (resSeq a, res2code a) : b) []
  where
    projectAA  (i, aa)         = [aa]
    insertGaps (i, _ ) (j, aa) = ['-' | _ <- [2..j-i]] ++ [aa]

-- | Scans a list and applies first argument to all consecutive pairs,
--   and second argument to the beginning or `lone wolf`, mapping to
--   a list of the same length.
scan2 f g []       = []
scan2 f g (b:bs) = g b:scan2' b bs
  where scan2' b (c:cs) = f b c:scan2' c cs
        scan2' b []     = []

-- | Convert a filename and Chain into a text of FASTA format record.
--   First argument tells if we want gaps included.
fastaRecord' :: Bool -> [Char] -> Chain -> [Char]
fastaRecord' withGaps ident c = ">" ++ header ++ "\n" ++ fastaSeq c
  where
    header = if chainId c == ' '
               then ident
               else ident ++ "|" ++ [chainId c]
    fastaSeq = if withGaps then fastaSequence else fastaGappedSequence

fastaRecord       = fastaRecord' False
fastaGappedRecord = fastaRecord' True


