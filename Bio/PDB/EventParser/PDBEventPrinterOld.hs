{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Bio.PDB.EventParser.PDBEventPrinter(print, isPrintable)

where

import qualified Prelude(String)
import Prelude((++), Bool(True, False), (.), ($), Int, (+), (>), show, Double)
import Text.Printf(hPrintf)
import System.IO(Handle, IO, stderr)
import qualified Data.ByteString.Char8 as BS
import Data.String(IsString)
import Control.Monad(mapM, return)

import Bio.PDB.EventParser.PDBEvents
import qualified Bio.PDB.EventParser.ExperimentalMethods as ExperimentalMethods

print :: Handle -> PDBEvent -> IO ()
print handle ATOM { no        = num,
                    atomtype  = atype,
                    restype   = rtype,
                    chain     = c,
                    resid     = rid,
                    resins    = rins,
                    altloc    = al,
                    coords    = Vector3 x y z,
                    occupancy = occ,
                    bfactor   = bf,
                    segid     = sid,
                    elt       = e,
                    charge    = ch,
                    hetatm    = isHet
                  } = hPrintf handle
              "%6s%5d  %-3s%c%-3s %c%4d%c   %8.3f%8.3f%8.3f%6.2f%6.2f       %-4s%-2s%-2s\n"
                      recname
                      num (BS.unpack atype) al (BS.unpack rtype) c rid rins
                      x y z occ bf
                      (BS.unpack sid) (BS.unpack e) (BS.unpack ch)
  where
    recname :: Prelude.String
    recname = if isHet then "HETATM" else "ATOM  "
print handle ANISOU { no       = n, 
                      atomtype = atype,
                      restype  = rtype,
                      chain    = c,
                      resid    = rid,
                      resins   = rins,
                      altloc   = al,
                      u_1_1    = u11,
                      u_2_2    = u22,
                      u_3_3    = u33,
                      u_1_2    = u12,
                      u_1_3    = u13,
                      u_2_3    = u23,
                      segid    = sid,
                      elt      = e,
                      charge   = ch 
                    } = hPrintf handle
              "ANISOU%5d  %-3s%c%-3s %c%4d%c %7d%7d%7d%7d%7d%7d   %-4s%-2s%-2s\n"
                      n (BS.unpack atype) al (BS.unpack rtype) c rid rins
                      u11 u22 u33 u12 u13 u23
                      (BS.unpack sid) (BS.unpack e) (BS.unpack ch)
print handle SIGUIJ { no       = n, 
                      atomtype = atype,
                      restype  = rtype,
                      chain    = c,
                      resid    = rid,
                      resins   = rins,
                      altloc   = al,
                      u_1_1    = u11,
                      u_2_2    = u22,
                      u_3_3    = u33,
                      u_1_2    = u12,
                      u_1_3    = u13,
                      u_2_3    = u23,
                      segid    = sid,
                      elt      = e,
                      charge   = ch 
                    } = hPrintf handle
              "SIGUIJ%5d  %-3s%c%-3s %c%4d%c %7d%7d%7d%7d%7d%7d   %-4s%-2s%-2s\n"
                      n (BS.unpack atype) al (BS.unpack rtype) c rid rins
                      u11 u22 u33 u12 u13 u23
                      (BS.unpack sid) (BS.unpack e) (BS.unpack ch)
print handle (HEADER { classification = c,
                       depDate        = d,
                       idCode         = i }) = hPrintf handle "HEADER    %-40s%9s   %4s\n"
                                                  (BS.unpack c)
                                                  (BS.unpack d)
                                                  (BS.unpack i)
print handle MODEL  { num=n } = hPrintf handle "MODEL     %4d\n" n
print handle END    = hPrintf handle "END\n"
print handle ENDMDL = hPrintf handle "ENDMDL\n"
print handle CONECT { atoms=ats } = do hPrintf handle "CONECT"
                                       mapM (hPrintf handle "%5d") ats
                                       hPrintf handle "\n"
print handle TER    { num     = n    ,
                      resname = r    ,
                      chain   = ch   ,
                      resid   = resi ,
                      insCode = i    } = hPrintf handle
              "TER   %5d     %c%-3s %c%4d\n" n i (BS.unpack r) ch resi
print handle MASTER { numRemark = nr,
                      numHet    = nhet,
                      numHelix  = nhel,
                      numSheet  = nsheet,
                      numTurn   = nturn,
                      numSite   = nsite,
                      numXform  = nxform,
                      numAts    = nats,
                      numMaster = nmaster,
                      numConect = ncon,
                      numSeqres = nseq } = do hPrintf handle "MASTER    %5d    0" nr
                                              mapM (hPrintf handle "%5d")
                                                   [nhet, nhel, nsheet,
                                                    nturn, nsite, nxform, nats,
                                                    nmaster, ncon, nseq]
                                              hPrintf handle "\n"
print handle REMARK { num  = n,
                      text = t } = do mapM (hPrintf handle "REMARK %4d %-80s\n" n .
                                            BS.unpack) t
                                      return ()
{-              KEYWDS { continuation  :: !Int,
                         aList         :: ![String] }    |
                AUTHOR { continuation  :: !Int,
                         aList         :: ![String] }    |
                REMARK { num           :: !Int,
                         text          :: ![String] }    -}
print handle KEYWDS { continuation = c,
                      aList        = l } = printList handle "KEYWDS" "," c l
print handle AUTHOR { continuation = c,
                      aList        = l } = printList handle "AUTHOR" "," c l
print handle EXPDTA { continuation = c,
                      expMethods   = e } = do mapM (hPrintf handle "EXPDTA   %c%-80s\n"
                                                            (showContinuation c) .
                                                    BS.unpack .
                                                    ExperimentalMethods.showExpMethod) e
                                              return ()
print handle TITLE { continuation = c,
                     title        = t } = hPrintf handle "TITLE   %c%-80s\n"
                                                         (showContinuation c)
                                                         (contd c $ BS.unpack t)
print handle SEQRES { serial  = sn, 
                      chain   = ch,
                      num     = n,
                      resList = l } = do hPrintf handle "SEQRES %3d %c %4d   " sn ch n
                                         mapM (hPrintf handle "%3s " .
                                               BS.unpack) l
                                         -- TODO: split when longer than X residues
                                         hPrintf handle "\n"
print handle COMPND { cont   = c,
                      tokens = ts } = printSpecList handle "COMPND" c ts
print handle SOURCE { cont   = c,
                      tokens = ts } = printSpecList handle "SOURCE" c ts
print handle SPLIT  { cont   = c,
                      codes  = cs } = printList handle "SPLIT " " " c cs
print handle ORIGXn { n = n,
                      o = vecs,
                      t = f   } = printMatrix handle "ORIGX" n vecs f 
print handle SCALEn { n = n,
                      o = vecs,
                      t = f   } = printMatrix handle "SCALE" n vecs f
print handle CRYST1 { a      = av,
                      b      = bv,
                      c      = cv,
                      alpha  = aa,
                      beta   = ba,
                      gamma  = ga,
                      spcGrp = grp,
                      zValue = z  } = hPrintf handle
                                              "CRYST1 %8.3f %8.3f %8.3f %6.2f %6.2f %6.2f %10s %4d\n"
                                              av bv cv
                                              aa ba ga
                                              (BS.unpack grp)
                                              z
print handle TVECT  { serial = sn,
                      vec    = Vector3 a b c } = hPrintf handle "TVECT %4d%10.5f%10.5f%10.5f\n" sn a b c
print handle JRNL   { cont    = c,
                      content = contents,
                      isFirst = aJRNL } = printJRNL contents
  where
    header :: String
    header  = if aJRNL then "JRNL        " else "REMARK    1 "
    [contd] = if c > 0 then show c else " "
    printJRNL ((k,v):cs) = do hPrintf handle "%12s%4s %c %s\n"
                                      (BS.unpack header)
                                      (BS.unpack k)
                                      contd
                                      (BS.unpack v)

-- print errors:
print handle (PDBParseError c r s) = hPrintf stderr "ERROR: In line %d column %d: %s" c r
                                                    (BS.unpack s)

-- print special case for missing...
print handle e                     = hPrintf stderr "UNIMPLEMENTED: %s\n"
                                                    (show e)

showContinuation 0                 = ' '
showContinuation x | [c] <- show x = c

contd 0 s = s
contd x s = ' ' : s
                                         
-- Temporary method for easier testing
isPrintable ATOM   {}           = True
isPrintable HEADER {}           = True
isPrintable END    {}           = True
isPrintable ENDMDL {}           = True
isPrintable MODEL  {}           = True
isPrintable CONECT {}           = True
isPrintable TER    {}           = True
isPrintable MASTER {}           = True
-- TODO: below
isPrintable AUTHOR {}           = True
isPrintable KEYWDS {}           = True
--isPrintable JRNL   {}           = True
isPrintable TITLE  {}           = True
isPrintable REMARK {}           = True
isPrintable EXPDTA {}           = True
isPrintable SEQRES {}           = True
isPrintable COMPND {}           = True
isPrintable SOURCE {}           = True
isPrintable SPLIT  {}           = True
isPrintable ORIGXn {}           = True
isPrintable SCALEn {}           = True
isPrintable CRYST1 {}           = True
isPrintable ANISOU {}           = True
isPrintable SIGUIJ {}           = True
isPrintable TVECT  {}           = True
isPrintable JRNL   {}           = True

isPrintable (PDBParseError c r s) = True
isPrintable _                     = False

printSpecList handle rectype c ((k, v): ls) = hPrintf handle "%6s   %c%-s:%-s;\n"
                                                             (BS.unpack rectype)
                                                             (showContinuation c)
                                                             (contd c $ BS.unpack k)
                                                             (BS.unpack v)

printList :: Handle -> BS.ByteString -> BS.ByteString -> Int -> [BS.ByteString] -> IO ()
printList handle label sep c l = hPrintf handle "%6s  %c %-80s\n" (BS.unpack label)
                                                                  (showContinuation c)
                                                                  str
  where str = BS.unpack (BS.intercalate sep l)

printMatrix :: Handle -> BS.ByteString -> Int -> [Vector3] -> [Double] -> IO ()
printMatrix handle ident n []         []     = return ()
printMatrix handle ident n (vec:vecs) (f:fs) = do hPrintf handle "%5s%c    " (BS.unpack ident) cn
                                                  mapM printEntry [a, b, c]
                                                  hPrintf handle "      %9.5f\n" f
                                                  printMatrix handle ident (n+1) vecs fs
  where [cn] = show n
        printEntry :: Double -> IO ()
        printEntry f = hPrintf handle "%10.6f" f
        Vector3 a b c = vec

