{-# LANGUAGE OverloadedStrings, PatternGuards, CPP #-}
-- | Low-level output routines: printing any 'PDBEvent'.
module Bio.PDB.EventParser.PDBEventPrinter(print, isPrintable)

where

import qualified Prelude(String)
import Prelude((++), Bool(True, False), (.), ($), Int, (+), (>), (<), show, Double)
import Text.Printf(hPrintf)
import System.IO(Handle, IO, stderr)
import qualified Data.ByteString.Char8 as BS
import Data.String(IsString)
import Control.Monad(mapM_, return)

import           Bio.PDB.EventParser.PDBEvents
import qualified Bio.PDB.EventParser.ExperimentalMethods as ExperimentalMethods
#ifdef HAVE_TEXT_FORMAT
import qualified Data.ByteString.Lazy as L
import           Data.Text.Lazy.Encoding(encodeUtf8)
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder as B
import qualified Data.Text.Format as F
import qualified Data.Text.Buildable as BD
#endif

-- | Prints a PDBEvent to a filehandle.
print :: Handle -> PDBEvent -> IO ()
print handle ATOM { no        = num,
                    atomtype  = atype,
                    restype   = rtype,
                    chain     = c,
                    resid     = rid,
                    resins    = rins,
                    altloc    = al,
                    coords    = V3 x y z,
                    occupancy = occ,
                    bfactor   = bf,
                    segid     = sid,
                    elt       = e,
                    charge    = ch,
                    hetatm    = isHet
                  } =
#ifndef HAVE_TEXT_FORMAT
    hPrintf handle
           "%6s%5d  %-3s%c%-3s %c%4d%c   %8.3f%8.3f%8.3f%6.2f%6.2f       %-4s%-2s%-2s\n"
                   recname
                   num (BS.unpack atype) al (BS.unpack rtype) c rid rins
                   x y z occ bf
                   (BS.unpack sid) (BS.unpack e) (BS.unpack ch)
  where
    recname :: Prelude.String
    recname = if isHet then "HETATM" else "ATOM  "
#else
    L.hPutStr handle . encodeUtf8 $ F.format "{}{} {}{}{} {}{}{}   {}{}{}{}{}       {}{}{}\n" args
  where
    -- ra justifies a ByteString to the right
    ra i = F.right i ' ' . decodeUtf8
    -- la justifies anything else (floating point or integer number) to the left
    la i = F.left  i ' '
    args = (recname, la 5 num, specfmt 4 3 atype,
            conv al, ra 3 rtype,
            conv c, la 4 rid,
            conv rins,
            ca x, ca y, ca z, pa occ, pa bf,
            ra 4 sid, ra 2 e, ra 2 ch)
    ca f = la 8 $ F.fixed 3 f -- align coordinate float
    pa f = la 6 $ F.fixed 2 f -- align property float
    recname = fromText $ if isHet then "HETATM" else "ATOM  "
    --conv :: Char -> Builder
    conv x = fromString [x]
    -- specfmt mimics erratic alignment of PDB atom types: up to three characters are justified left, after prefixing by single space.
    specfmt i j a = B.fromLazyText . LT.justifyRight i ' ' . LT.justifyLeft j ' ' . B.toLazyText . fromText . decodeUtf8 $ a
#endif

-- TODO: Note that this ANISOU code will be buggy for 4-letter atom codes that happen (rarely.)
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
                                       mapM_ (hPrintf handle "%5d") ats
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
                                              mapM_ (hPrintf handle "%5d")
                                                    [nhet, nhel, nsheet,
                                                     nturn, nsite, nxform, nats,
                                                     nmaster, ncon, nseq]
                                              hPrintf handle "\n"
print handle REMARK { num  = n,
                      text = t } = mapM_ (hPrintf handle "REMARK %4d %-80s\n" n .
                                          BS.unpack) t
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
                      expMethods   = e } = mapM_ (hPrintf handle "EXPDTA   %c%-80s\n"
                                                          (showContinuation c) .
                                                  BS.unpack .
                                                  ExperimentalMethods.showExpMethod) e
print handle TITLE { continuation = c,
                     title        = t } = hPrintf handle "TITLE   %c%-80s\n"
                                                         (showContinuation c)
                                                         (contd c $ BS.unpack t)
print handle SEQRES { serial  = sn, 
                      chain   = ch,
                      num     = n,
                      resList = l } = do hPrintf handle "SEQRES %3d %c %4d   " sn ch n
                                         mapM_ (hPrintf handle "%3s " .
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
                      vec    = V3 a b c } = hPrintf handle "TVECT %4d%10.5f%10.5f%10.5f\n" sn a b c
print handle JRNL   { cont    = c,
                      content = contents,
                      isFirst = aJRNL } = printJRNL contents
  where
    header :: String
    header  = if aJRNL then "JRNL        " else "REMARK    1 "
    [contd] = if c > 0 then show c else " "
    printJRNL ((k,v):cs) = hPrintf handle "%12s%4s %c %s\n"
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

-- | For indicating continuation of the record in previous line as a digit with line number.
showContinuation 0                 = ' '
showContinuation x | [c] <- show x = c

-- | For indicating continuation of the text in previous line by indent.
contd 0 s = s
contd x s = ' ' : s
                                         
-- | Reports whether a given PDB record is already printable
--   [temporary method, they all should be.]
--   Including errors.
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

-- | Prints a list of words as a PDB speclist record (see 'Bio.PDB.EventParser.ParseSpecListRecord'.)
printSpecList handle rectype c ((k, v): ls) = hPrintf handle "%6s   %c%-s:%-s;\n"
                                                             (BS.unpack rectype)
                                                             (showContinuation c)
                                                             (contd c $ BS.unpack k)
                                                             (BS.unpack v)

-- | Prints a list of words as a PDB list record (see 'Bio.PDB.EventParser.ParseSpecListRecord'.)
printList :: Handle -> BS.ByteString -> BS.ByteString -> Int -> [BS.ByteString] -> IO ()
printList handle label sep c l = hPrintf handle "%6s  %c %-80s\n" (BS.unpack label)
                                                                  (showContinuation c)
                                                                  str
  where str = BS.unpack (BS.intercalate sep l)

-- | Prints a matrix given as a list of 'V3 Double's.
printMatrix :: Handle -> BS.ByteString -> Int -> [V3 Double] -> [Double] -> IO ()
printMatrix handle ident n []         []     = return ()
printMatrix handle ident n (vec:vecs) (f:fs) = do hPrintf handle "%5s%c    " (BS.unpack ident) cn
                                                  mapM_ printEntry [a, b, c]
                                                  hPrintf handle "      %9.5f\n" f
                                                  printMatrix handle ident (n+1) vecs fs
  where [cn] = show n
        printEntry :: Double -> IO ()
        printEntry = hPrintf handle "%10.6f"
        V3 a b c = vec

