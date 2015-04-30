{-# LANGUAGE OverloadedStrings, CPP #-}

-- | Opening and reading a either normal or gzipped file in an efficient way -
-- either using strict 'ByteString' or mmap

module Bio.PDB.IO.OpenAnyFile(readFile, writeFile)

where

import Prelude hiding   (readFile, writeFile)
import System.Directory (doesFileExist, getPermissions, Permissions(..))
import System.IO.Error  (userError, IOError)
import System.IO        (withFile, IOMode(..))
import Control.Monad    (void)
-- if we have zlib:
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy   as BSL
import qualified Control.Exception      as Exc

-- if we have bzlib
--import qualified Codec.Compression.BZip as BZip

-- if we have MMap:
#ifdef HAVE_MMAP
import System.IO.MMap   (mmapFileByteString)
#endif

-- otherwise:
import qualified Data.ByteString.Char8 as BS

-- | Read file contents as strict 'ByteString'. Uses mmap if possible. May decompress file contents, if needed.
readFile fname = do r <- isReadable fname
                    if r
                      then
                        readFile' fname
                      else
                        throwNotFound fname

readFile' fname = do content <- simpleRead fname
                     let r = let codec = getCodec fname content
                             in BS.concat $ BSL.toChunks $ codec $ BSL.fromChunks [content]
                     return r

throwNotFound :: String -> IO a
throwNotFound fname = ioError $ userError $ concat ["Cannot read ", show fname, "!"]

getCodec fname c | (".gz" `BS.isSuffixOf` BS.pack fname) ||
                   (".Z"  `BS.isSuffixOf` BS.pack fname) = GZip.decompressWith (gzipParams c)
--getCodec fname c | (".bz2" `BS.isSuffixOf` (BS.pack fname)) = BZip.decompressWith (bzipParams c) -- DOESN'T WORK!!!
getCodec fname c                                         = id

gzipParams c = GZip.DecompressParams GZip.defaultWindowBits (fromIntegral (BS.length c * 5))
#if MIN_VERSION_zlib(0,5,4)
                                     Nothing
#endif
#if MIN_VERSION_zlib(0,6,1)
                                     True
#endif
  -- Upper bound: compression rate never exceeded 4.7 for big test files.

--bzipParams c = BZip.DecompressParams BZip.DefaultMemoryLevel (fromIntegral (BS.length c * 7 + 4*1024*1024)) -- Upper bound: compression rate never exceeded 6.7 for big test files + 4MiB buffering.

isReadable fname = do exists <- doesFileExist fname
                      if exists
                        then do perms <- getPermissions fname
                                return $! readable perms 
                        else return False

#ifndef HAVE_MMAP
simpleRead       = BS.readFile 
#else
simpleRead fname = mmapFileByteString fname Nothing `Exc.catch` \e -> do reportError (e :: IOError) -- cannot mmap
                                                                         BS.readFile fname
  where
    reportError e = do putStrLn $ concat [show e, "while trying to mmap('", fname, "')"]
#endif

-- | Write file contents as strict 'ByteString'.
writeFile fname writer = void $ withFile fname WriteMode writer 
