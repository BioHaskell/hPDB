{-# LANGUAGE OverloadedStrings  #-}

module Main where

import System.Environment(getProgName, getArgs)
import System.Exit(exitWith, ExitCode(ExitSuccess), exitFailure)
import System.Console.GetOpt
import Control.Exception(catch)
import Control.Exception.Base(SomeException)
import System.IO
import Prelude hiding(catch)
import Bio.PDB.EventParser.PDBParsingAbstractions
import Control.Monad(when)

import qualified Bio.PDB.EventParser.PDBEventPrinter as PDBEventPrinter
import qualified Bio.PDB.StructureBuilder(parse)

import qualified Data.ByteString.Char8 as BS
import qualified Bio.PDB.IO

data Options = Options

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           putStrLn $ usageInfo prg options

showVersion = putStrLn "Version 0.1" -- TODO: extract version from CABAL declaration

exitAfter function exitCode opts =
  do function
     exitWith exitCode
     return opts

defaultOptions = Options

--options :: Options -> IO Options
options  :: [OptDescr (Options -> IO Options)]
options = 
  [Option "V" ["version"]       (NoArg (exitAfter showVersion ExitSuccess))
           "Print program version.",
   Option "h" ["help"]          (NoArg (exitAfter showHelp    ExitSuccess))
           "Prints help"
  ]

processFile :: Options -> Bool -> String -> IO ()
processFile opts printFilename filename = do
            -- no need to do it after last file, takes little before first :-).
  structure <- Bio.PDB.IO.parse $ BS.pack filename
  Prelude.putStrLn $ show structure

main = do
  args <- getArgs
  let (actions, filenames, opts) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let show_filenames = length filenames > 1
  mapM_ (processFile opts show_filenames) filenames
  exitWith ExitSuccess -- TODO: only if no errors!

