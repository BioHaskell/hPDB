{-# LANGUAGE OverloadedStrings  #-}

module Main where

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Mem(performGC)
import qualified Control.Exception(catch)
import Control.Exception.Base(SomeException)
import System.IO
import Bio.PDB.EventParser.PDBParsingAbstractions
import Control.Monad(when)

import qualified Bio.PDB.EventParser.PDBEventPrinter as PDBEventPrinter
import qualified Bio.PDB.EventParser.PDBEvents as PDBEvents
import Bio.PDB.EventParser.PDBEventParser(parsePDBRecords)

import qualified Data.ByteString.Char8 as BS
import Bio.PDB.IO.OpenAnyFile as OpenAnyFile

data Options = Options  { optVerbosity :: Int,
                          eventFilter  :: String -> PDBEvents.PDBEvent -> Maybe String,
                          printing     :: Bool
                        }

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           putStrLn $ usageInfo prg options

showVersion = putStrLn "Version 0.1"

exitAfter function exitCode opts =
  do function
     exitWith exitCode
     return opts

defaultOptions = Options { optVerbosity = 0,
                           eventFilter  = \f a -> Just $ show a,
                           printing     = False
                         }

changeVerbosity :: (Int -> Int) -> Options -> IO Options
changeVerbosity aChange opt = return opt { optVerbosity = aChange (optVerbosity opt) }

--options :: Options -> IO Options
options  :: [OptDescr (Options -> IO Options)]
options = 
  [Option "v" ["verbose"]       (NoArg (changeVerbosity (+ 1)))
           "Increases log verbosity.",
   Option "q" ["quiet"]         (NoArg (changeVerbosity (flip (-) 1)))
           "Decreases log verbosity.",
   Option ""    ["errors-only"]   (NoArg (\opt -> return opt { eventFilter = isParseError }))
           "Show parse errors only.",
   Option ""    ["unhandled"]     (NoArg (\opt -> return opt { eventFilter = isUnhandled }))
           "Show parse errors only.",
   Option "p"   ["printer"]     (NoArg (\opt -> return opt { printing    = True }))
           "Show parse errors only.",
   Option "V" ["version"]       (NoArg (exitAfter showVersion ExitSuccess))
           "Print program version.",
   Option "h" ["help"]          (NoArg (exitAfter showHelp    ExitSuccess))
           "Prints help"
  ]

isParseError fname (PDBEvents.PDBParseError l c s) = Just $ show fname ++ ":" ++
                                                     show (PDBEvents.PDBParseError l c s)
isParseError _     _                               = Nothing

isUnhandled fname (PDBEvents.PDBParseError  l c s) = Just $ show fname ++ ":" ++ show (PDBEvents.PDBParseError  l c s)
isUnhandled fname (PDBEvents.PDBIgnoredLine s    ) = Just $ show fname ++ ":" ++ BS.unpack s
isUnhandled fname _                                = Nothing


processFile :: Options -> Bool -> String -> IO ()
processFile opts printFilename filename = (do
  performGC -- should munmap previous file and deallocate strings;
            -- no need to do it after last file, takes little before first :-).
  --input <- BS.readFile filename 
  input <- OpenAnyFile.readFile filename
  (Control.Monad.when printFilename $
     putStr $ concat ["File ", filename, " contains ", show (BS.length input), " characters.\n"])
  if not (printing opts)
    then parsePDBRecords filename input (processFileLoop filename opts   ) ()
    else parsePDBRecords filename input (\_ e ->
                                            if PDBEventPrinter.isPrintable e
                                              then PDBEventPrinter.print System.IO.stdout e
                                              else return ())
                                                    ())
      `Control.Exception.catch` 
  (\e -> putStrLn $ concat [show filename,
                            " exception: ",
                            show (e :: SomeException)])

-- Passes all events thru a filter, and prints string, if given back.
processFileLoop :: String -> Options -> () -> PDBEvents.PDBEvent -> IO ()
processFileLoop filename opts () evt = let fevt = eventFilter opts filename evt in
  case fevt of
    Just s  -> putStrLn s
    Nothing -> return ()

main = do
  args <- getArgs
  let (actions, filenames, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optVerbosity = verbosity } = opts
  -- <- Put your code here...
  -- mapM processFile
  let show_filenames = length filenames > 1
  mapM_ (processFile opts show_filenames) filenames
  exitWith ExitSuccess


