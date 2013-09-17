#! /usr/bin/env runhaskell

module Main(main) where

import Distribution.Simple
import System.Environment
import Control.Monad(when)
import Data.List(isPrefixOf)

-- Defaults to installing in --user package database.
isPkgDbOpts arg = ("--package-db" `isPrefixOf` arg  ) || (arg `elem` ["--user", "--global"])
haveNotPkgDbOpts   = not . any isPkgDbOpts

main = do args <- getArgs
          defaultMainArgs $ if (length args > 0 && (head args `elem` ["configure", "install"]) && haveNotPkgDbOpts args)
                              then (head args:"--user":tail args)
                              else args
            
