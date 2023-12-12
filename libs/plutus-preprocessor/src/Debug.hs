module Main where

import Cardano.Ledger.Plutus.Evaluate (debugPlutus)
import System.Environment (getArgs)

main :: IO ()
main = print . debugPlutus . head =<< getArgs
