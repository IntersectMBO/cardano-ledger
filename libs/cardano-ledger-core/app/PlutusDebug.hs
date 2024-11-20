module Main where

import Cardano.Ledger.Plutus.Evaluate (debugPlutus)
import Control.Monad ((<=<))
import System.Environment (getArgs)

main :: IO ()
main = mapM_ (print <=< debugPlutus) =<< getArgs
