{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Plutus.Evaluate (debugPlutus)
import System.Environment (getArgs)

main :: IO ()
main = mapM_ (print . debugPlutus @StandardCrypto) =<< getArgs
