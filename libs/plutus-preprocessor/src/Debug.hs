module Main where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (debugPlutus)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = getArgs >>= (print . debugPlutus PlutusV1 . head)
