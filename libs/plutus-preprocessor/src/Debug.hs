{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (PlutusDebug (..), PlutusDebugInfo (..), debugPlutus)
import System.Environment (getArgs)
import System.IO

deriving instance Show PlutusDebugInfo

main :: IO ()
main = print . debugPlutus . head =<< getArgs
