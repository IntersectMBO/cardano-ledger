{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (PlutusDebug (..), PlutusDebugInfo (..), debugPlutus)
import System.Environment (getArgs)
import System.IO

-- Do not remove these instances. They are here for two resons:
--
--  * Prevent usage of Show on these huge data types in production
--  * Allow printing for debugging.
deriving instance Show PlutusDebug

deriving instance Show PlutusDebugInfo

main :: IO ()
main = print . debugPlutus . head =<< getArgs
