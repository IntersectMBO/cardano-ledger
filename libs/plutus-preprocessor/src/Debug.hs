{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (PlutusDebug (..), PlutusDebugInfo (..), debugPlutus)
import Cardano.Ledger.BaseTypes (natVersion)
import System.Environment (getArgs)
import System.IO

deriving instance Show PlutusDebugInfo

main :: IO ()
main = print . debugPlutus (natVersion @7) . head =<< getArgs
