{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Dijkstra.HuddleSpec
import Data.Char (toLower)
import Test.Cardano.Ledger.Binary.Cuddle.GenerateCBOR

main :: IO ()
main =
  generateCBORMain
    [ eraCDDL (eraName @ShelleyEra) shelleyCDDL
    , eraCDDL (eraName @AllegraEra) allegraCDDL
    , eraCDDL (eraName @MaryEra) maryCDDL
    , eraCDDL (eraName @AlonzoEra) alonzoCDDL
    , eraCDDL (eraName @BabbageEra) babbageCDDL
    , eraCDDL (eraName @ConwayEra) conwayCDDL
    , eraCDDL (eraName @DijkstraEra) dijkstraCDDL
    ]
  where
    eraCDDL name cddl = (map toLower name, cddl)
