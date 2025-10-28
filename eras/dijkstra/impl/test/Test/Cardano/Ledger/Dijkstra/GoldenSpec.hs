{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.GoldenSpec (spec) where

import Cardano.Ledger.Binary (DeserialiseFailure (..))
import Cardano.Ledger.Binary.Decoding (DecoderError (..), decodeFull')
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (EraTx (..), eraProtVerLow)
import qualified Data.ByteString as BS
import Paths_cardano_ledger_dijkstra (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)
import Test.Cardano.Ledger.Dijkstra.Era ()

expectDecoderFailure :: FilePath -> DecoderError -> SpecWith ()
expectDecoderFailure path err =
  it path $ do
    bytes <- BS.readFile path
    decodeFull' @(Tx DijkstraEra) (eraProtVerLow @DijkstraEra) bytes
      `shouldBe` Left err

spec :: Spec
spec =
  describe "Golden" $ do
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @DijkstraEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @DijkstraEra
