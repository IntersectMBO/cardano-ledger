{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Golden (
  goldenListRedeemersDisallowed,
) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Binary (DecoderError (..), DeserialiseFailure (..))
import Cardano.Ledger.Dijkstra.Core (eraProtVerLow)
import Test.Cardano.Ledger.Common (Spec, it)
import Test.Cardano.Ledger.Conway.Binary.Golden (expectDecoderFailure, listRedeemersEnc)
import Test.Cardano.Ledger.Dijkstra.Era (DijkstraEraTest)

goldenListRedeemersDisallowed :: forall era. DijkstraEraTest era => Spec
goldenListRedeemersDisallowed =
  it "Decoding Redeemers encoded as a list fails" $
    expectDecoderFailure @(Redeemers era)
      (eraProtVerLow @era)
      listRedeemersEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (RedeemersRaw DijkstraEra))"
          (DeserialiseFailure 0 "List encoding of redeemers not supported starting with PV 12")
      )
