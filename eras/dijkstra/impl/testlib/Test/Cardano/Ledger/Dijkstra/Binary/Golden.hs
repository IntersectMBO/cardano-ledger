{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Golden (
  spec,
) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (DecoderError (..), DeserialiseFailure (..), Tokens (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core (
  EraTxBody (..),
  EraTxOut (..),
  TxLevel (..),
  eraProtVerLow,
  pattern DelegTxCert,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common (Spec, describe, it)
import Test.Cardano.Ledger.Conway.Binary.Golden (expectDecoderFailureAnn, listRedeemersEnc)
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Dijkstra.Era (DijkstraEraTest)

spec :: forall era. DijkstraEraTest era => Spec
spec = describe "Golden" $ do
  goldenListRedeemersDisallowed @era
  goldenDuplicateCertsDisallowed @era

goldenListRedeemersDisallowed :: forall era. DijkstraEraTest era => Spec
goldenListRedeemersDisallowed =
  it "Decoding Redeemers encoded as a list fails" $
    expectDecoderFailureAnn @(Redeemers era)
      (eraProtVerLow @era)
      listRedeemersEnc
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (RedeemersRaw DijkstraEra))"
          (DeserialiseFailure 0 "List encoding of redeemers not supported starting with PV 12")
      )

duplicateCertsTx :: forall era. DijkstraEraTest era => Version -> Enc
duplicateCertsTx v =
  mconcat
    [ E $ TkMapLen 4
    , Em [E @Int 0, Ev v $ Set.empty @TxIn]
    , Em [E @Int 1, Ev v $ [] @(TxOut era)]
    , Em [E @Int 2, E $ Coin 0]
    , Em
        [ E @Int 4
        , Em
            [ E $ TkTag 258
            , E $ TkListLen 2
            , Ev v cert
            , Ev v cert
            ]
        ]
    ]
  where
    cert = DelegTxCert @era (KeyHashObj (mkKeyHash 0)) (DelegStake (mkKeyHash 1))

goldenDuplicateCertsDisallowed :: forall era. DijkstraEraTest era => Spec
goldenDuplicateCertsDisallowed =
  it "Decoding a transaction body with duplicate certificates fails" $
    expectDecoderFailureAnn @(TxBody TopTx era)
      version
      (duplicateCertsTx @era version)
      ( DecoderErrorDeserialiseFailure
          "Annotator (MemoBytes (DijkstraTxBodyRaw TopTx DijkstraEra))"
          ( DeserialiseFailure
              143
              "Final number of elements: 1 does not match the total count that was decoded: 2"
          )
      )
  where
    version = eraProtVerLow @era
