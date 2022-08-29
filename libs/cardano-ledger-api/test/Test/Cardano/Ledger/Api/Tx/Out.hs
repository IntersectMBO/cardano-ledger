{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.Out
  ( txOutTests,
  )
where

import Cardano.Binary (serialize)
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams hiding (PParams)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Serialization (mkSized, sizedValue)
import qualified Data.ByteString.Lazy as BSL
import GHC.Records
import Lens.Micro
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

propSetBabbageMinTxOut ::
  forall era.
  ( EraTxOut era,
    AtLeastEra BabbageEra era,
    HasField "_coinsPerUTxOByte" (PParams era) Coin
  ) =>
  PParams era ->
  TxOut era ->
  Property
propSetBabbageMinTxOut pp txOut =
  within 1000000 $ -- just in case if there is a problem with termination
    let txOut' = sizedValue (setMinCoinSizedTxOut pp (mkSized txOut))
        size = toInteger (BSL.length (serialize txOut'))
     in (txOut' ^. coinTxOutL)
          === Coin ((160 + size) * unCoin (getField @"_coinsPerUTxOByte" pp))
  where
    _atLeastBabbage = atLeastEra @BabbageEra @era

babbageTxOutTests ::
  forall era.
  ( Arbitrary (PParams era),
    Arbitrary (TxOut era),
    Show (PParams era),
    EraTxOut era,
    AtLeastEra BabbageEra era,
    HasField "_coinsPerUTxOByte" (PParams era) Coin
  ) =>
  [TestTree]
babbageTxOutTests =
  [ testProperty "setBabbageMinTxOut" $ propSetBabbageMinTxOut @era
  ]

conwayTxOutTests ::
  forall era.
  ( Arbitrary (PParams era),
    Arbitrary (TxOut era),
    Show (PParams era),
    EraTxOut era,
    AtLeastEra BabbageEra era,
    AtLeastEra ConwayEra era,
    HasField "_coinsPerUTxOByte" (PParams era) Coin
  ) =>
  [TestTree]
conwayTxOutTests =
  concat
    [ babbageTxOutTests @era
    ]
  where
    _atLeastConway = atLeastEra @ConwayEra @era

txOutTests :: TestTree
txOutTests =
  testGroup "TxOut" $
    concat
      [ babbageTxOutTests @(BabbageEra StandardCrypto),
        conwayTxOutTests @(ConwayEra StandardCrypto)
      ]
