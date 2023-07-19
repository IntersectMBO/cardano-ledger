{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.Out (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.BaseTypes (pvMajor, strictMaybeToMaybe)
import Cardano.Ledger.Binary (Sized (sizedValue), mkSized, serialize)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Val as Val
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

propSetShelleyMinTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (TxOut era)
  , AtMostEra MaryEra era
  ) =>
  Spec
propSetShelleyMinTxOut =
  prop "setShelleyMinTxOut" $ \(pp :: PParams era) (txOut :: TxOut era) ->
    within 1000000 $ -- just in case if there is a problem with termination
      let txOut' = setMinCoinTxOut pp txOut
          val = txOut' ^. valueTxOutL
          minUTxOValue = unCoin $ pp ^. ppMinUTxOValueL
          minVal
            | Val.isAdaOnly val = 0
            | otherwise = (27 + Val.size val) * (minUTxOValue `quot` 27)
       in Val.coin val `shouldBe` Coin (max minVal minUTxOValue)
  where
    _atMostMary = atMostEra @MaryEra @era

propSetAlonzoMinTxOut :: Spec
propSetAlonzoMinTxOut =
  prop "setAlonzoMinTxOut" $ \(pp :: PParams Alonzo) (txOut :: TxOut Alonzo) ->
    within 1000000 $ -- just in case if there is a problem with termination
      let txOut' = setMinCoinTxOut pp txOut
          valSize = Val.size (txOut' ^. valueTxOutL)
          dataHashSize = maybe 0 (const 10) $ strictMaybeToMaybe (txOut' ^. dataHashTxOutL)
          sz = 27 + valSize + dataHashSize
       in (txOut' ^. coinTxOutL)
            `shouldBe` Coin (sz * unCoin (unCoinPerWord (pp ^. ppCoinsPerUTxOWordL)))

propSetBabbageMinTxOut ::
  forall era.
  ( EraTxOut era
  , BabbageEraPParams era
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (TxOut era)
  ) =>
  Spec
propSetBabbageMinTxOut =
  prop "setBabbageMinTxOut" $ \(pp :: PParams era) (txOut :: TxOut era) ->
    within 1000000 $ -- just in case if there is a problem with termination
      let txOut' = setMinCoinTxOut pp txOut
          sz = toInteger (BSL.length (serialize (pvMajor (pp ^. ppProtocolVersionL)) txOut'))
       in (txOut' ^. coinTxOutL)
            `shouldBe` Coin ((160 + sz) * unCoin (unCoinPerByte (pp ^. ppCoinsPerUTxOByteL)))

propSetEnsureMinTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (TxOut era)
  ) =>
  Spec
propSetEnsureMinTxOut =
  prop "setEnsureMinTxOut" $ \(pp :: PParams era) (txOut :: TxOut era) -> do
    ensureMinCoinTxOut pp (txOut & coinTxOutL .~ mempty)
      `shouldBe` setMinCoinTxOut pp (txOut & coinTxOutL .~ mempty)
    (ensureMinCoinTxOut pp txOut ^. coinTxOutL)
      `shouldSatisfy` (>= (setMinCoinTxOut pp txOut ^. coinTxOutL))
    let v = eraProtVerHigh @era
        txOutSz = mkSized v txOut
    ensureMinCoinSizedTxOut pp (mkSized v (txOut & coinTxOutL .~ mempty))
      `shouldBe` setMinCoinSizedTxOut pp (mkSized v (txOut & coinTxOutL .~ mempty))
    (sizedValue (ensureMinCoinSizedTxOut pp txOutSz) ^. coinTxOutL)
      `shouldSatisfy` (>= (sizedValue (setMinCoinSizedTxOut pp txOutSz) ^. coinTxOutL))

spec :: Spec
spec =
  describe "TxOut" $ do
    describe "ShelleyEra" $ do
      propSetShelleyMinTxOut @Shelley
      propSetEnsureMinTxOut @Shelley
    describe "AllegraEra" $ do
      propSetShelleyMinTxOut @Allegra
      propSetEnsureMinTxOut @Allegra
    describe "MaryEra" $ do
      propSetShelleyMinTxOut @Mary
      propSetEnsureMinTxOut @Mary
    describe "AlonzoEra" $ do
      propSetAlonzoMinTxOut
      propSetEnsureMinTxOut @Alonzo
    describe "BabbageEra" $ do
      propSetBabbageMinTxOut @Babbage
      propSetEnsureMinTxOut @Babbage
    describe "ConwayEra" $ do
      propSetBabbageMinTxOut @Conway
      propSetEnsureMinTxOut @Conway
