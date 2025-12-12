{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Api.Tx.Out (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.BaseTypes (pvMajor, strictMaybeToMaybe)
import Cardano.Ledger.Binary (Sized (sizedValue), mkSized, serialize)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Val as Val
import qualified Data.ByteString.Lazy as BSL
import Data.Default (Default (def))
import Data.Functor.Identity
import Data.Word (Word64)
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

genCompactCoin ::
  (Testable prop, EraTxOut era) => TxOut era -> (CompactForm Coin -> prop) -> Property
genCompactCoin txOut =
  let
    val = txOut ^. valueTxOutL
    -- NOTE: Came up with this by solving for `scaledMinDeposit`,
    -- using the part that can get out of bounds. I.e:
    -- `coinsPerUTxOWord * (utxoEntrySizeWithoutVal + size v)`
    -- where
    -- `coinsPerUTxOWord = quot mv (utxoEntrySizeWithoutVal + coinSize)`
    -- `utxoEntrySizeWithoutVal = 27`
    -- `coinSize = 0`
    -- Substituting `maxBound` for `mv` yields the formula below:
    maxCoin = 27 * quot (maxBound :: Word64) (27 + fromIntegral (Val.size val))
   in
    forAll
      ( CompactCoin
          <$> oneof [choose (0, 1000000), choose (0, maxCoin), fromIntegral <$> (arbitrary :: Gen Word)]
      )

propSetShelleyMinTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (TxOut era)
  , AtMostEra "Mary" era
  ) =>
  Spec
propSetShelleyMinTxOut =
  prop "setShelleyMinTxOut" $ \(txOut0 :: TxOut era) ->
    genCompactCoin txOut0 $ \cc ->
      within 1000000 $ -- just in case if there is a problem with termination
        let pp = def & ppMinUTxOValueCompactL .~ cc
            txOut1 = setMinCoinTxOut pp txOut0
            val = txOut1 ^. valueTxOutL
            minUTxOValue = unCoin $ pp ^. ppMinUTxOValueL
            minVal
              | Val.isAdaOnly val = 0
              | otherwise = (27 + Val.size val) * (minUTxOValue `quot` 27)
         in Val.coin val `shouldBe` Coin (max minVal minUTxOValue)
  where
    _atMostMary = atMostEra @"Mary" @era

propSetAlonzoMinTxOut :: Spec
propSetAlonzoMinTxOut =
  prop "setAlonzoMinTxOut" $ \(pp :: PParams AlonzoEra) (txOut :: TxOut AlonzoEra) ->
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
            `shouldBe` Coin ((160 + sz) * (fromIntegral . unCompactCoin . unCoinPerByte) (pp ^. ppCoinsPerUTxOByteL))

propSetEnsureMinTxOutWith ::
  forall era.
  EraTxOut era =>
  PParams era ->
  TxOut era ->
  IO ()
propSetEnsureMinTxOutWith pp txOut = do
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

propSetEnsureMinTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (TxOut era)
  ) =>
  Spec
propSetEnsureMinTxOut =
  prop "setEnsureMinTxOut" $ propSetEnsureMinTxOutWith @era

propSetMaryEnsureMinTxOut :: Spec
propSetMaryEnsureMinTxOut =
  prop "setMaryEnsureMinTxOut" $ \(txOut :: TxOut MaryEra) ->
    genCompactCoin txOut $ \cc -> do
      let pp = def & ppMinUTxOValueCompactL .~ cc
      propSetEnsureMinTxOutWith pp txOut

spec :: Spec
spec =
  describe "TxOut" $ do
    describe "ShelleyEra" $ do
      propSetShelleyMinTxOut @ShelleyEra
      propSetEnsureMinTxOut @ShelleyEra
    describe "AllegraEra" $ do
      propSetShelleyMinTxOut @AllegraEra
      propSetEnsureMinTxOut @AllegraEra
    describe "MaryEra" $ do
      propSetShelleyMinTxOut @MaryEra
      propSetMaryEnsureMinTxOut
    describe "AlonzoEra" $ do
      propSetAlonzoMinTxOut
      propSetEnsureMinTxOut @AlonzoEra
    describe "BabbageEra" $ do
      propSetBabbageMinTxOut @BabbageEra
      propSetEnsureMinTxOut @BabbageEra
    describe "ConwayEra" $ do
      propSetBabbageMinTxOut @ConwayEra
      propSetEnsureMinTxOut @ConwayEra
    describe "DijkstraEra" $ do
      propSetBabbageMinTxOut @DijkstraEra
      propSetEnsureMinTxOut @DijkstraEra
