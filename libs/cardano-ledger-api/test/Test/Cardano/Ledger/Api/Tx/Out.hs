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

import Cardano.Ledger.Alonzo.PParams hiding (PParams)
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.Babbage.PParams hiding (PParams)
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Binary (serialize)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.PParams hiding (PParams)
import qualified Cardano.Ledger.Val as Val
import qualified Data.ByteString.Lazy as BSL
import GHC.Records
import Lens.Micro
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

propSetShelleyMinTxOut ::
  forall era.
  ( EraTxOut era,
    Arbitrary (PParams era),
    Arbitrary (TxOut era),
    AtMostEra MaryEra era,
    HasField "_minUTxOValue" (PParams era) Coin
  ) =>
  TestTree
propSetShelleyMinTxOut = testProperty "setShelleyMinTxOut" prop
  where
    _atMostMary = atMostEra @MaryEra @era
    prop :: PParams era -> TxOut era -> Property
    prop pp txOut =
      within 1000000 $ -- just in case if there is a problem with termination
        let txOut' = setMinCoinTxOut pp txOut
            val = txOut' ^. valueTxOutL
            minUTxOValue = unCoin $ getField @"_minUTxOValue" pp
            minVal
              | Val.isAdaOnly val = 0
              | otherwise = (27 + Val.size val) * (minUTxOValue `quot` 27)
         in Val.coin val === Coin (max minVal minUTxOValue)

propSetAlonzoMinTxOut :: TestTree
propSetAlonzoMinTxOut = testProperty "setAlonzoMinTxOut" prop
  where
    prop :: PParams Alonzo -> TxOut Alonzo -> Property
    prop pp txOut =
      within 1000000 $ -- just in case if there is a problem with termination
        let txOut' = setMinCoinTxOut pp txOut
            valSize = Val.size (txOut' ^. valueTxOutL)
            dataHashSize = maybe 0 (const 10) $ strictMaybeToMaybe (txOut' ^. dataHashTxOutL)
            sz = 27 + valSize + dataHashSize
         in (txOut' ^. coinTxOutL) === Coin (sz * unCoin (getField @"_coinsPerUTxOWord" pp))

propSetBabbageMinTxOut ::
  forall era.
  ( EraTxOut era,
    Arbitrary (PParams era),
    Arbitrary (TxOut era),
    AtLeastEra BabbageEra era,
    HasField "_coinsPerUTxOByte" (PParams era) Coin
  ) =>
  TestTree
propSetBabbageMinTxOut = testProperty "setBabbageMinTxOut" prop
  where
    _atLeastBabbage = atLeastEra @BabbageEra @era
    prop :: PParams era -> TxOut era -> Property
    prop pp txOut =
      within 1000000 $ -- just in case if there is a problem with termination
        let txOut' = setMinCoinTxOut pp txOut
            sz = toInteger (BSL.length (serialize (eraProtVerLow @era) txOut'))
         in (txOut' ^. coinTxOutL)
              === Coin ((160 + sz) * unCoin (getField @"_coinsPerUTxOByte" pp))

txOutTests :: TestTree
txOutTests =
  testGroup "TxOut" $
    [ testGroup "ShelleyEra" [propSetShelleyMinTxOut @Shelley],
      testGroup "AllegraEra" [propSetShelleyMinTxOut @Allegra],
      testGroup "MaryEra" [propSetShelleyMinTxOut @Mary],
      testGroup "AlonzoEra" [propSetAlonzoMinTxOut],
      testGroup "BabbageEra" [propSetBabbageMinTxOut @Babbage],
      testGroup "ConwayEra" [propSetBabbageMinTxOut @Conway]
    ]
