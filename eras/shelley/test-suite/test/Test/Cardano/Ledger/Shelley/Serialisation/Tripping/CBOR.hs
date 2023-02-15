{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR (
  tests,
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decNoShareCBOR,
 )
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.RewardUpdate (
  FreeVars (..),
  Pulser,
  PulsingRewUpdate (..),
  RewardSnapShot (..),
 )
import qualified Cardano.Ledger.Shelley.Rules as STS
import qualified Cardano.Protocol.TPraos.BHeader as TP
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import Data.Maybe (fromJust)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

{-------------------------------------------------------------------------------
  Serialization Properties
-------------------------------------------------------------------------------}

testCoreTypes :: TestTree
testCoreTypes =
  testGroup
    "Core Types"
    [ testProperty "Header" $
        roundTripAnnExpectation @(TP.BHeader StandardCrypto)
    , testProperty "Block Header Hash" $
        roundTripExpectation @(TP.HashHeader StandardCrypto) cborTrip
    , testProperty "Bootstrap Witness" $
        roundTripAnnExpectation @(BootstrapWitness StandardCrypto)
    , testProperty "TxId" $
        roundTripExpectation @(TxId StandardCrypto) cborTrip
    , testProperty "Protocol State" $
        roundTripExpectation @(STS.PrtclState StandardCrypto) cborTrip
    , testProperty "SnapShots" $
        roundTripExpectation @(SnapShots StandardCrypto) (mkTrip encCBOR decNoShareCBOR)
    , testProperty "coin CompactCoin cbor" $
        roundTripExpectation @Coin (mkTrip (encCBOR . fromJust . toCompact) decCBOR)
    , testProperty "coin cbor CompactCoin" $
        roundTripExpectation @Coin (mkTrip encCBOR (fromCompact <$> decCBOR))
    , testProperty "RewardUpdate" $
        roundTripExpectation @(RewardUpdate StandardCrypto) cborTrip
    , testProperty "RewardSnapShot" $
        roundTripExpectation @(RewardSnapShot StandardCrypto) cborTrip
    , testProperty "RewardFreeVars" $
        roundTripExpectation @(FreeVars StandardCrypto) cborTrip
    , testProperty "RewardPulser" $
        roundTripExpectation @(Pulser StandardCrypto) cborTrip
    , testProperty "PulsingRewUpdate" $
        roundTripExpectation @(PulsingRewUpdate StandardCrypto) cborTrip
    ]

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    $ [ testProperty "Block" $
          roundTripAnnRangeExpectation @(Block (TP.BHeader StandardCrypto) Shelley)
            (eraProtVerLow @Shelley)
            (eraProtVerHigh @Shelley)
      , testProperty "TxBody" $
          roundTripAnnRangeExpectation @(ShelleyTxBody Shelley)
            (eraProtVerLow @Shelley)
            (eraProtVerHigh @Shelley)
      , testProperty "Tx" $
          roundTripAnnRangeExpectation @(ShelleyTx Shelley)
            (eraProtVerLow @Shelley)
            (eraProtVerHigh @Shelley)
      , testProperty "TxOut" $
          roundTripExpectation @(ShelleyTxOut Shelley) cborTrip
      , testProperty "LEDGER Predicate Failures" $
          roundTripExpectation @([STS.PredicateFailure (STS.ShelleyLEDGERS Shelley)]) cborTrip
      , testProperty "Ledger State" $
          roundTripExpectation @(LedgerState Shelley) (mkTrip encCBOR decNoShareCBOR)
      , testProperty "Epoch State" $
          roundTripExpectation @(EpochState Shelley) cborTrip
      , testProperty "NewEpoch State" $
          roundTripExpectation @(NewEpochState Shelley) cborTrip
      , testProperty "MultiSig" $
          roundTripAnnRangeExpectation @(MultiSig Shelley)
            (eraProtVerLow @Shelley)
            (eraProtVerHigh @Shelley)
      , testProperty "TxAuxData" $
          roundTripAnnRangeExpectation @(ShelleyTxAuxData Shelley)
            (eraProtVerLow @Shelley)
            (eraProtVerHigh @Shelley)
      , testProperty "Shelley Genesis" $
          roundTripExpectation @(ShelleyGenesis StandardCrypto) cborTrip
      , testProperty "NominalDiffTimeMicro" $
          roundTripExpectation @NominalDiffTimeMicro cborTrip
      , testCoreTypes
      ]
