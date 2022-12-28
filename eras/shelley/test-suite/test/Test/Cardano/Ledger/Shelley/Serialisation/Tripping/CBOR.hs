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
  FromCBOR (..),
  ToCBOR (..),
  fromNotSharedCBOR,
 )
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Shelley (ShelleyEra)
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
import qualified Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes as Mock
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
        roundTripAnnExpectation @(TP.BHeader Mock.C_Crypto)
    , testProperty "Block Header Hash" $
        roundTripExpectation @(TP.HashHeader Mock.C_Crypto) cborTrip
    , testProperty "Bootstrap Witness" $
        roundTripAnnExpectation @(BootstrapWitness Mock.C_Crypto)
    , testProperty "TxId" $
        roundTripExpectation @(TxId Mock.C_Crypto) cborTrip
    , testProperty "Protocol State" $
        roundTripExpectation @(STS.PrtclState Mock.C_Crypto) cborTrip
    , testProperty "SnapShots" $
        roundTripExpectation @(SnapShots Mock.C_Crypto) (mkTrip toCBOR fromNotSharedCBOR)
    , testProperty "coin CompactCoin cbor" $
        roundTripExpectation @Coin (mkTrip (toCBOR . fromJust . toCompact) fromCBOR)
    , testProperty "coin cbor CompactCoin" $
        roundTripExpectation @Coin (mkTrip toCBOR (fromCompact <$> fromCBOR))
    , testProperty "RewardUpdate" $
        roundTripExpectation @(RewardUpdate Mock.C_Crypto) cborTrip
    , testProperty "RewardSnapShot" $
        roundTripExpectation @(RewardSnapShot Mock.C_Crypto) cborTrip
    , testProperty "RewardFreeVars" $
        roundTripExpectation @(FreeVars Mock.C_Crypto) cborTrip
    , testProperty "RewardPulser" $
        roundTripExpectation @(Pulser Mock.C_Crypto) cborTrip
    , testProperty "PulsingRewUpdate" $
        roundTripExpectation @(PulsingRewUpdate Mock.C_Crypto) cborTrip
    ]

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    $ [ testProperty "Block" $
          roundTripAnnExpectation @(Block (TP.BHeader Mock.C_Crypto) Mock.C)
      , testProperty "TxBody" $
          roundTripAnnExpectation @(ShelleyTxBody Mock.C)
      , testProperty "Tx" $
          roundTripAnnExpectation @(ShelleyTx Mock.C)
      , testProperty "TxOut" $
          roundTripExpectation @(ShelleyTxOut Mock.C) cborTrip
      , testProperty "LEDGER Predicate Failures" $
          roundTripExpectation @([STS.PredicateFailure (STS.ShelleyLEDGERS Mock.C)]) cborTrip
      , testProperty "Ledger State" $
          roundTripExpectation @(LedgerState Mock.C) (mkTrip toCBOR fromNotSharedCBOR)
      , testProperty "Epoch State" $
          roundTripExpectation @(EpochState Mock.C) cborTrip
      , testProperty "NewEpoch State" $
          roundTripExpectation @(NewEpochState Mock.C) cborTrip
      , testProperty "MultiSig" $
          roundTripAnnExpectation @(MultiSig (ShelleyEra Mock.C_Crypto))
      , testProperty "TxAuxData" $
          roundTripAnnExpectation @(ShelleyTxAuxData Mock.C)
      , testProperty "Shelley Genesis" $
          roundTripExpectation @(ShelleyGenesis Mock.C) cborTrip
      , testProperty "NominalDiffTimeMicro" $
          roundTripExpectation @NominalDiffTimeMicro cborTrip
      , testCoreTypes
      ]
