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
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Compactible (Compactible (..))
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
import qualified Test.Cardano.Ledger.Binary.Plain.RoundTrip as Plain
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
    , -- , testProperty "SnapShots" $
      --     roundTripExpectation @(SnapShots StandardCrypto) (mkTrip toCBOR fromNotSharedCBOR)
      testProperty "coin CompactCoin cbor" $
        roundTripExpectation @Coin (mkTrip (toCBOR . fromJust . toCompact) fromCBOR)
    , testProperty "coin cbor CompactCoin" $
        roundTripExpectation @Coin (mkTrip toCBOR (fromCompact <$> fromCBOR))
    , testProperty "RewardUpdate" $
        Plain.roundTripExpectation @(RewardUpdate StandardCrypto) Plain.cborTrip
    , testProperty "RewardSnapShot" $
        Plain.roundTripExpectation @(RewardSnapShot StandardCrypto) Plain.cborTrip
    , testProperty "RewardFreeVars" $
        Plain.roundTripExpectation @(FreeVars StandardCrypto) Plain.cborTrip
    , testProperty "RewardPulser" $
        Plain.roundTripExpectation @(Pulser StandardCrypto) Plain.cborTrip
    , testProperty "PulsingRewUpdate" $
        Plain.roundTripExpectation @(PulsingRewUpdate StandardCrypto) Plain.cborTrip
    ]

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    $ [ testProperty "Block" $
          roundTripAnnExpectation @(Block (TP.BHeader StandardCrypto) Shelley)
      , testProperty "TxBody" $
          roundTripAnnExpectation @(ShelleyTxBody Shelley)
      , testProperty "Tx" $
          roundTripAnnExpectation @(ShelleyTx Shelley)
      , testProperty "TxOut" $
          roundTripExpectation @(ShelleyTxOut Shelley) cborTrip
      , testProperty "LEDGER Predicate Failures" $
          roundTripExpectation @([STS.PredicateFailure (STS.ShelleyLEDGERS Shelley)]) cborTrip
      , testProperty "Ledger State" $
          Plain.roundTripExpectation @(LedgerState Shelley) $
            Plain.mkTrip Plain.encCBOR Plain.decNoShareCBOR
      , testProperty "Epoch State" $
          Plain.roundTripExpectation @(EpochState Shelley) Plain.cborTrip
      , testProperty "NewEpoch State" $
          Plain.roundTripExpectation @(NewEpochState Shelley) Plain.cborTrip
      , testProperty "MultiSig" $
          roundTripAnnExpectation @(MultiSig Shelley)
      , testProperty "TxAuxData" $
          roundTripAnnExpectation @(ShelleyTxAuxData Shelley)
      , testProperty "Shelley Genesis" $
          Plain.roundTripExpectation @(ShelleyGenesis StandardCrypto) Plain.cborTrip
      , testProperty "NominalDiffTimeMicro" $
          roundTripExpectation @NominalDiffTimeMicro cborTrip
      , testCoreTypes
      ]
