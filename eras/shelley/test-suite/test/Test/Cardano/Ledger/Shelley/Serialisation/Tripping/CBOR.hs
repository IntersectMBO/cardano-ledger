{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR
  ( tests,
  )
where

import Cardano.Ledger.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    Version,
    fromNotSharedCBOR,
    shelleyProtVer,
  )
import Cardano.Ledger.CompactAddress (fromCborAddr, fromCborRewardAcnt)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.RewardUpdate
  ( FreeVars (..),
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

testsVersion :: Version -> TestTree
testsVersion v =
  testGroup
    ("Version: " ++ show v)
    [ testProperty "Addr" $
        roundTripExpectation @(Addr Mock.C_Crypto) v cborTrip,
      testProperty "Addr (fromCborAddr)" $
        roundTripExpectation @(Addr Mock.C_Crypto) v (mkTrip toCBOR fromCborAddr),
      testProperty "RewardAcnt" $
        roundTripExpectation @(RewardAcnt Mock.C_Crypto) v cborTrip,
      testProperty "RewardAcnt (fromCborRewardAcnt)" $
        roundTripExpectation @(RewardAcnt Mock.C_Crypto) v (mkTrip toCBOR fromCborRewardAcnt),
      testProperty "Header" $
        roundTripAnnExpectation @(TP.BHeader Mock.C_Crypto) v,
      testProperty "Block Header Hash" $
        roundTripExpectation @(TP.HashHeader Mock.C_Crypto) v cborTrip,
      testProperty "Bootstrap Witness" $
        roundTripAnnExpectation @(BootstrapWitness Mock.C_Crypto) v,
      testProperty "TxId" $
        roundTripExpectation @(TxId Mock.C_Crypto) v cborTrip,
      testProperty "Protocol State" $
        roundTripExpectation @(STS.PrtclState Mock.C_Crypto) v cborTrip,
      testProperty "SnapShots" $
        roundTripExpectation @(SnapShots Mock.C_Crypto) v (mkTrip toCBOR fromNotSharedCBOR),
      testProperty "coin CompactCoin cbor" $
        roundTripExpectation @Coin v (mkTrip (toCBOR . fromJust . toCompact) fromCBOR),
      testProperty "coin cbor CompactCoin" $
        roundTripExpectation @Coin v (mkTrip toCBOR (fromCompact <$> fromCBOR)),
      testProperty "RewardUpdate" $
        roundTripExpectation @(RewardUpdate Mock.C_Crypto) v cborTrip,
      testProperty "RewardSnapShot" $
        roundTripExpectation @(RewardSnapShot Mock.C_Crypto) v cborTrip,
      testProperty "RewardFreeVars" $
        roundTripExpectation @(FreeVars Mock.C_Crypto) v cborTrip,
      testProperty "RewardPulser" $
        roundTripExpectation @(Pulser Mock.C_Crypto) v cborTrip,
      testProperty "PulsingRewUpdate" $
        roundTripExpectation @(PulsingRewUpdate Mock.C_Crypto) v cborTrip
    ]

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    $ [ testProperty "Block" $
          roundTripAnnExpectation @(Block (TP.BHeader Mock.C_Crypto) Mock.C) v,
        testProperty "TxBody" $
          roundTripAnnExpectation @(ShelleyTxBody Mock.C) v,
        testProperty "Tx" $
          roundTripAnnExpectation @(ShelleyTx Mock.C) v,
        testProperty "TxOut" $
          roundTripExpectation @(ShelleyTxOut Mock.C) v cborTrip,
        testProperty "LEDGER Predicate Failures" $
          roundTripExpectation @([STS.PredicateFailure (STS.ShelleyLEDGERS Mock.C)]) v cborTrip,
        testProperty "Ledger State" $
          roundTripExpectation @(LedgerState Mock.C) v (mkTrip toCBOR fromNotSharedCBOR),
        testProperty "Epoch State" $
          roundTripExpectation @(EpochState Mock.C) v cborTrip,
        testProperty "NewEpoch State" $
          roundTripExpectation @(NewEpochState Mock.C) v cborTrip,
        testProperty "MultiSig" $
          roundTripAnnExpectation @(MultiSig (ShelleyEra Mock.C_Crypto)) v,
        testProperty "TxAuxData" $
          roundTripAnnExpectation @(ShelleyTxAuxData Mock.C) v,
        testProperty "Shelley Genesis" $
          roundTripExpectation @(ShelleyGenesis Mock.C) v cborTrip
      ]
      ++ map testsVersion [shelleyProtVer .. maxBound]
  where
    v = shelleyProtVer
