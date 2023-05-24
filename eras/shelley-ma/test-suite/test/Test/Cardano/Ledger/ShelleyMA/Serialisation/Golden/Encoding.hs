{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Golden tests that check CBOR token encoding.
module Test.Cardano.Ledger.ShelleyMA.Serialisation.Golden.Encoding (goldenEncodingTests) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Allegra.Scripts (
  Timelock (..),
  ValidityInterval (..),
 )
import Cardano.Ledger.Allegra.TxAuxData (pattern AllegraTxAuxData)
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), hashKey)
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Shelley.PParams (
  Update,
  pattern ProposedPPUpdates,
  pattern Update,
 )
import qualified Cardano.Ledger.Shelley.TxAuxData as TxAuxData
import Cardano.Ledger.Shelley.TxBody (
  RewardAcnt (..),
  ShelleyTxOut (..),
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxCert (
  pattern RegTxCert,
 )
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import qualified Cardano.Ledger.Val as Val
import Codec.CBOR.Encoding (Tokens (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborRangeFailureExpectation)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils (
  ToTokens (..),
  checkEncodingCBOR,
  checkEncodingCBORAnnotated,
  checkEncodingCBORDecodeFailure,
 )
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkGenKey, mkKeyPair)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

-- ===============================================
-- == Test Values for Building Timelock Scripts ==
-- ===============================================

policy1 :: Era era => Timelock era
policy1 = RequireAnyOf . StrictSeq.fromList $ []

policyID1 :: PolicyID StandardCrypto
policyID1 = PolicyID . (hashScript @Allegra) $ policy1

policyID2 :: PolicyID StandardCrypto
policyID2 = PolicyID . (hashScript @Allegra) . RequireAllOf . StrictSeq.fromList $ []

assetName1 :: SBS.ShortByteString
assetName1 = "a1"

assetName2 :: SBS.ShortByteString
assetName2 = "a2"

assetName3 :: SBS.ShortByteString
assetName3 = "a3"

-- ===========================================
-- == Test Values for Building Transactions ==
-- ===========================================

testGKeyHash :: Crypto c => KeyHash 'Genesis c
testGKeyHash = hashKey . snd . mkGenKey $ RawSeed 0 0 0 0 0

testAddrE :: Addr StandardCrypto
testAddrE =
  Addr
    Testnet
    (KeyHashObj . hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 1))
    StakeRefNull

testKeyHash :: KeyHash 'Staking StandardCrypto
testKeyHash = hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 2)

testStakeCred :: Credential 'Staking StandardCrypto
testStakeCred = KeyHashObj . hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 3)

testUpdate ::
  forall era.
  EraPParams era =>
  Update era
testUpdate =
  Update
    ( ProposedPPUpdates
        ( Map.singleton
            testGKeyHash
            (emptyPParamsUpdate & ppuNOptL .~ SJust 100)
        )
    )
    (EpochNo 0)

-- =============================================
-- == Golden Tests Common to Allegra and Mary ==
-- =============================================

scriptGoldenTest :: forall era. (Era era) => TestTree
scriptGoldenTest =
  let kh0 = hashKey . snd . mkGenKey $ RawSeed 0 0 0 0 0 :: KeyHash 'Witness (EraCrypto era)
      kh1 = hashKey . snd . mkGenKey $ RawSeed 1 1 1 1 1 :: KeyHash 'Witness (EraCrypto era)
   in checkEncodingCBORAnnotated
        (eraProtVerHigh @era)
        "timelock_script"
        ( RequireAllOf @era
            ( StrictSeq.fromList
                [ RequireMOf 1 $ StrictSeq.fromList [RequireSignature kh0, RequireSignature kh1]
                , RequireTimeStart (SlotNo 100)
                , RequireTimeExpire (SlotNo 101)
                ]
            )
        )
        ( T
            ( TkListLen 2
                . TkInteger 1 -- label for RequireAllOf
                . TkListLen 3 -- RequireMOf, RequireTimeStart, RequireTimeExpire
                . TkListLen 3 -- label, m, signatures
                . TkInteger 3 -- label for RequireMOf
                . TkInteger 1 -- m value
                . TkListLen 2 -- two possible signatures
                . TkListLen 2 -- credential wrapper
                . TkInteger 0 -- label for keyhash
            )
            <> S kh0 -- keyhash
            <> T
              ( TkListLen 2 -- credential wrapper
                  . TkInteger 0 -- label for keyhash
              )
            <> S kh1 -- keyhash
            <> T
              ( TkListLen 2 -- RequireTimeStart
                  . TkInteger 4 -- label for RequireTimeStart
                  . TkInteger 100 -- start slot
                  . TkListLen 2 -- RequireTimeExpire
                  . TkInteger 5 -- label for RequireTimeExpire
                  . TkInteger 101 -- expire slot
              )
        )

metadataNoScriptsGoldenTest :: forall era. Era era => TestTree
metadataNoScriptsGoldenTest =
  checkEncodingCBORAnnotated
    (eraProtVerHigh @era)
    "metadata_no_scripts"
    (AllegraTxAuxData @era (Map.singleton 17 (TxAuxData.I 42)) StrictSeq.empty)
    ( T
        ( TkListLen 2 -- structured metadata and auxiliary scripts
            . TkMapLen 1 -- metadata wrapper
            . TkInteger 17
            . TkInteger 42
            . TkListLen 0 -- empty scripts
        )
    )

-- CONTINUE also Scripts
metadataWithScriptsGoldenTest :: forall era. Era era => TestTree
metadataWithScriptsGoldenTest =
  checkEncodingCBORAnnotated
    (eraProtVerHigh @era)
    "metadata_with_scripts"
    ( AllegraTxAuxData @era
        (Map.singleton 17 (TxAuxData.I 42))
        (StrictSeq.singleton policy1)
    )
    ( T
        ( TkListLen 2 -- structured metadata and auxiliary scripts
            . TkMapLen 1 -- metadata wrapper
            . TkInteger 17
            . TkInteger 42
            . TkListLen 1 -- one script
        )
        <> S (policy1 @era)
    )

-- | Golden Tests for Allegra
goldenEncodingTestsAllegra :: TestTree
goldenEncodingTestsAllegra =
  testGroup
    "Allegra"
    [ checkEncodingCBOR
        (eraProtVerHigh @Mary)
        "value"
        (Val.inject (Coin 1) :: Value Allegra)
        (T (TkInteger 1))
    , scriptGoldenTest @Allegra
    , metadataNoScriptsGoldenTest @Allegra
    , metadataWithScriptsGoldenTest @Allegra
    , -- "minimal_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @Allegra testAddrE (Coin 2)
       in checkEncodingCBORAnnotated
            (eraProtVerHigh @Allegra)
            "minimal_txbody"
            ( AllegraTxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Withdrawals Map.empty)
                (Coin 9)
                (ValidityInterval SNothing SNothing)
                SNothing
                SNothing
            )
            ( T (TkMapLen 3)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> T (TkWord64 9)
            )
    , -- "full_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @Allegra testAddrE (Coin 2)
          reg = RegTxCert testStakeCred
          ras = Map.singleton (RewardAcnt Testnet (KeyHashObj testKeyHash)) (Coin 123)
          up = testUpdate
          mdh = hashTxAuxData @Allegra $ AllegraTxAuxData Map.empty StrictSeq.empty
       in checkEncodingCBORAnnotated
            (eraProtVerHigh @Allegra)
            "full_txn_body"
            ( AllegraTxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Withdrawals ras)
                (Coin 9)
                (ValidityInterval (SJust $ SlotNo 500) (SJust $ SlotNo 600))
                (SJust up)
                (SJust mdh)
            )
            ( T (TkMapLen 9)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> S (Coin 9)
                <> T (TkWord 3) -- Tx TTL
                <> S (SlotNo 600)
                <> T (TkWord 4) -- Tx Certs
                <> T (TkListLen 1) -- Seq list begin
                <> S reg
                <> T (TkWord 5) -- Tx Reward Withdrawals
                <> S ras
                <> T (TkWord 6) -- Tx Update
                <> S up
                <> T (TkWord 7) -- Tx AuxiliaryData Hash
                <> S mdh
                <> T (TkWord 8) -- Tx Validity Start
                <> S (SlotNo 500)
            )
    ]

-- | Golden Tests for Mary
goldenEncodingTestsMary :: TestTree
goldenEncodingTestsMary =
  testGroup
    "Mary"
    [ checkEncodingCBOR
        (eraProtVerHigh @Mary)
        "ada_only_value"
        (Val.inject (Coin 1) :: MaryValue StandardCrypto)
        (T (TkInteger 1))
    , checkEncodingCBOR
        (eraProtVerHigh @Mary)
        "not_just_ada_value"
        ( MaryValue @StandardCrypto 2 $
            MultiAsset $
              Map.fromList
                [
                  ( policyID1
                  , Map.fromList
                      [ (AssetName assetName1, 13)
                      , (AssetName assetName2, 17)
                      ]
                  )
                ,
                  ( policyID2
                  , Map.singleton (AssetName assetName3) 19
                  )
                ]
        )
        ( T
            ( TkListLen 2
                . TkInteger 2
                . TkMapLen 2
            )
            <> S policyID1
            <> T
              ( TkMapLen 2
                  . TkBytes (SBS.fromShort assetName1)
                  . TkInteger 13
                  . TkBytes (SBS.fromShort assetName2)
                  . TkInteger 17
              )
            <> S policyID2
            <> T
              ( TkMapLen 1
                  . TkBytes (SBS.fromShort assetName3)
                  . TkInteger 19
              )
        )
    , checkEncodingCBOR
        (eraProtVerHigh @Mary)
        "multiasset_with_negative"
        (MultiAsset $ Map.singleton policyID1 (Map.singleton (AssetName assetName1) (-19)))
        ( T
            ( TkMapLen 1
            )
            <> S policyID1
            <> T
              ( TkMapLen 1
                  . TkBytes (SBS.fromShort assetName1)
                  . TkInteger (-19)
              )
        )
    , checkEncodingCBORDecodeFailure
        (eraProtVerHigh @Mary)
        "value_with_negative"
        (MaryValue 1 $ MultiAsset $ Map.singleton policyID1 (Map.singleton (AssetName assetName1) (-19)))
        ( T
            ( TkListLen 2
                . TkInteger 1
                . TkMapLen 1
            )
            <> S policyID1
            <> T
              ( TkMapLen 1
                  . TkBytes (SBS.fromShort assetName1)
                  . TkInteger (-19)
              )
        )
    , scriptGoldenTest @Mary
    , metadataNoScriptsGoldenTest @Mary
    , metadataWithScriptsGoldenTest @Mary
    , -- "minimal_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @Mary testAddrE (Val.inject $ Coin 2)
       in checkEncodingCBORAnnotated
            (eraProtVerHigh @Mary)
            "minimal_txbody"
            ( MaryTxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Withdrawals Map.empty)
                (Coin 9)
                (ValidityInterval SNothing SNothing)
                SNothing
                SNothing
                mempty
            )
            ( T (TkMapLen 3)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> T (TkWord64 9)
            )
    , -- "full_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @Mary testAddrE (Val.inject $ Coin 2)
          reg = RegTxCert testStakeCred
          ras = Map.singleton (RewardAcnt Testnet (KeyHashObj testKeyHash)) (Coin 123)
          up = testUpdate
          mdh = hashTxAuxData @Allegra $ AllegraTxAuxData Map.empty StrictSeq.empty
          mint = Map.singleton policyID1 $ Map.singleton (AssetName assetName1) 13
       in checkEncodingCBORAnnotated
            (eraProtVerHigh @Mary)
            "full_txn_body"
            ( MaryTxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Withdrawals ras)
                (Coin 9)
                (ValidityInterval (SJust $ SlotNo 500) (SJust $ SlotNo 600))
                (SJust up)
                (SJust mdh)
                (MultiAsset mint)
            )
            ( T (TkMapLen 10)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> S (Coin 9)
                <> T (TkWord 3) -- Tx TTL
                <> S (SlotNo 600)
                <> T (TkWord 4) -- Tx Certs
                <> T (TkListLen 1)
                <> S reg
                <> T (TkWord 5) -- Tx Reward Withdrawals
                <> S ras
                <> T (TkWord 6) -- Tx Update
                <> S up
                <> T (TkWord 7) -- Tx AuxiliaryData Hash
                <> S mdh
                <> T (TkWord 8) -- Tx Validity Start
                <> S (SlotNo 500)
                <> T (TkWord 9) -- Tx Mint
                <> S mint
            )
    ]

assetName32Bytes :: Assertion
assetName32Bytes =
  roundTripCborRangeFailureExpectation (eraProtVerHigh @Mary) maxBound $
    AssetName "123456789-123456789-123456789-123"

-- | Golden Tests for Allegra and Mary
goldenEncodingTests :: TestTree
goldenEncodingTests =
  testGroup
    "Golden Encoding Tests"
    [ goldenEncodingTestsAllegra
    , goldenEncodingTestsMary
    , testCase "33 bytes asset name too big" assetName32Bytes
    ]
