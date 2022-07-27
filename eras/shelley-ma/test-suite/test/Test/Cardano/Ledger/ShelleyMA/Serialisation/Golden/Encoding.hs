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
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Crypto (..), EraScript (hashScript), PParamsUpdate, hashAuxiliaryData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), hashKey)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import qualified Cardano.Ledger.Shelley.Metadata as SMD
import Cardano.Ledger.Shelley.PParams
  ( ShelleyPParamsHKD (..),
    ShelleyPParamsUpdate,
    Update,
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    RewardAcnt (..),
    ShelleyTxOut (..),
    Wdrl (..),
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData (pattern MAAuxiliaryData)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    ValidityInterval (..),
  )
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..))
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import qualified Cardano.Ledger.Val as Val
import Codec.CBOR.Encoding (Tokens (..))
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.EraBuffet (AllegraEra, MaryEra, TestCrypto)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils
  ( ToTokens (..),
    checkEncodingCBOR,
    checkEncodingCBORAnnotated,
  )
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkGenKey, mkKeyPair)
import Test.Cardano.Ledger.TranslationTools (expectDecodeFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

type A = AllegraEra TestCrypto

type M = MaryEra TestCrypto

-- ===============================================
-- == Test Values for Building Timelock Scripts ==
-- ===============================================

policy1 :: CC.Crypto crypto => Timelock crypto
policy1 = RequireAnyOf . StrictSeq.fromList $ []

policyID1 :: PolicyID TestCrypto
policyID1 = PolicyID . (hashScript @A) $ policy1

policyID2 :: PolicyID TestCrypto
policyID2 = PolicyID . (hashScript @A) . RequireAllOf . StrictSeq.fromList $ []

assetName1 :: SBS.ShortByteString
assetName1 = "a1"

assetName2 :: SBS.ShortByteString
assetName2 = "a2"

assetName3 :: SBS.ShortByteString
assetName3 = "a3"

-- ===========================================
-- == Test Values for Building Transactions ==
-- ===========================================

testGKeyHash :: KeyHash 'Genesis TestCrypto
testGKeyHash = hashKey . snd . mkGenKey $ RawSeed 0 0 0 0 0

testAddrE :: Addr TestCrypto
testAddrE =
  Addr
    Testnet
    (KeyHashObj . hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 1))
    StakeRefNull

testKeyHash :: KeyHash 'Staking TestCrypto
testKeyHash = hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 2)

testStakeCred :: Credential 'Staking TestCrypto
testStakeCred = KeyHashObj . hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 3)

testUpdate ::
  forall era.
  ( Crypto era ~ TestCrypto,
    PParamsUpdate era ~ ShelleyPParamsUpdate era
  ) =>
  Update era
testUpdate =
  Update
    ( ProposedPPUpdates
        ( Map.singleton
            testGKeyHash
            ( ShelleyPParams
                { _minfeeA = SNothing,
                  _minfeeB = SNothing,
                  _maxBBSize = SNothing,
                  _maxTxSize = SNothing,
                  _maxBHSize = SNothing,
                  _keyDeposit = SNothing,
                  _poolDeposit = SNothing,
                  _eMax = SNothing,
                  _nOpt = SJust 100,
                  _a0 = SNothing,
                  _rho = SNothing,
                  _tau = SNothing,
                  _d = SNothing,
                  _extraEntropy = SNothing,
                  _protocolVersion = SNothing,
                  _minUTxOValue = SNothing,
                  _minPoolCost = SNothing
                }
            )
        )
    )
    (EpochNo 0)

-- =============================================
-- == Golden Tests Common to Allegra and Mary ==
-- =============================================

scriptGoldenTest :: forall era. (Era era) => TestTree
scriptGoldenTest =
  let kh0 = hashKey . snd . mkGenKey $ RawSeed 0 0 0 0 0 :: KeyHash 'Witness (Crypto era)
      kh1 = hashKey . snd . mkGenKey $ RawSeed 1 1 1 1 1 :: KeyHash 'Witness (Crypto era)
   in checkEncodingCBORAnnotated
        "timelock_script"
        ( RequireAllOf @(Crypto era)
            ( StrictSeq.fromList
                [ RequireMOf 1 $ StrictSeq.fromList [RequireSignature kh0, RequireSignature kh1],
                  RequireTimeStart (SlotNo 100),
                  RequireTimeExpire (SlotNo 101)
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

metadataNoScritpsGoldenTest :: forall era. (Era era, Core.Script era ~ Timelock (Crypto era)) => TestTree
metadataNoScritpsGoldenTest =
  checkEncodingCBORAnnotated
    "metadata_no_scripts"
    (MAAuxiliaryData @era (Map.singleton 17 (SMD.I 42)) StrictSeq.empty)
    ( T
        ( TkListLen 2 -- structured metadata and auxiliary scripts
            . TkMapLen 1 -- metadata wrapper
            . TkInteger 17
            . TkInteger 42
            . TkListLen 0 -- empty scripts
        )
    )

-- CONTINUE also Scritps
metadataWithScritpsGoldenTest :: forall era. (Era era, Core.Script era ~ Timelock (Crypto era)) => TestTree
metadataWithScritpsGoldenTest =
  checkEncodingCBORAnnotated
    "metadata_with_scripts"
    ( MAAuxiliaryData @era
        (Map.singleton 17 (SMD.I 42))
        (StrictSeq.singleton policy1)
    )
    ( T
        ( TkListLen 2 -- structured metadata and auxiliary scripts
            . TkMapLen 1 -- metadata wrapper
            . TkInteger 17
            . TkInteger 42
            . TkListLen 1 -- one script
        )
        <> S (policy1 @(Crypto era))
    )

-- | Golden Tests for Allegra
goldenEncodingTestsAllegra :: TestTree
goldenEncodingTestsAllegra =
  testGroup
    "Allegra"
    [ checkEncodingCBOR
        "value"
        (Val.inject (Coin 1) :: MaryValue TestCrypto)
        (T (TkInteger 1)),
      scriptGoldenTest @A,
      metadataNoScritpsGoldenTest @A,
      metadataWithScritpsGoldenTest @A,
      -- "minimal_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @A testAddrE (Coin 2)
       in checkEncodingCBORAnnotated
            "minimal_txbody"
            ( MATxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 9)
                (ValidityInterval SNothing SNothing)
                SNothing
                SNothing
                (Coin 0)
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
            ),
      -- "full_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @A testAddrE (Coin 2)
          reg = DCertDeleg (RegKey testStakeCred)
          ras = Map.singleton (RewardAcnt Testnet (KeyHashObj testKeyHash)) (Coin 123)
          up = testUpdate
          mdh = hashAuxiliaryData @A $ MAAuxiliaryData Map.empty StrictSeq.empty
       in checkEncodingCBORAnnotated
            "full_txn_body"
            ( MATxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Wdrl ras)
                (Coin 9)
                (ValidityInterval (SJust $ SlotNo 500) (SJust $ SlotNo 600))
                (SJust up)
                (SJust mdh)
                (Coin 0)
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
        "ada_only_value"
        (Val.inject (Coin 1) :: MaryValue TestCrypto)
        (T (TkInteger 1)),
      checkEncodingCBOR
        "not_just_ada_value"
        ( MaryValue @TestCrypto 2 $
            MultiAsset $
              Map.fromList
                [ ( policyID1,
                    Map.fromList
                      [ (AssetName assetName1, 13),
                        (AssetName assetName2, 17)
                      ]
                  ),
                  ( policyID2,
                    Map.singleton (AssetName assetName3) 19
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
        ),
      checkEncodingCBOR
        "value_with_negative"
        (MaryValue 0 $ MultiAsset $ Map.singleton policyID1 (Map.singleton (AssetName assetName1) (-19)))
        ( T
            ( TkListLen 2
                . TkInteger 0
                . TkMapLen 1
            )
            <> S policyID1
            <> T
              ( TkMapLen 1
                  . TkBytes (SBS.fromShort assetName1)
                  . TkInteger (-19)
              )
        ),
      scriptGoldenTest @M,
      metadataNoScritpsGoldenTest @M,
      metadataWithScritpsGoldenTest @M,
      -- "minimal_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @M testAddrE (Val.inject $ Coin 2)
       in checkEncodingCBORAnnotated
            "minimal_txbody"
            ( MATxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 9)
                (ValidityInterval SNothing SNothing)
                SNothing
                SNothing
                (Val.inject (Coin 0))
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
            ),
      -- "full_txn_body"
      let tin = mkTxInPartial genesisId 1
          tout = ShelleyTxOut @M testAddrE (Val.inject $ Coin 2)
          reg = DCertDeleg (RegKey testStakeCred)
          ras = Map.singleton (RewardAcnt Testnet (KeyHashObj testKeyHash)) (Coin 123)
          up = testUpdate
          mdh = hashAuxiliaryData @A $ MAAuxiliaryData Map.empty StrictSeq.empty
          mint = Map.singleton policyID1 $ Map.singleton (AssetName assetName1) 13
       in checkEncodingCBORAnnotated
            "full_txn_body"
            ( MATxBody
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Wdrl ras)
                (Coin 9)
                (ValidityInterval (SJust $ SlotNo 500) (SJust $ SlotNo 600))
                (SJust up)
                (SJust mdh)
                (MaryValue 0 (MultiAsset mint))
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
assetName32Bytes = expectDecodeFailure $ AssetName "123456789-123456789-123456789-123"

-- | Golden Tests for Allegra and Mary
goldenEncodingTests :: TestTree
goldenEncodingTests =
  testGroup
    "Golden Encoding Tests"
    [ goldenEncodingTestsAllegra,
      goldenEncodingTestsMary,
      testCase "33 bytes asset name too big" assetName32Bytes
    ]
