{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoBBODY (tests) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (pattern RequireTimeStart)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), hashDataTxWitsL)
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Network (..),
  ShelleyBase,
  StrictMaybe (..),
  natVersion,
  textToUrl,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeCredential)
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  GenDelegs (..),
  LedgerState (..),
  ProtVer (..),
 )
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyState (..),
  ShelleyPoolPredFailure (..),
 )
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject, (<->))
import Cardano.Protocol.Crypto (hashVerKeyVRF)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (STS (..), TRC (..))
import qualified Data.ByteString as BS (replicate)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.TreeDiff (ToExpr)
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkWitnessVKey)
import Test.Cardano.Ledger.Era (registerTestAccount)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  alwaysFailsHash,
  alwaysSucceedsHash,
  genericCont,
  initUTxO,
  mkGenesisTxIn,
  mkSingleRedeemer,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
 )
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkKeyPair',
  mkVRFKeyPair,
 )
import Test.Cardano.Protocol.TPraos.Create (VRFKeyPair (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

forge :: forall era. EraScript era => Integer -> Script era -> MultiAsset
forge n s = MultiAsset $ Map.singleton pid (Map.singleton an n)
  where
    pid = PolicyID (hashScript @era s)
    an = AssetName "an"

tests :: TestTree
tests =
  testGroup
    "Generic Tests, testing Alonzo PredicateFailures, in postAlonzo eras."
    [ alonzoBBODYexamplesP Alonzo
    , alonzoBBODYexamplesP Babbage
    , alonzoBBODYexamplesP Conway
    ]

alonzoBBODYexamplesP ::
  forall era.
  ( BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , InjectRuleFailure "BBODY" ShelleyPoolPredFailure era
  , STS (EraRule "BBODY" era)
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , ShelleyEraTxCert era
  , AlonzoEraTx era
  , Value era ~ MaryValue
  , ToExpr (PredicateFailure (EraRule "BBODY" era))
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  Proof era ->
  TestTree
alonzoBBODYexamplesP proof =
  testGroup
    (show proof ++ " BBODY examples")
    [ testCase "eight plutus scripts cases" $
        runSTS @"BBODY" @era
          (TRC (BbodyEnv @era defaultPParams def, initialBBodyState @era initUTxO, testAlonzoBlock @era))
          (genericCont "" $ Right testBBodyState)
    , testCase "block with bad pool md hash in tx" $
        runSTS @"BBODY" @era
          (TRC (BbodyEnv @era defaultPParams def, initialBBodyState initUTxO, testAlonzoBadPMDHBlock))
          (genericCont "" . Left . pure $ makeTooBig @era)
    ]

initialBBodyState ::
  forall era.
  ( State (EraRule "LEDGERS" era) ~ LedgerState era
  , AlonzoEraPParams era
  , AlonzoEraScript era
  , EraModel era
  ) =>
  UTxO era ->
  ShelleyBbodyState era
initialBBodyState utxo =
  BbodyState (LedgerState initialUtxoSt dpstate) (BlocksMade mempty)
  where
    initialUtxoSt =
      smartUTxOState defaultPParams utxo (fromCompact successDeposit) (Coin 0) def mempty
    ptr = Just (Ptr minBound minBound minBound)
    cred = scriptStakeCredSucceed @era
    dpstate =
      def
        & certDStateL
          .~ DState
            { dsAccounts =
                addToBalanceAccounts (Map.singleton cred (CompactCoin 1000)) $
                  registerTestAccount cred ptr successDeposit Nothing Nothing def
            , dsFutureGenDelegs = Map.empty
            , dsGenDelegs = GenDelegs Map.empty
            , dsIRewards = def
            }

testAlonzoBlock ::
  ( Value era ~ MaryValue
  , ShelleyEraTxCert era
  , AlonzoEraTx era
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  Block BHeaderView era
testAlonzoBlock =
  makeNaiveBlock
    [ validatingTx & isValidTxL .~ IsValid True
    , notValidatingTx & isValidTxL .~ IsValid False
    , validatingTxWithWithdrawal & isValidTxL .~ IsValid True
    , notValidatingTxWithWithdrawal & isValidTxL .~ IsValid False
    , validatingTxWithCert & isValidTxL .~ IsValid True
    , notValidatingTxWithCert & isValidTxL .~ IsValid False
    , validatingTxWithMint & isValidTxL .~ IsValid True
    , notValidatingTxWithMint & isValidTxL .~ IsValid False
    ]

testAlonzoBadPMDHBlock ::
  ( AlonzoEraTx era
  , EraModel era
  ) =>
  Block BHeaderView era
testAlonzoBadPMDHBlock = makeNaiveBlock [poolMDHTooBigTx & isValidTxL .~ IsValid True]

-- ============================== DATA ===============================

someDatum :: Era era => Data era
someDatum = Data (PV1.I 123)

anotherDatum :: Era era => Data era
anotherDatum = Data (PV1.I 0)

validatingTx ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
validatingTx =
  mkBasicTx validatingBody
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated $ validatingBody @era) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [always 3]
    & witsTxL . hashDataTxWitsL .~ [someDatum]
    & witsTxL . rdmrsTxWitsL .~ validatingRedeemers

validatingBody ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraScript era
  , EraModel era
  ) =>
  TxBody era
validatingBody =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 1)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 11)
    & outputsTxBodyL .~ SSeq.singleton validatingTxOut
    & feeTxBodyL .~ Coin 5
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemers (mkTxDats someDatum)

validatingRedeemers :: AlonzoEraScript era => Redeemers era
validatingRedeemers = mkSingleRedeemer (SpendingPurpose $ AsIx 0) (Data (PV1.I 42))

validatingTxOut :: EraTxOut era => TxOut era
validatingTxOut = mkBasicTxOut someAddr (inject $ Coin 4995)

notValidatingTx ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
notValidatingTx =
  mkBasicTx notValidatingBody
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBody) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [never 0]
    & witsTxL . hashDataTxWitsL .~ [anotherDatum]
    & witsTxL . rdmrsTxWitsL .~ notValidatingRedeemers
  where
    notValidatingBody =
      mkBasicTxBody @era
        & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 2)
        & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 12)
        & outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut someAddr (inject $ Coin 2995))
        & feeTxBodyL .~ Coin 5
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash
            defaultPParams
            [PlutusV1]
            notValidatingRedeemers
            (mkTxDats anotherDatum)
    notValidatingRedeemers = mkSingleRedeemer (SpendingPurpose $ AsIx 0) (Data (PV1.I 1))

validatingTxWithWithdrawal ::
  forall era.
  (AlonzoEraTxBody era, EraModel era, AlonzoEraTxWits era) =>
  Tx era
validatingTxWithWithdrawal =
  mkBasicTx validatingBodyWithWithdrawal
    & witsTxL . addrTxWitsL
      .~ [mkWitnessVKey (hashAnnotated $ validatingBodyWithWithdrawal @era) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [always 2]
    & witsTxL . rdmrsTxWitsL .~ validatingWithWithdrawalRedeemers

validatingBodyWithWithdrawal ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraScript era
  , EraModel era
  ) =>
  TxBody era
validatingBodyWithWithdrawal =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 5)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 15)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithWithdrawalOut
    & feeTxBodyL .~ Coin 5
    & withdrawalsTxBodyL
      .~ Withdrawals (Map.singleton (RewardAccount Testnet (scriptStakeCredSucceed @era)) $ Coin 1000)
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era
        defaultPParams
        [PlutusV1]
        validatingWithWithdrawalRedeemers
        mempty

validatingWithWithdrawalRedeemers :: AlonzoEraScript era => Redeemers era
validatingWithWithdrawalRedeemers = mkSingleRedeemer (RewardingPurpose $ AsIx 0) (Data (PV1.I 42))

validatingTxWithWithdrawalOut :: EraTxOut era => TxOut era
validatingTxWithWithdrawalOut = mkBasicTxOut someAddr . inject $ Coin 1995

notValidatingTxWithWithdrawal ::
  forall era.
  ( AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
notValidatingTxWithWithdrawal =
  mkBasicTx notValidatingBodyWithWithdrawal
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBodyWithWithdrawal) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [never 1]
    & witsTxL . rdmrsTxWitsL .~ notValidatingRedeemers
  where
    notValidatingBodyWithWithdrawal =
      mkBasicTxBody
        & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 6)
        & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 16)
        & outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut someAddr . inject $ Coin 1995)
        & feeTxBodyL .~ Coin 5
        & withdrawalsTxBodyL
          .~ Withdrawals (Map.singleton (RewardAccount Testnet $ scriptStakeCredFail @era) . inject $ Coin 1000)
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash defaultPParams [PlutusV1] notValidatingRedeemers mempty
    notValidatingRedeemers = mkSingleRedeemer (RewardingPurpose $ AsIx 0) (Data (PV1.I 0))

validatingTxWithCert ::
  forall era.
  ( ShelleyEraTxCert era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
validatingTxWithCert =
  mkBasicTx validatingBodyWithCert
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated $ validatingBodyWithCert @era) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [always 2]
    & witsTxL . rdmrsTxWitsL .~ validatingRedeemersWithCert

validatingBodyWithCert ::
  forall era.
  ( ShelleyEraTxCert era
  , AlonzoEraTxBody era
  , AlonzoEraScript era
  , EraModel era
  ) =>
  TxBody era
validatingBodyWithCert =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 3)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 13)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithCertOut
    & certsTxBodyL .~ SSeq.singleton (UnRegTxCert $ scriptStakeCredSucceed @era)
    & feeTxBodyL .~ Coin 5
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemersWithCert mempty

validatingRedeemersWithCert :: AlonzoEraScript era => Redeemers era
validatingRedeemersWithCert = mkSingleRedeemer (CertifyingPurpose $ AsIx 0) (Data (PV1.I 42))

validatingTxWithCertOut :: EraTxOut era => TxOut era
validatingTxWithCertOut =
  mkBasicTxOut someAddr . inject $ Coin 995 <> fromCompact successDeposit

notValidatingTxWithCert ::
  forall era.
  ( ShelleyEraTxCert era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
notValidatingTxWithCert =
  mkBasicTx notValidatingBodyWithCert
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBodyWithCert) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [never 1]
    & witsTxL . rdmrsTxWitsL .~ notValidatingRedeemersWithCert
  where
    notValidatingBodyWithCert =
      mkBasicTxBody
        & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 4)
        & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 14)
        & outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut someAddr . inject $ Coin 995)
        & certsTxBodyL .~ SSeq.singleton (UnRegTxCert $ scriptStakeCredFail @era)
        & feeTxBodyL .~ Coin 5
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] notValidatingRedeemersWithCert mempty
    notValidatingRedeemersWithCert = mkSingleRedeemer (CertifyingPurpose $ AsIx 0) (Data (PV1.I 0))

validatingTxWithMint ::
  forall era.
  ( Value era ~ MaryValue
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  Tx era
validatingTxWithMint =
  mkBasicTx validatingBodyWithMint
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated $ validatingBodyWithMint @era) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [always 2]
    & witsTxL . rdmrsTxWitsL .~ validatingRedeemersWithMint

validatingBodyWithMint ::
  forall era.
  ( Value era ~ MaryValue
  , AlonzoEraTxBody era
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  TxBody era
validatingBodyWithMint =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 7)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 17)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithMintOut
    & feeTxBodyL .~ Coin 5
    & mintTxBodyL .~ multiAsset @era
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemersWithMint mempty

validatingRedeemersWithMint :: AlonzoEraScript era => Redeemers era
validatingRedeemersWithMint = mkSingleRedeemer (MintingPurpose $ AsIx 0) (Data (PV1.I 42))

multiAsset :: forall era. EraModel era => MultiAsset
multiAsset = forge @era 1 $ always 2

validatingTxWithMintOut ::
  forall era.
  ( Value era ~ MaryValue
  , EraModel era
  ) =>
  TxOut era
validatingTxWithMintOut =
  mkBasicTxOut someAddr . MaryValue (Coin 995) $ multiAsset @era

notValidatingTxWithMint ::
  forall era.
  ( Value era ~ MaryValue
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , EraModel era
  ) =>
  Tx era
notValidatingTxWithMint =
  mkBasicTx notValidatingBodyWithMint
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated notValidatingBodyWithMint) someKeys]
    & witsTxL . hashScriptTxWitsL .~ [never 1]
    & witsTxL . rdmrsTxWitsL .~ notValidatingRedeemersWithMint
  where
    notValidatingBodyWithMint =
      mkBasicTxBody
        & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 8)
        & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 18)
        & outputsTxBodyL
          .~ [mkBasicTxOut someAddr $ MaryValue (Coin 995) ma]
        & feeTxBodyL .~ Coin 5
        & mintTxBodyL .~ ma
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash defaultPParams [PlutusV1] notValidatingRedeemersWithMint mempty
    notValidatingRedeemersWithMint = mkSingleRedeemer @era (MintingPurpose $ AsIx 0) (Data (PV1.I 0))
    ma = forge @era 1 (never 1)

poolMDHTooBigTx ::
  forall era.
  (ShelleyEraScript era, EraModel era) =>
  Tx era
poolMDHTooBigTx =
  -- Note that the UTXOW rule will no trigger the expected predicate failure,
  -- since it is checked in the POOL rule. BBODY will trigger it, however.
  mkBasicTx poolMDHTooBigTxBody
    & witsTxL . addrTxWitsL .~ [mkWitnessVKey (hashAnnotated poolMDHTooBigTxBody) someKeys]
  where
    poolMDHTooBigTxBody =
      mkBasicTxBody
        & inputsTxBodyL .~ [mkGenesisTxIn 3]
        & outputsTxBodyL .~ [mkBasicTxOut someAddr . inject $ Coin 995 <-> poolDeposit]
        & certsTxBodyL .~ [RegPoolTxCert poolParams]
        & feeTxBodyL .~ Coin 5
      where
        tooManyBytes = BS.replicate (standardHashSize + 1) 0
        poolParams =
          PoolParams
            { ppId = coerceKeyRole . hashKey $ vKey someKeys
            , ppVrf =
                hashVerKeyVRF @MockCrypto . vrfVerKey @MockCrypto . mkVRFKeyPair @MockCrypto $
                  RawSeed 0 0 0 0 0
            , ppPledge = Coin 0
            , ppCost = Coin 0
            , ppMargin = minBound
            , ppRewardAccount = RewardAccount Testnet $ scriptStakeCredSucceed @era
            , ppOwners = mempty
            , ppRelays = mempty
            , ppMetadata = SJust $ PoolMetadata (fromJust $ textToUrl 64 "") tooManyBytes
            }

-- ============================== Expected UTXO  ===============================

testBBodyState ::
  forall era.
  ( Value era ~ MaryValue
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , ShelleyEraTxCert era
  , AlonzoEraTxBody era
  , EraModel era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  ShelleyBbodyState era
testBBodyState =
  let utxo =
        UTxO $
          Map.fromList
            [ (TxIn (txIdTxBody @era validatingBody) minBound, validatingTxOut)
            , (TxIn (txIdTxBody @era validatingBodyWithCert) minBound, validatingTxWithCertOut)
            , (TxIn (txIdTxBody @era validatingBodyWithWithdrawal) minBound, validatingTxWithWithdrawalOut)
            , (TxIn (txIdTxBody @era validatingBodyWithMint) minBound, validatingTxWithMintOut)
            , (mkGenesisTxIn 11, mkBasicTxOut someAddr $ inject $ Coin 5)
            , (mkGenesisTxIn 2, alwaysFailsOutput)
            , (mkGenesisTxIn 13, mkBasicTxOut someAddr . inject $ Coin 5)
            , (mkGenesisTxIn 4, mkBasicTxOut someAddr . inject $ Coin 1000)
            , (mkGenesisTxIn 15, mkBasicTxOut someAddr . inject $ Coin 5)
            , (mkGenesisTxIn 6, mkBasicTxOut someAddr . inject $ Coin 1000)
            , (mkGenesisTxIn 17, mkBasicTxOut someAddr . inject $ Coin 5)
            , (mkGenesisTxIn 8, mkBasicTxOut someAddr . inject $ Coin 1000)
            , (mkGenesisTxIn 100, timelockOut)
            , (mkGenesisTxIn 101, unspendableOut)
            , (mkGenesisTxIn 102, alwaysSucceedsOutputV1)
            , (mkGenesisTxIn 103, nonScriptOutWithDatum)
            ]
      alwaysFailsOutput =
        mkBasicTxOut
          (someScriptAddr $ never @era 0)
          (inject $ Coin 3000)
          & dataHashTxOutL .~ SJust (hashData $ anotherDatum @era)
      timelockOut = mkBasicTxOut timelockAddr . inject $ Coin 1
      timelockAddr = mkAddr timelockHash $ mkKeyPair' @'Staking (RawSeed 0 0 0 0 2)
        where
          timelockHash =
            hashScript . fromNativeScript @era $
              RequireAllOf [RequireSignature $ theKeyHash 1, RequireTimeStart 100]
      -- This output is unspendable since it is locked by a plutus script,
      -- but has no datum hash.
      unspendableOut =
        mkBasicTxOut
          (someScriptAddr $ always @era 3)
          (inject $ Coin 5000)
      alwaysSucceedsOutputV1 =
        mkBasicTxOut
          (someScriptAddr $ always @era 3)
          (inject $ Coin 5000)
          & dataHashTxOutL .~ SJust (hashData $ someDatum @era)
      nonScriptOutWithDatum =
        mkBasicTxOut someAddr (inject $ Coin 1221)
          & dataHashTxOutL .~ SJust (hashData $ someDatum @era)
      poolID = hashKey . vKey . coerceKeyRole $ coldKeys
      example1UtxoSt =
        smartUTxOState defaultPParams utxo totalDeposits (Coin 40) def mempty
      -- the default CertState 'def' means that the 'totalDeposits' must be 0
      totalDeposits = Coin 0
   in BbodyState
        (LedgerState example1UtxoSt def)
        (BlocksMade $ Map.singleton poolID 1)

-- ============================== Helper functions ===============================

makeTooBig ::
  forall era.
  InjectRuleFailure "BBODY" ShelleyPoolPredFailure era =>
  PredicateFailure (EraRule "BBODY" era)
makeTooBig =
  injectFailure @"BBODY" @_ @era $
    PoolMedataHashTooBig (coerceKeyRole . hashKey $ vKey someKeys) (standardHashSize + 1)

coldKeys :: KeyPair 'BlockIssuer
coldKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 2 3 2 1)

makeNaiveBlock ::
  forall era. EraBlockBody era => [Tx era] -> Block BHeaderView era
makeNaiveBlock txs = Block bhView blockBody
  where
    bhView =
      BHeaderView
        { bhviewID = hashKey (vKey coldKeys)
        , bhviewBSize = fromIntegral $ bBodySize (ProtVer (eraProtVerLow @era) 0) blockBody
        , bhviewHSize = 0
        , bhviewBHash = hashBlockBody blockBody
        , bhviewSlot = SlotNo 0
        }
    blockBody = mkBasicBlockBody & txSeqBlockBodyL .~ StrictSeq.fromList txs

scriptStakeCredFail :: forall era. (ShelleyEraScript era, EraModel era) => StakeCredential
scriptStakeCredFail = ScriptHashObj (alwaysFailsHash @era 1)

scriptStakeCredSucceed :: forall era. (ShelleyEraScript era, EraModel era) => StakeCredential
scriptStakeCredSucceed = ScriptHashObj (alwaysSucceedsHash @era 2)

-- | The deposit made when 'scriptStakeCredSucceed' was registered. It is also
--   The Refund when 'scriptStakeCredSucceed' is de-registered.
successDeposit :: CompactForm Coin
successDeposit = CompactCoin 7

-- ============================== PParams ===============================

poolDeposit :: Coin
poolDeposit = Coin 5

defaultPParams :: AlonzoEraPParams era => PParams era
defaultPParams =
  emptyPParams
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1]
    & ppMaxValSizeL .~ 1000000000
    & ppMaxTxExUnitsL .~ ExUnits 1000000 1000000
    & ppMaxBlockExUnitsL .~ ExUnits 1000000 1000000
    & ppProtocolVersionL .~ ProtVer (natVersion @5) 0
    & ppCollateralPercentageL .~ 100
    & ppKeyDepositL .~ Coin 2
    & ppPoolDepositL .~ poolDeposit
