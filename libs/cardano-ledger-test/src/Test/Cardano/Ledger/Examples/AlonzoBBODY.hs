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
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), unTxDatsL)
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Network (..),
  StrictMaybe (..),
  textToUrl,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  AlonzoEraPParams,
  AlonzoEraScript,
  AlonzoEraTx (..),
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  AlonzoEraTxWits (..),
  AsIx (..),
  MaryEraTxBody (..),
  pattern CertifyingPurpose,
  pattern MintingPurpose,
  pattern RewardingPurpose,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.PoolParams (PoolMetadata (..))
import Cardano.Ledger.Shelley.API (
  GenDelegs (..),
  LedgerState (..),
  PoolParams (..),
  ProtVer (..),
 )
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyState (..),
  ShelleyPoolPredFailure (..),
 )
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMap (UView (RewDepUView))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (inject, (<->))
import Cardano.Protocol.Crypto (hashVerKeyVRF)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (STS (..))
import qualified Data.ByteString as BS (replicate)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Examples.AlonzoAPI (defaultPParams)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  alwaysFailsHash,
  alwaysSucceedsHash,
  initUTxO,
  mkGenesisTxIn,
  mkSingleRedeemer,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
  testBBODY,
 )
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkKeyPair',
  mkVRFKeyPair,
  standardHashSize,
 )
import Test.Cardano.Protocol.TPraos.Create (VRFKeyPair (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

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
  ( Value era ~ MaryValue
  , Reflect era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , AlonzoEraTest era
  , InjectRuleFailure "BBODY" ShelleyPoolPredFailure era
  ) =>
  Proof era ->
  TestTree
alonzoBBODYexamplesP proof =
  testGroup
    (show proof ++ " BBODY examples")
    [ testCase "eight plutus scripts cases" $
        testBBODY
          (BBODY proof)
          (initialBBodyState initUTxO)
          testAlonzoBlock
          (Right testBBodyState)
          defaultPParams
    , testCase "block with bad pool md hash in tx" $
        testBBODY
          (BBODY proof)
          (initialBBodyState initUTxO)
          testAlonzoBadPMDHBlock
          (Left . pure $ makeTooBig @era)
          defaultPParams
    ]

initialBBodyState ::
  forall era.
  ( EraGov era
  , EraStake era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , EraCertState era
  , AlonzoEraPParams era
  , ShelleyEraScript era
  ) =>
  UTxO era ->
  ShelleyBbodyState era
initialBBodyState utxo =
  BbodyState (LedgerState initialUtxoSt dpstate) (BlocksMade mempty)
  where
    initialUtxoSt =
      smartUTxOState defaultPParams utxo (UM.fromCompact successDeposit) (Coin 0) def mempty
    dpstate =
      def
        & certDStateL
          .~ DState
            { dsUnified =
                UM.insert
                  (scriptStakeCredSuceed @era)
                  (UM.RDPair (UM.CompactCoin 1000) successDeposit)
                  (RewDepUView UM.empty)
            , dsFutureGenDelegs = Map.empty
            , dsGenDelegs = GenDelegs Map.empty
            , dsIRewards = def
            }

testAlonzoBlock ::
  ( EraSegWits era
  , Value era ~ MaryValue
  , ShelleyEraTxCert era
  , AlonzoEraTx era
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
  forall era.
  (EraSegWits era, ShelleyEraScript era) =>
  Block BHeaderView era
testAlonzoBadPMDHBlock = makeNaiveBlock @era [poolMDHTooBigTx]

-- ============================== DATA ===============================

someDatum :: Era era => Data era
someDatum = Data (PV1.I 123)

anotherDatum :: Era era => Data era
anotherDatum = Data (PV1.I 0)

validatingTx ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
validatingTx =
  mkBasicTx validatingBody
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . datsTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ undefined

-- [ WitnessesI
--    [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)]
--    , ScriptWits' [always 3 pf]
--    , DataWits' [someDatum]
--    , RdmrWits $ validatingRedeemers pf
--    ]
-- ]

validatingBody ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraScript era
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

-- [ Inputs' [mkGenesisTxIn 1]
-- , Collateral' [mkGenesisTxIn 11]
-- , Outputs' [validatingTxOut pf]
-- , Txfee (Coin 5)
-- , WppHash
--    ( newScriptIntegrityHash
--        pf
--        (pp pf)
--        [PlutusV1]
--        (validatingRedeemers pf)
--        (mkTxDats someDatum)
--    )
-- ]

validatingRedeemers :: AlonzoEraScript era => Redeemers era
validatingRedeemers = mkSingleRedeemer (SpendingPurpose $ AsIx 0) (Data (PV1.I 42))

validatingTxOut :: EraTxOut era => TxOut era
validatingTxOut = mkBasicTxOut someAddr (inject $ Coin 4995)

notValidatingTx ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
notValidatingTx =
  mkBasicTx notValidatingBody
    & witsTxL . addrTxWitsL .~ Set.singleton undefined
    & witsTxL . scriptTxWitsL .~ Map.singleton undefined undefined
    & witsTxL . datsTxWitsL . unTxDatsL .~ undefined
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
    notValidatingRedeemers = mkSingleRedeemer (SpendingPurpose $ AsIx 1) (Data (PV1.I 1))

validatingTxWithWithdrawal ::
  forall era.
  (EraTx era, AlonzoEraTxBody era, AlonzoEraScript era) =>
  Tx era
validatingTxWithWithdrawal =
  mkBasicTx mkBasicTxBody
    & bodyTxL .~ validatingBodyWithWithdrawal

-- newTx
--  pf
--  [ Body (validatingBodyWithWithdrawal pf)
--  , WitnessesI
--      [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithWithdrawal pf)) (someKeys pf)]
--      , ScriptWits' [always 2 pf]
--      , RdmrWits $ validatingWithWithdrawalRedeemers pf
--      ]
--  ]

validatingBodyWithWithdrawal ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraScript era
  ) =>
  TxBody era
validatingBodyWithWithdrawal =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 5)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 15)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithWithdrawalOut
    & feeTxBodyL .~ Coin 5
    & withdrawalsTxBodyL
      .~ Withdrawals (Map.singleton (RewardAccount Testnet (scriptStakeCredSuceed @era)) $ Coin 1000)
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
  (EraTx era, AlonzoEraTxWits era, AlonzoEraTxBody era) =>
  Tx era
notValidatingTxWithWithdrawal =
  mkBasicTx notValidatingBodyWithWithdrawal
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ notValidatingRedeemers
  where
    -- newTx
    --  pf
    --  [ Body notValidatingBodyWithWithdrawal
    --  , WitnessesI
    --      [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithWithdrawal) (someKeys pf)]
    --      , ScriptWits' [never 1 pf]
    --      , RdmrWits notValidatingRedeemers
    --      ]
    --  ]

    notValidatingBodyWithWithdrawal =
      mkBasicTxBody
        & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 6)
        & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 16)
        & outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut someAddr . inject $ Coin 1995)
        & feeTxBodyL .~ Coin 5
        & withdrawalsTxBodyL
          .~ Withdrawals (Map.singleton (RewardAccount Testnet $ scriptStakeCredFail @era) . inject $ Coin 1995)
        & scriptIntegrityHashTxBodyL
          .~ newScriptIntegrityHash defaultPParams [PlutusV1] notValidatingRedeemers mempty
    notValidatingRedeemers = mkSingleRedeemer (RewardingPurpose $ AsIx 0) (Data (PV1.I 0))

validatingTxWithCert ::
  forall era.
  ( EraTx era
  , ShelleyEraTxCert era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
validatingTxWithCert =
  mkBasicTx validatingBodyWithCert
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ undefined

-- newTx
--  pf
--  [ Body (validatingBodyWithCert pf)
--  , WitnessesI
--      [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithCert pf)) (someKeys pf)]
--      , ScriptWits' [always 2 pf]
--      , RdmrWits $ validatingRedeemrsWithCert pf
--      ]
--  ]

validatingBodyWithCert ::
  forall era.
  ( ShelleyEraTxCert era
  , AlonzoEraTxBody era
  , AlonzoEraScript era
  ) =>
  TxBody era
validatingBodyWithCert =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 3)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 13)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithCertOut
    & certsTxBodyL .~ SSeq.singleton (UnRegTxCert $ scriptStakeCredSuceed @era)
    & feeTxBodyL .~ Coin 5
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemrsWithCert mempty

validatingRedeemrsWithCert :: AlonzoEraScript era => Redeemers era
validatingRedeemrsWithCert = mkSingleRedeemer (CertifyingPurpose $ AsIx 0) (Data (PV1.I 42))

validatingTxWithCertOut :: EraTxOut era => TxOut era
validatingTxWithCertOut =
  mkBasicTxOut someAddr . inject $ Coin 995 <> UM.fromCompact successDeposit

notValidatingTxWithCert ::
  forall era.
  ( EraTx era
  , ShelleyEraTxCert era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
notValidatingTxWithCert =
  mkBasicTx notValidatingBodyWithCert
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ undefined
  where
    -- newTx
    --  pf
    --  [ Body notValidatingBodyWithCert
    --  , WitnessesI
    --      [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithCert) (someKeys pf)]
    --      , ScriptWits' [never 1 pf]
    --      , RdmrWits notValidatingRedeemersWithCert
    --      ]
    --  ]

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
  ( EraTx era
  , Value era ~ MaryValue
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
validatingTxWithMint =
  mkBasicTx validatingBodyWithMint
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ undefined

-- newTx
--  pf
--  [ Body (validatingBodyWithMint pf)
--  , WitnessesI
--      [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithMint pf)) (someKeys pf)]
--      , ScriptWits' [always 2 pf]
--      , RdmrWits $ validatingRedeemersWithMint pf
--      ]
--  ]

validatingBodyWithMint ::
  forall era.
  ( Value era ~ MaryValue
  , AlonzoEraTxBody era
  , AlonzoEraScript era
  ) =>
  TxBody era
validatingBodyWithMint =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.singleton (mkGenesisTxIn 7)
    & collateralInputsTxBodyL .~ Set.singleton (mkGenesisTxIn 17)
    & outputsTxBodyL .~ SSeq.singleton validatingTxWithMintOut
    & feeTxBodyL .~ Coin 5
    & mintTxBodyL .~ multiAsset @era 1 (fromNativeScript $ RequireAllOf mempty)
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash @era defaultPParams [PlutusV1] validatingRedeemersWithMint mempty

validatingRedeemersWithMint :: AlonzoEraScript era => Redeemers era
validatingRedeemersWithMint = mkSingleRedeemer (MintingPurpose $ AsIx 0) (Data (PV1.I 42))

multiAsset :: forall era. EraScript era => Integer -> Script era -> MultiAsset
multiAsset n s =
  MultiAsset $
    Map.singleton
      (PolicyID $ hashScript s)
      (Map.singleton (AssetName "an") n)

validatingTxWithMintOut ::
  forall era.
  ( Value era ~ MaryValue
  , EraTxOut era
  , EraScript era
  ) =>
  TxOut era
validatingTxWithMintOut =
  mkBasicTxOut someAddr . MaryValue (Coin 995) $ multiAsset @era undefined undefined

notValidatingTxWithMint ::
  forall era.
  ( EraTx era
  , Value era ~ MaryValue
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  ) =>
  Tx era
notValidatingTxWithMint =
  mkBasicTx notValidatingBodyWithMint
    & witsTxL . addrTxWitsL .~ undefined
    & witsTxL . scriptTxWitsL .~ undefined
    & witsTxL . rdmrsTxWitsL .~ undefined
  where
    -- newTx
    --  pf
    --  [ Body notValidatingBodyWithMint
    --  , WitnessesI
    --      [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithMint) (someKeys pf)]
    --      , ScriptWits' [never 1 pf]
    --      , RdmrWits notValidatingRedeemersWithMint
    --      ]
    --  ]

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
    ma = undefined -- forge @era 1 (never 1 pf)

poolMDHTooBigTx ::
  forall era.
  (EraTx era, ShelleyEraScript era) =>
  Tx era
poolMDHTooBigTx =
  -- Note that the UTXOW rule will no trigger the expected predicate failure,
  -- since it is checked in the POOL rule. BBODY will trigger it, however.
  mkBasicTx poolMDHTooBigTxBody
    & witsTxL . addrTxWitsL .~ undefined
  where
    -- newTx
    --  pf
    --  [ Body poolMDHTooBigTxBody
    --  , WitnessesI
    --      [ AddrWits' [mkWitnessVKey (hashAnnotated poolMDHTooBigTxBody) (someKeys pf)]
    --      ]
    --  ]

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
            , ppRewardAccount = RewardAccount Testnet $ scriptStakeCredSuceed @era
            , ppOwners = mempty
            , ppRelays = mempty
            , ppMetadata = SJust $ PoolMetadata (fromJust $ textToUrl 64 "") tooManyBytes
            }

-- ============================== Expected UTXO  ===============================

testBBodyState ::
  forall era.
  ( Value era ~ MaryValue
  , EraGov era
  , EraStake era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , ShelleyEraTxCert era
  , EraCertState era
  , AlonzoEraTxBody era
  , AlonzoEraScript era
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
          (someScriptAddr . fromNativeScript @era $ RequireAnyOf mempty)
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
          (someScriptAddr . fromNativeScript @era $ RequireAllOf mempty)
          (inject $ Coin 5000)
      alwaysSucceedsOutputV1 =
        mkBasicTxOut
          (someScriptAddr . fromNativeScript @era $ RequireAllOf mempty)
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
  forall era. EraSegWits era => [Tx era] -> Block BHeaderView era
makeNaiveBlock txs = Block bhView txSeq
  where
    bhView =
      BHeaderView
        { bhviewID = hashKey (vKey coldKeys)
        , bhviewBSize = fromIntegral $ bBodySize (ProtVer (eraProtVerLow @era) 0) txSeq
        , bhviewHSize = 0
        , bhviewBHash = hashTxSeq txSeq
        , bhviewSlot = SlotNo 0
        }
    txSeq = toTxSeq $ StrictSeq.fromList txs

scriptStakeCredFail :: forall era. ShelleyEraScript era => StakeCredential
scriptStakeCredFail = ScriptHashObj (alwaysFailsHash @era)

scriptStakeCredSuceed :: forall era. ShelleyEraScript era => StakeCredential
scriptStakeCredSuceed = ScriptHashObj (alwaysSucceedsHash @era)

-- | The deposit made when 'scriptStakeCredSuceed' was registered. It is also
--   The Refund when 'scriptStakeCredSuceed' is de-registered.
successDeposit :: UM.CompactForm Coin
successDeposit = UM.CompactCoin 7

-- ============================== PParams ===============================

poolDeposit :: Coin
poolDeposit = Coin 5
