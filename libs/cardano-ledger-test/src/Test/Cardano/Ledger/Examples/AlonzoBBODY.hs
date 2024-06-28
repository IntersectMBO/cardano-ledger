{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoBBODY (tests) where

import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoBbodyPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  Network (..),
  StrictMaybe (..),
  natVersion,
  textToUrl,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (ConwayCertsPredFailure (..), ConwayLedgerPredFailure (..))
import qualified Cardano.Ledger.Conway.Rules as Conway (
  ConwayBbodyPredFailure (..),
  ConwayCertPredFailure (..),
 )
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
  StakeReference (..),
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  KeyRole (..),
  coerceKeyRole,
  hashKey,
  hashVerKeyVRF,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.PoolParams (PoolMetadata (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (
  CertState (..),
  DState (..),
  GenDelegs (..),
  LedgerState (..),
  PoolParams (..),
  ProtVer (..),
  UTxO (..),
 )
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (smartUTxOState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyState (..),
  ShelleyDelegsPredFailure (..),
  ShelleyDelplPredFailure (..),
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure (..),
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMap (UView (RewDepUView))
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (inject, (<->))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended (STS (..))
import qualified Data.ByteString as BS (replicate)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW (mkSingleRedeemer)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  alwaysFailsHash,
  alwaysSucceedsHash,
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
  testBBODY,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxField (..),
  TxOutField (..),
  WitnessesField (..),
 )
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag (..))
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (
  HasTokens (..),
  PostShelley,
  Scriptic (..),
  after,
  matchkey,
 )
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkVRFKeyPair,
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
  ( HasTokens era
  , PostShelley era
  , Value era ~ MaryValue (EraCrypto era)
  , EraSegWits era
  , Reflect era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  ) =>
  Proof era ->
  TestTree
alonzoBBODYexamplesP proof =
  testGroup
    (show proof ++ " BBODY examples")
    [ testCase "eight plutus scripts cases" $
        testBBODY
          (BBODY proof)
          (initialBBodyState proof (initUTxO proof))
          (testAlonzoBlock proof)
          (Right (testBBodyState proof))
          (pp proof)
    , testCase "block with bad pool md hash in tx" $
        testBBODY
          (BBODY proof)
          (initialBBodyState proof (initUTxO proof))
          (testAlonzoBadPMDHBlock proof)
          (Left . pure $ makeTooBig proof)
          (pp proof)
    ]

initialBBodyState ::
  ( EraTxOut era
  , PostShelley era
  , EraGov era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  ) =>
  Proof era ->
  UTxO era ->
  ShelleyBbodyState era
initialBBodyState pf utxo =
  BbodyState (LedgerState initialUtxoSt dpstate) (BlocksMade mempty)
  where
    initialUtxoSt =
      smartUTxOState (pp pf) utxo (UM.fromCompact successDeposit) (Coin 0) def mempty
    dpstate =
      def
        { certDState =
            DState
              { dsUnified =
                  UM.insert
                    (scriptStakeCredSuceed pf)
                    (UM.RDPair (UM.CompactCoin 1000) successDeposit)
                    (RewDepUView UM.empty)
              , dsFutureGenDelegs = Map.empty
              , dsGenDelegs = GenDelegs Map.empty
              , dsIRewards = def
              }
        }

testAlonzoBlock ::
  ( GoodCrypto (EraCrypto era)
  , HasTokens era
  , Scriptic era
  , EraSegWits era
  , Value era ~ MaryValue (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Block (BHeaderView (EraCrypto era)) era
testAlonzoBlock pf =
  makeNaiveBlock
    [ trustMeP pf True $ validatingTx pf
    , trustMeP pf False $ notValidatingTx pf
    , trustMeP pf True $ validatingTxWithWithdrawal pf
    , trustMeP pf False $ notValidatingTxWithWithdrawal pf
    , trustMeP pf True $ validatingTxWithCert pf
    , trustMeP pf False $ notValidatingTxWithCert pf
    , trustMeP pf True $ validatingTxWithMint pf
    , trustMeP pf False $ notValidatingTxWithMint pf
    ]

testAlonzoBadPMDHBlock ::
  GoodCrypto (EraCrypto era) => Proof era -> Block (BHeaderView (EraCrypto era)) era
testAlonzoBadPMDHBlock pf@Alonzo = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock pf@Babbage = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock pf@Conway = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock other = error ("testAlonzoBadPMDHBlock does not work in era " ++ show other)

-- ============================== DATA ===============================

someDatum :: Era era => Data era
someDatum = Data (PV1.I 123)

anotherDatum :: Era era => Data era
anotherDatum = Data (PV1.I 0)

validatingTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingTx pf =
  newTx
    pf
    [ Body (validatingBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [someDatum]
        , RdmrWits $ validatingRedeemers pf
        ]
    ]

validatingBody :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
validatingBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1]
    , Collateral' [mkGenesisTxIn 11]
    , Outputs' [validatingTxOut pf]
    , Txfee (Coin 5)
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (validatingRedeemers pf)
            (mkTxDats someDatum)
        )
    ]

validatingRedeemers :: Era era => Proof era -> Redeemers era
validatingRedeemers pf = mkSingleRedeemer pf Spending (Data (PV1.I 42))

validatingTxOut :: EraTxOut era => Proof era -> TxOut era
validatingTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]

notValidatingTx ::
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTx pf =
  newTx
    pf
    [ Body notValidatingBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBody) (someKeys pf)]
        , ScriptWits' [never 0 pf]
        , DataWits' [anotherDatum]
        , RdmrWits notValidatingRedeemers
        ]
    ]
  where
    notValidatingBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 2]
        , Collateral' [mkGenesisTxIn 12]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 2995)]]
        , Txfee (Coin 5)
        , WppHash
            ( newScriptIntegrityHash
                pf
                (pp pf)
                [PlutusV1]
                notValidatingRedeemers
                (mkTxDats anotherDatum)
            )
        ]
    notValidatingRedeemers = mkSingleRedeemer pf Spending (Data (PV1.I 1))

validatingTxWithWithdrawal ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxWithWithdrawal pf =
  newTx
    pf
    [ Body (validatingBodyWithWithdrawal pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithWithdrawal pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingWithWithdrawalRedeemers pf
        ]
    ]

validatingBodyWithWithdrawal :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingBodyWithWithdrawal pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 5]
    , Collateral' [mkGenesisTxIn 15]
    , Outputs' [validatingTxWithWithdrawalOut pf]
    , Txfee (Coin 5)
    , Withdrawals'
        ( Withdrawals $
            Map.singleton
              (RewardAccount Testnet (scriptStakeCredSuceed pf))
              (Coin 1000)
        )
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (validatingWithWithdrawalRedeemers pf)
            mempty
        )
    ]

validatingWithWithdrawalRedeemers :: Era era => Proof era -> Redeemers era
validatingWithWithdrawalRedeemers pf = mkSingleRedeemer pf Rewarding (Data (PV1.I 42))

validatingTxWithWithdrawalOut :: EraTxOut era => Proof era -> TxOut era
validatingTxWithWithdrawalOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]

notValidatingTxWithWithdrawal ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithWithdrawal pf =
  newTx
    pf
    [ Body notValidatingBodyWithWithdrawal
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithWithdrawal) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits notValidatingRedeemers
        ]
    ]
  where
    notValidatingBodyWithWithdrawal =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 6]
        , Collateral' [mkGenesisTxIn 16]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]]
        , Txfee (Coin 5)
        , Withdrawals'
            ( Withdrawals $
                Map.singleton
                  (RewardAccount Testnet (scriptStakeCredFail pf))
                  (Coin 1000)
            )
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemers mempty)
        ]
    notValidatingRedeemers = mkSingleRedeemer pf Rewarding (Data (PV1.I 0))

validatingTxWithCert ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
validatingTxWithCert pf =
  newTx
    pf
    [ Body (validatingBodyWithCert pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithCert pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingRedeemrsWithCert pf
        ]
    ]

validatingBodyWithCert ::
  (Scriptic era, EraTxBody era, ShelleyEraTxCert era) => Proof era -> TxBody era
validatingBodyWithCert pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingTxWithCertOut pf]
    , Certs' [UnRegTxCert (scriptStakeCredSuceed pf)]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemrsWithCert pf) mempty)
    ]

validatingRedeemrsWithCert :: Era era => Proof era -> Redeemers era
validatingRedeemrsWithCert pf = mkSingleRedeemer pf Certifying (Data (PV1.I 42))

validatingTxWithCertOut :: EraTxOut era => Proof era -> TxOut era
validatingTxWithCertOut pf =
  newTxOut
    pf
    [ Address (someAddr pf)
    , Amount (inject $ Coin 995 <> UM.fromCompact successDeposit)
    ]

notValidatingTxWithCert ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithCert pf =
  newTx
    pf
    [ Body notValidatingBodyWithCert
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithCert) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits notValidatingRedeemersWithCert
        ]
    ]
  where
    notValidatingBodyWithCert =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 4]
        , Collateral' [mkGenesisTxIn 14]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]]
        , Certs' [UnRegTxCert (scriptStakeCredFail pf)]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemersWithCert mempty)
        ]
    notValidatingRedeemersWithCert = mkSingleRedeemer pf Certifying (Data (PV1.I 0))

validatingTxWithMint ::
  forall era.
  ( Scriptic era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxWithMint pf =
  newTx
    pf
    [ Body (validatingBodyWithMint pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingBodyWithMint pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingRedeemersWithMint pf
        ]
    ]

validatingBodyWithMint ::
  (HasTokens era, EraTxBody era, Scriptic era, Value era ~ MaryValue (EraCrypto era)) =>
  Proof era ->
  TxBody era
validatingBodyWithMint pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 7]
    , Collateral' [mkGenesisTxIn 17]
    , Outputs' [validatingTxWithMintOut pf]
    , Txfee (Coin 5)
    , Mint (multiAsset pf)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingRedeemersWithMint pf) mempty)
    ]

validatingRedeemersWithMint :: Era era => Proof era -> Redeemers era
validatingRedeemersWithMint pf = mkSingleRedeemer pf Minting (Data (PV1.I 42))

multiAsset :: forall era. (Scriptic era, HasTokens era) => Proof era -> MultiAsset (EraCrypto era)
multiAsset pf = forge @era 1 (always 2 pf)

validatingTxWithMintOut ::
  forall era.
  ( HasTokens era
  , EraTxOut era
  , Scriptic era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  TxOut era
validatingTxWithMintOut pf =
  newTxOut pf [Address (someAddr pf), Amount (MaryValue (Coin 995) (multiAsset pf))]

notValidatingTxWithMint ::
  forall era.
  ( Scriptic era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithMint pf =
  newTx
    pf
    [ Body notValidatingBodyWithMint
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notValidatingBodyWithMint) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits notValidatingRedeemersWithMint
        ]
    ]
  where
    notValidatingBodyWithMint =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 8]
        , Collateral' [mkGenesisTxIn 18]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (MaryValue (Coin 995) ma)]]
        , Txfee (Coin 5)
        , Mint ma
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemersWithMint mempty)
        ]
    notValidatingRedeemersWithMint = mkSingleRedeemer pf Minting (Data (PV1.I 0))
    ma = forge @era 1 (never 1 pf)

poolMDHTooBigTx ::
  forall era.
  ( Scriptic era
  , EraTxBody era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
poolMDHTooBigTx pf =
  -- Note that the UTXOW rule will no trigger the expected predicate failure,
  -- since it is checked in the POOL rule. BBODY will trigger it, however.
  newTx
    pf
    [ Body poolMDHTooBigTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated poolMDHTooBigTxBody) (someKeys pf)]
        ]
    ]
  where
    poolMDHTooBigTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 3]
        , Outputs' [newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 995 <-> poolDeposit)]]
        , Certs' [RegPoolTxCert poolParams]
        , Txfee (Coin 5)
        ]
      where
        tooManyBytes = BS.replicate (hashsize @(EraCrypto era) + 1) 0
        poolParams =
          PoolParams
            { ppId = coerceKeyRole . hashKey . vKey $ someKeys pf
            , ppVrf = hashVerKeyVRF . vrfVerKey . mkVRFKeyPair @(EraCrypto era) $ RawSeed 0 0 0 0 0
            , ppPledge = Coin 0
            , ppCost = Coin 0
            , ppMargin = minBound
            , ppRewardAccount = RewardAccount Testnet (scriptStakeCredSuceed pf)
            , ppOwners = mempty
            , ppRelays = mempty
            , ppMetadata = SJust $ PoolMetadata (fromJust $ textToUrl 64 "") tooManyBytes
            }

-- ============================== Expected UTXO  ===============================

testBBodyState ::
  forall era.
  ( GoodCrypto (EraCrypto era)
  , HasTokens era
  , PostShelley era
  , EraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraGov era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  ShelleyBbodyState era
testBBodyState pf =
  let utxo =
        UTxO $
          Map.fromList
            [ (TxIn (txIdTxBody (validatingBody pf)) minBound, validatingTxOut pf)
            , (TxIn (txIdTxBody (validatingBodyWithCert pf)) minBound, validatingTxWithCertOut pf)
            , (TxIn (txIdTxBody (validatingBodyWithWithdrawal pf)) minBound, validatingTxWithWithdrawalOut pf)
            , (TxIn (txIdTxBody (validatingBodyWithMint pf)) minBound, validatingTxWithMintOut pf)
            , (mkGenesisTxIn 11, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)])
            , (mkGenesisTxIn 2, alwaysFailsOutput)
            , (mkGenesisTxIn 13, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)])
            , (mkGenesisTxIn 4, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 1000)])
            , (mkGenesisTxIn 15, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)])
            , (mkGenesisTxIn 6, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 1000)])
            , (mkGenesisTxIn 17, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)])
            , (mkGenesisTxIn 8, newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 1000)])
            , (mkGenesisTxIn 100, timelockOut)
            , (mkGenesisTxIn 101, unspendableOut)
            , (mkGenesisTxIn 102, alwaysSucceedsOutputV1)
            , (mkGenesisTxIn 103, nonScriptOutWithDatum)
            ]
      alwaysFailsOutput =
        newTxOut
          pf
          [ Address (someScriptAddr (never 0 pf))
          , Amount (inject $ Coin 3000)
          , DHash' [hashData $ anotherDatum @era]
          ]
      timelockOut = newTxOut pf [Address $ timelockAddr, Amount (inject $ Coin 1)]
      timelockAddr = Addr Testnet pCred sCred
        where
          (_ssk, svk) = mkKeyPair @(EraCrypto era) (RawSeed 0 0 0 0 2)
          pCred = ScriptHashObj timelockHash
          sCred = StakeRefBase . KeyHashObj . hashKey $ svk
          timelockHash = hashScript @era $ fromNativeScript $ allOf [matchkey 1, after 100] pf
      -- This output is unspendable since it is locked by a plutus script,
      -- but has no datum hash.
      unspendableOut =
        newTxOut
          pf
          [ Address (someScriptAddr (always 3 pf))
          , Amount (inject $ Coin 5000)
          ]
      alwaysSucceedsOutputV1 =
        newTxOut
          pf
          [ Address (someScriptAddr (always 3 pf))
          , Amount (inject $ Coin 5000)
          , DHash' [hashData $ someDatum @era]
          ]
      nonScriptOutWithDatum =
        newTxOut
          pf
          [ Address (someAddr pf)
          , Amount (inject $ Coin 1221)
          , DHash' [hashData $ someDatum @era]
          ]
      poolID = hashKey . vKey . coerceKeyRole $ coldKeys
      example1UtxoSt =
        smartUTxOState (pp pf) utxo totalDeposits (Coin 40) def mempty
      -- the default CertState 'def' means that the 'totalDeposits' must be 0
      totalDeposits = Coin 0
   in BbodyState
        (LedgerState example1UtxoSt def)
        (BlocksMade $ Map.singleton poolID 1)

-- ============================== Helper functions ===============================

makeTooBig :: Proof era -> PredicateFailure (EraRule "BBODY" era)
makeTooBig proof@Alonzo =
  ShelleyInAlonzoBbodyPredFailure
    . LedgersFailure
    . LedgerFailure
    . DelegsFailure
    . DelplFailure
    . PoolFailure
    $ PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof@Babbage =
  ShelleyInAlonzoBbodyPredFailure
    . LedgersFailure
    . LedgerFailure
    . DelegsFailure
    . DelplFailure
    . PoolFailure
    $ PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof@Conway =
  Conway.LedgersFailure
    . LedgerFailure
    . ConwayCertsFailure
    . CertFailure
    . Conway.PoolFailure
    $ PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof = error ("makeTooBig does not work in era " ++ show proof)

coldKeys :: Crypto c => KeyPair 'BlockIssuer c
coldKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 2 3 2 1)

makeNaiveBlock ::
  forall era. EraSegWits era => [Tx era] -> Block (BHeaderView (EraCrypto era)) era
makeNaiveBlock txs = UnsafeUnserialisedBlock bhView txSeq
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

scriptStakeCredFail :: forall era. Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredFail pf = ScriptHashObj (alwaysFailsHash 1 pf)

scriptStakeCredSuceed :: forall era. Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

-- | The deposit made when 'scriptStakeCredSuceed' was registered. It is also
--   The Refund when 'scriptStakeCredSuceed' is de-registered.
successDeposit :: UM.CompactForm Coin
successDeposit = UM.CompactCoin 7

hashsize :: forall c. Crypto c => Int
hashsize = fromIntegral $ sizeHash ([] @(HASH c))

-- ============================== PParams ===============================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ zeroTestingCostModels [PlutusV1]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , CollateralPercentage 100
  , KeyDeposit (Coin 2)
  , PoolDeposit poolDeposit
  ]

poolDeposit :: Coin
poolDeposit = Coin 5

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs
