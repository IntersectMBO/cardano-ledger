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

module Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW (tests, mkSingleRedeemer) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  TxIx,
  mkTxIxPartial,
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (
  ProtVer (..),
  UTxO (..),
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  smartUTxOState,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..), inject, (<+>))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  alwaysFailsHash,
  alwaysSucceedsHash,
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
  someScriptAddr,
  testUTXOW,
  timelockScript,
  timelockStakeCred,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  PParamsField (..),
  TxBodyField (..),
  TxField (..),
  TxOutField (..),
  WitnessesField (..),
 )
import Test.Cardano.Ledger.Generic.GenState (PlutusPurposeTag (..), mkRedeemers, mkRedeemersFromTags)
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (HasTokens (..), PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

tests :: TestTree
tests =
  testGroup
    "Generic Tests for valid transactions, testing Alonzo UTXOW PredicateFailures, in postAlonzo eras."
    [ alonzoUTXOWTests (Alonzo Mock)
    , alonzoUTXOWTests (Babbage Mock)
    -- alonzoUTXOWTests (Conway Mock) TODO
    ]

alonzoUTXOWTests ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , HasTokens era
  , Reflect era
  , PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto,
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  TestTree
alonzoUTXOWTests pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "validating SPEND script" $
            testU
              pf
              (trustMeP pf True $ validatingTx pf)
              (Right . validatingState $ pf)
        , testCase "not validating SPEND script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTx pf)
              (Right . notValidatingState $ pf)
        , testCase "validating CERT script" $
            testU
              pf
              (trustMeP pf True $ validatingWithCertTx pf)
              (Right . validatingWithCertState $ pf)
        , testCase "not validating CERT script" $
            testU
              pf
              (trustMeP pf False $ notValidatingWithCertTx pf)
              (Right . notValidatingWithCertState $ pf)
        , testCase "validating WITHDRAWAL script" $
            testU
              pf
              (trustMeP pf True $ validatingWithWithdrawalTx pf)
              (Right . validatingWithWithdrawalState $ pf)
        , testCase "not validating WITHDRAWAL script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTxWithWithdrawal pf)
              (Right . notValidatingWithWithdrawalState $ pf)
        , testCase "validating MINT script" $
            testU
              pf
              (trustMeP pf True $ validatingWithMintTx pf)
              (Right . validatingWithMintState $ pf)
        , testCase "not validating MINT script" $
            testU
              pf
              (trustMeP pf False $ notValidatingWithMintTx pf)
              (Right . notValidatingWithMintState $ pf)
        , testCase "validating scripts everywhere" $
            testU
              pf
              (trustMeP pf True $ validatingManyScriptsTx pf)
              (Right . validatingManyScriptsState $ pf)
        , testCase "acceptable supplimentary datum" $
            testU
              pf
              (trustMeP pf True $ validatingSupplimentaryDatumTx pf)
              (Right . validatingSupplimentaryDatumState $ pf)
        , testCase "multiple identical certificates" $
            testU
              pf
              (trustMeP pf True $ validatingMultipleEqualCertsTx pf)
              (Right . validatingMultipleEqualCertsState $ pf)
        , testCase "non-script output with datum" $
            testU
              pf
              (trustMeP pf True $ validatingNonScriptOutWithDatumTx pf)
              (Right . validatingNonScriptOutWithDatumState $ pf)
        ]
    ]

-- =========================================================================
-- ============================== DATA ========================================

-- =========================================================================
--  Example 1: Process a SPEND transaction with a succeeding Plutus script.
-- =========================================================================

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
        , DataWits' [Data (PV1.I 123)]
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
            (mkTxDats (Data (PV1.I 123)))
        )
    ]

validatingRedeemers :: Era era => Proof era -> Redeemers era
validatingRedeemers proof = mkSingleRedeemer proof Spending (Data (PV1.I 42))

validatingTxOut :: EraTxOut era => Proof era -> TxOut era
validatingTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]

validatingState ::
  forall era.
  ( EraTxBody era
  , PostShelley era
  , EraGov era
  ) =>
  Proof era ->
  UTxOState era
validatingState pf = smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingBody pf) (validatingTxOut pf)) 1

-- ======================================================================
--  Example 2: Process a SPEND transaction with a failing Plutus script.
-- ======================================================================
datumExample2 :: Era era => Data era
datumExample2 = Data (PV1.I 0)

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
    [ Body body
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated body) (someKeys pf)]
        , ScriptWits' [never 0 pf]
        , DataWits' [datumExample2]
        , RdmrWits redeemers
        ]
    ]
  where
    body =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 2]
        , Collateral' [mkGenesisTxIn 12]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 2995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers (mkTxDats datumExample2))
        ]
    redeemers = mkSingleRedeemer pf Spending (Data (PV1.I 1))

notValidatingState ::
  ( EraTxBody era
  , PostShelley era
  , EraGov era
  ) =>
  Proof era ->
  UTxOState era
notValidatingState pf = smartUTxOState (pp pf) (expectedUTxO' pf ExpectFailure 2) (Coin 0) (Coin 5) def mempty

-- =========================================================================
--  Example 3: Process a CERT transaction with a succeeding Plutus script.
-- =========================================================================

validatingWithCertTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
validatingWithCertTx pf =
  newTx
    pf
    [ Body (validatingWithCertBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingWithCertBody pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingWithCertRedeemers pf
        ]
    ]

validatingWithCertBody :: (Scriptic era, EraTxBody era, ShelleyEraTxCert era) => Proof era -> TxBody era
validatingWithCertBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingWithCertTxOut pf]
    , Certs' [UnRegTxCert (scriptStakeCredSuceed pf)]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingWithCertRedeemers pf) mempty)
    ]

validatingWithCertTxOut :: EraTxOut era => Proof era -> TxOut era
validatingWithCertTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

validatingWithCertRedeemers :: Era era => Proof era -> Redeemers era
validatingWithCertRedeemers proof = mkSingleRedeemer proof Certifying (Data (PV1.I 42))

validatingWithCertState ::
  ( EraTxBody era
  , PostShelley era
  , EraGov era
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  UTxOState era
validatingWithCertState pf = smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingWithCertBody pf) (validatingWithCertTxOut pf)) 3

-- =====================================================================
--  Example 4: Process a CERT transaction with a failing Plutus script.
-- =====================================================================

notValidatingWithCertTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
notValidatingWithCertTx pf =
  newTx
    pf
    [ Body body
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated body) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    body =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 4]
        , Collateral' [mkGenesisTxIn 14]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]]
        , Certs' [UnRegTxCert (scriptStakeCredFail pf)]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers = mkSingleRedeemer pf Certifying (Data (PV1.I 0))

notValidatingWithCertState ::
  ( EraTxBody era
  , PostShelley era
  , EraGov era
  ) =>
  Proof era ->
  UTxOState era
notValidatingWithCertState pf =
  smartUTxOState
    (pp pf)
    (expectedUTxO' pf ExpectFailure 4)
    (Coin 0)
    (Coin 5)
    def
    mempty

-- ==============================================================================
--  Example 5: Process a WITHDRAWAL transaction with a succeeding Plutus script.
-- ==============================================================================

validatingWithWithdrawalTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingWithWithdrawalTx pf =
  newTx
    pf
    [ Body (validatingWithWithdrawalBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingWithWithdrawalBody pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingWithWithdrawalRedeemers pf
        ]
    ]

validatingWithWithdrawalBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingWithWithdrawalBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 5]
    , Collateral' [mkGenesisTxIn 15]
    , Outputs' [validatingWithWithdrawalTxOut pf]
    , Txfee (Coin 5)
    , Withdrawals'
        ( Withdrawals $
            Map.singleton
              (RewardAcnt Testnet (scriptStakeCredSuceed pf))
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

validatingWithWithdrawalTxOut :: EraTxOut era => Proof era -> TxOut era
validatingWithWithdrawalTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]

validatingWithWithdrawalState ::
  (EraTxBody era, PostShelley era, EraGov era) =>
  Proof era ->
  UTxOState era
validatingWithWithdrawalState pf =
  smartUTxOState
    (pp pf)
    utxo
    (Coin 0)
    (Coin 5)
    def
    mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingWithWithdrawalBody pf) (validatingWithWithdrawalTxOut pf)) 5

-- ===========================================================================
--  Example 6: Process a WITHDRAWAL transaction with a failing Plutus script.
-- ===========================================================================

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
    [ Body body
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated body) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    body =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 6]
        , Collateral' [mkGenesisTxIn 16]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]]
        , Txfee (Coin 5)
        , Withdrawals'
            ( Withdrawals $
                Map.singleton
                  (RewardAcnt Testnet (scriptStakeCredFail pf))
                  (Coin 1000)
            )
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers = mkSingleRedeemer pf Rewarding (Data (PV1.I 0))

notValidatingWithWithdrawalState ::
  (EraTxBody era, PostShelley era, EraGov era) =>
  Proof era ->
  UTxOState era
notValidatingWithWithdrawalState pf =
  smartUTxOState
    (pp pf)
    (expectedUTxO' pf ExpectFailure 6)
    (Coin 0)
    (Coin 5)
    def
    mempty

-- =============================================================================
--  Example 7: Process a MINT transaction with a succeeding Plutus script.
-- =============================================================================

validatingWithMintTx ::
  forall era.
  ( Scriptic era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingWithMintTx pf =
  newTx
    pf
    [ Body (validatingWithMintBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingWithMintBody pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingWithMintRedeemers pf
        ]
    ]

validatingWithMintBody ::
  (HasTokens era, EraTxBody era, Scriptic era, Value era ~ MaryValue (EraCrypto era)) =>
  Proof era ->
  TxBody era
validatingWithMintBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 7]
    , Collateral' [mkGenesisTxIn 17]
    , Outputs' [validatingWithMintTxOut pf]
    , Txfee (Coin 5)
    , Mint (multiAsset pf)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (validatingWithMintRedeemers pf) mempty)
    ]

validatingWithMintRedeemers :: Era era => Proof era -> Redeemers era
validatingWithMintRedeemers pf = mkSingleRedeemer pf Minting (Data (PV1.I 42))

multiAsset :: forall era. (Scriptic era, HasTokens era) => Proof era -> MultiAsset (EraCrypto era)
multiAsset pf = forge @era 1 (always 2 pf)

validatingWithMintTxOut ::
  ( HasTokens era
  , EraTxOut era
  , Scriptic era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  TxOut era
validatingWithMintTxOut pf =
  newTxOut pf [Address (someAddr pf), Amount (MaryValue mempty (multiAsset pf) <+> inject (Coin 995))]

validatingWithMintState ::
  forall era.
  (PostShelley era, EraTxBody era, HasTokens era, Value era ~ MaryValue (EraCrypto era), EraGov era) =>
  Proof era ->
  UTxOState era
validatingWithMintState pf =
  smartUTxOState
    (pp pf)
    utxo
    (Coin 0)
    (Coin 5)
    def
    mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingWithMintBody pf) (validatingWithMintTxOut pf)) 7

-- ==============================================================================
--  Example 8: Process a MINT transaction with a failing Plutus script.
-- ==============================================================================

notValidatingWithMintTx ::
  forall era.
  ( Scriptic era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingWithMintTx pf =
  newTx
    pf
    [ Body body
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated body) (someKeys pf)]
        , ScriptWits' [never 1 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    body =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 8]
        , Collateral' [mkGenesisTxIn 18]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (MaryValue mempty mint <+> inject (Coin 995))]]
        , Txfee (Coin 5)
        , Mint mint
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers = mkSingleRedeemer pf Minting (Data (PV1.I 0))
    mint = forge @era 1 (never 1 pf)

notValidatingWithMintState ::
  (EraTxBody era, PostShelley era, EraGov era) =>
  Proof era ->
  UTxOState era
notValidatingWithMintState pf = smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def zero
  where
    utxo = expectedUTxO' pf ExpectFailure 8

-- ====================================================================================
--  Example 9: Process a transaction with a succeeding script in every place possible,
--  and also with succeeding timelock scripts.
-- ====================================================================================

validatingManyScriptsTx ::
  forall era.
  ( PostShelley era
  , HasTokens era
  , EraTxBody era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
validatingManyScriptsTx pf =
  newTx
    pf
    [ Body (validatingManyScriptsBody pf)
    , WitnessesI
        [ AddrWits' $
            map
              (mkWitnessVKey . hashAnnotated . validatingManyScriptsBody $ pf)
              [someKeys pf, theKeyPair 1]
        , ScriptWits'
            [ always 2 pf
            , always 3 pf
            , timelockScript 0 pf
            , timelockScript 1 pf
            , timelockScript 2 pf
            ]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits $ validatingManyScriptsRedeemers pf
        ]
    ]

validatingManyScriptsBody ::
  ( HasTokens era
  , EraTxBody era
  , PostShelley era
  , Value era ~ MaryValue (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  TxBody era
validatingManyScriptsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1, mkGenesisTxIn 100]
    , Collateral' [mkGenesisTxIn 11]
    , Outputs' [validatingManyScriptsTxOut pf]
    , Txfee (Coin 5)
    , Certs'
        [ UnRegTxCert (timelockStakeCred pf)
        , UnRegTxCert (scriptStakeCredSuceed pf)
        ]
    , Withdrawals'
        ( Withdrawals $
            Map.fromList
              [ (RewardAcnt Testnet (scriptStakeCredSuceed pf), Coin 0)
              , (RewardAcnt Testnet (timelockStakeCred pf), Coin 0)
              ]
        )
    , Mint (validatingManyScriptsMint pf)
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (validatingManyScriptsRedeemers pf)
            (mkTxDats (Data (PV1.I 123)))
        )
    , Vldt (ValidityInterval SNothing (SJust $ SlotNo 1))
    ]

validatingManyScriptsRedeemers :: Era era => Proof era -> Redeemers era
validatingManyScriptsRedeemers proof =
  mkRedeemersFromTags
    proof
    [ ((Spending, 0), (Data (PV1.I 101), ExUnits 5000 5000))
    , ((Certifying, 1), (Data (PV1.I 102), ExUnits 5000 5000))
    , ((Rewarding, 0), (Data (PV1.I 103), ExUnits 5000 5000))
    , ((Minting, 1), (Data (PV1.I 104), ExUnits 5000 5000))
    ]

validatingManyScriptsMint :: forall era. (PostShelley era, HasTokens era) => Proof era -> MultiAsset (EraCrypto era)
validatingManyScriptsMint pf = forge @era 1 (always 2 pf) <> forge @era 1 (timelockScript 1 pf)

validatingManyScriptsTxOut :: (HasTokens era, EraTxOut era, PostShelley era, Value era ~ MaryValue (EraCrypto era)) => Proof era -> TxOut era
validatingManyScriptsTxOut pf =
  newTxOut
    pf
    [ Address (someAddr pf)
    , Amount (MaryValue mempty (validatingManyScriptsMint pf) <+> inject (Coin 4996))
    ]

validatingManyScriptsState ::
  forall era.
  ( EraTxBody era
  , PostShelley era
  , HasTokens era
  , Value era ~ MaryValue (EraCrypto era)
  , EraGov era
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  UTxOState era
validatingManyScriptsState pf =
  smartUTxOState
    (pp pf)
    (UTxO utxo)
    (Coin 0)
    (Coin 5)
    def
    mempty
  where
    utxo =
      Map.insert
        (TxIn (txIdTxBody (validatingManyScriptsBody pf)) minBound)
        (validatingManyScriptsTxOut pf)
        $ Map.filterWithKey
          (\k _ -> k /= mkGenesisTxIn 1 && k /= mkGenesisTxIn 100)
          (unUTxO $ initUTxO pf)

-- ====================================================================================
--  Example 10: A transaction with an acceptable supplimentary datum
-- ====================================================================================

validatingSupplimentaryDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingSupplimentaryDatumTx pf =
  newTx
    pf
    [ Body (validatingSupplimentaryDatumBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingSupplimentaryDatumBody pf)) (someKeys pf)]
        , DataWits' [Data (PV1.I 123)]
        ]
    ]

validatingSupplimentaryDatumBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingSupplimentaryDatumBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Outputs' [validatingSupplimentaryDatumTxOut pf]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [] (mkRedeemers pf []) (mkTxDats (Data (PV1.I 123))))
    ]

validatingSupplimentaryDatum :: Era era => Data era
validatingSupplimentaryDatum = Data (PV1.I 123)

validatingSupplimentaryDatumTxOut :: forall era. (EraTxBody era, Scriptic era) => Proof era -> TxOut era
validatingSupplimentaryDatumTxOut pf =
  newTxOut
    pf
    [ Address (someScriptAddr (always 3 pf))
    , Amount (inject $ Coin 995)
    , DHash' [hashData $ validatingSupplimentaryDatum @era]
    ]

validatingSupplimentaryDatumState ::
  forall era.
  (EraTxBody era, PostShelley era, EraGov era) =>
  Proof era ->
  UTxOState era
validatingSupplimentaryDatumState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingSupplimentaryDatumBody pf) (validatingSupplimentaryDatumTxOut pf)) 3

-- ====================================================================================
--  Example 11: A transaction with multiple identical certificates
-- ====================================================================================

validatingMultipleEqualCertsTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
validatingMultipleEqualCertsTx pf =
  newTx
    pf
    [ Body (validatingMultipleEqualCertsBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingMultipleEqualCertsBody pf)) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits $ validatingMultipleEqualCertsRedeemers pf
        ]
    ]

validatingMultipleEqualCertsBody :: (EraTxBody era, Scriptic era, ShelleyEraTxCert era) => Proof era -> TxBody era
validatingMultipleEqualCertsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingMultipleEqualCertsOut pf]
    , Certs'
        [ UnRegTxCert (scriptStakeCredSuceed pf)
        , UnRegTxCert (scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
        ]
    , Txfee (Coin 5)
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (validatingMultipleEqualCertsRedeemers pf)
            mempty
        )
    ]

validatingMultipleEqualCertsRedeemers :: Era era => Proof era -> Redeemers era
validatingMultipleEqualCertsRedeemers pf = mkSingleRedeemer pf Certifying (Data (PV1.I 42))

validatingMultipleEqualCertsOut :: EraTxOut era => Proof era -> TxOut era
validatingMultipleEqualCertsOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

validatingMultipleEqualCertsState ::
  (EraTxBody era, PostShelley era, EraGov era, ShelleyEraTxCert era) =>
  Proof era ->
  UTxOState era
validatingMultipleEqualCertsState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingMultipleEqualCertsBody pf) (validatingMultipleEqualCertsOut pf)) 3

-- ====================================================================================
--  Example 12: Attaching a datum (hash) to a non-script output.
--
--  Note that a when spending a non-script output with a datum hash, the datum cannot
--  be supplied, because it is considered extraneous,
--  as in the 'notOkSupplimentaryDatumTx' example.
-- ====================================================================================

validatingNonScriptOutWithDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
validatingNonScriptOutWithDatumTx pf =
  newTx
    pf
    [ Body (validatingNonScriptOutWithDatumTxBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (validatingNonScriptOutWithDatumTxBody pf)) (someKeys pf)]
        ]
    ]

validatingNonScriptOutWithDatumTxBody :: EraTxBody era => Proof era -> TxBody era
validatingNonScriptOutWithDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 103]
    , Outputs' [validatingNonScriptOutWithDatumTxOut pf]
    , Txfee (Coin 5)
    ]

validatingNonScriptOutWithDatumTxOut :: EraTxOut era => Proof era -> TxOut era
validatingNonScriptOutWithDatumTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1216)]

validatingNonScriptOutWithDatumState ::
  ( PostShelley era
  , EraTxBody era
  , EraGov era
  ) =>
  Proof era ->
  UTxOState era
validatingNonScriptOutWithDatumState pf =
  smartUTxOState (pp pf) utxo (Coin 0) (Coin 5) def mempty
  where
    utxo = expectedUTxO' pf (ExpectSuccess (validatingNonScriptOutWithDatumTxBody pf) (validatingNonScriptOutWithDatumTxOut pf)) 103

-- ============================== HELPER FUNCTIONS ===============================

--  This is a helper type for the expectedUTxO function.
--  ExpectSuccess indicates that we created a valid transaction
--  where the IsValid flag is true.
data Expect era
  = ExpectSuccess (TxBody era) (TxOut era)
  | ExpectSuccessInvalid
  | ExpectFailure

-- | In each of our main eight examples, the UTxO map obtained
-- by applying the transaction is straightforward. This function
-- captures the logic.
--
-- Each example transaction (given a number i) will use
-- (TxIn genesisId (10+i), someOutput) for its' single input,
-- and (TxIn genesisId i, collateralOutput) for its' single collateral output.
--
-- If we expect the transaction script to validate, then
-- the UTxO for (TxIn genesisId i) will be consumed and a UTxO will be created.
-- Otherwise, the UTxO for (TxIn genesisId (10+i)) will be consumed.
expectedUTxO ::
  forall era.
  (HasCallStack, EraTxBody era) =>
  UTxO era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO initUtxo ex idx = UTxO utxo
  where
    utxo = case ex of
      ExpectSuccess txb newOut ->
        Map.insert (TxIn (txIdTxBody txb) minBound) newOut (filteredUTxO (mkTxIxPartial idx))
      ExpectSuccessInvalid -> filteredUTxO (mkTxIxPartial idx)
      ExpectFailure -> filteredUTxO (mkTxIxPartial (10 + idx))
    filteredUTxO :: TxIx -> Map.Map (TxIn (EraCrypto era)) (TxOut era)
    filteredUTxO x = Map.filterWithKey (\(TxIn _ i) _ -> i /= x) $ unUTxO initUtxo

expectedUTxO' ::
  (HasCallStack, EraTxBody era, PostShelley era) =>
  Proof era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO' pf = expectedUTxO (initUTxO pf)

testU ::
  forall era.
  ( PostShelley era
  , Reflect era
  , HasCallStack
  ) =>
  Proof era ->
  Tx era ->
  Either [PredicateFailure (EraRule "UTXOW" era)] (State (EraRule "UTXOW" era)) ->
  Assertion
testU pf = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf)

scriptStakeCredFail :: Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredFail pf = ScriptHashObj (alwaysFailsHash 1 pf)

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

-- ============================== PPARAMS ===============================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ zeroTestingCostModels [PlutusV1]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , CollateralPercentage 100
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs

mkSingleRedeemer :: Proof era -> PlutusPurposeTag -> Data era -> Redeemers era
mkSingleRedeemer proof tag datum =
  mkRedeemersFromTags proof [((tag, 0), (datum, ExUnits 5000 5000))]
