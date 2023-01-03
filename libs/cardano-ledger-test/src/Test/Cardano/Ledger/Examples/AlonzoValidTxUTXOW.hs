{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoValidTxUTXOW (tests) where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (
  CostModels (..),
  ExUnits (..),
 )
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  TxIx,
  mkTxIxPartial,
  natVersion,
 )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (
  ProtVer (..),
  UTxO (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  UTxOState (..),
  smartUTxOState,
 )
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  DelegCert (..),
  RewardAcnt (..),
  Wdrl (..),
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject, (<+>))
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
  freeCostModelV1,
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
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (HasTokens (..), PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
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
  , GoodCrypto (EraCrypto era)
  , HasTokens era
  , Default (State (EraRule "PPUP" era))
  , EraTx era
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
        , RdmrWits validatingRedeemers
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
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers (mkTxDats (Data (PV1.I 123))))
    ]

validatingRedeemers :: Era era => Redeemers era
validatingRedeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

validatingTxOut :: EraTxOut era => Proof era -> TxOut era
validatingTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]

validatingState ::
  forall era.
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
validatingState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
    redeemers =
      Redeemers
        ( Map.fromList
            [
              ( RdmrPtr Tag.Spend 0
              , (Data (PV1.I 1), ExUnits 5000 5000)
              )
            ]
        )

notValidatingState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
notValidatingState pf = smartUTxOState (expectedUTxO' pf ExpectFailure 2) (Coin 0) (Coin 5) def

-- =========================================================================
--  Example 3: Process a CERT transaction with a succeeding Plutus script.
-- =========================================================================

validatingWithCertTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
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
        , RdmrWits validatingWithCertRedeemers
        ]
    ]

validatingWithCertBody :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
validatingWithCertBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingWithCertTxOut pf]
    , Certs' [DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf)]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingWithCertRedeemers mempty)
    ]

validatingWithCertTxOut :: EraTxOut era => Proof era -> TxOut era
validatingWithCertTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

validatingWithCertRedeemers :: Era era => Redeemers era
validatingWithCertRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (Data (PV1.I 42), ExUnits 5000 5000)

validatingWithCertState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
validatingWithCertState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        , Certs' [DCertDeleg (DeRegKey $ scriptStakeCredFail pf)]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers =
      Redeemers $
        Map.singleton (RdmrPtr Tag.Cert 0) (Data (PV1.I 0), ExUnits 5000 5000)

notValidatingWithCertState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
notValidatingWithCertState pf = smartUTxOState (expectedUTxO' pf ExpectFailure 4) (Coin 0) (Coin 5) def

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
        , RdmrWits validatingWithWithdrawalRedeemers
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
    , Wdrls
        ( Wdrl $
            Map.singleton
              (RewardAcnt Testnet (scriptStakeCredSuceed pf))
              (Coin 1000)
        )
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingWithWithdrawalRedeemers mempty)
    ]

validatingWithWithdrawalRedeemers :: Era era => Redeemers era
validatingWithWithdrawalRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Rewrd 0) (Data (PV1.I 42), ExUnits 5000 5000)

validatingWithWithdrawalTxOut :: EraTxOut era => Proof era -> TxOut era
validatingWithWithdrawalTxOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]

validatingWithWithdrawalState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
validatingWithWithdrawalState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        , Wdrls
            ( Wdrl $
                Map.singleton
                  (RewardAcnt Testnet (scriptStakeCredFail pf))
                  (Coin 1000)
            )
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers =
      Redeemers $ Map.singleton (RdmrPtr Tag.Rewrd 0) (Data (PV1.I 0), ExUnits 5000 5000)

notValidatingWithWithdrawalState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
notValidatingWithWithdrawalState pf = smartUTxOState (expectedUTxO' pf ExpectFailure 6) (Coin 0) (Coin 5) def

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
        , RdmrWits validatingWithMintRedeemers
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
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingWithMintRedeemers mempty)
    ]

validatingWithMintRedeemers :: Era era => Redeemers era
validatingWithMintRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Mint 0) (Data (PV1.I 42), ExUnits 5000 5000)

multiAsset :: forall era. (Scriptic era, HasTokens era) => Proof era -> MultiAsset (EraCrypto era)
multiAsset pf = forge @era 1 (always 2 pf)

validatingWithMintTxOut :: (HasTokens era, EraTxOut era, Scriptic era, Value era ~ MaryValue (EraCrypto era)) => Proof era -> TxOut era
validatingWithMintTxOut pf = newTxOut pf [Address (someAddr pf), Amount (MaryValue 0 (multiAsset pf) <+> inject (Coin 995))]

validatingWithMintState ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era, EraTxBody era, HasTokens era, Value era ~ MaryValue (EraCrypto era)) =>
  Proof era ->
  UTxOState era
validatingWithMintState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (MaryValue 0 mint <+> inject (Coin 995))]]
        , Txfee (Coin 5)
        , Mint mint
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers =
      Redeemers $
        Map.singleton (RdmrPtr Tag.Mint 0) (Data (PV1.I 0), ExUnits 5000 5000)
    mint = forge @era 1 (never 1 pf)

notValidatingWithMintState ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
notValidatingWithMintState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        , RdmrWits validatingManyScriptsRedeemers
        ]
    ]

validatingManyScriptsBody ::
  (HasTokens era, EraTxBody era, PostShelley era, Value era ~ MaryValue (EraCrypto era)) =>
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
        [ DCertDeleg (DeRegKey $ timelockStakeCred pf)
        , DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf)
        ]
    , Wdrls
        ( Wdrl $
            Map.fromList
              [ (RewardAcnt Testnet (scriptStakeCredSuceed pf), Coin 0)
              , (RewardAcnt Testnet (timelockStakeCred pf), Coin 0)
              ]
        )
    , Mint (validatingManyScriptsMint pf)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingManyScriptsRedeemers (mkTxDats (Data (PV1.I 123))))
    , Vldt (ValidityInterval SNothing (SJust $ SlotNo 1))
    ]

validatingManyScriptsRedeemers :: Era era => Redeemers era
validatingManyScriptsRedeemers =
  Redeemers . Map.fromList $
    [ (RdmrPtr Tag.Spend 0, (Data (PV1.I 101), ExUnits 5000 5000))
    , (RdmrPtr Tag.Cert 1, (Data (PV1.I 102), ExUnits 5000 5000))
    , (RdmrPtr Tag.Rewrd 0, (Data (PV1.I 103), ExUnits 5000 5000))
    , (RdmrPtr Tag.Mint 1, (Data (PV1.I 104), ExUnits 5000 5000))
    ]

validatingManyScriptsMint :: forall era. (PostShelley era, HasTokens era) => Proof era -> MultiAsset (EraCrypto era)
validatingManyScriptsMint pf = forge @era 1 (always 2 pf) <> forge @era 1 (timelockScript 1 pf)

validatingManyScriptsTxOut :: (HasTokens era, EraTxOut era, PostShelley era, Value era ~ MaryValue (EraCrypto era)) => Proof era -> TxOut era
validatingManyScriptsTxOut pf =
  newTxOut
    pf
    [ Address (someAddr pf)
    , Amount (MaryValue 0 (validatingManyScriptsMint pf) <+> inject (Coin 4996))
    ]

validatingManyScriptsState ::
  forall era.
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era, HasTokens era, Value era ~ MaryValue (EraCrypto era)) =>
  Proof era ->
  UTxOState era
validatingManyScriptsState pf = smartUTxOState (UTxO utxo) (Coin 0) (Coin 5) def
  where
    utxo =
      Map.insert (TxIn (txid (validatingManyScriptsBody pf)) minBound) (validatingManyScriptsTxOut pf) $
        Map.filterWithKey
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
    , WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) (mkTxDats (Data (PV1.I 123))))
    ]

validatingSupplimentaryDatum :: Era era => Data era
validatingSupplimentaryDatum = Data (PV1.I 123)

validatingSupplimentaryDatumTxOut :: forall era. (EraTxBody era, Scriptic era) => Proof era -> TxOut era
validatingSupplimentaryDatumTxOut pf =
  newTxOut
    pf
    [ Address (someScriptAddr (always 3 pf) pf)
    , Amount (inject $ Coin 995)
    , DHash' [hashData $ validatingSupplimentaryDatum @era]
    ]

validatingSupplimentaryDatumState ::
  forall era.
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
validatingSupplimentaryDatumState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        , RdmrWits validatingMultipleEqualCertsRedeemers
        ]
    ]

validatingMultipleEqualCertsBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingMultipleEqualCertsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3]
    , Collateral' [mkGenesisTxIn 13]
    , Outputs' [validatingMultipleEqualCertsOut pf]
    , Certs'
        [ DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf)
        , DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
        ]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingMultipleEqualCertsRedeemers mempty)
    ]

validatingMultipleEqualCertsRedeemers :: Era era => Redeemers era
validatingMultipleEqualCertsRedeemers =
  Redeemers $
    Map.fromList
      [ (RdmrPtr Tag.Cert 0, (Data (PV1.I 42), ExUnits 5000 5000))
      ]

validatingMultipleEqualCertsOut :: EraTxOut era => Proof era -> TxOut era
validatingMultipleEqualCertsOut pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

validatingMultipleEqualCertsState ::
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
validatingMultipleEqualCertsState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
  ( Default (State (EraRule "PPUP" era))
  , PostShelley era
  , EraTxBody era
  ) =>
  Proof era ->
  UTxOState era
validatingNonScriptOutWithDatumState pf = smartUTxOState utxo (Coin 0) (Coin 5) def
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
        Map.insert (TxIn (txid txb) minBound) newOut (filteredUTxO (mkTxIxPartial idx))
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
  ( GoodCrypto (EraCrypto era)
  , Default (State (EraRule "PPUP" era))
  , PostShelley era
  , EraTx era
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
  [ Costmdls . CostModels $ Map.singleton PlutusV1 freeCostModelV1
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @5) 0
  , CollateralPercentage 100
  ]

pp :: Proof era -> PParams era
pp pf = newPParams pf defaultPPs
