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

module Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW (tests) where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Tx (
  IsValid (..),
  ScriptPurpose (..),
 )
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..), unRedeemers)
import Cardano.Ledger.BaseTypes (
  Network (..),
  ProtVer (..),
  StrictMaybe (..),
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  asWitness,
  hashKey,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxBody (
  RewardAcnt (..),
 )
import Cardano.Ledger.Shelley.TxCert (pattern UnRegTxCert)
import Cardano.Ledger.Val (inject, (<+>))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.CostModel (freeV1CostModels)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  AlonzoBased (..),
  alwaysSucceedsHash,
  initUTxO,
  keyBy,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
  testUTXOW,
  testUTXOWsubset,
  testUTXOspecialCase,
  timelockHash,
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
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

tests :: TestTree
tests =
  testGroup
    "Generic Tests for invalid transactions, testing Alonzo UTXOW PredicateFailures, in postAlonzo eras."
    [ alonzoUTXOWTests (Alonzo Mock)
    , alonzoUTXOWTests (Babbage Mock)
    -- alonzoUTXOWTests (Conway Mock) TODO
    ]

alonzoUTXOWTests ::
  forall era.
  ( AlonzoBased era (PredicateFailure (EraRule "UTXOW" era))
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , GoodCrypto (EraCrypto era)
  , HasTokens era
  , EraTx era
  , PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto,
  , Value era ~ MaryValue (EraCrypto era)
  , EraGov era
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  TestTree
alonzoUTXOWTests pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "invalid transactions"
        [ testCase "wrong network ID" $
            testU
              pf
              (trustMeP pf True $ incorrectNetworkIDTx pf)
              ( Left
                  [ fromUtxo @era (WrongNetworkInTxBody Testnet Mainnet)
                  ]
              )
        , testCase "missing required key witness" $
            testU
              pf
              (trustMeP pf True $ missingRequiredWitnessTx pf)
              ( Left [(fromPredFail @era . MissingRequiredSigners . Set.singleton) extraneousKeyHash]
              )
        , testCase "missing redeemer" $
            testU
              pf
              (trustMeP pf True $ missingRedeemerTx pf)
              ( Left
                  [ fromUtxos @era . CollectErrors $
                      [NoRedeemer (Spending (mkGenesisTxIn 1))]
                  , fromPredFail $
                      MissingRedeemers @era
                        [(Spending (mkGenesisTxIn 1), alwaysSucceedsHash 3 pf)]
                  ]
              )
        , testCase "wrong wpp hash" $
            testU
              pf
              (trustMeP pf True $ wrongWppHashTx pf)
              ( Left
                  [ fromPredFail @era $
                      PPViewHashesDontMatch
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            (Redeemers mempty)
                            (mkTxDats (Data (PV1.I 123)))
                        )
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            wrongWpphashRedeemers
                            (mkTxDats (Data (PV1.I 123)))
                        )
                  ]
              )
        , testCase "missing 1-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing1phaseScriptWitnessTx pf)
              ( Left
                  [ fromUtxow @era . MissingScriptWitnessesUTXOW . Set.singleton $
                      timelockHash 0 pf
                  ]
              )
        , testCase "missing 2-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing2phaseScriptWitnessTx pf)
              ( Left
                  [ -- these redeemers are associated with phase-1 scripts
                    fromPredFail @era . ExtraRedeemers $
                      [ RdmrPtr Tag.Mint 1
                      , RdmrPtr Tag.Cert 1
                      , RdmrPtr Tag.Rewrd 0
                      ]
                  , fromUtxow @era . MissingScriptWitnessesUTXOW . Set.singleton $
                      alwaysSucceedsHash 2 pf
                  ]
              )
        , testCase "redeemer with incorrect label" $
            testU
              pf
              (trustMeP pf True $ wrongRedeemerLabelTx pf)
              ( Left
                  [ fromUtxos @era (CollectErrors [NoRedeemer (Spending (mkGenesisTxIn 1))])
                  , -- now "wrong redeemer label" means there are both unredeemable scripts and extra redeemers
                    fromPredFail @era . MissingRedeemers $
                      [
                        ( Spending (mkGenesisTxIn 1)
                        , alwaysSucceedsHash 3 pf
                        )
                      ]
                  , fromPredFail @era . ExtraRedeemers $ [RdmrPtr Tag.Mint 0]
                  ]
              )
        , testCase "missing datum" $
            testU
              pf
              (trustMeP pf True $ missingDatumTx pf)
              ( Left
                  [ fromPredFail @era $
                      MissingRequiredDatums
                        (Set.singleton $ hashData @era (Data (PV1.I 123)))
                        mempty
                  ]
              )
        , testCase "phase 1 script failure" $
            testU
              pf
              (trustMeP pf True $ phase1FailureTx pf)
              ( Left
                  [ fromUtxow @era $
                      ScriptWitnessNotValidatingUTXOW $
                        Set.fromList
                          [ timelockHash 0 pf
                          , timelockHash 1 pf
                          , timelockHash 2 pf
                          ]
                  ]
              )
        , testCase "valid transaction marked as invalid" $
            testU
              pf
              (trustMeP pf False $ validatingTx pf)
              ( Left [fromUtxos @era (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]
              )
        , testCase "invalid transaction marked as valid" $
            testUTXOspecialCase
              (UTXOW pf)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notValidatingTx pf)
              ( Left
                  [ fromUtxos @era
                      ( ValidationTagMismatch
                          (IsValid True)
                          (FailedUnexpectedly (quietPlutusFailure :| []))
                      )
                  ]
              )
        , testCase "too many execution units for tx" $
            testU
              pf
              (trustMeP pf True $ tooManyExUnitsTx pf)
              ( Left
                  [ fromUtxo @era $
                      ExUnitsTooBigUTxO
                        (ExUnits {exUnitsMem = 1000000, exUnitsSteps = 1000000})
                        (ExUnits {exUnitsMem = 1000001, exUnitsSteps = 5000})
                  ]
              )
        , testCase "missing signature for collateral input" $
            testU
              pf
              (trustMeP pf True $ missingCollateralSigTx pf)
              ( Left
                  [ fromUtxow @era
                      ( MissingVKeyWitnessesUTXOW
                          ( Set.fromList
                              [ asWitness $
                                  hashKey (vKey $ someKeys pf)
                              ]
                          )
                      )
                  ]
              )
        , testCase "insufficient collateral" $
            testUTXOW
              (UTXOW pf)
              (initUTxO pf)
              (newPParams pf $ defaultPPs ++ [CollateralPercentage 150])
              (trustMeP pf True $ validatingTx pf)
              ( Left [fromUtxo @era (InsufficientCollateral (Coin 5) (Coin 8))]
              )
        , testCase "two-phase UTxO with no datum hash" $
            testU
              pf
              (trustMeP pf True $ plutusOutputWithNoDataTx pf)
              ( Left
                  [ fromPredFail @era $ UnspendableUTxONoDatumHash . Set.singleton $ mkGenesisTxIn 101
                  ]
              )
        , testCase "unacceptable supplimentary datum" $
            testUTXOWsubset
              (UTXOW pf) -- Special rules apply here, use (expected `isSubset` computed)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notOkSupplimentaryDatumTx pf)
              ( Left
                  [ fromPredFail @era $
                      NotAllowedSupplementalDatums
                        (Set.singleton $ hashData @era totallyIrrelevantDatum)
                        mempty
                  ]
              )
        , testCase "unacceptable extra redeemer" $
            testU
              pf
              (trustMeP pf True $ extraRedeemersTx pf)
              ( Left
                  [ fromPredFail @era $
                      ExtraRedeemers
                        [RdmrPtr Tag.Spend 7]
                  ]
              )
        , testCase "multiple equal plutus-locked certs" $
            testU
              pf
              (trustMeP pf True $ multipleEqualCertsInvalidTx pf)
              ( Left
                  [ fromPredFail @era $ ExtraRedeemers [RdmrPtr Tag.Cert 1]
                  ]
              )
        , testCase "no cost model" $
            testU
              pf
              (trustMeP pf True $ noCostModelTx pf)
              ( Left [fromUtxos @era (CollectErrors [NoCostModel PlutusV2])]
              )
        ]
    ]

-- =========================================================================
-- ============================== DATA ========================================

incorrectNetworkIDTx :: (EraTx era, GoodCrypto (EraCrypto era)) => Proof era -> Tx era
incorrectNetworkIDTx pf =
  newTx
    pf
    [ Body incorrectNetworkIDTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated incorrectNetworkIDTxBody) (someKeys pf)]
        ]
    ]
  where
    incorrectNetworkIDTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 3]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]]
        , Txfee (Coin 5)
        , Txnetworkid (SJust Mainnet)
        ]

missingRequiredWitnessTx :: (EraTx era, GoodCrypto (EraCrypto era)) => Proof era -> Tx era
missingRequiredWitnessTx pf =
  newTx
    pf
    [ Body missingRequiredWitnessTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated missingRequiredWitnessTxBody) (someKeys pf)]
        ]
    ]
  where
    missingRequiredWitnessTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 3]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]]
        , Txfee (Coin 5)
        , ReqSignerHashes' [extraneousKeyHash]
        ]

missingRedeemerTx ::
  (Scriptic era, EraTx era, GoodCrypto (EraCrypto era)) =>
  Proof era ->
  Tx era
missingRedeemerTx pf =
  newTx
    pf
    [ Body (missingRedeemerTxBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (missingRedeemerTxBody pf)) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        ]
    ]

missingRedeemerTxBody :: EraTxBody era => Proof era -> TxBody era
missingRedeemerTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1]
    , Collateral' [mkGenesisTxIn 11]
    , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (Redeemers mempty) (mkTxDats (Data (PV1.I 123))))
    ]

wrongWppHashTx ::
  (Scriptic era, EraTx era, GoodCrypto (EraCrypto era)) =>
  Proof era ->
  Tx era
wrongWppHashTx pf =
  newTx
    pf
    [ Body (missingRedeemerTxBody pf)
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated (missingRedeemerTxBody pf)) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits wrongWpphashRedeemers
        ]
    ]

wrongWpphashRedeemers :: Era era => Redeemers era
wrongWpphashRedeemers =
  Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

missing1phaseScriptWitnessTx ::
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
missing1phaseScriptWitnessTx pf =
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
            , -- intentionally missing -> timelockScript 0 pf,
              timelockScript 1 pf
            , timelockScript 2 pf
            ]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits validatingManyScriptsRedeemers
        ]
    ]

missing2phaseScriptWitnessTx ::
  forall era.
  ( PostShelley era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
missing2phaseScriptWitnessTx pf =
  newTx
    pf
    [ Body (validatingManyScriptsBody pf)
    , WitnessesI
        [ AddrWits' $
            map
              (mkWitnessVKey . hashAnnotated . validatingManyScriptsBody $ pf)
              [someKeys pf, theKeyPair 1]
        , ScriptWits'
            [ -- intentionally missing -> always 2 pf,
              always 3 pf
            , timelockScript 0 pf
            , timelockScript 1 pf
            , timelockScript 2 pf
            ]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits validatingManyScriptsRedeemers
        ]
    ]

validatingManyScriptsBody ::
  forall era.
  (HasTokens era, EraTxBody era, PostShelley era, Value era ~ MaryValue (EraCrypto era), ShelleyEraTxCert era) =>
  Proof era ->
  TxBody era
validatingManyScriptsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1, mkGenesisTxIn 100]
    , Collateral' [mkGenesisTxIn 11]
    , Outputs' [txOut]
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
    , Mint mint
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingManyScriptsRedeemers (mkTxDats (Data (PV1.I 123))))
    , Vldt (ValidityInterval SNothing (SJust $ SlotNo 1))
    ]
  where
    txOut =
      newTxOut
        pf
        [ Address (someAddr pf)
        , Amount (MaryValue 0 mint <+> inject (Coin 4996))
        ]
    mint = forge @era 1 (always 2 pf) <> forge @era 1 (timelockScript 1 pf)

validatingManyScriptsRedeemers :: Era era => Redeemers era
validatingManyScriptsRedeemers =
  Redeemers . Map.fromList $
    [ (RdmrPtr Tag.Spend 0, (Data (PV1.I 101), ExUnits 5000 5000))
    , (RdmrPtr Tag.Cert 1, (Data (PV1.I 102), ExUnits 5000 5000))
    , (RdmrPtr Tag.Rewrd 0, (Data (PV1.I 103), ExUnits 5000 5000))
    , (RdmrPtr Tag.Mint 1, (Data (PV1.I 104), ExUnits 5000 5000))
    ]

wrongRedeemerLabelTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
wrongRedeemerLabelTx pf =
  newTx
    pf
    [ Body wrongRedeemerLabelTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated wrongRedeemerLabelTxBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits misPurposedRedeemer
        ]
    ]
  where
    wrongRedeemerLabelTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] misPurposedRedeemer (mkTxDats (Data (PV1.I 123))))
        ]
    misPurposedRedeemer =
      Redeemers $ -- The label *should* be Spend, not Mint
        Map.singleton (RdmrPtr Tag.Mint 0) (Data (PV1.I 42), ExUnits 5000 5000)

missingDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
missingDatumTx pf =
  newTx
    pf
    [ Body missingDatumTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated missingDatumTxBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    missingDatumTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

phase1FailureTx ::
  forall era.
  ( PostShelley era
  , HasTokens era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , Value era ~ MaryValue (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
phase1FailureTx pf =
  newTx
    pf
    [ Body (validatingManyScriptsBody pf)
    , WitnessesI
        [ AddrWits'
            [ mkWitnessVKey
                (hashAnnotated $ validatingManyScriptsBody pf)
                (someKeys pf)
            ]
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
    , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
    , Txfee (Coin 5)
    , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemers (mkTxDats (Data (PV1.I 123))))
    ]

validatingRedeemers :: Era era => Redeemers era
validatingRedeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

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
        , DataWits' [Data (PV1.I 0)]
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
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers (mkTxDats (Data (PV1.I 0))))
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

tooManyExUnitsTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
tooManyExUnitsTx pf =
  newTx
    pf
    [ Body tooManyExUnitsTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated tooManyExUnitsTxBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits redeemers
        ]
    ]
  where
    tooManyExUnitsTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers (mkTxDats (Data (PV1.I 123))))
        ]
    redeemers =
      Redeemers $
        Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 1000001 5000)

missingCollateralSigTx ::
  forall era.
  (Scriptic era, EraTx era) =>
  Proof era ->
  Tx era
missingCollateralSigTx pf =
  newTx
    pf
    [ Body (validatingBody pf)
    , WitnessesI
        [ ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits $ Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)
        ]
    ]

plutusOutputWithNoDataTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
plutusOutputWithNoDataTx pf =
  newTx
    pf
    [ Body plutusOutputWithNoDataTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated plutusOutputWithNoDataTxBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    plutusOutputWithNoDataTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 101]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

notOkSupplimentaryDatumTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
notOkSupplimentaryDatumTx pf =
  newTx
    pf
    [ Body notOkSupplimentaryDatumTxBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated notOkSupplimentaryDatumTxBody) (someKeys pf)]
        , DataWits' [totallyIrrelevantDatum]
        ]
    ]
  where
    notOkSupplimentaryDatumTxBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 3]
        , Outputs' [outputWithNoDatum]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) totallyIrrelevantTxDats)
        ]
    totallyIrrelevantTxDats = TxDats $ keyBy hashData [totallyIrrelevantDatum]
    outputWithNoDatum = newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 995)]

totallyIrrelevantDatum :: Era era => Data era
totallyIrrelevantDatum = Data (PV1.I 1729)

extraRedeemersTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  ) =>
  Proof era ->
  Tx era
extraRedeemersTx pf =
  newTx
    pf
    [ Body extraRedeemersBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated extraRedeemersBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits extraRedeemersEx
        ]
    ]
  where
    extraRedeemersBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 1]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] extraRedeemersEx (mkTxDats (Data (PV1.I 123))))
        ]
    extraRedeemersEx = Redeemers $ Map.insert (RdmrPtr Tag.Spend 7) (Data (PV1.I 42), ExUnits 432 444) (unRedeemers redeemers)
    redeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

multipleEqualCertsInvalidTx ::
  forall era.
  ( Scriptic era
  , EraTx era
  , GoodCrypto (EraCrypto era)
  , ShelleyEraTxCert era
  ) =>
  Proof era ->
  Tx era
multipleEqualCertsInvalidTx pf =
  newTx
    pf
    [ Body multipleEqualCertsInvalidBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated multipleEqualCertsInvalidBody) (someKeys pf)]
        , ScriptWits' [always 2 pf]
        , RdmrWits redeemers
        ]
    ]
  where
    multipleEqualCertsInvalidBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 3]
        , Collateral' [mkGenesisTxIn 13]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]]
        , Certs'
            [ UnRegTxCert (scriptStakeCredSuceed pf)
            , UnRegTxCert (scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
            ]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] redeemers mempty)
        ]
    redeemers =
      Redeemers $
        Map.fromList
          [ (RdmrPtr Tag.Cert 0, (Data (PV1.I 42), ExUnits 5000 5000))
          , (RdmrPtr Tag.Cert 1, (Data (PV1.I 42), ExUnits 5000 5000))
          ]

noCostModelTx ::
  forall era.
  ( Scriptic era
  , GoodCrypto (EraCrypto era)
  , EraTx era
  ) =>
  Proof era ->
  Tx era
noCostModelTx pf =
  newTx
    pf
    [ Body noCostModelBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated noCostModelBody) (someKeys pf)]
        , ScriptWits' [alwaysAlt 3 pf]
        , DataWits' [Data (PV1.I 123)]
        , RdmrWits redeemers
        ]
    ]
  where
    noCostModelBody =
      newTxBody
        pf
        [ Inputs' [mkGenesisTxIn 102]
        , Collateral' [mkGenesisTxIn 11]
        , Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]]
        , Txfee (Coin 5)
        , WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] redeemers (mkTxDats (Data (PV1.I 123))))
        ]
    redeemers = Redeemers $ Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

-- ============================== HELPER FUNCTIONS ===============================

testU ::
  forall era.
  ( GoodCrypto (EraCrypto era)
  , PostShelley era
  , EraTx era
  , HasCallStack
  ) =>
  Proof era ->
  Tx era ->
  Either [PredicateFailure (EraRule "UTXOW" era)] (State (EraRule "UTXOW" era)) ->
  Assertion
testU pf = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf)

quietPlutusFailure :: FailureDescription
quietPlutusFailure = PlutusFailure "human" "debug"

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

extraneousKeyHash :: Crypto c => KeyHash 'Witness c
extraneousKeyHash = hashKey . snd . mkKeyPair $ RawSeed 0 0 0 0 99

-- ============================== PPARAMS ===============================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls freeV1CostModels
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @7) 0
  , CollateralPercentage 100
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs
