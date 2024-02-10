{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW (tests, spendingPurpose1) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIxItem (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (
  Network (..),
  ProtVer (..),
  StrictMaybe (..),
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
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
import Cardano.Ledger.Plutus (Data (..), ExUnits (..), Language (..), emptyCostModels, hashData)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (fromElems)
import qualified Data.Set as Set
import GHC.Stack
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  alwaysSucceedsHash,
  initUTxO,
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
import Test.Cardano.Ledger.Generic.GenState (
  PlutusPurposeTag (..),
  mkPlutusPurposePointer,
  mkRedeemers,
  mkRedeemersFromTags,
 )
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (HasTokens (..), PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

spendingPurpose1 :: Proof era -> PlutusPurpose AsIxItem era
spendingPurpose1 = \case
  Shelley {} -> error "Unsupported"
  Allegra {} -> error "Unsupported"
  Mary {} -> error "Unsupported"
  Alonzo {} -> AlonzoSpending (AsIxItem 1 (mkGenesisTxIn 1))
  Babbage {} -> AlonzoSpending (AsIxItem 1 (mkGenesisTxIn 1))
  Conway {} -> ConwaySpending (AsIxItem 1 (mkGenesisTxIn 1))

spendingPurposeAsItem1 :: Proof era -> PlutusPurpose AsItem era
spendingPurposeAsItem1 = \case
  Shelley {} -> error "Unsupported"
  Allegra {} -> error "Unsupported"
  Mary {} -> error "Unsupported"
  Alonzo {} -> AlonzoSpending (AsItem (mkGenesisTxIn 1))
  Babbage {} -> AlonzoSpending (AsItem (mkGenesisTxIn 1))
  Conway {} -> ConwaySpending (AsItem (mkGenesisTxIn 1))

tests :: TestTree
tests =
  testGroup
    "Generic Tests for invalid transactions, testing Alonzo UTXOW PredicateFailures, in postAlonzo eras."
    [ alonzoUTXOWTests Alonzo
    , alonzoUTXOWTests Babbage
    , alonzoUTXOWTests Conway
    ]

alonzoUTXOWTests ::
  forall era.
  ( State (EraRule "UTXOW" era) ~ UTxOState era
  , HasTokens era
  , Reflect era
  , PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto,
  , Value era ~ MaryValue (EraCrypto era)
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure era
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
                  [ injectFailure (WrongNetworkInTxBody Testnet Mainnet)
                  ]
              )
        , testCase "missing required key witness" $
            testU
              pf
              (trustMeP pf True $ missingRequiredWitnessTx pf)
              ( Left
                  [ injectFailure $ MissingVKeyWitnessesUTXOW $ Set.singleton extraneousKeyHash
                  ]
              )
        , testCase "missing redeemer" $
            testU
              pf
              (trustMeP pf True $ missingRedeemerTx pf)
              ( Left
                  [ injectFailure $ CollectErrors [NoRedeemer $ spendingPurposeAsItem1 pf]
                  , injectFailure $
                      MissingRedeemers @era [(spendingPurposeAsItem1 pf, alwaysSucceedsHash 3 pf)]
                  ]
              )
        , testCase "wrong wpp hash" $
            testU
              pf
              (trustMeP pf True $ wrongWppHashTx pf)
              ( Left
                  [ injectFailure $
                      PPViewHashesDontMatch
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            (mkRedeemers pf [])
                            (mkTxDats (Data (PV1.I 123)))
                        )
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            (wrongWpphashRedeemers pf)
                            (mkTxDats (Data (PV1.I 123)))
                        )
                  ]
              )
        , testCase "missing 1-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing1phaseScriptWitnessTx pf)
              ( Left
                  [ injectFailure . MissingScriptWitnessesUTXOW . Set.singleton $
                      timelockHash 0 pf
                  ]
              )
        , testCase "missing 2-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing2phaseScriptWitnessTx pf)
              ( Left
                  [ -- these redeemers are associated with phase-1 scripts
                    injectFailure . ExtraRedeemers $
                      [ mkPlutusPurposePointer pf Minting 0
                      , mkPlutusPurposePointer pf Certifying 1
                      , mkPlutusPurposePointer pf Rewarding 0
                      ]
                  , injectFailure . MissingScriptWitnessesUTXOW . Set.singleton $
                      alwaysSucceedsHash 2 pf
                  ]
              )
        , testCase "redeemer with incorrect label" $
            testU
              pf
              (trustMeP pf True $ wrongRedeemerLabelTx pf)
              ( Left
                  [ injectFailure (CollectErrors [NoRedeemer $ spendingPurposeAsItem1 pf])
                  , -- now "wrong redeemer label" means there are both unredeemable
                    -- scripts and extra redeemers
                    injectFailure $
                      MissingRedeemers [(spendingPurposeAsItem1 pf, alwaysSucceedsHash 3 pf)]
                  , injectFailure $ ExtraRedeemers [mkPlutusPurposePointer pf Minting 1]
                  ]
              )
        , testCase "missing datum" $
            testU
              pf
              (trustMeP pf True $ missingDatumTx pf)
              ( Left
                  [ injectFailure $
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
                  [ injectFailure $
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
              ( Left [injectFailure (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]
              )
        , testCase "invalid transaction marked as valid" $
            testUTXOspecialCase
              (UTXOW pf)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notValidatingTx pf)
              ( Left
                  [ injectFailure
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
                  [ injectFailure $
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
                  [ injectFailure
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
              ( Left [injectFailure (InsufficientCollateral (Coin 5) (Coin 8))]
              )
        , testCase "two-phase UTxO with no datum hash" $
            testU
              pf
              (trustMeP pf True $ plutusOutputWithNoDataTx pf)
              ( Left
                  [ injectFailure $ UnspendableUTxONoDatumHash . Set.singleton $ mkGenesisTxIn 101
                  ]
              )
        , testCase "unacceptable supplimentary datum" $
            testUTXOWsubset
              (UTXOW pf) -- Special rules apply here, use (expected `isSubset` computed)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notOkSupplimentaryDatumTx pf)
              ( Left
                  [ injectFailure $
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
                  [ injectFailure $
                      ExtraRedeemers [mkPlutusPurposePointer pf Spending 7]
                  ]
              )
        , testCase "multiple equal plutus-locked certs" $
            testU
              pf
              (trustMeP pf True $ multipleEqualCertsInvalidTx pf)
              ( Left
                  [ injectFailure $ ExtraRedeemers [mkPlutusPurposePointer pf Certifying 1]
                  ]
              )
        , testCase "no cost model" $
            let pp' = updatePParams pf (pp pf) (Costmdls emptyCostModels)
             in testUTXOW
                  (UTXOW pf)
                  (initUTxO pf)
                  pp'
                  (trustMeP pf True $ noCostModelTx pf pp')
                  ( Left [injectFailure (CollectErrors [NoCostModel PlutusV1])]
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
    , WppHash
        ( newScriptIntegrityHash
            pf
            (pp pf)
            [PlutusV1]
            (mkRedeemers pf [])
            (mkTxDats (Data (PV1.I 123)))
        )
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
        , RdmrWits $ wrongWpphashRedeemers pf
        ]
    ]

wrongWpphashRedeemers :: Era era => Proof era -> Redeemers era
wrongWpphashRedeemers pf =
  mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

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
        , RdmrWits $ validatingManyScriptsRedeemers pf
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
        , RdmrWits $ validatingManyScriptsRedeemers pf
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
              [ (RewardAccount Testnet (scriptStakeCredSuceed pf), Coin 0)
              , (RewardAccount Testnet (timelockStakeCred pf), Coin 0)
              ]
        )
    , Mint mint
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
  where
    txOut =
      newTxOut
        pf
        [ Address (someAddr pf)
        , Amount (MaryValue (Coin 4996) mint)
        ]
    mint = forge @era 1 (always 2 pf) <> forge @era 1 (timelockScript 1 pf)

validatingManyScriptsRedeemers :: Era era => Proof era -> Redeemers era
validatingManyScriptsRedeemers pf =
  mkRedeemersFromTags
    pf
    [ ((Spending, 0), (Data (PV1.I 101), ExUnits 5000 5000))
    , ((Certifying, 1), (Data (PV1.I 102), ExUnits 5000 5000))
    , ((Rewarding, 0), (Data (PV1.I 103), ExUnits 5000 5000))
    , ((Minting, 0), (Data (PV1.I 104), ExUnits 5000 5000))
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
      -- The label *should* be Spend, not Mint
      mkRedeemersFromTags pf [((Minting, 1), (Data (PV1.I 42), ExUnits 5000 5000))]

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
    redeemers = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

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
        , RdmrWits $ validatingManyScriptsRedeemers pf
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
        , RdmrWits $ validatingRedeemers pf
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
validatingRedeemers pf = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

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
    redeemers = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 1), ExUnits 5000 5000))]

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
    redeemers = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 1000001 5000))]

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
        , RdmrWits $ mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]
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
    redeemers = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

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
        , WppHash (newScriptIntegrityHash pf (pp pf) [] (mkRedeemers pf []) totallyIrrelevantTxDats)
        ]
    totallyIrrelevantTxDats = TxDats $ Map.fromElems hashData [totallyIrrelevantDatum]
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
    extraRedeemersEx =
      mkRedeemersFromTags pf $
        [ ((Spending, 7), (Data (PV1.I 42), ExUnits 432 444))
        , ((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))
        ]

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
      mkRedeemersFromTags
        pf
        [ ((Certifying, 0), (Data (PV1.I 42), ExUnits 5000 5000))
        , ((Certifying, 1), (Data (PV1.I 42), ExUnits 5000 5000))
        ]

noCostModelTx ::
  forall era.
  ( Scriptic era
  , GoodCrypto (EraCrypto era)
  , EraTx era
  ) =>
  Proof era ->
  PParams era ->
  Tx era
noCostModelTx pf pp' =
  newTx
    pf
    [ Body noCostModelBody
    , WitnessesI
        [ AddrWits' [mkWitnessVKey (hashAnnotated noCostModelBody) (someKeys pf)]
        , ScriptWits' [always 3 pf]
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
        , WppHash (newScriptIntegrityHash pf pp' [PlutusV1] redeemers (mkTxDats (Data (PV1.I 123))))
        ]
    redeemers = mkRedeemersFromTags pf [((Spending, 0), (Data (PV1.I 42), ExUnits 5000 5000))]

-- ============================== HELPER FUNCTIONS ===============================

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

quietPlutusFailure :: FailureDescription
quietPlutusFailure = PlutusFailure "human" "debug"

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

extraneousKeyHash :: Crypto c => KeyHash 'Witness c
extraneousKeyHash = hashKey . snd . mkKeyPair $ RawSeed 0 0 0 0 99

-- ============================== PPARAMS ===============================

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls $ zeroTestingCostModels [PlutusV1]
  , MaxValSize 1000000000
  , MaxTxExUnits $ ExUnits 1000000 1000000
  , MaxBlockExUnits $ ExUnits 1000000 1000000
  , ProtocolVersion $ ProtVer (natVersion @7) 0
  , CollateralPercentage 100
  ]

pp :: EraPParams era => Proof era -> PParams era
pp pf = newPParams pf defaultPPs
