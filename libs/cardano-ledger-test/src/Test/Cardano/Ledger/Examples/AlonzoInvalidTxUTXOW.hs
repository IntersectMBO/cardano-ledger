{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.AlonzoInvalidTxUTXOW (tests, spendingPurpose1) where

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
  AsIxItem (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  ProtVer (..),
  natVersion,
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Credential (
  Credential (..),
  StakeCredential,
 )
import Cardano.Ledger.Keys (
  asWitness,
  hashKey,
 )
import Cardano.Ledger.Plutus (Data (..), ExUnits (..), Language (..), emptyCostModels)
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.Core hiding (TranslationError)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules as Shelley (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Val (inject)
import Control.State.Transition.Extended hiding (Assertion)
import Data.List.NonEmpty (NonEmpty (..))
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
  testUTXOspecialCase,
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
  mkRedeemersFromTags,
 )
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
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
  , Reflect era
  , PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto,
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
        [ testCase "invalid transaction marked as valid" $
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
              ( Left [injectFailure (InsufficientCollateral (DeltaCoin 5) (Coin 8))]
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
        , WppHash
            (newScriptIntegrityHash pf (pp pf) [PlutusV1] extraRedeemersEx (mkTxDats (Data (PV1.I 123))))
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
  Either (NonEmpty (PredicateFailure (EraRule "UTXOW" era))) (State (EraRule "UTXOW" era)) ->
  Assertion
testU pf = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf)

quietPlutusFailure :: FailureDescription
quietPlutusFailure = PlutusFailure "human" "debug"

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential (EraCrypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

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
