{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Tools (tests, testExUnitCalculation) where

import Cardano.Crypto.DSIGN
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (CostModel, CostModels (..), ExUnits (..), Script)
import Cardano.Ledger.Alonzo.Tools (BasicFailure (..), evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, exBudgetToExUnits, transExUnits)
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Babbage.PParams as Babbage.PParams
import Cardano.Ledger.Babbage.Tx
  ( Data,
    DataHash,
  )
import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, StrictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Ledger.Crypto
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (DCert, Wdrl)
import Cardano.Ledger.Shelley.LedgerState (IncrementalStake (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules.Utxo (PredicateFailure, UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessVKey)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart, mkSlotLength)
import Control.State.Transition.Extended (STS (BaseM, Environment, Signal, State), TRC (TRC))
import Data.Array (Array, array)
import Data.Default.Class (Default (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField (getField))
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Examples.TwoPhaseValidation (datumExample1, initUTxO, someKeys, testSystemStart, validatingBody, validatingRedeemersEx1)
import Test.Cardano.Ledger.Generic.Fields (PParamsField (..), TxField (..), WitnessesField (..))
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (Alonzo, Babbage))
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic, always)
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.QuickCheck (Gen, Property, arbitrary, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup "ExUnit tools" $
    [ testProperty "Plutus ExUnit translation round-trip" exUnitsTranslationRoundTrip,
      testGroup
        "Alonzo"
        [ testCase "calculate ExUnits" (exampleExUnitCalc (Alonzo Mock)),
          testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc (Alonzo Mock))
        ],
      testGroup
        "Babbage"
        [ testCase "calculate ExUnits" (exampleExUnitCalc (Babbage Mock)),
          testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc (Babbage Mock))
        ]
    ]

-- ExUnits should remain intact when translating to and from the plutus type
exUnitsTranslationRoundTrip :: Gen Property
exUnitsTranslationRoundTrip = do
  e <- arbitrary
  let result = (exBudgetToExUnits . transExUnits) e
  pure $
    counterexample
      ( "Before: " <> show e
          <> "\n After: "
          <> show result
      )
      $ result == Just e

-- checks plutus script validation against a tx which has had
-- its ex units replaced by the output of evaluateTransactionExecutionUnits
testExUnitCalculation ::
  forall era m.
  ( MonadFail m,
    BaseM (Core.EraRule "UTXOS" era) ~ ShelleyBase,
    State (Core.EraRule "UTXOS" era) ~ UTxOState era,
    Environment (Core.EraRule "UTXOS" era) ~ UtxoEnv era,
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    Era era,
    ExtendedUTxO era,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "txdats" (Core.Witnesses era) (TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Show (PredicateFailure (Core.EraRule "UTXOS" era)),
    STS (Core.EraRule "UTXOS" era),
    Core.Script era ~ Script era
  ) =>
  Proof era ->
  Core.Tx era ->
  UTxOState era ->
  UtxoEnv era ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m ()
testExUnitCalculation proof tx utxoState ue ei ss costmdls err = do
  tx' <- updateTxExUnits proof tx utxo ei ss costmdls err
  _ <-
    failLeft err $
      runShelleyBase $
        applySTSTest @(Core.EraRule "UTXOS" era) (TRC (ue, utxoState, tx'))
  pure ()
  where
    utxo = _utxo utxoState

exampleExUnitCalc ::
  forall era.
  ( BaseM (Core.EraRule "UTXOS" era) ~ ShelleyBase,
    State (Core.EraRule "UTXOS" era) ~ UTxOState era,
    Environment (Core.EraRule "UTXOS" era) ~ UtxoEnv era,
    Signable
      (Ledger.Crypto.DSIGN (Crypto era))
      ( Crypto.Hash
          (Ledger.Crypto.HASH (Crypto era))
          EraIndependentTxBody
      ),
    Signal (Core.EraRule "UTXOS" era) ~ Core.Tx era,
    ExtendedUTxO era,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "txdats" (Core.Witnesses era) (TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    STS (Core.EraRule "UTXOS" era),
    Scriptic era,
    PostShelley era,
    Default (State (Core.EraRule "PPUP" era)),
    Core.Script era ~ Script era
  ) =>
  Proof era ->
  IO ()
exampleExUnitCalc proof =
  testExUnitCalculation
    proof
    (exampleTx proof)
    (ustate proof)
    (uenv proof)
    exampleEpochInfo
    testSystemStart
    costmodels
    assertFailure

exampleInvalidExUnitCalc ::
  forall era.
  ( ExtendedUTxO era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "txdats" (Core.Witnesses era) (TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    Core.Script era ~ Script era,
    Signable
      (Ledger.Crypto.DSIGN (Crypto era))
      ( Crypto.Hash
          (Ledger.Crypto.HASH (Crypto era))
          EraIndependentTxBody
      ),
    Scriptic era
  ) =>
  Proof era ->
  IO ()
exampleInvalidExUnitCalc proof = do
  res <-
    evaluateTransactionExecutionUnits @era
      (pparams proof)
      (exampleTx proof)
      (UTxO mempty)
      exampleEpochInfo
      testSystemStart
      costmodels
  case res of
    Left (UnknownTxIns _) -> pure ()
    Left (BadTranslation _) ->
      assertFailure "evaluateTransactionExecutionUnits should not fail from BadTranslation"
    Right _ -> assertFailure "evaluateTransactionExecutionUnits should have failed"

exampleTx ::
  ( Scriptic era,
    Signable (Ledger.Crypto.DSIGN (Crypto era)) (Crypto.Hash (Ledger.Crypto.HASH (Crypto era)) EraIndependentTxBody)
  ) =>
  Proof era ->
  Core.Tx era
exampleTx pf =
  newTx
    pf
    [ Body (validatingBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx1
        ]
    ]

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: Proof era -> UtxoEnv era
uenv pf = UtxoEnv (SlotNo 0) (pparams pf) mempty (GenDelegs mempty)

costmodels :: Array Language CostModel
costmodels = array (PlutusV1, PlutusV1) [(PlutusV1, testingCostModelV1)]

ustate :: (PostShelley era, Default (State (Core.EraRule "PPUP" era))) => Proof era -> UTxOState era
ustate pf =
  UTxOState
    { _utxo = initUTxO pf,
      _deposited = Coin 0,
      _fees = Coin 0,
      _ppups = def,
      _stakeDistro = IStake mempty mempty
    }

updateTxExUnits ::
  forall era m.
  ( MonadFail m,
    Era era,
    ExtendedUTxO era,
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "txdats" (Core.Witnesses era) (TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Core.Script era ~ Script era
  ) =>
  Proof era ->
  Core.Tx era ->
  UTxO era ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m (Core.Tx era)
updateTxExUnits proof tx utxo ei ss costmdls err = do
  res <- evaluateTransactionExecutionUnits (pparams proof) tx utxo ei ss costmdls
  case res of
    Left e -> err (show e)
    -- rdmrs :: Map RdmrPtr ExUnits
    Right rdmrs -> (replaceRdmrs proof tx) <$> traverse (failLeft err) rdmrs

replaceRdmrs :: forall era. (Era era, HasField "txrdmrs" (Core.Witnesses era) (Redeemers era)) => Proof era -> Core.Tx era -> (Map RdmrPtr ExUnits) -> Core.Tx era
replaceRdmrs pf tx rdmrs = updateTx pf tx (WitnessesI [RdmrWits newrdmrs])
  where
    newrdmrs = foldr replaceRdmr (getField @"txrdmrs" (getField @"wits" tx)) (Map.assocs rdmrs)

    replaceRdmr :: (RdmrPtr, ExUnits) -> Redeemers era -> Redeemers era
    replaceRdmr (ptr, ex) x@(Redeemers r) =
      case Map.lookup ptr r of
        Just (dat, _ex) -> Redeemers $ Map.insert ptr (dat, ex) r
        Nothing -> x

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)

pparams :: Proof era -> Core.PParams era
pparams proof =
  newPParams
    proof
    [ Costmdls . CostModels $ Map.singleton PlutusV1 testingCostModelV1,
      MaxValSize 1000000000,
      MaxTxExUnits $ ExUnits 100000000 100000000,
      MaxBlockExUnits $ ExUnits 100000000 100000000,
      ProtocolVersion $ ProtVer 5 0
    ]
