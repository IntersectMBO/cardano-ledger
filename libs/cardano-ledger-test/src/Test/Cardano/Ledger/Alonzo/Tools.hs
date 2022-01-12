{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Tools (tests, testExUnitCalculation) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (UTXOS)
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..))
import Cardano.Ledger.Alonzo.Tools (BasicFailure (..), evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx
  ( ValidatedTx (..),
  )
import Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits, transExUnits)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, Redeemers (..), txrdmrs)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (IncrementalStake (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..), makeWitnessVKey)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart, mkSlotLength)
import Control.State.Transition.Extended (TRC (..))
import Data.Array (Array, array)
import Data.Default.Class (Default (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Examples.TwoPhaseValidation (A, datumExample1, initUTxO, someKeys, testSystemStart, validatingBody, validatingRedeemersEx1)
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (Alonzo))
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.QuickCheck (Gen, Property, arbitrary, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup "ExUnit tools" $
    [ testProperty "Plutus ExUnit translation round-trip" exUnitsTranslationRoundTrip,
      testCase "calculate ExUnits" exampleExUnitCalc,
      testCase "attempt calculate ExUnits with invalid tx" exampleInvalidExUnitCalc
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
  MonadFail m =>
  Core.Tx A ->
  UTxOState A ->
  UtxoEnv A ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m ()
testExUnitCalculation tx utxoState ue ei ss costmdls err = do
  tx' <- updateTxExUnits tx utxo ei ss costmdls err
  _ <-
    failLeft err $
      runShelleyBase $
        applySTSTest @(UTXOS A) (TRC (ue, utxoState, tx'))
  pure ()
  where
    utxo = _utxo utxoState

exampleExUnitCalc :: IO ()
exampleExUnitCalc =
  testExUnitCalculation
    exampleTx
    ustate
    uenv
    exampleEpochInfo
    testSystemStart
    costmodels
    assertFailure

exampleInvalidExUnitCalc :: IO ()
exampleInvalidExUnitCalc = do
  res <-
    evaluateTransactionExecutionUnits
      pparams
      exampleTx
      (UTxO mempty)
      exampleEpochInfo
      testSystemStart
      costmodels
  case res of
    Left (UnknownTxIns _) -> pure ()
    Left (BadTranslation _) ->
      assertFailure "evaluateTransactionExecutionUnits should not fail from BadTranslation"
    Right _ -> assertFailure "evaluateTransactionExecutionUnits should have failed"

exampleTx :: Core.Tx A
exampleTx =
  let pf = Alonzo Mock
   in newTx
        Override
        pf
        [ Body (validatingBody pf),
          Witnesses'
            [ AddrWits [makeWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)],
              ScriptWits [always 3 pf],
              DataWits [datumExample1],
              RdmrWits validatingRedeemersEx1
            ]
        ]

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: UtxoEnv A
uenv = UtxoEnv (SlotNo 0) pparams mempty (GenDelegs mempty)

costmodels :: Array Language CostModel
costmodels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]

ustate :: UTxOState A
ustate =
  UTxOState
    { _utxo = initUTxO (Alonzo Mock),
      _deposited = Coin 0,
      _fees = Coin 0,
      _ppups = def,
      _stakeDistro = IStake mempty mempty
    }

-- Requires ex units, but there are no fees
pparams :: PParams A
pparams =
  newPParams
    (Alonzo Mock)
    [ Costmdls $ Map.singleton PlutusV1 $ fromJust defaultCostModel,
      MaxValSize 1000000000,
      MaxTxExUnits $ ExUnits 100000000 100000000,
      MaxBlockExUnits $ ExUnits 100000000 100000000,
      ProtocolVersion $ ProtVer 5 0
    ]

updateTxExUnits ::
  MonadFail m =>
  Core.Tx A ->
  UTxO A ->
  EpochInfo m ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m (Core.Tx A)
updateTxExUnits tx utxo ei ss costmdls err = do
  res <- evaluateTransactionExecutionUnits pparams tx utxo ei ss costmdls
  case res of
    Left e -> err (show e)
    -- rdmrs :: Map RdmrPtr ExUnits
    Right rdmrs -> (replaceRdmrs tx) <$> traverse (failLeft err) rdmrs

replaceRdmrs :: Core.Tx A -> Map RdmrPtr ExUnits -> Core.Tx A
replaceRdmrs tx rdmrs = tx {wits = wits'}
  where
    wits' = (wits tx) {txrdmrs = newrdmrs}
    newrdmrs = foldr replaceRdmr (txrdmrs (wits tx)) (Map.assocs rdmrs)

    replaceRdmr :: (RdmrPtr, ExUnits) -> Redeemers A -> Redeemers A
    replaceRdmr (ptr, ex) x@(Redeemers r) =
      case Map.lookup ptr r of
        Just (dat, _ex) -> Redeemers $ Map.insert ptr (dat, ex) r
        Nothing -> x

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)
