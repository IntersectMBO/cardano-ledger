{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Tools (tests) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Api.Tx (RedeemerReport, TransactionScriptFailure (..), evalTxExUnits)
import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  exBudgetToExUnits,
  transExUnits,
 )
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.Shelley.State
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended (STS (BaseM, Environment, Signal, State), TRC (TRC))
import Data.Default (Default (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.Proof (Proof (Alonzo, Babbage))
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic, always)
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck (Gen, Property, arbitrary, counterexample, testProperty)

tests :: TestTree
tests =
  testGroup
    "ExUnit tools"
    [ testProperty "Plutus ExUnit translation round-trip" exUnitsTranslationRoundTrip
    , testGroup
        "Alonzo"
        [ testCase "calculate ExUnits" (exampleExUnitCalc Alonzo)
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc Alonzo)
        ]
    , testGroup
        "Babbage"
        [ testCase "calculate ExUnits" (exampleExUnitCalc Babbage)
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc Babbage)
        ]
    ]

-- ExUnits should remain intact when translating to and from the plutus type
exUnitsTranslationRoundTrip :: Gen Property
exUnitsTranslationRoundTrip = do
  e <- arbitrary
  let result = exBudgetToExUnits (transExUnits e)
  pure
    $ counterexample
      ( "Before: "
          <> show e
          <> "\n After: "
          <> show result
      )
    $ result == Just e

testSystemStart :: SystemStart
testSystemStart = SystemStart $ posixSecondsToUTCTime 0

-- checks plutus script validation against a tx which has had
-- its ex units replaced by the output of evalTxExUnits
testExUnitCalculation ::
  forall era m.
  ( MonadFail m
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , AlonzoEraTx era
  , STS (EraRule "UTXOS" era)
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  ) =>
  Tx era ->
  UTxOState era ->
  UtxoEnv era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  (forall a. String -> m a) ->
  m ()
testExUnitCalculation tx utxoState ue ei ss err = do
  tx' <- updateTxExUnits tx utxo ei ss err
  _ <-
    failLeft err $
      runShelleyBase $
        applySTSTest @(EraRule "UTXOS" era) (TRC (ue, utxoState, tx'))
  pure ()
  where
    utxo = utxosUtxo utxoState

exampleExUnitCalc ::
  forall era.
  ( BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , AlonzoEraTx era
  , PostShelley era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraPlutusContext era
  , EraGov era
  , EraStake era
  , EraCertState era
  ) =>
  Proof era ->
  IO ()
exampleExUnitCalc proof =
  testExUnitCalculation
    (exampleTx proof (AlonzoSpending (AsIx 0)))
    (ustate proof)
    uenv
    exampleEpochInfo
    testSystemStart
    assertFailure

exampleInvalidExUnitCalc ::
  forall era.
  ( PostShelley era
  , AlonzoEraTx era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraPlutusContext era
  ) =>
  Proof era ->
  IO ()
exampleInvalidExUnitCalc proof =
  let report =
        evalTxExUnits @era
          testPParams
          (exampleTx proof (AlonzoSpending (AsIx 1)))
          (initUTxO proof)
          exampleEpochInfo
          testSystemStart
   in case [(rdmrPtr, failure) | (rdmrPtr, Left failure) <- Map.toList report] of
        [] ->
          assertFailure "evalTxExUnits should have produced a failing report"
        [(_, failure)] ->
          RedeemerPointsToUnknownScriptHash (AlonzoSpending (AsIx 1))
            @=? failure
        failures ->
          assertFailure $
            "evalTxExUnits produce failing scripts with unexpected errors: "
              ++ show failures

exampleTx ::
  ( Scriptic era
  , AlonzoEraTx era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  ) =>
  Proof era ->
  PlutusPurpose AsIx era ->
  Tx era
exampleTx pf ptr =
  mkBasicTx (validatingBody pf)
    & witsTxL .~ wits
  where
    wits =
      mkBasicTxWits
        & addrTxWitsL
          .~ Set.fromList [mkWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)]
        & hashScriptTxWitsL .~ [always 3 pf]
        & hashDataTxWitsL .~ [Data (PV1.I 123)]
        & rdmrsTxWitsL . unRedeemersL
          %~ Map.insert ptr (Data (PV1.I 42), ExUnits 5000 5000)

validatingBody ::
  forall era.
  ( Scriptic era
  , AlonzoEraTxBody era
  , AlonzoEraScript era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  ) =>
  Proof era ->
  TxBody era
validatingBody pf =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList [mkGenesisTxIn 1]
    & collateralInputsTxBodyL .~ Set.fromList [mkGenesisTxIn 11]
    & outputsTxBodyL
      .~ SSeq.fromList [mkBasicTxOut (someAddr pf) (inject $ Coin 4995)]
    & feeTxBodyL .~ Coin 5
    & scriptIntegrityHashTxBodyL
      .~ newScriptIntegrityHash pf testPParams [PlutusV1] redeemers (mkTxDats (Data (PV1.I 123)))
  where
    redeemers =
      Redeemers $
        Map.singleton (AlonzoSpending @_ @era (AsIx 0)) (Data (PV1.I 42), ExUnits 5000 5000)

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: (AlonzoEraPParams era, EraCertState era) => UtxoEnv era
uenv = UtxoEnv (SlotNo 0) testPParams def

ustate ::
  ( EraTxOut era
  , PostShelley era
  , EraStake era
  , EraGov era
  ) =>
  Proof era ->
  UTxOState era
ustate pf =
  UTxOState
    { utxosUtxo = initUTxO pf
    , utxosDeposited = Coin 0
    , utxosFees = Coin 0
    , utxosGovState = def
    , utxosInstantStake = mempty
    , utxosDonation = mempty
    }

updateTxExUnits ::
  forall era m.
  ( MonadFail m
  , AlonzoEraTx era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  ) =>
  Tx era ->
  UTxO era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  (forall a. String -> m a) ->
  m (Tx era)
updateTxExUnits tx utxo ei ss err =
  let res :: RedeemerReport era
      res = evalTxExUnits testPParams tx utxo ei ss
   in updateRdmrUnits tx <$> traverse (failLeft err) res

updateRdmrUnits ::
  forall era.
  (AlonzoEraTxWits era, EraTx era) =>
  Tx era ->
  Map (PlutusPurpose AsIx era) ExUnits ->
  Tx era
updateRdmrUnits tx rdmrs = tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ updateFrom rdmrs
  where
    -- Update only the keys that are already in the old map
    updateFrom new old = Map.foldrWithKey (\k eu -> Map.adjust (eu <$) k) old new

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)

testPParams :: forall era. AlonzoEraPParams era => PParams era
testPParams =
  emptyPParams
    & ppCostModelsL .~ zeroTestingCostModels [PlutusV1]
    & ppMaxValSizeL .~ 1000000000
    & ppMaxTxExUnitsL .~ ExUnits 100000000 100000000
    & ppMaxBlockExUnitsL .~ ExUnits 100000000 100000000
    & ppProtocolVersionL .~ ProtVer (eraProtVerHigh @era) 0
