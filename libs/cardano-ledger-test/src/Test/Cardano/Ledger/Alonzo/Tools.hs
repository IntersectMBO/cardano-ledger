{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Tools (tests) where

import Cardano.Crypto.DSIGN
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, ExUnits (..), Tag (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO, exBudgetToExUnits, transExUnits)
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Api.Tx (TransactionScriptFailure (..), evalTxExUnits)
import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (IncrementalStake (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val (Val (inject))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended (STS (BaseM, Environment, Signal, State), TRC (TRC))
import Data.Default.Class (Default (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.CostModel (freeV1CostModels)
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
import Test.Cardano.Ledger.Generic.Proof (Evidence (Mock), Proof (Alonzo, Babbage))
import Test.Cardano.Ledger.Generic.Scriptic (PostShelley, Scriptic, always)
import Test.Cardano.Ledger.Generic.Updaters
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
        [ testCase "calculate ExUnits" (exampleExUnitCalc (Alonzo Mock))
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc (Alonzo Mock))
        ]
    , testGroup
        "Babbage"
        [ testCase "calculate ExUnits" (exampleExUnitCalc (Babbage Mock))
        , testCase "attempt calculate ExUnits with invalid tx" (exampleInvalidExUnitCalc (Babbage Mock))
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
  , ExtendedUTxO era
  , STS (EraRule "UTXOS" era)
  , Script era ~ AlonzoScript era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext 'PlutusV1 era
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
  , Signable (DSIGN (EraCrypto era)) (Crypto.Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , ExtendedUTxO era
  , STS (EraRule "UTXOS" era)
  , AlonzoEraTx era
  , PostShelley era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraGovernance era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  Proof era ->
  IO ()
exampleExUnitCalc proof =
  testExUnitCalculation
    (exampleTx proof (RdmrPtr Spend 0))
    (ustate proof)
    uenv
    exampleEpochInfo
    testSystemStart
    assertFailure

exampleInvalidExUnitCalc ::
  forall era.
  ( ExtendedUTxO era
  , PostShelley era
  , AlonzoEraTx era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , Signable
      (DSIGN (EraCrypto era))
      (Crypto.Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraPlutusContext 'PlutusV1 era
  ) =>
  Proof era ->
  IO ()
exampleInvalidExUnitCalc proof = do
  let result =
        evalTxExUnits @era
          testPParams
          (exampleTx proof (RdmrPtr Spend 1))
          (initUTxO proof)
          exampleEpochInfo
          testSystemStart
  case result of
    Left err ->
      assertFailure $
        "evalTxExUnits should not have failed, but it did with: " ++ show err
    Right report ->
      case [(rdmrPtr, failure) | (rdmrPtr, Left failure) <- Map.toList report] of
        [] ->
          assertFailure "evalTxExUnits should have produced a failing report"
        [(_, failure)] ->
          RedeemerPointsToUnknownScriptHash (RdmrPtr Spend 1)
            @=? failure
        failures ->
          assertFailure $
            "evalTxExUnits produce failing scripts with unexpected errors: "
              ++ show failures

exampleTx ::
  ( Scriptic era
  , AlonzoEraTx era
  , Signable (DSIGN (EraCrypto era)) (Crypto.Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  Proof era ->
  RdmrPtr ->
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
        & rdmrsTxWitsL
          .~ Redeemers (Map.singleton ptr (Data (PV1.I 42), ExUnits 5000 5000))

validatingBody :: (Scriptic era, AlonzoEraTxBody era) => Proof era -> TxBody era
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
        Map.singleton (RdmrPtr Tag.Spend 0) (Data (PV1.I 42), ExUnits 5000 5000)

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: AlonzoEraPParams era => UtxoEnv era
uenv = UtxoEnv (SlotNo 0) testPParams def (GenDelegs mempty)

ustate ::
  ( EraTxOut era
  , PostShelley era
  , EraGovernance era
  ) =>
  Proof era ->
  UTxOState era
ustate pf =
  UTxOState
    { utxosUtxo = initUTxO pf
    , utxosDeposited = Coin 0
    , utxosFees = Coin 0
    , utxosGovernance = def
    , utxosStakeDistr = IStake mempty mempty
    }

updateTxExUnits ::
  forall era m.
  ( MonadFail m
  , AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  Tx era ->
  UTxO era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  (forall a. String -> m a) ->
  m (Tx era)
updateTxExUnits tx utxo ei ss err =
  let res = evalTxExUnits testPParams tx utxo ei ss
   in case res of
        Left e -> err (show e)
        Right (rdmrs :: Map RdmrPtr (Either (TransactionScriptFailure era) ExUnits)) ->
          replaceRdmrs tx <$> traverse (failLeft err) rdmrs

replaceRdmrs ::
  forall era.
  (AlonzoEraTxWits era, EraTx era) =>
  Tx era ->
  Map RdmrPtr ExUnits ->
  Tx era
replaceRdmrs tx rdmrs = tx & witsTxL . rdmrsTxWitsL .~ newRdmrs
  where
    newRdmrs = Map.foldrWithKey replaceRdmr (tx ^. witsTxL . rdmrsTxWitsL) rdmrs

    replaceRdmr :: RdmrPtr -> ExUnits -> Redeemers era -> Redeemers era
    replaceRdmr ptr ex x@(Redeemers r) =
      case Map.lookup ptr r of
        Just (dat, _ex) -> Redeemers $ Map.insert ptr (dat, ex) r
        Nothing -> x

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)

testPParams :: forall era. AlonzoEraPParams era => PParams era
testPParams =
  emptyPParams
    & ppCostModelsL .~ freeV1CostModels
    & ppMaxValSizeL .~ 1000000000
    & ppMaxTxExUnitsL .~ ExUnits 100000000 100000000
    & ppMaxBlockExUnitsL .~ ExUnits 100000000 100000000
    & ppProtocolVersionL .~ ProtVer (eraProtVerHigh @era) 0
