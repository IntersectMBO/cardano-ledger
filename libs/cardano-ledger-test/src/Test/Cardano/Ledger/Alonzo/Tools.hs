{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Tools (tests, testExUnitCalculation) where

import Cardano.Crypto.DSIGN
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, CostModel, CostModels (..), ExUnits (..), Tag (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag
import Cardano.Ledger.Alonzo.Tools (TransactionScriptFailure (..), evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, exBudgetToExUnits, transExUnits)
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import qualified Cardano.Ledger.Babbage.PParams as Babbage.PParams
import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (GenDelegs (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (IncrementalStake (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), UTxO (..), makeWitnessVKey)
import Cardano.Ledger.Val (Val (inject))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended (STS (BaseM, Environment, Signal, State), TRC (TRC))
import Data.Array (Array, array)
import Data.Default.Class (Default (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Records (HasField)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as Plutus
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Examples.STSTestUtils
  ( initUTxO,
    mkGenesisTxIn,
    mkTxDats,
    someAddr,
    someKeys,
  )
import Test.Cardano.Ledger.Generic.Fields
  ( PParamsField (..),
    TxBodyField (..),
    TxField (..),
    TxOutField (..),
    WitnessesField (..),
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
-- its ex units replaced by the output of evaluateTransactionExecutionUnits
testExUnitCalculation ::
  forall era m.
  ( MonadFail m,
    BaseM (EraRule "UTXOS" era) ~ ShelleyBase,
    State (EraRule "UTXOS" era) ~ UTxOState era,
    Environment (EraRule "UTXOS" era) ~ UtxoEnv era,
    Signal (EraRule "UTXOS" era) ~ Tx era,
    AlonzoEraTx era,
    ExtendedUTxO era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    STS (EraRule "UTXOS" era),
    Script era ~ AlonzoScript era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  Proof era ->
  Tx era ->
  UTxOState era ->
  UtxoEnv era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m ()
testExUnitCalculation proof tx utxoState ue ei ss costmdls err = do
  tx' <- updateTxExUnits proof tx utxo ei ss costmdls err
  _ <-
    failLeft err $
      runShelleyBase $
        applySTSTest @(EraRule "UTXOS" era) (TRC (ue, utxoState, tx'))
  pure ()
  where
    utxo = utxosUtxo utxoState

exampleExUnitCalc ::
  forall era.
  ( BaseM (EraRule "UTXOS" era) ~ ShelleyBase,
    State (EraRule "UTXOS" era) ~ UTxOState era,
    Environment (EraRule "UTXOS" era) ~ UtxoEnv era,
    Signable (CC.DSIGN (EraCrypto era)) (Crypto.Hash (CC.HASH (EraCrypto era)) EraIndependentTxBody),
    Signal (EraRule "UTXOS" era) ~ Tx era,
    ExtendedUTxO era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    STS (EraRule "UTXOS" era),
    AlonzoEraTx era,
    PostShelley era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    Default (State (EraRule "PPUP" era)),
    Script era ~ AlonzoScript era
  ) =>
  Proof era ->
  IO ()
exampleExUnitCalc proof =
  testExUnitCalculation
    proof
    (exampleTx proof (RdmrPtr Spend 0))
    (ustate proof)
    (uenv proof)
    exampleEpochInfo
    testSystemStart
    costmodels
    assertFailure

exampleInvalidExUnitCalc ::
  forall era.
  ( ExtendedUTxO era,
    PostShelley era,
    AlonzoEraTx era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    Script era ~ AlonzoScript era,
    Signable
      (CC.DSIGN (EraCrypto era))
      (Crypto.Hash (CC.HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  Proof era ->
  IO ()
exampleInvalidExUnitCalc proof = do
  let result =
        evaluateTransactionExecutionUnits @era
          (pparams proof)
          (exampleTx proof (RdmrPtr Spend 1))
          (initUTxO proof)
          exampleEpochInfo
          testSystemStart
          costmodels
  case result of
    Left err ->
      assertFailure $
        "evaluateTransactionExecutionUnits should not have failed, but it did with: " ++ show err
    Right report ->
      case [(rdmrPtr, failure) | (rdmrPtr, Left failure) <- Map.toList report] of
        [] ->
          assertFailure "evaluateTransactionExecutionUnits should have produced a failing report"
        [(_, failure)] ->
          RedeemerPointsToUnknownScriptHash (RdmrPtr Spend 1)
            @=? failure
        failures ->
          assertFailure $
            "evaluateTransactionExecutionUnits produce failing scripts with unexpected errors: "
              ++ show failures

exampleTx ::
  ( Scriptic era,
    EraTxBody era,
    Signable (CC.DSIGN (EraCrypto era)) (Crypto.Hash (CC.HASH (EraCrypto era)) EraIndependentTxBody)
  ) =>
  Proof era ->
  RdmrPtr ->
  Tx era
exampleTx pf ptr =
  newTx
    pf
    [ Body (validatingBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [Data (Plutus.I 123)],
          RdmrWits $
            Redeemers $
              Map.singleton ptr (Data (Plutus.I 42), ExUnits 5000 5000)
        ]
    ]

validatingBody :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
validatingBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pparams pf) [PlutusV1] redeemers (mkTxDats (Data (Plutus.I 123))))
    ]
  where
    redeemers =
      Redeemers $
        Map.singleton (RdmrPtr Tag.Spend 0) (Data (Plutus.I 42), ExUnits 5000 5000)

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: Proof era -> UtxoEnv era
uenv pf = UtxoEnv (SlotNo 0) (pparams pf) def (GenDelegs mempty)

costmodels :: Array Language CostModel
costmodels = array (PlutusV1, PlutusV1) [(PlutusV1, testingCostModelV1)]

ustate ::
  (EraTxOut era, PostShelley era, Default (State (EraRule "PPUP" era))) =>
  Proof era ->
  UTxOState era
ustate pf =
  UTxOState
    { utxosUtxo = initUTxO pf,
      utxosDeposited = Coin 0,
      utxosFees = Coin 0,
      utxosPpups = def,
      utxosStakeDistr = IStake mempty mempty
    }

updateTxExUnits ::
  forall era m.
  ( MonadFail m,
    AlonzoEraTx era,
    ExtendedUTxO era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    Script era ~ AlonzoScript era
  ) =>
  Proof era ->
  Tx era ->
  UTxO era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  Array Language CostModel ->
  (forall a. String -> m a) ->
  m (Tx era)
updateTxExUnits proof tx utxo ei ss costmdls err =
  let res = evaluateTransactionExecutionUnits (pparams proof) tx utxo ei ss costmdls
   in case res of
        Left e -> err (show e)
        Right (rdmrs :: Map RdmrPtr (Either (TransactionScriptFailure (EraCrypto era)) ExUnits)) ->
          replaceRdmrs proof tx <$> traverse (failLeft err) rdmrs

replaceRdmrs ::
  forall era.
  (AlonzoEraTxWits era, EraTx era) =>
  Proof era ->
  Tx era ->
  Map RdmrPtr ExUnits ->
  Tx era
replaceRdmrs pf tx rdmrs = updateTx pf tx (WitnessesI [RdmrWits newrdmrs])
  where
    newrdmrs = foldr replaceRdmr (tx ^. witsTxL . rdmrsTxWitsL) (Map.assocs rdmrs)

    replaceRdmr :: (RdmrPtr, ExUnits) -> Redeemers era -> Redeemers era
    replaceRdmr (ptr, ex) x@(Redeemers r) =
      case Map.lookup ptr r of
        Just (dat, _ex) -> Redeemers $ Map.insert ptr (dat, ex) r
        Nothing -> x

failLeft :: (Monad m, Show e) => (String -> m a) -> Either e a -> m a
failLeft _ (Right a) = pure a
failLeft err (Left e) = err (show e)

pparams :: Proof era -> PParams era
pparams proof =
  newPParams
    proof
    [ Costmdls . CostModels $ Map.singleton PlutusV1 testingCostModelV1,
      MaxValSize 1000000000,
      MaxTxExUnits $ ExUnits 100000000 100000000,
      MaxBlockExUnits $ ExUnits 100000000 100000000,
      ProtocolVersion $ ProtVer (natVersion @5) 0
    ]
