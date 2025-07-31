{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Tools (
  exampleExUnitCalc,
  exampleInvalidExUnitCalc,
) where

import Cardano.Ledger.Allegra.Scripts (AllegraEraScript)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext, EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), eraLanguages)
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Api.Tx (
  RedeemerReport,
  TransactionScriptFailure (..),
  evalTxExUnits,
 )
import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase, inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
 )
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.Shelley.State
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended (STS (..), TRC (TRC))
import Data.Default (Default (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common (ToExpr, ansiExprString)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Examples.STSTestUtils (
  EraModel (..),
  initUTxO,
  mkGenesisTxIn,
  mkTxDats,
  someAddr,
  someKeys,
 )
import Test.Cardano.Ledger.Generic.GenState (EraGenericGen (..))
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty.HUnit (assertFailure, (@=?))

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
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  , AlonzoEraTx era
  , STS (EraRule "UTXOS" era)
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  , EraGenericGen era
  , ToExpr (TransactionScriptFailure era)
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
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraGenericGen era
  , EraPlutusTxInfo PlutusV1 era
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  , ToExpr (TransactionScriptFailure era)
  ) =>
  IO ()
exampleExUnitCalc =
  testExUnitCalculation @era
    (exampleTx (AlonzoSpending (AsIx 0)))
    ustate
    uenv
    exampleEpochInfo
    testSystemStart
    assertFailure

exampleInvalidExUnitCalc ::
  forall era.
  ( AlonzoEraTx era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraGenericGen era
  , EraPlutusTxInfo PlutusV1 era
  ) =>
  IO ()
exampleInvalidExUnitCalc =
  let report =
        evalTxExUnits @era
          testPParams
          (exampleTx (AlonzoSpending (AsIx 1)))
          initUTxO
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
  forall era.
  ( AlonzoEraTx era
  , PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraGenericGen era
  ) =>
  PlutusPurpose AsIx era ->
  Tx era
exampleTx ptr = mkBasicTx validatingBody & witsTxL .~ wits
  where
    wits =
      mkBasicTxWits
        & addrTxWitsL
          .~ Set.fromList [mkWitnessVKey (hashAnnotated $ validatingBody @era) someKeys]
        & hashScriptTxWitsL .~ [always 3]
        & hashDataTxWitsL .~ [Data (PV1.I 123)]
        & rdmrsTxWitsL . unRedeemersL
          %~ Map.insert ptr (Data (PV1.I 42), ExUnits 5000 5000)

validatingBody ::
  forall era.
  ( PlutusPurpose AsIx era ~ AlonzoPlutusPurpose AsIx era
  , EraGenericGen era
  , AlonzoEraScript era
  ) =>
  TxBody era
validatingBody =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList [mkGenesisTxIn 1]
    & setCollateralInputs (Set.fromList [mkGenesisTxIn 11])
    & outputsTxBodyL
      .~ SSeq.fromList [mkBasicTxOut someAddr (inject $ Coin 4995)]
    & feeTxBodyL .~ Coin 5
    & setScriptIntegrityHash
      (newScriptIntegrityHash testPParams [PlutusV1] redeemers (mkTxDats (Data (PV1.I 123))))
  where
    redeemers = mkRedeemers [(AlonzoSpending @_ @era (AsIx 0), (Data (PV1.I 42), ExUnits 5000 5000))]

exampleEpochInfo :: Monad m => EpochInfo m
exampleEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

uenv :: (EraGenericGen era, AlonzoEraScript era) => UtxoEnv era
uenv = UtxoEnv (SlotNo 0) testPParams def

ustate ::
  ( AllegraEraScript era
  , AlonzoEraTxOut era
  , EraModel era
  ) =>
  UTxOState era
ustate =
  UTxOState
    { utxosUtxo = initUTxO
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
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  , EraGenericGen era
  , ToExpr (TransactionScriptFailure era)
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

failLeft :: (Monad m, ToExpr e) => (String -> m a) -> Either e a -> m a
failLeft err = either (err . ansiExprString) pure

testPParams :: forall era. (EraGenericGen era, AlonzoEraScript era) => PParams era
testPParams =
  emptyPParams @era
    & ppCostModelsT .~ zeroTestingCostModels (eraLanguages @era)
    & ppMaxValSizeT .~ 1000000000
    & ppMaxTxExUnitsT .~ ExUnits 100000000 100000000
    & ppMaxBlockExUnitsT .~ ExUnits 100000000 100000000
    & ppProtocolVersionL .~ ProtVer (eraProtVerHigh @era) 0
