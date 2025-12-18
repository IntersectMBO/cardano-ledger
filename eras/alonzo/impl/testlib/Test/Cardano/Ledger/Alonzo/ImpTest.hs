{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.ImpTest (
  module Test.Cardano.Ledger.Mary.ImpTest,
  AlonzoEraImp (..),
  impLookupPlutusScript,
  malformedPlutus,
  addCollateralInput,
  impGetPlutusContexts,
  alonzoFixupTx,
  plutusTestScripts,
  impGetScriptContext,
  impLookupScriptContext,
  impPlutusWithContexts,
  impScriptPredicateFailure,
  submitPhase2Invalid_,
  submitPhase2Invalid,
  impAlonzoExpectTxSuccess,
  impComputeScriptIntegrity,
  computeScriptIntegrityHash,
  computeScriptIntegrity,
  -- Fixup
  fixupDatums,
  fixupOutputDatums,
  fixupPPHash,
  fixupRedeemers,
  fixupRedeemerIndices,
  fixupScriptWits,
  alonzoFixupFees,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  collectPlutusScriptsWithContext,
  evalPlutusScriptsWithLogs,
  evalTxExUnits,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyPredFailure,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure,
  TagMismatchDescription (..),
  scriptFailureToFailureDescription,
 )
import Cardano.Ledger.Alonzo.Scripts (toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.Tx (ScriptIntegrity, hashScriptIntegrity, mkScriptIntegrity)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL, unTxDatsL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (Globals (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (
  Data (..),
  Datum (..),
  ExUnits (..),
  Language (..),
  Plutus (..),
  PlutusBinary (..),
  PlutusLanguage,
  PlutusWithContext (..),
  Prices (..),
  SLanguage (..),
  ScriptResult (..),
  hashData,
  hashPlutusScript,
  plutusLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState (
  curPParamsEpochStateL,
  nesEsL,
  utxoL,
 )
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..), txouts)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (forM)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl (use)
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.ImpTest
import Test.Cardano.Ledger.Plutus (
  PlutusArgs (..),
  ScriptTestContext (..),
  testingCostModel,
 )
import Test.Cardano.Ledger.Plutus.Examples
import Test.Cardano.Ledger.Plutus.Guardrail (guardrailScript)

class
  ( MaryEraImp era
  , AlonzoEraTest era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , TxAuxData era ~ AlonzoTxAuxData era
  , ToExpr (ContextError era)
  , ToExpr (PlutusPurpose AsItem era)
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  ) =>
  AlonzoEraImp era
  where
  scriptTestContexts :: Map ScriptHash ScriptTestContext

makeCollateralInput :: ShelleyEraImp era => ImpTestM era TxIn
makeCollateralInput = do
  -- TODO: make more accurate
  let collateral = Coin 30_000_000
  addr <- freshKeyAddr_
  withFixup fixupTx $ sendCoinTo addr collateral

addCollateralInput ::
  AlonzoEraImp era =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
addCollateralInput tx
  | not (null (tx ^. bodyTxL . collateralInputsTxBodyL)) = pure tx
  | otherwise = do
      ctx <- impGetPlutusContexts tx
      if null ctx
        then pure tx
        else do
          impAnn "addCollateralInput" $ do
            collateralInput <- makeCollateralInput
            pure $ tx & bodyTxL . collateralInputsTxBodyL <>~ Set.singleton collateralInput

impLookupPlutusScript ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash ->
  Maybe (PlutusScript era)
impLookupPlutusScript sh = do
  ScriptTestContext plutus _ <- impLookupScriptContext @era sh
  mkPlutusScript plutus

impGetPlutusContexts ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era [(PlutusPurpose AsIxItem era, ScriptHash, ScriptTestContext)]
impGetPlutusContexts tx = do
  let txBody = tx ^. bodyTxL
  utxo <- getsNES utxoL
  let AlonzoScriptsNeeded asn = getScriptsNeeded utxo txBody
  mbyContexts <- forM asn $ \(prp, sh) -> do
    pure $ (prp,sh,) <$> impLookupScriptContext @era sh
  pure $ catMaybes mbyContexts

fixupRedeemerIndices ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
fixupRedeemerIndices tx = impAnn "fixupRedeemerIndices" $ do
  (rootTxIn, _) <- getImpRootTxOut
  let
    txInputs = tx ^. bodyTxL . inputsTxBodyL
    rootTxIndex = toEnum $ Set.findIndex rootTxIn txInputs
    updateIndex (SpendingPurpose (AsIx i))
      | i >= rootTxIndex = SpendingPurpose . AsIx $ succ i
    updateIndex x = x
  pure $ tx & witsTxL . rdmrsTxWitsL . unRedeemersL %~ Map.mapKeys updateIndex

fixupRedeemers ::
  forall era.
  (AlonzoEraImp era, HasCallStack) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
fixupRedeemers tx = impAnn "fixupRedeemers" $ do
  contexts <- impGetPlutusContexts tx
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let oldRedeemers = tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL
  txWithMaxExUnits <- txWithMaxRedeemers tx
  let newMaxRedeemers = txWithMaxExUnits ^. witsTxL . rdmrsTxWitsL . unRedeemersL
  utxo <- getUTxO
  Globals {systemStart, epochInfo} <- use impGlobalsL
  let reports = evalTxExUnits pp txWithMaxExUnits utxo epochInfo systemStart
  exUnitsPerPurpose <-
    fmap (Map.mapMaybe id) $ forM reports $ \case
      Left err -> do
        logString $ "Execution Units estimation error: " <> show err
        pure Nothing
      Right exUnits ->
        pure $ Just exUnits
  let
    mkNewRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      let ptr = hoistPlutusPurpose @era toAsIx prpIdx
       in case Map.lookup ptr oldRedeemers of
            Just redeemer -> pure $ Just (ptr, redeemer)
            Nothing ->
              case Map.lookup ptr exUnitsPerPurpose of
                Nothing -> do
                  logString $ "Missing Redeemer Ptr from execution estimation: " <> show ptr
                  pure Nothing
                Just exUnits ->
                  pure $ Just (ptr, (Data dat, exUnits))
  newRedeemers <- Map.fromList . catMaybes <$> mapM mkNewRedeemers contexts
  pure $
    tx
      & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.unions [oldRedeemers, newRedeemers, newMaxRedeemers]

txWithMaxRedeemers ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
txWithMaxRedeemers tx = do
  contexts <- impGetPlutusContexts tx
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let
    maxExUnit = pp ^. ppMaxTxExUnitsL
    mkNewMaxRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose @era toAsIx prpIdx, (Data dat, maxExUnit))
    newMaxRedeemers = Map.fromList (mkNewMaxRedeemers <$> contexts)
  pure $ tx & witsTxL . rdmrsTxWitsL . unRedeemersL .~ newMaxRedeemers

fixupScriptWits ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
fixupScriptWits tx = impAnn "fixupScriptWits" $ do
  contexts <- impGetPlutusContexts tx
  utxo <- getUTxO
  let ScriptsProvided provided = getScriptsProvided utxo tx
  let contextsToAdd = filter (\(_, sh, _) -> not (Map.member sh provided)) contexts
  scriptWits <- forM contextsToAdd $ \(_, sh, ScriptTestContext plutus _) ->
    (sh,) . fromPlutusScript <$> mkPlutusScript plutus
  pure $
    tx
      & witsTxL . scriptTxWitsL <>~ Map.fromList scriptWits

fixupDatums ::
  forall era l.
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx l era ->
  ImpTestM era (Tx l era)
fixupDatums tx = impAnn "fixupDatums" $ do
  contexts <- impGetPlutusContexts tx
  let purposes = (^. _1) <$> contexts
  datums <- traverse collectDatums purposes
  pure $
    tx
      & witsTxL . datsTxWitsL . unTxDatsL
        <>~ fromElems hashData (catMaybes datums)
  where
    collectDatums :: PlutusPurpose AsIxItem era -> ImpTestM era (Maybe (Data era))
    collectDatums purpose = do
      let txIn = unAsItem <$> toSpendingPurpose (hoistPlutusPurpose toAsItem purpose)
      mbyTxOut <- traverse (impGetUTxO @era) txIn
      case mbyTxOut of
        Just txOut -> getData txOut
        Nothing -> pure Nothing

    getData :: TxOut era -> ImpTestM era (Maybe (Data era))
    getData txOut =
      let sh = txOutScriptHash txOut
       in case txOut ^. datumTxOutF of
            DatumHash dh -> case Map.lookup sh (scriptTestContexts @era) of
              Just x | hashData @era (spendDatum x) == dh -> pure . Just $ spendDatum x
              _ -> do
                logText $
                  "Script not found in `scriptTestContexts`:\n"
                    <> T.pack (show sh)
                    <> "\n\nThe transaction will likely fail. To fix this, add the script to `scriptTestContexts`."
                pure Nothing
            _ -> pure Nothing

    txOutScriptHash txOut
      | Addr _ (ScriptHashObj sh) _ <- txOut ^. addrTxOutL = sh
      | otherwise = error "TxOut does not have a payment script"

    spendDatum (ScriptTestContext _ (PlutusArgs _ (Just d))) = Data d
    spendDatum _ = error "Context does not have a spending datum"

fixupPPHash ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
fixupPPHash tx = impAnn "fixupPPHash" $ do
  integrityHash <- computeScriptIntegrityHash tx
  pure $
    tx
      & bodyTxL . scriptIntegrityHashTxBodyL .~ integrityHash

fixupOutputDatums ::
  forall era l.
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (Tx l era)
fixupOutputDatums tx = impAnn "fixupOutputDatums" $ do
  let
    addDatum txOut =
      case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj sh) _
          | Just (ScriptTestContext _ (PlutusArgs _ (Just spendDatum))) <- impLookupScriptContext @era sh
          , NoDatum <- txOut ^. datumTxOutF ->
              txOut & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)
        _ -> txOut
  pure $ tx & bodyTxL . outputsTxBodyL %~ fmap addDatum

alonzoFixupTx ::
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
alonzoFixupTx =
  addNativeScriptTxWits
    >=> fixupAuxDataHash
    >=> addCollateralInput
    >=> addRootTxIn
    -- We need to update the indices after adding the rootTxIn because the
    -- indices of inputs might get bumped if the rootTxIn appears before them
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
    >=> fixupRedeemerIndices
    >=> fixupTxOuts
    >=> alonzoFixupFees
    >=> fixupRedeemers
    >=> fixupPPHash
    >=> updateAddrTxWits

alonzoFixupFees ::
  forall era. (HasCallStack, AlonzoEraImp era) => Tx TopTx era -> ImpTestM era (Tx TopTx era)
alonzoFixupFees tx = do
  let originalRedeemers = tx ^. witsTxL . rdmrsTxWitsL
  txWithMax <- txWithMaxRedeemers tx
  -- we are maximizing the fees relative to the the redeemers, in order to break the circular dependency
  -- of the fee being impacted by the redeemers and viceversa
  txWithFees <- fixupFees txWithMax
  pure $ txWithFees & witsTxL . rdmrsTxWitsL .~ originalRedeemers

mkScriptTestEntry ::
  PlutusLanguage l =>
  Plutus l ->
  PlutusArgs ->
  (ScriptHash, ScriptTestContext)
mkScriptTestEntry script args =
  ( hashPlutusScript script
  , ScriptTestContext
      { stcScript = script
      , stcArgs = args
      }
  )

plutusTestScripts ::
  forall l.
  PlutusLanguage l =>
  SLanguage l ->
  Map.Map ScriptHash ScriptTestContext
plutusTestScripts lang =
  Map.fromList $
    [ mkScriptTestEntry (malformedPlutus @l) $ PlutusArgs (P.I 0) (Just $ P.I 7)
    , mkScriptTestEntry (alwaysSucceedsNoDatum lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysSucceedsWithDatum lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (alwaysFailsNoDatum lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysFailsWithDatum lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (redeemerSameAsDatum lang) $ PlutusArgs (P.I 3) (Just $ P.I 3)
    , mkScriptTestEntry (evenDatum lang) $ PlutusArgs (P.I 3) (Just $ P.I 26)
    , mkScriptTestEntry (evenRedeemerNoDatum lang) $ PlutusArgs (P.I 2) Nothing
    , mkScriptTestEntry (evenRedeemerWithDatum lang) $ PlutusArgs (P.I 22) (Just $ P.I 5)
    , mkScriptTestEntry (purposeIsWellformedNoDatum lang) $ PlutusArgs (P.I 2) Nothing
    , mkScriptTestEntry (purposeIsWellformedWithDatum lang) $ PlutusArgs (P.I 22) (Just $ P.I 5)
    , mkScriptTestEntry (datumIsWellformed lang) $ PlutusArgs (P.I 221) (Just $ P.I 5)
    , mkScriptTestEntry (inputsOutputsAreNotEmptyNoDatum lang) $ PlutusArgs (P.I 122) Nothing
    , mkScriptTestEntry (inputsOutputsAreNotEmptyWithDatum lang) $ PlutusArgs (P.I 222) (Just $ P.I 5)
    , mkScriptTestEntry guardrailScript $ PlutusArgs (P.I 0) Nothing
    ]
      ++ [ mkScriptTestEntry (inputsOverlapsWithRefInputs lang) $ PlutusArgs (P.I 0) Nothing
         | plutusLanguage lang >= PlutusV2
         ]

malformedPlutus :: Plutus l
malformedPlutus = Plutus (PlutusBinary "invalid")

instance ShelleyEraImp AlonzoEra where
  initGenesis =
    pure
      AlonzoGenesis
        { agCoinsPerUTxOWord = CoinPerWord (Coin 34_482)
        , agPlutusV1CostModel = testingCostModel PlutusV1
        , agPrices =
            Prices
              { prMem = 577 %! 10_000
              , prSteps = 721 %! 10_000_000
              }
        , agMaxTxExUnits =
            ExUnits
              { exUnitsMem = 10_000_000
              , exUnitsSteps = 10_000_000_000
              }
        , agMaxBlockExUnits =
            ExUnits
              { exUnitsMem = 200_000_000
              , exUnitsSteps = 200_000_000_000
              }
        , agMaxValSize = 5000
        , agCollateralPercentage = 150
        , agMaxCollateralInputs = 3
        , agExtraConfig = Nothing
        }

  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx
  expectTxSuccess = impAlonzoExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer
  genRegTxCert = shelleyGenRegTxCert
  genUnRegTxCert = shelleyGenUnRegTxCert
  delegStakeTxCert = shelleyDelegStakeTxCert

instance MaryEraImp AlonzoEra

instance AlonzoEraImp AlonzoEra where
  scriptTestContexts = plutusTestScripts SPlutusV1

impLookupScriptContext ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash ->
  Maybe ScriptTestContext
impLookupScriptContext sh = Map.lookup sh $ scriptTestContexts @era

impGetScriptContext ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash ->
  ImpTestM era ScriptTestContext
impGetScriptContext sh =
  impAnn ("Getting script context for " <> show sh)
    . expectJust
    $ impLookupScriptContext @era sh

impPlutusWithContexts ::
  (HasCallStack, AlonzoEraImp era) => Tx TopTx era -> ImpTestM era [PlutusWithContext]
impPlutusWithContexts tx = do
  globals <- use impGlobalsL
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  case collectPlutusScriptsWithContext (epochInfo globals) (systemStart globals) pp tx utxo of
    Left errs ->
      assertFailure $
        "Did not expect to get context translation failures: " ++ unlines (map show $ NonEmpty.toList errs)
    Right pwcs -> pure pwcs

impScriptPredicateFailure ::
  (HasCallStack, AlonzoEraImp era) => Tx TopTx era -> ImpTestM era (AlonzoUtxosPredFailure era)
impScriptPredicateFailure tx = do
  plutusWithContexts <- impPlutusWithContexts tx
  when (null plutusWithContexts) $
    assertFailure "Could not find any plutus scripts in the transaction"
  case evalPlutusScriptsWithLogs plutusWithContexts of
    (logs, Passes _) ->
      assertFailure $
        "Plutus script: \n"
          ++ unlines (map show plutusWithContexts)
          ++ "passed unexpectedly: \n"
          ++ unlines (map show logs)
    (_, Fails _ failures) ->
      pure $
        ValidationTagMismatch
          (IsValid True)
          (FailedUnexpectedly (scriptFailureToFailureDescription <$> failures))

submitPhase2Invalid_ ::
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era ()
submitPhase2Invalid_ = void . submitPhase2Invalid

submitPhase2Invalid ::
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx TopTx era ->
  ImpTestM era (Tx TopTx era)
submitPhase2Invalid tx = do
  fixedUpTx <-
    impAnn "Check that tx fails with IsValid True" $ do
      tx ^. isValidTxL `shouldBe` IsValid True
      (predFailure, fixedUpTx) <- expectLeft =<< trySubmitTx tx
      scriptPredicateFailure <- impScriptPredicateFailure fixedUpTx
      predFailure `shouldBeExpr` pure (injectFailure scriptPredicateFailure)
      pure fixedUpTx
  impAnn "Submit tx with IsValid False" $ do
    withNoFixup $ submitTx $ fixedUpTx & isValidTxL .~ IsValid False

impAlonzoExpectTxSuccess ::
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx TopTx era -> ImpTestM era ()
impAlonzoExpectTxSuccess tx = do
  utxo <- getsNES utxoL
  let inputs = tx ^. bodyTxL . inputsTxBodyL
      collaterals = tx ^. bodyTxL . collateralInputsTxBodyL
      outputs = Map.toList . unUTxO . txouts $ tx ^. bodyTxL
  if tx ^. isValidTxL == IsValid True
    then do
      impAnn "Inputs should be gone from UTxO" $
        expectUTxOContent utxo [(txIn, isNothing) | txIn <- Set.toList inputs]
      impAnn "Collateral inputs should still be in UTxO" $
        expectUTxOContent utxo [(txIn, isJust) | txIn <- Set.toList $ collaterals \\ inputs]
      impAnn "Outputs should be in UTxO" $
        expectUTxOContent utxo [(txIn, (== Just txOut)) | (txIn, txOut) <- outputs]
    else do
      impAnn "Non-collateral inputs should still be in UTxO" $
        expectUTxOContent utxo [(txIn, isJust) | txIn <- Set.toList $ inputs \\ collaterals]
      impAnn "Collateral inputs should not be in UTxO" $
        expectUTxOContent utxo [(txIn, isNothing) | txIn <- Set.toList collaterals]
      impAnn "Outputs should not be in UTxO" $
        expectUTxOContent utxo [(txIn, isNothing) | (txIn, _txOut) <- outputs]

computeScriptIntegrity ::
  AlonzoEraImp era =>
  PParams era ->
  UTxO era ->
  Tx l era ->
  StrictMaybe (ScriptIntegrity era)
computeScriptIntegrity pp utxo tx = mkScriptIntegrity pp tx scriptsProvided scriptsNeeded
  where
    scriptsProvided = getScriptsProvided utxo tx
    scriptsNeeded = getScriptsHashesNeeded . getScriptsNeeded utxo $ tx ^. bodyTxL

impComputeScriptIntegrity ::
  AlonzoEraImp era =>
  Tx l era ->
  ImpTestM era (StrictMaybe (ScriptIntegrity era))
impComputeScriptIntegrity tx =
  computeScriptIntegrity <$> getsPParams id <*> getUTxO <*> pure tx

computeScriptIntegrityHash ::
  AlonzoEraImp era => Tx l era -> ImpTestM era (StrictMaybe ScriptIntegrityHash)
computeScriptIntegrityHash tx = fmap hashScriptIntegrity <$> impComputeScriptIntegrity tx
