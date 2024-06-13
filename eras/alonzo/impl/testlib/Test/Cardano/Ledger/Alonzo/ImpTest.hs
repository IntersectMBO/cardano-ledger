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
  initAlonzoImpNES,
  impLookupPlutusScriptMaybe,
  malformedPlutus,
  addCollateralInput,
  impGetPlutusContexts,
  alonzoFixupTx,
  plutusTestScripts,
  impGetScriptContext,
  impGetScriptContextMaybe,
  impPlutusWithContexts,
  impScriptPredicateFailure,
  expectPhase2Invalid,
  -- Fixup
  fixupDatums,
  fixupOutputDatums,
  fixupPPHash,
  fixupRedeemers,
  fixupScriptWits,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams (getLanguageView)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalTxExUnits)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  collectPlutusScriptsWithContext,
  evalPlutusScriptsWithLogs,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
  scriptFailureToFailureDescription,
 )
import Cardano.Ledger.Alonzo.Scripts (
  ExUnits (..),
  plutusScriptLanguage,
  toAsItem,
  toAsIx,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..), hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (Globals (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), hashData)
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..), ScriptResult (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusBinary (..),
  PlutusLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), ScriptsProvided (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl (use, (%=))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.ImpTest
import Test.Cardano.Ledger.Plutus (
  PlutusArgs (..),
  ScriptTestContext (..),
  testingCostModels,
 )
import Test.Cardano.Ledger.Plutus.Examples

class
  ( MaryEraImp era
  , AlonzoEraScript era
  , AlonzoEraTxWits era
  , AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  AlonzoEraImp era
  where
  scriptTestContexts :: Map (ScriptHash (EraCrypto era)) ScriptTestContext

initAlonzoImpNES ::
  forall era.
  ( AlonzoEraPParams era
  , ShelleyEraImp era
  , AlonzoEraScript era
  ) =>
  NewEpochState era ->
  NewEpochState era
initAlonzoImpNES = nesEsL . curPParamsEpochStateL %~ initPParams
  where
    initPParams pp =
      pp
        & ppMaxValSizeL .~ 1_000_000_000
        & ppMaxTxExUnitsL .~ ExUnits 10_000_000 10_000_000
        & ppCostModelsL
          .~ testingCostModels
            [PlutusV1 .. eraMaxLanguage @era]

makeCollateralInput :: ShelleyEraImp era => ImpTestM era (TxIn (EraCrypto era))
makeCollateralInput = do
  -- TODO: make more accurate
  let collateral = Coin 10_000_000
  (_, addr) <- freshKeyAddr
  withFixup fixupTx $ sendCoinTo addr collateral

addCollateralInput ::
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
addCollateralInput tx = impAnn "addCollateralInput" $ do
  ctx <- impGetPlutusContexts tx
  if null ctx
    then pure tx
    else do
      collateralInput <- makeCollateralInput
      pure $
        tx
          & bodyTxL . collateralInputsTxBodyL <>~ Set.singleton collateralInput

impLookupPlutusScriptMaybe ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash (EraCrypto era) ->
  Maybe (PlutusScript era)
impLookupPlutusScriptMaybe sh =
  (\(ScriptTestContext plutus _) -> mkPlutusScript plutus) =<< impGetScriptContextMaybe @era sh

impGetPlutusContexts ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era [(PlutusPurpose AsIxItem era, ScriptHash (EraCrypto era), ScriptTestContext)]
impGetPlutusContexts tx = do
  let txBody = tx ^. bodyTxL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let AlonzoScriptsNeeded asn = getScriptsNeeded utxo txBody
  mbyContexts <- forM asn $ \(prp, sh) -> do
    pure $ (prp,sh,) <$> impGetScriptContextMaybe @era sh
  pure $ catMaybes mbyContexts

fixupRedeemerIndices ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupRedeemerIndices tx = impAnn "fixupRedeemerIndices" $ do
  (rootTxIn, _) <- lookupImpRootTxOut
  let
    txInputs = tx ^. bodyTxL . inputsTxBodyL
    rootTxIndex = toEnum $ Set.findIndex rootTxIn txInputs
    updateIndex (SpendingPurpose (AsIx i))
      | i >= rootTxIndex = SpendingPurpose . AsIx $ succ i
    updateIndex x = x
  pure $
    tx
      & witsTxL . rdmrsTxWitsL
        %~ (\(Redeemers m) -> Redeemers $ Map.mapKeys updateIndex m)

fixupRedeemers ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupRedeemers tx = impAnn "fixupRedeemers" $ do
  contexts <- impGetPlutusContexts tx
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let
    maxExUnit = pp ^. ppMaxTxExUnitsL
    mkNewMaxRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose @era toAsIx prpIdx, (Data dat, maxExUnit))
    Redeemers oldRedeemers = tx ^. witsTxL . rdmrsTxWitsL
    newMaxRedeemers = Map.fromList (mkNewMaxRedeemers <$> contexts)
    txWithMaxExUnits =
      tx & witsTxL . rdmrsTxWitsL .~ Redeemers newMaxRedeemers
  utxo <- getUTxO
  Globals {systemStart, epochInfo} <- use impGlobalsL
  let reports = evalTxExUnits pp txWithMaxExUnits utxo epochInfo systemStart
  exUnitsPerPurpose <-
    fmap (Map.mapMaybe id) $ forM reports $ \case
      Left err -> do
        logEntry $ "Execution Units estimation error: " ++ show err
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
                  logEntry $ "Missing Redeemer Ptr from execution estimation: " ++ show ptr
                  pure Nothing
                Just exUnits ->
                  pure $ Just (ptr, (Data dat, exUnits))
  newRedeemers <- Map.fromList . catMaybes <$> mapM mkNewRedeemers contexts
  pure $
    tx
      & witsTxL . rdmrsTxWitsL .~ Redeemers (Map.unions [oldRedeemers, newRedeemers, newMaxRedeemers])

fixupScriptWits ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupScriptWits tx = impAnn "fixupScriptWits" $ do
  contexts <- impGetPlutusContexts tx
  utxo <- getUTxO
  let ScriptsProvided provided = getScriptsProvided utxo tx
  let contextsToAdd = filter (\(_, sh, _) -> not (Map.member sh provided)) contexts
  let
    plutusToScript ::
      forall l.
      PlutusLanguage l =>
      Plutus l ->
      ImpTestM era (Script era)
    plutusToScript p =
      case mkPlutusScript @era p of
        Just x -> pure $ fromPlutusScript x
        Nothing -> error "Plutus version not supported by era"
  scriptWits <- forM contextsToAdd $ \(_, sh, ScriptTestContext plutus _) ->
    (sh,) <$> plutusToScript plutus
  pure $
    tx
      & witsTxL . scriptTxWitsL <>~ Map.fromList scriptWits

fixupDatums ::
  forall era.
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupDatums tx = impAnn "fixupDatums" $ do
  contexts <- impGetPlutusContexts tx
  let purposes = (^. _1) <$> contexts
  datums <- traverse collectDatums purposes
  let TxDats prevDats = tx ^. witsTxL . datsTxWitsL
  pure $
    tx
      & witsTxL . datsTxWitsL
        .~ TxDats
          (Map.union prevDats $ fromElems hashData (catMaybes datums))
  where
    collectDatums :: PlutusPurpose AsIxItem era -> ImpTestM era (Maybe (Data era))
    collectDatums purpose = do
      let txIn = unAsItem <$> toSpendingPurpose (hoistPlutusPurpose toAsItem purpose)
      txOut <- traverse (impLookupUTxO @era) txIn
      pure $ getData =<< txOut

    getData :: TxOut era -> Maybe (Data era)
    getData txOut = case txOut ^. datumTxOutF of
      DatumHash _dh -> spendDatum <$> Map.lookup (txOutScriptHash txOut) (scriptTestContexts @era)
      _ -> Nothing

    txOutScriptHash txOut
      | Addr _ (ScriptHashObj sh) _ <- txOut ^. addrTxOutL = sh
      | otherwise = error "TxOut does not have a payment script"

    spendDatum (ScriptTestContext _ (PlutusArgs _ (Just d))) = Data d
    spendDatum _ = error "Context does not have a spending datum"

fixupPPHash ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupPPHash tx = impAnn "fixupPPHash" $ do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  let
    scriptHashes :: Set (ScriptHash (EraCrypto era))
    scriptHashes = getScriptsHashesNeeded . getScriptsNeeded utxo $ tx ^. bodyTxL
    plutusLanguage sh = do
      let mbyPlutus = impLookupPlutusScriptMaybe sh
      pure $ getLanguageView pp . plutusScriptLanguage @era <$> mbyPlutus
  langs <- traverse plutusLanguage $ Set.toList scriptHashes
  let
    integrityHash =
      hashScriptIntegrity
        (Set.fromList $ catMaybes langs)
        (tx ^. witsTxL . rdmrsTxWitsL)
        (tx ^. witsTxL . datsTxWitsL)
  pure $
    tx
      & bodyTxL . scriptIntegrityHashTxBodyL .~ integrityHash

fixupOutputDatums ::
  forall era.
  AlonzoEraImp era =>
  Tx era ->
  ImpTestM era (Tx era)
fixupOutputDatums tx = impAnn "fixupOutputDatums" $ do
  let
    addDatum txOut =
      case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj sh) _
          | Just (ScriptTestContext _ (PlutusArgs _ (Just spendDatum))) <- impGetScriptContextMaybe @era sh
          , NoDatum <- txOut ^. datumTxOutF ->
              txOut & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)
        _ -> txOut
  pure $ tx & bodyTxL . outputsTxBodyL %~ fmap addDatum

alonzoFixupTx ::
  ( HasCallStack
  , AlonzoEraImp era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
alonzoFixupTx =
  addNativeScriptTxWits
    >=> addCollateralInput
    >=> addRootTxIn
    -- We need to update the indices after adding the rootTxIn because the
    -- indices of inputs might get bumped if the rootTxIn appears before them
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
    >=> fixupRedeemerIndices
    >=> fixupRedeemers
    >=> fixupPPHash
    >=> fixupFees
    >=> updateAddrTxWits

mkScriptTestEntry ::
  (PlutusLanguage l, Crypto c) =>
  Plutus l ->
  PlutusArgs ->
  (ScriptHash c, ScriptTestContext)
mkScriptTestEntry script args =
  ( hashPlutusScript script
  , ScriptTestContext
      { stcScript = script
      , stcArgs = args
      }
  )

plutusTestScripts ::
  forall c l.
  (Crypto c, PlutusLanguage l) =>
  SLanguage l ->
  Map.Map (ScriptHash c) ScriptTestContext
plutusTestScripts lang =
  Map.fromList
    [ mkScriptTestEntry (alwaysSucceedsNoDatum lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysSucceedsWithDatum lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (alwaysFailsNoDatum lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysFailsWithDatum lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (redeemerSameAsDatum lang) $ PlutusArgs (P.I 3) (Just $ P.I 3)
    , mkScriptTestEntry (evenDatum lang) $ PlutusArgs (P.I 3) (Just $ P.I 26)
    , mkScriptTestEntry (evenRedeemerNoDatum lang) $ PlutusArgs (P.I 2) Nothing
    , mkScriptTestEntry (evenRedeemerWithDatum lang) $ PlutusArgs (P.I 22) (Just $ P.I 5)
    , mkScriptTestEntry (malformedPlutus @l) $ PlutusArgs (P.I 0) (Just $ P.I 7)
    ]

malformedPlutus :: Plutus l
malformedPlutus = Plutus (PlutusBinary "invalid")

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AlonzoEra c)
  where
  initImpTestState = impNESL %= initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  MaryEraImp (AlonzoEra c)

instance MaryEraImp (AlonzoEra c) => AlonzoEraImp (AlonzoEra c) where
  scriptTestContexts = plutusTestScripts SPlutusV1

impGetScriptContextMaybe ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash (EraCrypto era) ->
  Maybe ScriptTestContext
impGetScriptContextMaybe sh = Map.lookup sh $ scriptTestContexts @era

impGetScriptContext ::
  forall era.
  AlonzoEraImp era =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era ScriptTestContext
impGetScriptContext sh =
  impAnn ("Getting script context for " <> show sh)
    . expectJust
    $ impGetScriptContextMaybe @era sh

impPlutusWithContexts ::
  (HasCallStack, AlonzoEraImp era) => Tx era -> ImpTestM era [PlutusWithContext (EraCrypto era)]
impPlutusWithContexts tx = do
  globals <- use impGlobalsL
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  case collectPlutusScriptsWithContext (epochInfo globals) (systemStart globals) pp tx utxo of
    Left errs ->
      assertFailure $ "Did not expect to get context translation failures: " ++ unlines (map show errs)
    Right pwcs -> pure pwcs

impScriptPredicateFailure ::
  (HasCallStack, AlonzoEraImp era) => Tx era -> ImpTestM era (AlonzoUtxosPredFailure era)
impScriptPredicateFailure tx = do
  plutusWithContexts <- impPlutusWithContexts tx
  when (null plutusWithContexts) $
    assertFailure "Could not find any plutus scripts in the transaction"
  case evalPlutusScriptsWithLogs tx plutusWithContexts of
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

expectPhase2Invalid ::
  ( HasCallStack
  , AlonzoEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  Tx era ->
  ImpTestM era ()
expectPhase2Invalid tx = do
  (predFailure, fixedUpTx) <- expectLeft =<< trySubmitTx tx
  scriptPredicateFailure <- impScriptPredicateFailure fixedUpTx
  predFailure `shouldBeExpr` pure (injectFailure scriptPredicateFailure)
  withNoFixup $ submitTx_ $ fixedUpTx & isValidTxL .~ IsValid False
