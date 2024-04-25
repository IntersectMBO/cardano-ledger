{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
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
  addCollateralInput,
  impGetPlutusContexts,
  alonzoFixupTx,
  plutusTestScripts,
  impGetScriptContext,
  impGetScriptContextMaybe,
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
import Cardano.Ledger.Alonzo.Scripts (
  ExUnits (..),
  plutusScriptLanguage,
  toAsItem,
  toAsIx,
 )
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), hashData)
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus,
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
import Lens.Micro.Mtl ((%=))
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
  (AlonzoEraImp era, ScriptsNeeded era ~ AlonzoScriptsNeeded era) =>
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
  (ScriptsNeeded era ~ AlonzoScriptsNeeded era, AlonzoEraImp era) =>
  Tx era ->
  ImpTestM
    era
    [(PlutusPurpose AsIxItem era, ScriptHash (EraCrypto era), ScriptTestContext)]
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
  ( ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraImp era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupRedeemers tx = impAnn "fixupRedeemers" $ do
  contexts <- impGetPlutusContexts tx
  exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
  let
    mkNewRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose @era toAsIx prpIdx, (Data dat, exUnits))
    Redeemers oldRedeemers = tx ^. witsTxL . rdmrsTxWitsL
    newRedeemers = Map.fromList (mkNewRedeemers <$> contexts)
  pure $
    tx
      & witsTxL . rdmrsTxWitsL .~ Redeemers (Map.union oldRedeemers newRedeemers)

fixupScriptWits ::
  forall era.
  ( ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraImp era
  ) =>
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
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
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
    isDatum (Datum _) = True
    isDatum _ = False
    addDatum txOut =
      case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj sh) _ -> do
          case impGetScriptContextMaybe @era sh of
            Just (ScriptTestContext _ (PlutusArgs _ mbySpendDatum))
              | not $ isDatum (txOut ^. datumTxOutF) -> do
                  spendDatum <-
                    impAnn "Looking up spend datum" $
                      expectJust mbySpendDatum
                  pure $
                    txOut
                      & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)
            _ -> pure txOut
        _ -> pure txOut
  newOutputs <- traverse addDatum $ tx ^. bodyTxL . outputsTxBodyL
  pure $
    tx
      & bodyTxL . outputsTxBodyL .~ newOutputs

alonzoFixupTx ::
  ( ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , HasCallStack
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
    >=> fixupRedeemerIndices
    >=> fixupRedeemers
    >=> fixupScriptWits
    >=> fixupOutputDatums
    >=> fixupDatums
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
  (Crypto c, PlutusLanguage l) =>
  SLanguage l ->
  Map.Map (ScriptHash c) ScriptTestContext
plutusTestScripts lang =
  Map.fromList
    [ mkScriptTestEntry (alwaysSucceeds2 lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysSucceeds3 lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (alwaysFails2 lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (alwaysFails3 lang) $ PlutusArgs (P.I 0) (Just $ P.I 0)
    , mkScriptTestEntry (guessTheNumber2 lang) $ PlutusArgs (P.I 3) Nothing
    , mkScriptTestEntry (guessTheNumber3 lang) $ PlutusArgs (P.I 3) (Just $ P.I 3)
    , mkScriptTestEntry (evendata3 lang) $ PlutusArgs (P.I 4) (Just $ P.I 0)
    , mkScriptTestEntry (odddata3 lang) $ PlutusArgs (P.I 3) (Just $ P.I 0)
    , mkScriptTestEntry (evenRedeemer3 lang) $ PlutusArgs (P.I 0) (Just $ P.I 2)
    , mkScriptTestEntry (oddRedeemer3 lang) $ PlutusArgs (P.I 0) (Just $ P.I 3)
    , mkScriptTestEntry (sumsTo103 lang) $ PlutusArgs (P.I 4) (Just $ P.I 6)
    , mkScriptTestEntry (oddRedeemer2 lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (evenRedeemer2 lang) $ PlutusArgs (P.I 0) Nothing
    , mkScriptTestEntry (redeemerIs102 lang) $ PlutusArgs (P.I 0) Nothing
    ]

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
