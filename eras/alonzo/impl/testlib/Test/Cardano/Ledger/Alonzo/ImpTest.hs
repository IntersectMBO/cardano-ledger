{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.ImpTest (
  module ImpTest,
  initAlonzoImpNES,
  PlutusArgs (..),
  impAddPlutusScript,
  alonzoFixupTx,
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (AlonzoEraScript (..), AlonzoEraTxOut (..), Era (..), EraGov)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, getLanguageView, ppCostModelsL, ppMaxTxExUnitsL, ppMaxValSizeL)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), plutusScriptLanguage, toAsIx)
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  EraIndependentTxBody,
  EraScript (..),
  EraTx (..),
  EraTxBody (..),
  EraTxWits (..),
  ScriptHash,
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), binaryDataToData, hashData)
import Cardano.Ledger.Plutus.Language (Language (..), Plutus, PlutusLanguage, hashPlutusScript)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..), txinLookup)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (forM)
import Data.Default.Class (Default)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs (..), ScriptTestContext (..), testingCostModels)
import Test.Cardano.Ledger.Shelley.ImpTest as ImpTest

initAlonzoImpNES ::
  forall era.
  ( AlonzoEraPParams era
  , Default (StashedAVVMAddresses era)
  , ShelleyEraImp era
  , AlonzoEraScript era
  ) =>
  Coin ->
  NewEpochState era
initAlonzoImpNES rootCoin =
  initShelleyImpNES rootCoin
    & nesEsL . curPParamsEpochStateL %~ initPParams
  where
    initPParams pp =
      pp
        & ppMaxValSizeL .~ 1_000_000_000
        & ppMaxTxExUnitsL .~ ExUnits 10_000_000 10_000_000
        & ppCostModelsL
          .~ testingCostModels
            [PlutusV1 .. eraMaxLanguage @era]

makeCollateralInput :: ImpTestM era (TxIn (EraCrypto era))
makeCollateralInput = do
  x : xs <- use impCollateralTxIdsL
  impCollateralTxIdsL .= xs
  pure x

addCollateralInput ::
  (AlonzoEraTxBody era, EraTx era) =>
  Tx era ->
  ImpTestM era (Tx era)
addCollateralInput tx = do
  collInput <- makeCollateralInput
  pure $
    tx
      & bodyTxL . collateralInputsTxBodyL <>~ Set.singleton collInput

impLookupPlutusScript ::
  AlonzoEraScript era =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era (Maybe (PlutusScript era))
impLookupPlutusScript sh = do
  mbyCtx <- getScriptTestContext sh
  case mbyCtx of
    Just (ScriptTestContext plutus _) -> pure $ mkPlutusScript plutus
    Nothing -> pure Nothing

fixupPlutusScripts ::
  forall era.
  ( EraUTxO era
  , EraGov era
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupPlutusScripts tx = do
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let
    txBody = tx ^. bodyTxL
    AlonzoScriptsNeeded asn = getScriptsNeeded utxo txBody
  mbyContexts <- forM asn $ \(prp, sh) -> do
    ctx <- getScriptTestContext sh
    pure $ (prp,sh,) <$> ctx
  let
    contexts = catMaybes mbyContexts
    plutusToScript ::
      forall l.
      PlutusLanguage l =>
      Plutus l ->
      ImpTestM era (Script era)
    plutusToScript p =
      case mkPlutusScript @era p of
        Just x -> pure $ fromPlutusScript x
        Nothing -> error "Plutus version not supported by era"
  scriptWits <- forM contexts $ \(_, sh, ScriptTestContext plutus _) ->
    (sh,) <$> plutusToScript plutus
  exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
  let
    mkNewRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose @era toAsIx prpIdx, (Data dat, exUnits))
    newRedeemers = Redeemers . Map.fromList $ mkNewRedeemers <$> contexts
    mkTxDats (_, _, ScriptTestContext _ (PlutusArgs _ datum)) =
      (\d -> (hashData d, d)) . Data <$> datum
    txDats = mkTxDats <$> contexts
    mkInputDats txIn =
      case txinLookup txIn utxo of
        Just txOut ->
          case txOut ^. datumTxOutF of
            Datum bin -> do
              let dat = binaryDataToData bin
              Just (hashData dat, dat)
            _ -> Nothing
        Nothing -> error $ "TxIn not found in the UTxO: " <> show txIn
    inputDats = mkInputDats <$> toList (txBody ^. inputsTxBodyL)
  pure $
    tx
      & witsTxL . scriptTxWitsL <>~ Map.fromList scriptWits
      & witsTxL . rdmrsTxWitsL <>~ newRedeemers
      & witsTxL . datsTxWitsL
        <>~ TxDats
          (Map.fromList . catMaybes $ txDats ++ inputDats)

fixupPPHash ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , EraGov era
  , EraUTxO era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupPPHash tx = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  utxo <- getUTxO
  let
    scriptHashes :: Set (ScriptHash (EraCrypto era))
    scriptHashes = getScriptsHashesNeeded . getScriptsNeeded utxo $ tx ^. bodyTxL
    plutusLanguage sh = do
      mbyPlutus <- impLookupPlutusScript sh
      pure $ getLanguageView pp . plutusScriptLanguage <$> mbyPlutus
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

alonzoFixupTx ::
  ( ShelleyEraImp era
  , AlonzoEraTxWits era
  , AlonzoEraTxBody era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
alonzoFixupTx =
  addNativeScriptTxWits
    >=> addCollateralInput
    >=> addRootTxIn
    >=> fixupPlutusScripts
    >=> fixupPPHash
    >=> fixupFees
    >=> updateAddrTxWits

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AlonzoEra c)
  where
  initImpNES = initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx

impAddPlutusScript ::
  forall era.
  AlonzoEraScript era =>
  ScriptTestContext ->
  ImpTestM era (ScriptHash (EraCrypto era))
impAddPlutusScript stc@(ScriptTestContext plutus _) = do
  let sh = hashPlutusScript plutus
  impScriptsL %= Map.insert sh stc
  pure sh
