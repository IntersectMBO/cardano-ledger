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
  impLookupPlutusScript,
  fixupPPHash,
  fixupPlutusScripts,
  addCollateralInput,
  alonzoFixupTx,
  impGetPlutusContexts,
  fixupDatums,
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (AlonzoEraScript (..), AlonzoEraTxOut (..), AsIxItem, Era (..), EraGov)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, getLanguageView, ppCostModelsL, ppMaxTxExUnitsL, ppMaxValSizeL)
import Cardano.Ledger.Alonzo.Scripts (
  AsItem (..),
  ExUnits (..),
  plutusScriptLanguage,
  toAsItem,
  toAsIx,
 )
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  EraIndependentTxBody,
  EraScript (..),
  EraTx (..),
  EraTxOut (..),
  EraTxWits (..),
  ScriptHash,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Plutus.Data (Data (..), Datum (..), hashData)
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
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (forM)
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Imp.Common (expectJust)
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

impGetPlutusContexts ::
  (ScriptsNeeded era ~ AlonzoScriptsNeeded era, EraUTxO era) =>
  Tx era ->
  ImpTestM
    era
    [(PlutusPurpose AsIxItem era, ScriptHash (EraCrypto era), ScriptTestContext)]
impGetPlutusContexts tx = do
  let txBody = tx ^. bodyTxL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  let AlonzoScriptsNeeded asn = getScriptsNeeded utxo txBody
  mbyContexts <- forM asn $ \(prp, sh) -> do
    ctx <- getScriptTestContext sh
    pure $ (prp,sh,) <$> ctx
  pure $ catMaybes mbyContexts

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
  contexts <- impGetPlutusContexts tx
  exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
  let
    mkNewRedeemers (prpIdx, _, ScriptTestContext _ (PlutusArgs dat _)) =
      (hoistPlutusPurpose @era toAsIx prpIdx, (Data dat, exUnits))
    Redeemers oldRedeemers = tx ^. witsTxL . rdmrsTxWitsL
    newRedeemers = Map.fromList $ mkNewRedeemers <$> contexts
  pure $
    tx
      & witsTxL . rdmrsTxWitsL <>~ Redeemers (Map.union oldRedeemers newRedeemers)

fixupScriptWits ::
  forall era.
  ( ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraScript era
  , EraUTxO era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupScriptWits tx = do
  contexts <- impGetPlutusContexts tx
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
  scriptWits <- forM contexts $ \(_, sh, ScriptTestContext plutus _) ->
    (sh,) <$> plutusToScript plutus
  pure $
    tx
      & witsTxL . scriptTxWitsL <>~ Map.fromList scriptWits

fixupDatums ::
  forall era.
  ( HasCallStack
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , AlonzoEraTxOut era
  , ShelleyEraImp era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
fixupDatums tx = do
  contexts <- impGetPlutusContexts tx
  scripts <- use impScriptsL
  let
    txOutScriptHash txOut
      | Addr _ (ScriptHashObj sh) _ <- txOut ^. addrTxOutL = sh
      | otherwise = error "TxOut does not have a payment script"
    spendDatum (ScriptTestContext _ (PlutusArgs _ (Just d))) = Data d
    spendDatum _ = error "Context does not have a spending datum"
    filterInline (purpose, _, _) = do
      AsItem txIn <- expectJust . toSpendingPurpose $ hoistPlutusPurpose toAsItem purpose
      txOut <- impLookupUTxO @era txIn
      pure $ case txOut ^. datumTxOutF of
        DatumHash _dh -> spendDatum <$> Map.lookup (txOutScriptHash txOut) scripts
        _ -> Nothing
  datums <- traverse filterInline contexts
  let TxDats prevDats = tx ^. witsTxL . datsTxWitsL
  pure $
    tx
      & witsTxL . datsTxWitsL
        .~ TxDats
          (Map.union prevDats $ fromElems hashData (catMaybes datums))

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
  , AlonzoEraUTxO era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
alonzoFixupTx =
  addNativeScriptTxWits
    >=> addCollateralInput
    >=> addRootTxIn
    >=> fixupPlutusScripts
    >=> fixupScriptWits
    >=> fixupDatums
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
