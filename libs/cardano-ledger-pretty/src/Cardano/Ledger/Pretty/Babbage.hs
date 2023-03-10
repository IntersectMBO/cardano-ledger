{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Pretty.Babbage where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts.Data (BinaryData, binaryDataToData)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxOut (..),
  Datum (..),
  adHash',
  certs',
  collateralInputs',
  collateralReturn',
  mint',
  outputs',
  referenceInputs',
  reqSignerHashes',
  scriptIntegrityHash',
  spendInputs',
  totalCollateral',
  txfee',
  txnetworkid',
  update',
  vldt',
  withdrawals',
 )
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Pretty hiding (
  ppTxBody,
  ppTxOut,
 )
import Cardano.Ledger.Pretty.Alonzo (
  ppAuxDataHash,
  ppCostModels,
  ppData,
  ppExUnits,
  ppPrices,
 )
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppValidityInterval)
import Control.State.Transition.Extended
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Lens.Micro
import Prettyprinter ((<+>))

-- =====================================

instance Crypto c => PrettyA (PParams (BabbageEra c)) where
  prettyA = ppBabbagePParams

ppBabbagePParams :: BabbageEraPParams era => PParams era -> PDoc
ppBabbagePParams pp =
  ppRecord
    "PParams"
    [ ("minfeeA", ppCoin $ pp ^. ppMinFeeAL)
    , ("minfeeB", ppCoin $ pp ^. ppMinFeeBL)
    , ("maxBBSize", ppNatural $ pp ^. ppMaxBBSizeL)
    , ("maxTxSize", ppNatural $ pp ^. ppMaxTxSizeL)
    , ("maxBHSize", ppNatural $ pp ^. ppMaxBHSizeL)
    , ("keyDeposit", ppCoin $ pp ^. ppKeyDepositL)
    , ("poolDeposit", ppCoin $ pp ^. ppPoolDepositL)
    , ("eMax", ppEpochNo $ pp ^. ppEMaxL)
    , ("nOpt", ppNatural $ pp ^. ppNOptL)
    , ("a0", (ppRational . unboundRational) $ pp ^. ppA0L)
    , ("rho", ppUnitInterval $ pp ^. ppRhoL)
    , ("tau", ppUnitInterval $ pp ^. ppTauL)
    , ("protocolVersion", ppProtVer $ pp ^. ppProtocolVersionL)
    , ("minPoolCost", ppCoin $ pp ^. ppMinPoolCostL)
    , ("coinPerByte", (ppCoin . unCoinPerByte) $ pp ^. ppCoinsPerUTxOByteL)
    , ("costmdls", ppCostModels $ pp ^. ppCostModelsL)
    , ("prices", ppPrices $ pp ^. ppPricesL)
    , ("maxTxExUnits", ppExUnits $ pp ^. ppMaxTxExUnitsL)
    , ("maxBlockExUnits", ppExUnits $ pp ^. ppMaxBlockExUnitsL)
    , ("maxValSize", ppNatural $ pp ^. ppMaxValSizeL)
    , ("collateral%", ppNatural $ pp ^. ppCollateralPercentageL)
    , ("maxCollateralInputs", ppNatural $ pp ^. ppMaxCollateralInputsL)
    ]

ppBabbagePParamsUpdate :: BabbageEraPParams era => PParamsUpdate era -> PDoc
ppBabbagePParamsUpdate pp =
  ppRecord
    "PParamsUdate"
    [ ("minfeeA", ppStrictMaybe ppCoin $ pp ^. ppuMinFeeAL)
    , ("minfeeB", ppStrictMaybe ppCoin $ pp ^. ppuMinFeeBL)
    , ("maxBBSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxBBSizeL)
    , ("maxTxSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxTxSizeL)
    , ("maxBHSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxBHSizeL)
    , ("keyDeposit", ppStrictMaybe ppCoin $ pp ^. ppuKeyDepositL)
    , ("poolDeposit", ppStrictMaybe ppCoin $ pp ^. ppuPoolDepositL)
    , ("eMax", ppStrictMaybe ppEpochNo $ pp ^. ppuEMaxL)
    , ("nOpt", ppStrictMaybe ppNatural $ pp ^. ppuNOptL)
    , ("a0", ppStrictMaybe (ppRational . unboundRational) $ pp ^. ppuA0L)
    , ("rho", ppStrictMaybe ppUnitInterval $ pp ^. ppuRhoL)
    , ("tau", ppStrictMaybe ppUnitInterval $ pp ^. ppuTauL)
    , ("protocolVersion", ppStrictMaybe ppProtVer $ pp ^. ppuProtocolVersionL)
    , ("minPoolCost", ppStrictMaybe ppCoin $ pp ^. ppuMinPoolCostL)
    , ("coinPerByte", ppStrictMaybe (ppCoin . unCoinPerByte) $ pp ^. ppuCoinsPerUTxOByteL)
    , ("costmdls", ppStrictMaybe ppCostModels $ pp ^. ppuCostModelsL)
    , ("prices", ppStrictMaybe ppPrices $ pp ^. ppuPricesL)
    , ("maxTxExUnits", ppStrictMaybe ppExUnits $ pp ^. ppuMaxTxExUnitsL)
    , ("maxBlockExUnits", ppStrictMaybe ppExUnits $ pp ^. ppuMaxBlockExUnitsL)
    , ("maxValSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxValSizeL)
    , ("collateral%", ppStrictMaybe ppNatural $ pp ^. ppuCollateralPercentageL)
    , ("maxCollateralInputs", ppStrictMaybe ppNatural $ pp ^. ppuMaxCollateralInputsL)
    ]

instance Crypto c => PrettyA (PParamsUpdate (BabbageEra c)) where
  prettyA = ppBabbagePParamsUpdate

ppBabbageUtxoPred ::
  ( PrettyA (AlonzoUtxoPredFailure era)
  , PrettyA (TxOut era)
  ) =>
  BabbageUtxoPredFailure era ->
  PDoc
ppBabbageUtxoPred (AlonzoInBabbageUtxoPredFailure x) = prettyA x
ppBabbageUtxoPred (IncorrectTotalCollateralField c1 c2) =
  ppRecord
    "IncorrectTotalCollateralField"
    [("collateral provided", ppCoin c1), ("collateral declared", ppCoin c2)]
ppBabbageUtxoPred (BabbageOutputTooSmallUTxO xs) =
  ppSexp "BabbageOutputTooSmallUTxO" [ppList (ppPair prettyA ppCoin) xs]

instance
  ( PrettyA (AlonzoUtxoPredFailure era)
  , PrettyA (TxOut era)
  ) =>
  PrettyA (BabbageUtxoPredFailure era)
  where
  prettyA = ppBabbageUtxoPred

ppBabbageUtxowPred ::
  ( PrettyA (AlonzoUtxowPredFailure era)
  , PrettyA (PredicateFailure (EraRule "UTXO" era))
  ) =>
  BabbageUtxowPredFailure era ->
  PDoc
ppBabbageUtxowPred (AlonzoInBabbageUtxowPredFailure pf) = prettyA pf
ppBabbageUtxowPred (UtxoFailure pf) = prettyA pf
ppBabbageUtxowPred (MalformedScriptWitnesses scripts) =
  ppSexp "MalformedScriptWitnesses" [ppSet ppScriptHash scripts]
ppBabbageUtxowPred (MalformedReferenceScripts scripts) =
  ppSexp "MalformedReferenceScripts" [ppSet ppScriptHash scripts]

instance
  ( PrettyA (AlonzoUtxowPredFailure era)
  , PrettyA (PredicateFailure (EraRule "UTXO" era))
  ) =>
  PrettyA (BabbageUtxowPredFailure era)
  where
  prettyA = ppBabbageUtxowPred

ppTxOut ::
  forall era.
  ( EraTxOut era
  , EraScript era
  , PrettyA (Script era)
  , PrettyA (Value era)
  ) =>
  BabbageTxOut era ->
  PDoc
ppTxOut (BabbageTxOut addr val datum mscript) =
  ppRecord
    "TxOut"
    [ ("address", ppAddr addr)
    , ("value", prettyA val)
    , ("datum", ppDatum datum)
    ,
      ( "reference script"
      , case mscript of
          (SJust s) -> prettyA s <+> ppScriptHash (hashScript @era s)
          SNothing -> ppString "?-"
      )
    ]

instance
  ( EraTxOut era
  , EraScript era
  , PrettyA (Script era)
  , PrettyA (Value era)
  ) =>
  PrettyA (BabbageTxOut era)
  where
  prettyA = ppTxOut

ppDatum :: Era era => Datum era -> PDoc
ppDatum NoDatum = ppString "NoDatum"
ppDatum (DatumHash x) = ppSexp "DatumHash" [ppDataHash x]
ppDatum (Datum x) = ppSexp "Datum" [ppBinaryData x]

instance Era era => PrettyA (Datum era) where prettyA = ppDatum

ppBinaryData :: Era era => BinaryData era -> PDoc
ppBinaryData x = ppData (binaryDataToData x)

instance Era era => PrettyA (BinaryData era) where prettyA = ppBinaryData

ppDataHash :: DataHash era -> PDoc
ppDataHash x = ppSafeHash x

instance PrettyA (DataHash era) where prettyA = ppDataHash

ppTxBody ::
  ( PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , PrettyA (DCert era)
  ) =>
  BabbageTxBody era ->
  PDoc
ppTxBody x =
  ppRecord
    "TxBody(Babbage)"
    [ ("spending inputs", ppSet ppTxIn (spendInputs' x))
    , ("collateral inputs", ppSet ppTxIn (collateralInputs' x))
    , ("reference inputs", ppSet ppTxIn (referenceInputs' x))
    , ("outputs", ppStrictSeq prettyA (outputs' x))
    , ("collateral return", ppStrictMaybe prettyA (collateralReturn' x))
    , ("total collateral", ppStrictMaybe ppCoin (totalCollateral' x))
    , ("certificates", ppStrictSeq prettyA (certs' x))
    , ("withdrawals", ppWithdrawals (withdrawals' x))
    , ("txfee", ppCoin (txfee' x))
    , ("vldt", ppValidityInterval (vldt' x))
    , ("update", ppStrictMaybe ppUpdate (update' x))
    , ("reqSignerHashes", ppSet ppKeyHash (reqSignerHashes' x))
    , ("mint", ppMultiAsset (mint' x))
    , ("scriptIntegrityHash", ppStrictMaybe ppSafeHash (scriptIntegrityHash' x))
    , ("adHash", ppStrictMaybe ppAuxDataHash (adHash' x))
    , ("txnetworkid", ppStrictMaybe ppNetwork (txnetworkid' x))
    ]

instance
  ( EraTxOut era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , PrettyA (DCert era)
  ) =>
  PrettyA (BabbageTxBody era)
  where
  prettyA = ppTxBody
