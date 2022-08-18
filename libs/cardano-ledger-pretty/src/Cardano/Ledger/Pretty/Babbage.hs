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

import Cardano.Ledger.Alonzo.Data (BinaryData, binaryDataToData)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Babbage.PParams (BabbagePParams, BabbagePParamsHKD (..), BabbagePParamsUpdate)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..), BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxBody
  ( BabbageTxBody (..),
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
    wdrls',
  )
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational))
import Cardano.Ledger.Core
import Cardano.Ledger.Pretty hiding
  ( ppPParams,
    ppPParamsUpdate,
    ppTxBody,
    ppTxOut,
  )
import Cardano.Ledger.Pretty.Alonzo
  ( ppAuxDataHash,
    ppCostModels,
    ppData,
    ppExUnits,
    ppPrices,
  )
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppValidityInterval)
import Control.State.Transition.Extended
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Prettyprinter ((<+>))

-- =====================================

ppPParamsId :: BabbagePParams era -> PDoc
ppPParamsId (BabbagePParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
  ppRecord
    "PParams"
    [ ("minfeeA", lift ppNatural feeA),
      ("minfeeB", lift ppNatural feeB),
      ("maxBBSize", lift ppNatural mbb),
      ("maxTxSize", lift ppNatural mtx),
      ("maxBHSize", lift ppNatural mbh),
      ("keyDeposit", lift ppCoin kd),
      ("poolDeposit", lift ppCoin pd),
      ("eMax", lift ppEpochNo em),
      ("nOpt", lift ppNatural no),
      ("a0", lift (ppRational . unboundRational) a0),
      ("rho", lift ppUnitInterval rho),
      ("tau", lift ppUnitInterval tau),
      ("protocolVersion", lift ppProtVer pv),
      ("minPoolCost", lift ppCoin mpool),
      ("adaPerWord", lift ppCoin ada),
      ("costmdls", lift ppCostModels cost),
      ("prices", lift ppPrices prices),
      ("maxTxExUnits", lift ppExUnits mxEx),
      ("maxBlockExUnits", lift ppExUnits mxBEx),
      ("maxValSize", lift ppNatural mxV),
      ("collateral%", lift ppNatural c),
      ("maxCollateralInputs", lift ppNatural mxC)
    ]
  where
    lift f = f

ppPParams :: BabbagePParams era -> PDoc
ppPParams = ppPParamsId

instance PrettyA (BabbagePParams era) where
  prettyA = ppPParams

ppPParamsStrictMaybe :: BabbagePParamsUpdate era -> PDoc
ppPParamsStrictMaybe (BabbagePParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
  ppRecord
    "PParams"
    [ ("minfeeA", lift ppNatural feeA),
      ("minfeeB", lift ppNatural feeB),
      ("maxBBSize", lift ppNatural mbb),
      ("maxTxSize", lift ppNatural mtx),
      ("maxBHSize", lift ppNatural mbh),
      ("keyDeposit", lift ppCoin kd),
      ("poolDeposit", lift ppCoin pd),
      ("eMax", lift ppEpochNo em),
      ("nOpt", lift ppNatural no),
      ("a0", lift (ppRational . unboundRational) a0),
      ("rho", lift ppUnitInterval rho),
      ("tau", lift ppUnitInterval tau),
      ("protocolVersion", lift ppProtVer pv),
      ("minPoolCost", lift ppCoin mpool),
      ("adaPerWord", lift ppCoin ada),
      ("costmdls", lift ppCostModels cost),
      ("prices", lift ppPrices prices),
      ("maxTxExUnits", lift ppExUnits mxEx),
      ("maxBlockExUnits", lift ppExUnits mxBEx),
      ("maxValSize", lift ppNatural mxV),
      ("collateral%", lift ppNatural c),
      ("maxCollateralInputs", lift ppNatural mxC)
    ]
  where
    lift f = ppStrictMaybe f

ppPParamsUpdate :: BabbagePParamsUpdate era -> PDoc
ppPParamsUpdate = ppPParamsStrictMaybe

instance PrettyA (BabbagePParamsUpdate era) where
  prettyA = ppPParamsUpdate

ppBabbageUtxoPred ::
  ( PrettyA (AlonzoUtxoPredFailure era),
    PrettyA (TxOut era)
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
  ( PrettyA (AlonzoUtxoPredFailure era),
    PrettyA (TxOut era)
  ) =>
  PrettyA (BabbageUtxoPredFailure era)
  where
  prettyA = ppBabbageUtxoPred

ppBabbageUtxowPred ::
  ( PrettyA (AlonzoUtxowPredFailure era),
    PrettyA (PredicateFailure (EraRule "UTXO" era))
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
  ( PrettyA (AlonzoUtxowPredFailure era),
    PrettyA (PredicateFailure (EraRule "UTXO" era))
  ) =>
  PrettyA (BabbageUtxowPredFailure era)
  where
  prettyA = ppBabbageUtxowPred

ppTxOut ::
  forall era.
  ( EraTxOut era,
    EraScript era,
    PrettyA (Script era),
    PrettyA (Value era)
  ) =>
  BabbageTxOut era ->
  PDoc
ppTxOut (BabbageTxOut addr val datum mscript) =
  ppRecord
    "TxOut"
    [ ("address", ppAddr addr),
      ("value", prettyA val),
      ("datum", ppDatum datum),
      ( "reference script",
        case mscript of
          (SJust s) -> prettyA s <+> ppScriptHash (hashScript @era s)
          SNothing -> ppString "?-"
      )
    ]

instance
  ( EraTxOut era,
    EraScript era,
    PrettyA (Script era),
    PrettyA (Value era)
  ) =>
  PrettyA (BabbageTxOut era)
  where
  prettyA = ppTxOut

ppDatum :: Datum era -> PDoc
ppDatum NoDatum = ppString "NoDatum"
ppDatum (DatumHash x) = ppSexp "DatumHash" [ppDataHash x]
ppDatum (Datum x) = ppSexp "Datum" [ppBinaryData x]

instance PrettyA (Datum era) where prettyA = ppDatum

ppBinaryData :: BinaryData era -> PDoc
ppBinaryData x = ppData (binaryDataToData x)

instance PrettyA (BinaryData era) where prettyA = ppBinaryData

ppDataHash :: DataHash era -> PDoc
ppDataHash x = ppSafeHash x

instance PrettyA (DataHash era) where prettyA = ppDataHash

ppTxBody ::
  ( EraScript era,
    PrettyA (Value era),
    PrettyA (PParamsUpdate era),
    PrettyA (Script era),
    EraTxOut era
  ) =>
  BabbageTxBody era ->
  PDoc
ppTxBody x =
  -- (TxBody si ci ri o cr tc c w fee vi u rsh mnt sdh axh ni) =
  ppRecord
    "TxBody(Babbage)"
    [ ("spending inputs", ppSet ppTxIn (spendInputs' x)),
      ("collateral inputs", ppSet ppTxIn (collateralInputs' x)),
      ("reference inputs", ppSet ppTxIn (referenceInputs' x)),
      ("outputs", ppStrictSeq ppTxOut (outputs' x)),
      ("collateral return", ppStrictMaybe ppTxOut (collateralReturn' x)),
      ("total collateral", ppStrictMaybe ppCoin (totalCollateral' x)),
      ("certificates", ppStrictSeq ppDCert (certs' x)),
      ("withdrawals", ppWdrl (wdrls' x)),
      ("txfee", ppCoin (txfee' x)),
      ("vldt", ppValidityInterval (vldt' x)),
      ("update", ppStrictMaybe ppUpdate (update' x)),
      ("reqSignerHashes", ppSet ppKeyHash (reqSignerHashes' x)),
      ("mint", ppMultiAsset (mint' x)),
      ("scriptIntegrityHash", ppStrictMaybe ppSafeHash (scriptIntegrityHash' x)),
      ("adHash", ppStrictMaybe ppAuxDataHash (adHash' x)),
      ("txnetworkid", ppStrictMaybe ppNetwork (txnetworkid' x))
    ]

instance
  ( EraTxOut era,
    EraScript era,
    PrettyA (Value era),
    PrettyA (PParamsUpdate era),
    PrettyA (Script era)
  ) =>
  PrettyA (BabbageTxBody era)
  where
  prettyA = ppTxBody
