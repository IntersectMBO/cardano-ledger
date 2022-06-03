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

import Cardano.Ledger.Alonzo.Data (BinaryData, DataHash, binaryDataToData)
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure)
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail)
import Cardano.Ledger.Babbage.PParams (PParams, PParams' (..), PParamsUpdate)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (..))
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPred (..))
import Cardano.Ledger.Babbage.TxBody
  ( Datum (..),
    TxBody (..),
    TxOut (..),
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
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational), StrictMaybe)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era, ValidateScript (hashScript))
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
import Cardano.Ledger.Pretty.Mary (ppValidityInterval, ppValue)
import Control.State.Transition.Extended
import Data.Functor.Identity (Identity (..))
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Prettyprinter ((<+>))

-- =====================================

ppPParamsId :: PParams' Identity era -> PDoc
ppPParamsId (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
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

ppPParams :: PParams era -> PDoc
ppPParams = ppPParamsId

instance PrettyA (PParams era) where
  prettyA = ppPParams

ppPParamsStrictMaybe :: PParams' StrictMaybe era -> PDoc
ppPParamsStrictMaybe (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
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

ppPParamsUpdate :: PParamsUpdate era -> PDoc
ppPParamsUpdate = ppPParamsStrictMaybe

instance PrettyA (PParamsUpdate era) where
  prettyA = ppPParamsUpdate

ppBabbageUtxoPred ::
  ( PrettyA (UtxoPredicateFailure era),
    PrettyA (Core.TxOut era)
  ) =>
  BabbageUtxoPred era ->
  PDoc
ppBabbageUtxoPred (FromAlonzoUtxoFail x) = prettyA x
ppBabbageUtxoPred (IncorrectTotalCollateralField c1 c2) =
  ppRecord
    "IncorrectTotalCollateralField"
    [("collateral provided", ppCoin c1), ("collateral declared", ppCoin c2)]
ppBabbageUtxoPred (BabbageOutputTooSmallUTxO xs) =
  ppSexp "BabbageOutputTooSmallUTxO" [ppList (ppPair prettyA ppCoin) xs]

instance
  ( PrettyA (UtxoPredicateFailure era),
    PrettyA (Core.TxOut era)
  ) =>
  PrettyA (BabbageUtxoPred era)
  where
  prettyA = ppBabbageUtxoPred

ppBabbageUtxowPred ::
  ( PrettyA (UtxowPredicateFail era),
    PrettyA (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  BabbageUtxowPred era ->
  PDoc
ppBabbageUtxowPred (FromAlonzoUtxowFail pf) = prettyA pf
ppBabbageUtxowPred (UtxoFailure pf) = prettyA pf
ppBabbageUtxowPred (MalformedScriptWitnesses scripts) =
  ppSexp "MalformedScriptWitnesses" [ppSet ppScriptHash scripts]
ppBabbageUtxowPred (MalformedReferenceScripts scripts) =
  ppSexp "MalformedReferenceScripts" [ppSet ppScriptHash scripts]

instance
  ( PrettyA (UtxowPredicateFail era),
    PrettyA (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  PrettyA (BabbageUtxowPred era)
  where
  prettyA = ppBabbageUtxowPred

ppTxOut ::
  forall era.
  ( Era era,
    ValidateScript era,
    PrettyA (Core.Script era),
    PrettyA (Core.Value era)
  ) =>
  TxOut era ->
  PDoc
ppTxOut (TxOut addr val datum mscript) =
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
  ( Era era,
    ValidateScript era,
    PrettyA (Core.Script era),
    PrettyA (Core.Value era)
  ) =>
  PrettyA (TxOut era)
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
  ( ValidateScript era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsDelta era),
    PrettyA (Core.Script era)
  ) =>
  TxBody era ->
  PDoc
ppTxBody x =
  -- (TxBody si ci ri o cr tc c w fee vi u rsh mnt sdh axh ni) =
  ppRecord
    "TxBody(Alonzo)"
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
      ("mint", ppValue (mint' x)),
      ("scriptIntegrityHash", ppStrictMaybe ppSafeHash (scriptIntegrityHash' x)),
      ("adHash", ppStrictMaybe ppAuxDataHash (adHash' x)),
      ("txnetworkid", ppStrictMaybe ppNetwork (txnetworkid' x))
    ]

instance
  ( Era era,
    ValidateScript era,
    PrettyA (Core.Value era),
    PrettyA (Core.PParamsDelta era),
    PrettyA (Core.Script era)
  ) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody
