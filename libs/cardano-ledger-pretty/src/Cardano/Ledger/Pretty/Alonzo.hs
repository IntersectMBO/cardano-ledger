{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Pretty.Alonzo where

import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxSeq
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.AuxiliaryData as Core
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational), StrictMaybe)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era, ValidateScript, hashScript)
import Cardano.Ledger.Pretty hiding (ppPParams, ppPParamsUpdate, ppTx, ppTxBody, ppTxOut)
import Cardano.Ledger.Pretty.Mary (ppTimelock, ppValidityInterval, ppValue)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Prettyprinter as PP

ppTxSeq ::
  ( PrettyA (Core.Script era),
    Era era,
    PrettyA (Core.AuxiliaryData era),
    PrettyA (Core.TxBody era)
  ) =>
  TxSeq era ->
  PDoc
ppTxSeq (TxSeq xs) =
  ppSexp "Alonzo TxSeq" [ppStrictSeq ppTx xs]

instance
  ( PrettyA (Core.Script era),
    Era era,
    PrettyA (Core.AuxiliaryData era),
    PrettyA (Core.TxBody era)
  ) =>
  PrettyA (TxSeq era)
  where
  prettyA = ppTxSeq

ppLanguage :: Language -> PDoc
ppLanguage PlutusV1 = ppString "PlutusV1"
ppLanguage PlutusV2 = ppString "PlutusV2"

instance PrettyA Language where prettyA = ppLanguage

ppTag :: Tag -> PDoc
ppTag x = ppString (show x)

instance PrettyA Tag where prettyA = ppTag

ppScript :: forall era. (ValidateScript era, Core.Script era ~ Script era) => Script era -> PDoc
ppScript s@(PlutusScript v _) = ppString ("PlutusScript " <> show v <> " ") PP.<+> ppScriptHash (hashScript @era s)
ppScript (TimelockScript x) = ppTimelock x

instance (ValidateScript era, Core.Script era ~ Script era) => PrettyA (Script era) where prettyA = ppScript

ppExUnits :: ExUnits -> PDoc
ppExUnits (ExUnits mem step) =
  ppRecord "ExUnits" [("memory", ppNatural mem), ("steps", ppNatural step)]

instance PrettyA ExUnits where prettyA = ppExUnits

ppCostModel :: CostModel -> PDoc
ppCostModel (CostModel m) =
  ppSexp "CostModel" [ppMap text ppInteger m]

instance PrettyA CostModel where prettyA = ppCostModel

ppPrices :: Prices -> PDoc
ppPrices Prices {prMem, prSteps} =
  ppRecord
    "Prices"
    [ ("prMem", ppRational $ unboundRational prMem),
      ("prSteps", ppRational $ unboundRational prSteps)
    ]

instance PrettyA Prices where prettyA = ppPrices

instance PrettyA (PParams era) where
  prettyA = ppPParams

ppPParamsUpdate :: PParams' StrictMaybe era -> PDoc
ppPParamsUpdate (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
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
      ("d", lift ppUnitInterval d),
      ("extraEntropy", lift ppNonce ex),
      ("protocolVersion", lift ppProtVer pv),
      ("minPoolCost", lift ppCoin mpool),
      ("adaPerWord", lift ppCoin ada),
      ("costmdls", lift (ppMap ppLanguage ppCostModel) cost),
      ("prices", lift ppPrices prices),
      ("maxTxExUnits", lift ppExUnits mxEx),
      ("maxBlockExUnits", lift ppExUnits mxBEx),
      ("maxValSize", lift ppNatural mxV),
      ("collateral%", lift ppNatural c),
      ("maxCollateralInputs", lift ppNatural mxC)
    ]
  where
    lift pp x = ppStrictMaybe pp x

instance PrettyA (PParams' StrictMaybe era) where
  prettyA = ppPParamsUpdate

ppPlutusData :: Plutus.Data -> PDoc
ppPlutusData (Plutus.Constr tag args) = ppSexp "Constr" [ppInteger tag, ppList ppPlutusData args]
ppPlutusData (Plutus.Map pairs) = ppSexp "Map" [ppList (ppPair ppPlutusData ppPlutusData) pairs]
ppPlutusData (Plutus.List xs) = ppSexp "List" [ppList ppPlutusData xs]
ppPlutusData (Plutus.I i) = ppSexp "I" [ppInteger i]
ppPlutusData (Plutus.B bytes) = ppSexp "B" [ppLong bytes]

instance PrettyA Plutus.Data where prettyA = ppPlutusData

ppData :: Data era -> PDoc
ppData (Data d) = ppSexp "Data" [ppPlutusData d]

instance PrettyA (Data era) where prettyA = ppData

ppAuxiliaryData :: (PrettyA (Core.Script era)) => AuxiliaryData era -> PDoc
ppAuxiliaryData (AuxiliaryData' m s) =
  ppSexp "AuxiliaryData" [ppMap ppWord64 ppMetadatum m, ppStrictSeq prettyA s]

instance (PrettyA (Core.Script era)) => PrettyA (AuxiliaryData era) where prettyA = ppAuxiliaryData

ppPParams :: PParams era -> PDoc
ppPParams (PParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
  ppRecord
    "PParams"
    [ ("minfeeA", ppNatural feeA),
      ("minfeeB", ppNatural feeB),
      ("maxBBSize", ppNatural mbb),
      ("maxTxSize", ppNatural mtx),
      ("maxBHSize", ppNatural mbh),
      ("keyDeposit", ppCoin kd),
      ("poolDeposit", ppCoin pd),
      ("eMax", ppEpochNo em),
      ("nOpt", ppNatural no),
      ("a0", ppRational (unboundRational a0)),
      ("rho", ppUnitInterval rho),
      ("tau", ppUnitInterval tau),
      ("d", ppUnitInterval d),
      ("extraEntropy", ppNonce ex),
      ("protocolVersion", ppProtVer pv),
      ("minPoolCost", ppCoin mpool),
      ("adaPerWord", ppCoin ada),
      ("costmdls", ppMap ppLanguage ppCostModel cost),
      ("prices", ppPrices prices),
      ("maxTxExUnits", ppExUnits mxEx),
      ("maxBlockExUnits", ppExUnits mxBEx),
      ("maxValSize", ppNatural mxV),
      ("collateral%", ppNatural c),
      ("maxCollateralInputs", ppNatural mxC)
    ]

ppTxOut :: (Era era, PrettyA (Core.Value era)) => TxOut era -> PDoc
ppTxOut (TxOut addr val dhash) =
  ppSexp "TxOut" [ppAddr addr, prettyA val, ppStrictMaybe ppSafeHash dhash]

ppTxBody ::
  (AlonzoBody era, PrettyA (Core.Value era), PrettyA (Core.PParamsDelta era)) =>
  TxBody era ->
  PDoc
ppTxBody (TxBody i ifee o c w fee vi u rsh mnt sdh axh ni) =
  ppRecord
    "TxBody(Alonzo)"
    [ ("inputs", ppSet ppTxIn i),
      ("collateral", ppSet ppTxIn ifee),
      ("outputs", ppStrictSeq ppTxOut o),
      ("certificates", ppStrictSeq ppDCert c),
      ("withdrawals", ppWdrl w),
      ("txfee", ppCoin fee),
      ("vldt", ppValidityInterval vi),
      ("update", ppStrictMaybe ppUpdate u),
      ("reqSignerHashes", ppSet ppKeyHash rsh),
      ("mint", ppValue mnt),
      ("scriptIntegrityHash", ppStrictMaybe ppSafeHash sdh),
      ("adHash", ppStrictMaybe ppAuxDataHash axh),
      ("txnetworkid", ppStrictMaybe ppNetwork ni)
    ]

ppAuxDataHash :: Core.AuxiliaryDataHash crypto -> PDoc
ppAuxDataHash (Core.AuxiliaryDataHash axh) = ppSafeHash axh

instance
  (AlonzoBody era, PrettyA (Core.Value era), PrettyA (Core.PParamsDelta era)) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody

instance (Era era, PrettyA (Core.Value era)) => PrettyA (TxOut era) where
  prettyA x = ppTxOut x

ppRdmrPtr :: RdmrPtr -> PDoc
ppRdmrPtr (RdmrPtr tag w) = ppSexp "RdmrPtr" [ppTag tag, ppWord64 w]

instance PrettyA RdmrPtr where prettyA = ppRdmrPtr

ppTxWitness :: (Era era, PrettyA (Core.Script era)) => TxWitness era -> PDoc
ppTxWitness (TxWitness' vk wb sc da (Redeemers rd)) =
  ppRecord
    "TxWitness"
    [ ("keys", ppSet ppWitVKey vk),
      ("bootstrap witnesses", ppSet ppBootstrapWitness wb),
      ("scripts map", ppMap ppScriptHash prettyA sc),
      ("Data map", ppMap ppSafeHash ppData (unTxDats da)),
      ("Redeemer map", ppMap ppRdmrPtr (ppPair ppData ppExUnits) rd)
    ]

instance
  (Era era, PrettyA (Core.Script era)) =>
  PrettyA (TxWitness era)
  where
  prettyA = ppTxWitness

ppIsValid :: IsValid -> PDoc
ppIsValid (IsValid True) = ppString "True"
ppIsValid (IsValid False) = ppString "False"

instance PrettyA IsValid where prettyA = ppIsValid

ppTx ::
  ( Era era,
    PrettyA (Core.Script era),
    PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era)
  ) =>
  ValidatedTx era ->
  PDoc
ppTx (ValidatedTx b w iv aux) =
  ppRecord
    "Tx"
    [ ("body", prettyA b),
      ("wits", ppTxWitness w),
      ("isValid", ppIsValid iv),
      ("auxiliaryData", ppStrictMaybe prettyA aux)
    ]

instance
  ( Era era,
    PrettyA (Core.Script era),
    PrettyA (Core.TxBody era),
    PrettyA (Core.AuxiliaryData era)
  ) =>
  PrettyA (ValidatedTx era)
  where
  prettyA = ppTx
