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

import Cardano.Ledger.Alonzo.Data (
  AlonzoTxAuxData (AlonzoTxAuxData, atadMetadata),
  Data (..),
  getAlonzoTxAuxDataScripts,
 )
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (
  AlonzoPParams,
  AlonzoPParamsHKD (AlonzoPParams),
  AlonzoPParamsUpdate,
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  CostModel,
  CostModels (CostModels),
  ExUnits (ExUnits),
  Prices (..),
  Tag,
  getCostModelLanguage,
  getCostModelParams,
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  AlonzoTx (AlonzoTx),
  AlonzoTxBody (AlonzoTxBody),
  IsValid (..),
 )
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody, AlonzoTxOut (AlonzoTxOut))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (AlonzoTxSeq))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational))
import Cardano.Ledger.Core
import Cardano.Ledger.Pretty hiding (ppPParams, ppPParamsUpdate, ppTx, ppTxBody, ppTxOut)
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppTimelock, ppValidityInterval)
import Cardano.Ledger.SafeHash (SafeToHash)
import qualified PlutusLedgerApi.V1 as PV1
import qualified Prettyprinter as PP

ppTxSeq ::
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  , PrettyA (Tx era)
  ) =>
  AlonzoTxSeq era ->
  PDoc
ppTxSeq (AlonzoTxSeq xs) =
  ppSexp "Alonzo TxSeq" [ppStrictSeq prettyA xs]

instance
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  , PrettyA (Tx era)
  ) =>
  PrettyA (AlonzoTxSeq era)
  where
  prettyA = ppTxSeq

ppLanguage :: Language -> PDoc
ppLanguage PlutusV1 = ppString "PlutusV1"
ppLanguage PlutusV2 = ppString "PlutusV2"

instance PrettyA Language where
  prettyA = ppLanguage

ppTag :: Tag -> PDoc
ppTag x = ppString (show x)

instance PrettyA Tag where
  prettyA = ppTag

ppScript ::
  forall era.
  (EraScript era, Script era ~ AlonzoScript era) =>
  AlonzoScript era ->
  PDoc
ppScript s@(PlutusScript v _) = ppString ("PlutusScript " <> show v <> " ") PP.<+> ppScriptHash (hashScript @era s)
ppScript (TimelockScript x) = ppTimelock x

instance (EraScript era, Script era ~ AlonzoScript era) => PrettyA (AlonzoScript era) where
  prettyA = ppScript

ppExUnits :: ExUnits -> PDoc
ppExUnits (ExUnits mem step) =
  ppRecord "ExUnits" [("memory", ppNatural mem), ("steps", ppNatural step)]

instance PrettyA ExUnits where
  prettyA = ppExUnits

ppCostModel :: CostModel -> PDoc
ppCostModel cm =
  ppSexp "CostModel" [ppLanguage (getCostModelLanguage cm), ppList ppInteger (getCostModelParams cm)]

instance PrettyA CostModel where
  prettyA = ppCostModel

ppCostModels :: CostModels -> PDoc
ppCostModels (CostModels cms) = ppMap ppLanguage ppCostModel cms

ppPrices :: Prices -> PDoc
ppPrices Prices {prMem, prSteps} =
  ppRecord
    "Prices"
    [ ("prMem", ppRational $ unboundRational prMem)
    , ("prSteps", ppRational $ unboundRational prSteps)
    ]

instance PrettyA Prices where
  prettyA = ppPrices

instance PrettyA (AlonzoPParams era) where
  prettyA = ppPParams

ppPParamsUpdate :: AlonzoPParamsUpdate era -> PDoc
ppPParamsUpdate (AlonzoPParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
  ppRecord
    "PParams"
    [ ("minfeeA", lift ppNatural feeA)
    , ("minfeeB", lift ppNatural feeB)
    , ("maxBBSize", lift ppNatural mbb)
    , ("maxTxSize", lift ppNatural mtx)
    , ("maxBHSize", lift ppNatural mbh)
    , ("keyDeposit", lift ppCoin kd)
    , ("poolDeposit", lift ppCoin pd)
    , ("eMax", lift ppEpochNo em)
    , ("nOpt", lift ppNatural no)
    , ("a0", lift (ppRational . unboundRational) a0)
    , ("rho", lift ppUnitInterval rho)
    , ("tau", lift ppUnitInterval tau)
    , ("d", lift ppUnitInterval d)
    , ("extraEntropy", lift ppNonce ex)
    , ("protocolVersion", lift ppProtVer pv)
    , ("minPoolCost", lift ppCoin mpool)
    , ("adaPerWord", lift ppCoin ada)
    , ("costmdls", lift ppCostModels cost)
    , ("prices", lift ppPrices prices)
    , ("maxTxExUnits", lift ppExUnits mxEx)
    , ("maxBlockExUnits", lift ppExUnits mxBEx)
    , ("maxValSize", lift ppNatural mxV)
    , ("collateral%", lift ppNatural c)
    , ("maxCollateralInputs", lift ppNatural mxC)
    ]
  where
    lift pp x = ppStrictMaybe pp x

instance PrettyA (AlonzoPParamsUpdate era) where
  prettyA = ppPParamsUpdate

ppPlutusData :: PV1.Data -> PDoc
ppPlutusData (PV1.Constr tag args) = ppSexp "Constr" [ppInteger tag, ppList ppPlutusData args]
ppPlutusData (PV1.Map pairs) = ppSexp "Map" [ppList (ppPair ppPlutusData ppPlutusData) pairs]
ppPlutusData (PV1.List xs) = ppSexp "List" [ppList ppPlutusData xs]
ppPlutusData (PV1.I i) = ppSexp "I" [ppInteger i]
ppPlutusData (PV1.B bytes) = ppSexp "B" [ppLong bytes]

instance PrettyA PV1.Data where
  prettyA = ppPlutusData

ppData :: Era era => Data era -> PDoc
ppData (Data d) = ppSexp "Data" [ppPlutusData d]

instance Era era => PrettyA (Data era) where
  prettyA = ppData

ppAuxiliaryData ::
  (PrettyA (Script era), EraTx era, Script era ~ AlonzoScript era) =>
  AlonzoTxAuxData era ->
  PDoc
ppAuxiliaryData auxData@AlonzoTxAuxData {atadMetadata = metadata} =
  ppSexp
    "AuxiliaryData"
    [ ppMap ppWord64 ppMetadatum metadata
    , ppStrictSeq prettyA (getAlonzoTxAuxDataScripts auxData)
    ]

instance
  (PrettyA (Script era), EraTx era, Script era ~ AlonzoScript era) =>
  PrettyA (AlonzoTxAuxData era)
  where
  prettyA = ppAuxiliaryData

ppPParams :: AlonzoPParams era -> PDoc
ppPParams (AlonzoPParams feeA feeB mbb mtx mbh kd pd em no a0 rho tau d ex pv mpool ada cost prices mxEx mxBEx mxV c mxC) =
  ppRecord
    "PParams"
    [ ("minfeeA", ppNatural feeA)
    , ("minfeeB", ppNatural feeB)
    , ("maxBBSize", ppNatural mbb)
    , ("maxTxSize", ppNatural mtx)
    , ("maxBHSize", ppNatural mbh)
    , ("keyDeposit", ppCoin kd)
    , ("poolDeposit", ppCoin pd)
    , ("eMax", ppEpochNo em)
    , ("nOpt", ppNatural no)
    , ("a0", ppRational (unboundRational a0))
    , ("rho", ppUnitInterval rho)
    , ("tau", ppUnitInterval tau)
    , ("d", ppUnitInterval d)
    , ("extraEntropy", ppNonce ex)
    , ("protocolVersion", ppProtVer pv)
    , ("minPoolCost", ppCoin mpool)
    , ("adaPerWord", ppCoin ada)
    , ("costmdls", ppCostModels cost)
    , ("prices", ppPrices prices)
    , ("maxTxExUnits", ppExUnits mxEx)
    , ("maxBlockExUnits", ppExUnits mxBEx)
    , ("maxValSize", ppNatural mxV)
    , ("collateral%", ppNatural c)
    , ("maxCollateralInputs", ppNatural mxC)
    ]

ppTxOut :: (EraTxOut era, PrettyA (Value era)) => AlonzoTxOut era -> PDoc
ppTxOut (AlonzoTxOut addr val dhash) =
  ppSexp "TxOut" [ppAddr addr, prettyA val, ppStrictMaybe ppSafeHash dhash]

ppTxBody ::
  (AlonzoEraTxBody era, PrettyA (TxOut era), PrettyA (PParamsUpdate era)) =>
  AlonzoTxBody era ->
  PDoc
ppTxBody (AlonzoTxBody i ifee o c w fee vi u rsh mnt sdh axh ni) =
  ppRecord
    "TxBody(Alonzo)"
    [ ("inputs", ppSet ppTxIn i)
    , ("collateral", ppSet ppTxIn ifee)
    , ("outputs", ppStrictSeq prettyA o)
    , ("certificates", ppStrictSeq ppDCert c)
    , ("withdrawals", ppWdrl w)
    , ("txfee", ppCoin fee)
    , ("vldt", ppValidityInterval vi)
    , ("update", ppStrictMaybe ppUpdate u)
    , ("reqSignerHashes", ppSet ppKeyHash rsh)
    , ("mint", ppMultiAsset mnt)
    , ("scriptIntegrityHash", ppStrictMaybe ppSafeHash sdh)
    , ("adHash", ppStrictMaybe ppAuxDataHash axh)
    , ("txnetworkid", ppStrictMaybe ppNetwork ni)
    ]

ppAuxDataHash :: AuxiliaryDataHash c -> PDoc
ppAuxDataHash (AuxiliaryDataHash axh) = ppSafeHash axh

instance
  (AlonzoEraTxBody era, PrettyA (TxOut era), PrettyA (PParamsUpdate era)) =>
  PrettyA (AlonzoTxBody era)
  where
  prettyA = ppTxBody

instance (EraTxOut era, PrettyA (Value era)) => PrettyA (AlonzoTxOut era) where
  prettyA x = ppTxOut x

ppRdmrPtr :: RdmrPtr -> PDoc
ppRdmrPtr (RdmrPtr tag w) = ppSexp "RdmrPtr" [ppTag tag, ppWord64 w]

instance PrettyA RdmrPtr where
  prettyA = ppRdmrPtr

ppTxWitness :: (Era era, PrettyA (Script era)) => AlonzoTxWits era -> PDoc
ppTxWitness (AlonzoTxWits' vk wb sc da (Redeemers rd)) =
  ppRecord
    "AlonzoTxWits"
    [ ("keys", ppSet ppWitVKey vk)
    , ("bootstrap witnesses", ppSet ppBootstrapWitness wb)
    , ("scripts map", ppMap ppScriptHash prettyA sc)
    , ("Data map", ppMap ppSafeHash ppData (unTxDats da))
    , ("Redeemer map", ppMap ppRdmrPtr (ppPair ppData ppExUnits) rd)
    ]

instance
  (Era era, PrettyA (Script era)) =>
  PrettyA (AlonzoTxWits era)
  where
  prettyA = ppTxWitness

ppIsValid :: IsValid -> PDoc
ppIsValid (IsValid True) = ppString "True"
ppIsValid (IsValid False) = ppString "False"

instance PrettyA IsValid where
  prettyA = ppIsValid

ppTx ::
  ( PrettyA (TxBody era)
  , PrettyA (TxWits era)
  , PrettyA (TxAuxData era)
  ) =>
  AlonzoTx era ->
  PDoc
ppTx (AlonzoTx b w iv aux) =
  ppRecord
    "Tx"
    [ ("body", prettyA b)
    , ("wits", prettyA w)
    , ("isValid", ppIsValid iv)
    , ("auxiliaryData", ppStrictMaybe prettyA aux)
    ]

instance
  ( PrettyA (TxWits era)
  , Era era
  , PrettyA (TxBody era)
  , PrettyA (TxAuxData era)
  ) =>
  PrettyA (AlonzoTx era)
  where
  prettyA = ppTx
