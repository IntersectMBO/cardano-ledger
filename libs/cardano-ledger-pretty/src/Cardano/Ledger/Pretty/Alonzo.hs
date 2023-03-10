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

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  CostModel,
  CostModels (..),
  ExUnits (ExUnits),
  Prices (..),
  Tag,
  getCostModelLanguage,
  getCostModelParams,
 )
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx,
  AlonzoTx (AlonzoTx),
  AlonzoTxBody (AlonzoTxBody),
  IsValid (..),
 )
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (AlonzoTxAuxData, atadMetadata),
  getAlonzoTxAuxDataScripts,
 )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (AlonzoTxOut))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (AlonzoTxSeq))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Pretty hiding (ppPParams, ppPParamsUpdate, ppTx, ppTxBody, ppTxOut)
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppTimelock, ppValidityInterval)
import Cardano.Ledger.SafeHash (SafeToHash)
import Lens.Micro
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
ppLanguage PlutusV3 = ppString "PlutusV3"

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
ppCostModels cms = ppMap ppLanguage ppCostModel (costModelsValid cms)

ppPrices :: Prices -> PDoc
ppPrices Prices {prMem, prSteps} =
  ppRecord
    "Prices"
    [ ("prMem", ppRational $ unboundRational prMem)
    , ("prSteps", ppRational $ unboundRational prSteps)
    ]

instance PrettyA Prices where
  prettyA = ppPrices

instance Crypto c => PrettyA (PParams (AlonzoEra c)) where
  prettyA = ppAlonzoPParams

ppAlonzoPParamsUpdate :: Crypto c => PParamsUpdate (AlonzoEra c) -> PDoc
ppAlonzoPParamsUpdate pp =
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
    , ("d", ppStrictMaybe ppUnitInterval $ pp ^. ppuDL)
    , ("extraEntropy", ppStrictMaybe ppNonce $ pp ^. ppuExtraEntropyL)
    , ("protocolVersion", ppStrictMaybe ppProtVer $ pp ^. ppuProtocolVersionL)
    , ("minPoolCost", ppStrictMaybe ppCoin $ pp ^. ppuMinPoolCostL)
    , ("coinPerWord", ppStrictMaybe (ppCoin . unCoinPerWord) $ pp ^. ppuCoinsPerUTxOWordL)
    , ("costmdls", ppStrictMaybe ppCostModels $ pp ^. ppuCostModelsL)
    , ("prices", ppStrictMaybe ppPrices $ pp ^. ppuPricesL)
    , ("maxTxExUnits", ppStrictMaybe ppExUnits $ pp ^. ppuMaxTxExUnitsL)
    , ("maxBlockExUnits", ppStrictMaybe ppExUnits $ pp ^. ppuMaxBlockExUnitsL)
    , ("maxValSize", ppStrictMaybe ppNatural $ pp ^. ppuMaxValSizeL)
    , ("collateral%", ppStrictMaybe ppNatural $ pp ^. ppuCollateralPercentageL)
    , ("maxCollateralInputs", ppStrictMaybe ppNatural $ pp ^. ppuMaxCollateralInputsL)
    ]

instance Crypto c => PrettyA (PParamsUpdate (AlonzoEra c)) where
  prettyA = ppAlonzoPParamsUpdate

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

ppAlonzoPParams :: Crypto c => PParams (AlonzoEra c) -> PDoc
ppAlonzoPParams pp =
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
    , ("d", ppUnitInterval $ pp ^. ppDL)
    , ("extraEntropy", ppNonce $ pp ^. ppExtraEntropyL)
    , ("protocolVersion", ppProtVer $ pp ^. ppProtocolVersionL)
    , ("minPoolCost", ppCoin $ pp ^. ppMinPoolCostL)
    , ("coinPerWord", (ppCoin . unCoinPerWord) $ pp ^. ppCoinsPerUTxOWordL)
    , ("costmdls", ppCostModels $ pp ^. ppCostModelsL)
    , ("prices", ppPrices $ pp ^. ppPricesL)
    , ("maxTxExUnits", ppExUnits $ pp ^. ppMaxTxExUnitsL)
    , ("maxBlockExUnits", ppExUnits $ pp ^. ppMaxBlockExUnitsL)
    , ("maxValSize", ppNatural $ pp ^. ppMaxValSizeL)
    , ("collateral%", ppNatural $ pp ^. ppCollateralPercentageL)
    , ("maxCollateralInputs", ppNatural $ pp ^. ppMaxCollateralInputsL)
    ]

ppTxOut :: (EraTxOut era, PrettyA (Value era)) => AlonzoTxOut era -> PDoc
ppTxOut (AlonzoTxOut addr val dhash) =
  ppSexp "TxOut" [ppAddr addr, prettyA val, ppStrictMaybe ppSafeHash dhash]

ppTxBody ::
  ( AlonzoEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , PrettyA (DCert era)
  ) =>
  AlonzoTxBody era ->
  PDoc
ppTxBody (AlonzoTxBody i ifee o c w fee vi u rsh mnt sdh axh ni) =
  ppRecord
    "TxBody(Alonzo)"
    [ ("inputs", ppSet ppTxIn i)
    , ("collateral", ppSet ppTxIn ifee)
    , ("outputs", ppStrictSeq prettyA o)
    , ("certificates", ppStrictSeq prettyA c)
    , ("withdrawals", ppWithdrawals w)
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
  ( AlonzoEraTxBody era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  , PrettyA (DCert era)
  ) =>
  PrettyA (AlonzoTxBody era)
  where
  prettyA = ppTxBody

instance (EraTxOut era, PrettyA (Value era)) => PrettyA (AlonzoTxOut era) where
  prettyA = ppTxOut

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
