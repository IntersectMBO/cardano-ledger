{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Pretty.Alonzo () where

import Cardano.Ledger.Alonzo (AlonzoEra, AlonzoTxOut)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Language (Language (..))
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
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (AlonzoTxSeq))
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes (BoundedRational (unboundRational))
import Cardano.Ledger.Crypto
import Cardano.Ledger.SafeHash (SafeToHash)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Prettyprinter (viaShow)
import qualified Prettyprinter as PP
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Mary ()
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut(..))

ppTxSeq ::
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  , PrettyA (Tx era)
  ) =>
  AlonzoTxSeq era ->
  PDoc
ppTxSeq (AlonzoTxSeq xs) =
  ppSexp "Alonzo TxSeq" [prettyA xs]

instance
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  , PrettyA (Tx era)
  ) =>
  PrettyA (AlonzoTxSeq era)
  where
  prettyA = ppTxSeq

ppLanguage :: Language -> PDoc
ppLanguage PlutusV1 = "PlutusV1"
ppLanguage PlutusV2 = "PlutusV2"

instance PrettyA Language where
  prettyA = ppLanguage

instance PrettyA Tag where
  prettyA = viaShow

instance (EraScript era, Script era ~ AlonzoScript era) => PrettyA (AlonzoScript era) where
  prettyA s@(PlutusScript v _) = "PlutusScript " <> prettyA v <> " " PP.<+> prettyA (hashScript @era s)
  prettyA (TimelockScript x) = prettyA x

instance PrettyA ExUnits where
  prettyA (ExUnits mem step) =
    ppRecord "ExUnits" [("memory", prettyA mem), ("steps", prettyA step)]

instance PrettyA CostModel where
  prettyA cm =
    ppSexp "CostModel" [ppLanguage (getCostModelLanguage cm), prettyA $ getCostModelParams cm]

ppPrices :: Prices -> PDoc
ppPrices Prices {prMem, prSteps} =
  ppRecord
    "Prices"
    [ ("prMem", prettyA $ unboundRational prMem)
    , ("prSteps", prettyA $ unboundRational prSteps)
    ]

instance PrettyA Prices where
  prettyA = ppPrices

instance Crypto c => PrettyA (PParams (AlonzoEra c)) where
  prettyA = ppAlonzoPParams

ppAlonzoPParamsUpdate :: Crypto c => PParamsUpdate (AlonzoEra c) -> PDoc
ppAlonzoPParamsUpdate pp =
  ppRecord
    "PParamsUdate"
    [ ("minfeeA", prettyA $ pp ^. ppuMinFeeAL)
    , ("minfeeB", prettyA $ pp ^. ppuMinFeeBL)
    , ("maxBBSize", prettyA $ pp ^. ppuMaxBBSizeL)
    , ("maxTxSize", prettyA $ pp ^. ppuMaxTxSizeL)
    , ("maxBHSize", prettyA $ pp ^. ppuMaxBHSizeL)
    , ("keyDeposit", prettyA $ pp ^. ppuKeyDepositL)
    , ("poolDeposit", prettyA $ pp ^. ppuPoolDepositL)
    , ("eMax", prettyA $ pp ^. ppuEMaxL)
    , ("nOpt", prettyA $ pp ^. ppuNOptL)
    , ("a0", prettyA $ pp ^. ppuA0L)
    , ("rho", prettyA $ pp ^. ppuRhoL)
    , ("tau", prettyA $ pp ^. ppuTauL)
    , ("d", prettyA $ pp ^. ppuDL)
    , ("extraEntropy", prettyA $ pp ^. ppuExtraEntropyL)
    , ("protocolVersion", prettyA $ pp ^. ppuProtocolVersionL)
    , ("minPoolCost", prettyA $ pp ^. ppuMinPoolCostL)
    , ("coinPerWord", prettyA $ pp ^. ppuCoinsPerUTxOWordL)
    , ("costmdls", prettyA $ pp ^. ppuCostModelsL)
    , ("prices", prettyA $ pp ^. ppuPricesL)
    , ("maxTxExUnits", prettyA $ pp ^. ppuMaxTxExUnitsL)
    , ("maxBlockExUnits", prettyA $ pp ^. ppuMaxBlockExUnitsL)
    , ("maxValSize", prettyA $ pp ^. ppuMaxValSizeL)
    , ("collateral%", prettyA $ pp ^. ppuCollateralPercentageL)
    , ("maxCollateralInputs", prettyA $ pp ^. ppuMaxCollateralInputsL)
    ]

deriving newtype instance PrettyA CostModels

instance Crypto c => PrettyA (PParamsUpdate (AlonzoEra c)) where
  prettyA = ppAlonzoPParamsUpdate

instance PrettyA PV1.Data where
  prettyA (PV1.Constr tag args) = ppSexp "Constr" [prettyA tag, prettyA args]
  prettyA (PV1.Map pairs) = ppSexp "Map" [prettyA pairs]
  prettyA (PV1.List xs) = ppSexp "List" [prettyA xs]
  prettyA (PV1.I i) = ppSexp "I" [prettyA i]
  prettyA (PV1.B bytes) = ppSexp "B" [prettyA bytes]

instance Era era => PrettyA (Data era) where
  prettyA (Data d) = ppSexp "Data" [prettyA d]

instance
  (PrettyA (Script era), EraTx era, Script era ~ AlonzoScript era) =>
  PrettyA (AlonzoTxAuxData era)
  where
  prettyA auxData@AlonzoTxAuxData {atadMetadata = metadata} =
    ppSexp
      "AuxiliaryData"
      [ prettyA metadata
      , prettyA (getAlonzoTxAuxDataScripts auxData)
      ]

ppAlonzoPParams :: Crypto c => PParams (AlonzoEra c) -> PDoc
ppAlonzoPParams pp =
  ppRecord
    "PParams"
    [ ("minfeeA", prettyA $ pp ^. ppMinFeeAL)
    , ("minfeeB", prettyA $ pp ^. ppMinFeeBL)
    , ("maxBBSize", prettyA $ pp ^. ppMaxBBSizeL)
    , ("maxTxSize", prettyA $ pp ^. ppMaxTxSizeL)
    , ("maxBHSize", prettyA $ pp ^. ppMaxBHSizeL)
    , ("keyDeposit", prettyA $ pp ^. ppKeyDepositL)
    , ("poolDeposit", prettyA $ pp ^. ppPoolDepositL)
    , ("eMax", prettyA $ pp ^. ppEMaxL)
    , ("nOpt", prettyA $ pp ^. ppNOptL)
    , ("a0", prettyA $ pp ^. ppA0L)
    , ("rho", prettyA $ pp ^. ppRhoL)
    , ("tau", prettyA $ pp ^. ppTauL)
    , ("d", prettyA $ pp ^. ppDL)
    , ("extraEntropy", prettyA $ pp ^. ppExtraEntropyL)
    , ("protocolVersion", prettyA $ pp ^. ppProtocolVersionL)
    , ("minPoolCost", prettyA $ pp ^. ppMinPoolCostL)
    , ("coinPerWord", prettyA $ pp ^. ppCoinsPerUTxOWordL)
    , ("costmdls", prettyA $ pp ^. ppCostModelsL)
    , ("prices", prettyA $ pp ^. ppPricesL)
    , ("maxTxExUnits", prettyA $ pp ^. ppMaxTxExUnitsL)
    , ("maxBlockExUnits", prettyA $ pp ^. ppMaxBlockExUnitsL)
    , ("maxValSize", prettyA $ pp ^. ppMaxValSizeL)
    , ("collateral%", prettyA $ pp ^. ppCollateralPercentageL)
    , ("maxCollateralInputs", prettyA $ pp ^. ppMaxCollateralInputsL)
    ]

instance PrettyA CoinPerWord where
  prettyA = prettyA . unCoinPerWord

instance
  (AlonzoEraTxBody era, PrettyA (TxOut era), PrettyA (PParamsUpdate era)) =>
  PrettyA (AlonzoTxBody era)
  where
  prettyA (AlonzoTxBody i ifee o c w fee vi u rsh mnt sdh axh ni) =
    ppRecord
      "TxBody(Alonzo)"
      [ ("inputs", prettyA i)
      , ("collateral", prettyA ifee)
      , ("outputs", prettyA o)
      , ("certificates", prettyA c)
      , ("withdrawals", prettyA w)
      , ("txfee", prettyA fee)
      , ("vldt", prettyA vi)
      , ("update", prettyA u)
      , ("reqSignerHashes", prettyA rsh)
      , ("mint", prettyA mnt)
      , ("scriptIntegrityHash", prettyA sdh)
      , ("adHash", prettyA axh)
      , ("txnetworkid", prettyA ni)
      ]

instance (EraTxOut era, PrettyA (Value era)) => PrettyA (AlonzoTxOut era) where
  prettyA (AlonzoTxOut addr val dhash) =
    ppSexp "TxOut" [prettyA addr, prettyA val, prettyA dhash]

ppRdmrPtr :: RdmrPtr -> PDoc
ppRdmrPtr (RdmrPtr tag w) = ppSexp "RdmrPtr" [prettyA tag, prettyA w]

instance PrettyA RdmrPtr where
  prettyA = ppRdmrPtr

ppTxWitness :: (Era era, PrettyA (Script era)) => AlonzoTxWits era -> PDoc
ppTxWitness (AlonzoTxWits' vk wb sc da (Redeemers rd)) =
  ppRecord
    "AlonzoTxWits"
    [ ("keys", prettyA vk)
    , ("bootstrap witnesses", prettyA wb)
    , ("scripts map", prettyA sc)
    , ("Data map", prettyA (unTxDats da))
    , ("Redeemer map", prettyA rd)
    ]

instance
  (Era era, PrettyA (Script era)) =>
  PrettyA (AlonzoTxWits era)
  where
  prettyA = ppTxWitness

instance PrettyA IsValid where
  prettyA (IsValid True) = "True"
  prettyA (IsValid False) = "False"

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
    , ("isValid", prettyA iv)
    , ("auxiliaryData", prettyA aux)
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
