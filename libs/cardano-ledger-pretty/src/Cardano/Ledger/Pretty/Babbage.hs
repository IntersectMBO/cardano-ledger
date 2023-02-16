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
import Cardano.Ledger.Crypto
import Cardano.Ledger.Pretty.Alonzo ()
import Control.State.Transition.Extended
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Lens.Micro
import Prettyprinter ((<+>))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Alonzo ()

-- =====================================

instance Crypto c => PrettyA (PParams (BabbageEra c)) where
  prettyA pp =
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
      , ("protocolVersion", prettyA $ pp ^. ppProtocolVersionL)
      , ("minPoolCost", prettyA $ pp ^. ppMinPoolCostL)
      , ("coinPerByte", prettyA $ pp ^. ppCoinsPerUTxOByteL)
      , ("costmdls", prettyA $ pp ^. ppCostModelsL)
      , ("prices", prettyA $ pp ^. ppPricesL)
      , ("maxTxExUnits", prettyA $ pp ^. ppMaxTxExUnitsL)
      , ("maxBlockExUnits", prettyA $ pp ^. ppMaxBlockExUnitsL)
      , ("maxValSize", prettyA $ pp ^. ppMaxValSizeL)
      , ("collateral%", prettyA $ pp ^. ppCollateralPercentageL)
      , ("maxCollateralInputs", prettyA $ pp ^. ppMaxCollateralInputsL)
      ]

instance Crypto c => PrettyA (PParamsUpdate (BabbageEra c)) where
  prettyA pp =
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
      , ("protocolVersion", prettyA $ pp ^. ppuProtocolVersionL)
      , ("minPoolCost", prettyA $ pp ^. ppuMinPoolCostL)
      , ("coinPerByte", prettyA $ pp ^. ppuCoinsPerUTxOByteL)
      , ("costmdls", prettyA $ pp ^. ppuCostModelsL)
      , ("prices", prettyA $ pp ^. ppuPricesL)
      , ("maxTxExUnits", prettyA $ pp ^. ppuMaxTxExUnitsL)
      , ("maxBlockExUnits", prettyA $ pp ^. ppuMaxBlockExUnitsL)
      , ("maxValSize", prettyA $ pp ^. ppuMaxValSizeL)
      , ("collateral%", prettyA $ pp ^. ppuCollateralPercentageL)
      , ("maxCollateralInputs", prettyA $ pp ^. ppuMaxCollateralInputsL)
      ]


instance
  ( PrettyA (AlonzoUtxoPredFailure era)
  , PrettyA (TxOut era)
  ) =>
  PrettyA (BabbageUtxoPredFailure era)
  where
    prettyA (AlonzoInBabbageUtxoPredFailure x) = prettyA x
    prettyA (IncorrectTotalCollateralField c1 c2) =
      ppRecord
        "IncorrectTotalCollateralField"
        [("collateral provided", prettyA c1), ("collateral declared", prettyA c2)]
    prettyA (BabbageOutputTooSmallUTxO xs) =
      ppSexp "BabbageOutputTooSmallUTxO" [prettyA xs]

instance
  ( PrettyA (AlonzoUtxowPredFailure era)
  , PrettyA (PredicateFailure (EraRule "UTXO" era))
  ) =>
  PrettyA (BabbageUtxowPredFailure era)
  where
    prettyA (AlonzoInBabbageUtxowPredFailure pf) = prettyA pf
    prettyA (UtxoFailure pf) = prettyA pf
    prettyA (MalformedScriptWitnesses scripts) =
      ppSexp "MalformedScriptWitnesses" [prettyA scripts]
    prettyA (MalformedReferenceScripts scripts) =
      ppSexp "MalformedReferenceScripts" [prettyA scripts]

instance
  ( EraTxOut era
  , EraScript era
  , PrettyA (Script era)
  , PrettyA (Value era)
  ) =>
  PrettyA (BabbageTxOut era)
  where
  prettyA (BabbageTxOut addr val datum mscript) =
    ppRecord
      "TxOut"
      [ ("address", prettyA addr)
      , ("value", prettyA val)
      , ("datum", prettyA datum)
      ,
        ( "reference script"
        , case mscript of
            (SJust s) -> prettyA s <+> prettyA (hashScript @era s)
            SNothing -> "?-"
        )
      ]

instance Era era => PrettyA (Datum era)

instance Era era => PrettyA (BinaryData era) where 
  prettyA = prettyA . binaryDataToData

instance
  ( EraTxOut era
  , PrettyA (TxOut era)
  , PrettyA (PParamsUpdate era)
  ) =>
  PrettyA (BabbageTxBody era)
  where
  prettyA x =
    ppRecord
      "TxBody(Babbage)"
      [ ("spending inputs", prettyA (spendInputs' x))
      , ("collateral inputs", prettyA (collateralInputs' x))
      , ("reference inputs", prettyA (referenceInputs' x))
      , ("outputs", prettyA (outputs' x))
      , ("collateral return", prettyA (collateralReturn' x))
      , ("total collateral", prettyA (totalCollateral' x))
      , ("certificates", prettyA (certs' x))
      , ("withdrawals", prettyA (withdrawals' x))
      , ("txfee", prettyA (txfee' x))
      , ("vldt", prettyA (vldt' x))
      , ("update", prettyA (update' x))
      , ("reqSignerHashes", prettyA (reqSignerHashes' x))
      , ("mint", prettyA (mint' x))
      , ("scriptIntegrityHash", prettyA (scriptIntegrityHash' x))
      , ("adHash", prettyA (adHash' x))
      , ("txnetworkid", prettyA (txnetworkid' x))
      ]

instance PrettyA CoinPerByte where
  prettyA = prettyA . unCoinPerByte
