{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Alonzo.Core (
  AlonzoEraTxOut (..),
  AlonzoEraTxBody (..),
  ScriptIntegrityHash,
  AlonzoEraPParams (..),
  CoinPerWord (..),
  ppCoinsPerUTxOWordL,
  ppCostModelsL,
  ppPricesL,
  ppMaxTxExUnitsL,
  ppMaxBlockExUnitsL,
  ppMaxValSizeL,
  ppCollateralPercentageL,
  ppMaxCollateralInputsL,
  ppuCoinsPerUTxOWordL,
  ppuCostModelsL,
  ppuPricesL,
  ppuMaxTxExUnitsL,
  ppuMaxBlockExUnitsL,
  ppuMaxValSizeL,
  ppuCollateralPercentageL,
  ppuMaxCollateralInputsL,
  module Cardano.Ledger.Mary.Core,
)
where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits, Prices)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.HKD (HKD, HKDFunctor)
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Plutus.Data (Datum)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

class (AlonzoEraPParams era, EraTxOut era) => AlonzoEraTxOut era where
  dataHashTxOutL :: Lens' (TxOut era) (StrictMaybe (DataHash (EraCrypto era)))

  datumTxOutF :: SimpleGetter (TxOut era) (Datum era)

class (MaryEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  reqSignerHashesTxBodyL :: Lens' (TxBody era) (Set (KeyHash 'Witness (EraCrypto era)))

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))

  networkIdTxBodyL :: Lens' (TxBody era) (StrictMaybe Network)

newtype CoinPerWord = CoinPerWord {unCoinPerWord :: Coin}
  deriving stock (Eq, Ord)
  deriving newtype (EncCBOR, DecCBOR, ToJSON, FromJSON, NFData, NoThunks, Show)

class EraPParams era => AlonzoEraPParams era where
  hkdCoinsPerUTxOWordL ::
    (HKDFunctor f, ExactEra AlonzoEra era) =>
    Lens' (PParamsHKD f era) (HKD f CoinPerWord)

  hkdCostModelsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f CostModels)

  hkdPricesL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Prices)

  hkdMaxTxExUnitsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f ExUnits)

  hkdMaxBlockExUnitsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f ExUnits)

  hkdMaxValSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  hkdCollateralPercentageL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  hkdMaxCollateralInputsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

ppCoinsPerUTxOWordL ::
  forall era.
  (AlonzoEraPParams era, ExactEra AlonzoEra era) =>
  Lens' (PParams era) CoinPerWord
ppCoinsPerUTxOWordL = ppLens . hkdCoinsPerUTxOWordL @era @Identity

ppCostModelsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) CostModels
ppCostModelsL = ppLens . hkdCostModelsL @era @Identity

ppPricesL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Prices
ppPricesL = ppLens . hkdPricesL @era @Identity

ppMaxTxExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) ExUnits
ppMaxTxExUnitsL = ppLens . hkdMaxTxExUnitsL @era @Identity

ppMaxBlockExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) ExUnits
ppMaxBlockExUnitsL = ppLens . hkdMaxBlockExUnitsL @era @Identity

ppMaxValSizeL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppMaxValSizeL = ppLens . hkdMaxValSizeL @era @Identity

ppCollateralPercentageL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppCollateralPercentageL = ppLens . hkdCollateralPercentageL @era @Identity

ppMaxCollateralInputsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppMaxCollateralInputsL = ppLens . hkdMaxCollateralInputsL @era @Identity

ppuCoinsPerUTxOWordL ::
  forall era.
  (AlonzoEraPParams era, ExactEra AlonzoEra era) =>
  Lens' (PParamsUpdate era) (StrictMaybe CoinPerWord)
ppuCoinsPerUTxOWordL = ppuLens . hkdCoinsPerUTxOWordL @era @StrictMaybe

ppuCostModelsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe CostModels)
ppuCostModelsL = ppuLens . hkdCostModelsL @era @StrictMaybe

ppuPricesL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Prices)
ppuPricesL = ppuLens . hkdPricesL @era @StrictMaybe

ppuMaxTxExUnitsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxTxExUnitsL = ppuLens . hkdMaxTxExUnitsL @era @StrictMaybe

ppuMaxBlockExUnitsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxBlockExUnitsL = ppuLens . hkdMaxBlockExUnitsL @era @StrictMaybe

ppuMaxValSizeL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxValSizeL = ppuLens . hkdMaxValSizeL @era @StrictMaybe

ppuCollateralPercentageL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCollateralPercentageL = ppuLens . hkdCollateralPercentageL @era @StrictMaybe

ppuMaxCollateralInputsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxCollateralInputsL = ppuLens . hkdMaxCollateralInputsL @era @StrictMaybe
