{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Cardano.Ledger.Alonzo.PParams.Class where
import Cardano.Ledger.PParams
import Lens.Micro (Lens')
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Coin (Coin(..))
import Cardano.Ledger.Alonzo.Scripts (CostModels(..), Prices (..), ExUnits (..))
import Numeric.Natural (Natural)
import Control.Monad.Identity (Identity)
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Core (ProtVerAtMost)

class EraPParams era => AlonzoEraPParams era where
  hkdCoinsPerUTxOWordL :: ProtVerAtMost era 6 => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdCostmdlsL :: Lens' (PParamsHKD f era) (HKD f CostModels)
  hkdPricesL :: Lens' (PParamsHKD f era) (HKD f Prices)
  hkdMaxTxExUnitsL :: Lens' (PParamsHKD f era) (HKD f ExUnits)
  hkdMaxBlockExUnitsL :: Lens' (PParamsHKD f era) (HKD f ExUnits)
  hkdMaxValSizeL :: Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCollateralPercentageL :: Lens' (PParamsHKD f era) (HKD f Natural)
  hkdMaxCollateralInputsL :: Lens' (PParamsHKD f era) (HKD f Natural)

ppCoinsPerUTxOWordL :: forall era. (AlonzoEraPParams era, ProtVerAtMost era 6) => Lens' (PParams era) Coin
ppCoinsPerUTxOWordL = ppLens . hkdCoinsPerUTxOWordL @era @Identity

ppCostmdlsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) CostModels
ppCostmdlsL = ppLens . hkdCostmdlsL @era @Identity

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

ppuCoinsPerUTxOWordL :: forall era. (AlonzoEraPParams era, ProtVerAtMost era 6) => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuCoinsPerUTxOWordL = ppuLens . hkdCoinsPerUTxOWordL @era @StrictMaybe

ppuCostmdlsL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe CostModels)
ppuCostmdlsL = ppuLens . hkdCostmdlsL @era @StrictMaybe

ppuPricesL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Prices)
ppuPricesL = ppuLens . hkdPricesL @era @StrictMaybe

ppuMaxTxExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxTxExUnitsL = ppuLens . hkdMaxTxExUnitsL @era @StrictMaybe

ppuMaxBlockExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxBlockExUnitsL = ppuLens . hkdMaxBlockExUnitsL @era @StrictMaybe

ppuMaxValSizeL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxValSizeL = ppuLens . hkdMaxValSizeL @era @StrictMaybe

ppuCollateralPercentageL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCollateralPercentageL = ppuLens . hkdCollateralPercentageL @era @StrictMaybe

ppuMaxCollateralInputsL :: forall era. AlonzoEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxCollateralInputsL = ppuLens . hkdMaxCollateralInputsL @era @StrictMaybe
