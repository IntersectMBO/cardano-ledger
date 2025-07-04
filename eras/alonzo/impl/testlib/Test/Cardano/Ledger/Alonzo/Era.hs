{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Era (
  module Test.Cardano.Ledger.Mary.Era,
  AlonzoEraTest,
  alonzoValidTxOut,
  alonzoGenPParams,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Scripts (ExUnits, isPlutusScript)
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (Language (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.TreeDiff
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen, chooseInt, elements)
import Test.Cardano.Ledger.Era (GenSize (..))
import Test.Cardano.Ledger.Mary.Era
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( MaryEraTest era
  , EraPlutusContext era
  , AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , AlonzoEraUTxO era
  , ToExpr (PlutusScript era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsIxItem era)
  , Script era ~ AlonzoScript era
  ) =>
  AlonzoEraTest era

alonzoValidTxOut ::
  ( EraTxOut era
  , AlonzoEraScript era
  ) =>
  Map ScriptHash (Script era) -> TxOut era -> Bool
alonzoValidTxOut scripts txOut = case txOut ^. addrTxOutL of
  Addr _ KeyHashObj {} _ -> True
  Addr _ (ScriptHashObj sh) _ ->
    case Map.lookup sh scripts of
      Just s -> isPlutusScript s
      _ -> False
  AddrBootstrap {} -> False

alonzoGenPParams :: forall era. AlonzoEraTest era => GenSize -> Gen (PParams era)
alonzoGenPParams gsize = do
  pp <- shelleyGenPParams
  maxTxExUnits <- arbitrary :: Gen ExUnits
  maxCollateralInputs <- elements [1 .. collInputsMax gsize]
  collateralPercentage <- fromIntegral <$> chooseInt (1, 10000)
  pure $
    pp
      & ppMaxTxExUnitsL .~ maxTxExUnits
      & ppCostModelsL .~ zeroCostModels @era
      & ppMaxValSizeL .~ 1000
      & ppMaxCollateralInputsL .~ maxCollateralInputs
      & ppCollateralPercentageL .~ collateralPercentage

instance EraTest AlonzoEra where
  validTxOut = alonzoValidTxOut
  zeroCostModels = zeroTestingCostModels [PlutusV1]
  genPParams = alonzoGenPParams

instance ShelleyEraTest AlonzoEra

instance AllegraEraTest AlonzoEra

instance MaryEraTest AlonzoEra

instance AlonzoEraTest AlonzoEra
