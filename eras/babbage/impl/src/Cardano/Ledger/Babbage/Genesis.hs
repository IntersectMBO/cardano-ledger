{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Babbage.Genesis (
  AlonzoGenesis (..),
  extendPPWithGenesis,
)
where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Babbage.PParams (BabbagePParams, extendPP)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  ShelleyPParams era1 ->
  AlonzoGenesis ->
  BabbagePParams era2
extendPPWithGenesis
  pp
  AlonzoGenesis
    { coinsPerUTxOWord
    , costmdls
    , prices
    , maxTxExUnits
    , maxBlockExUnits
    , maxValSize
    , collateralPercentage
    , maxCollateralInputs
    } =
    extendPP
      pp
      coinsPerUTxOWord
      costmdls
      prices
      maxTxExUnits
      maxBlockExUnits
      maxValSize
      collateralPercentage
      maxCollateralInputs
