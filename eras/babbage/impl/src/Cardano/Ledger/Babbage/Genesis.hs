{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Ledger.Babbage.Genesis
  ( AlonzoGenesis (..),
    extendPPWithGenesis,
    augmentPPWithGenesis,
  )
where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Babbage.PParams (BabbagePParams, BabbagePParamsHKD (..), extendPP)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Data.Functor.Identity (Identity)

augmentPPWithGenesis ::
  BabbagePParamsHKD Identity era1 ->
  AlonzoGenesis ->
  BabbagePParamsHKD Identity era2
augmentPPWithGenesis
  BabbagePParams
    { _minfeeA,
      _minfeeB,
      _maxBBSize,
      _maxTxSize,
      _maxBHSize,
      _keyDeposit,
      _poolDeposit,
      _eMax,
      _nOpt,
      _a0,
      _rho,
      _tau,
      _protocolVersion,
      _minPoolCost
    }
  AlonzoGenesis
    { coinsPerUTxOWord = _coinsPerUTxOByte,
      costmdls = _costmdls,
      prices = _prices,
      maxTxExUnits = _maxTxExUnits,
      maxBlockExUnits = _maxBlockExUnits,
      maxValSize = _maxValSize,
      collateralPercentage = _collateralPercentage,
      maxCollateralInputs = _maxCollateralInputs
    } =
    BabbagePParams {..}

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  ShelleyPParams era1 ->
  AlonzoGenesis ->
  BabbagePParams era2
extendPPWithGenesis
  pp
  AlonzoGenesis
    { coinsPerUTxOWord,
      costmdls,
      prices,
      maxTxExUnits,
      maxBlockExUnits,
      maxValSize,
      collateralPercentage,
      maxCollateralInputs
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
