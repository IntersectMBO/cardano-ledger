{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Babbage.Genesis
  ( AlonzoGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Babbage.PParams
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Data.Functor.Identity

-- | Given the missing pieces turn a Shelley.PParams' into an Params'
extendPPWithGenesis ::
  Shelley.PParams' Identity era1 ->
  AlonzoGenesis ->
  PParams' Identity era2
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
