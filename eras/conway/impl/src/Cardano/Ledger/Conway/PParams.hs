{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Conway.PParams
  ( BabbagePParamsHKD (..),
    BabbagePParams,
    emptyPParams,
    BabbagePParamsUpdate,
    emptyPParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    retractPP,
    extendPP,
  )
where

import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraPParams (ConwayEra c) where
  type PParams (ConwayEra c) = BabbagePParams (ConwayEra c)
  type PParamsUpdate (ConwayEra c) = BabbagePParamsUpdate (ConwayEra c)
  applyPPUpdates = updatePParams
