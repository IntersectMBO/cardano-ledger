{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Pool where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Conway.TreeDiff

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (PoolEnv era)
  where
  type SpecRep (PoolEnv era) = Agda.PParams

  toSpecRep (PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate ctx (ShelleyPoolPredFailure era) where
  type SpecRep (ShelleyPoolPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e
