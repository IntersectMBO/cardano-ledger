{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Constrained.Conway.Instances.Ledgers () where

import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv)
import Constrained (HasSimpleRep, HasSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (EraSpecPParams, IsConwayUniv)

instance HasSimpleRep (ShelleyLedgersEnv era)
instance
  ( EraSpecPParams era
  , IsConwayUniv fn
  , Eq (PParamsHKD Identity era)
  ) =>
  HasSpec fn (ShelleyLedgersEnv era)
