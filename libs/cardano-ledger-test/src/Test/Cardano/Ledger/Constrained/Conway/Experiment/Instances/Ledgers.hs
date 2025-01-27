{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Constrained.Conway.Experiment.Instances.Ledgers () where

import Cardano.Ledger.Conway.Core (EraPParams (..))
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv)
import Constrained.Experiment.API
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Instances.Ledger (EraSpecPParams)

instance Typeable era => HasSimpleRep (ShelleyLedgersEnv era)
instance
  ( EraSpecPParams era
  , Eq (PParamsHKD Identity era)
  ) =>
  HasSpec (ShelleyLedgersEnv era)
