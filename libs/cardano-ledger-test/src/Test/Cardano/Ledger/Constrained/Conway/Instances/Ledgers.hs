{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Constrained.Conway.Instances.Ledgers () where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Shelley.Rules (Identity, ShelleyLedgersEnv)
import Constrained.API
import Data.Typeable
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger (EraSpecPParams)

instance Typeable era => HasSimpleRep (ShelleyLedgersEnv era)

instance
  ( EraSpecPParams era
  , EraGov era
  , EraTxOut era
  , Eq (PParamsHKD Identity era)
  ) =>
  HasSpec (ShelleyLedgersEnv era)
