{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Orphans () where

import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Crypto (DSIGN)
import Shelley.Spec.Ledger.Keys

-- We need this here for the tests, but should not be in the actual library because
-- a Num instance for this type does not make sense in the general case.
deriving instance Num (DSIGN.VerKeyDSIGN (DSIGN crypto)) => Num (VKey kd crypto)
