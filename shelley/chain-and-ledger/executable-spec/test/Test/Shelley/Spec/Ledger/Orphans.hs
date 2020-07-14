{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Orphans () where

import Cardano.Binary (serializeEncoding', toCBOR)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.OCert (KESPeriod (..))

-- We need this here for the tests, but should not be in the actual library because
-- a Num instance for this type does not make sense in the general case.
deriving instance Num (DSIGN.VerKeyDSIGN (DSIGN crypto)) => Num (VKey kd crypto)

instance (KES.KESAlgorithm c) => SignableRepresentation (KES.VerKeyKES c, Natural, KESPeriod) where
  getSignableRepresentation (vk, nat, KESPeriod p) =
    serializeEncoding' $
      KES.encodeVerKeyKES vk <> toCBOR nat <> toCBOR p
