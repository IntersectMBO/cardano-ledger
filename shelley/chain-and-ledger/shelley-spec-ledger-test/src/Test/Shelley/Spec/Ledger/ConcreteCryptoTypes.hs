{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.ConcreteCryptoTypes where

import Cardano.Crypto.DSIGN (MockDSIGN, VerKeyDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.Hash (ShortHash)
import Cardano.Crypto.KES (MockKES)
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Shelley.Spec.Ledger.Crypto
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)
import Shelley.Spec.Ledger.BaseTypes (Seed)

data C

-- | Mocking constraints used in generators
type Mock c =
  ( Crypto c,
    KES.Signable (KES c) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN c)
      ~ SignableRepresentation,
    VRF.Signable
      (VRF c)
      Seed
  )

-- | Additional mocking constraints used in examples.
type ExMock c =
  ( Mock c,
    Num (DSIGN.SignKeyDSIGN (DSIGN c)),
    Num (VerKeyDSIGN (DSIGN c)),
    (VRF c) ~ FakeVRF
  )

instance Crypto C where
  type HASH C = ShortHash
  type ADDRHASH C = ShortHash
  type DSIGN C = MockDSIGN
  type KES C = MockKES 10
  type VRF C = FakeVRF
