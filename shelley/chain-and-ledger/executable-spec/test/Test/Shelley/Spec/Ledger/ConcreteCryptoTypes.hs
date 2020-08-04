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
import Shelley.Spec.Ledger.Crypto
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

data C

type Mock c =
  ( Crypto c,
    Num (DSIGN.SignKeyDSIGN (DSIGN c)),
    Num (VerKeyDSIGN (DSIGN c)),
    (VRF c) ~ FakeVRF,
    KES.Signable (KES c) ~ SignableRepresentation,
    DSIGN.Signable (DSIGN c) ~ SignableRepresentation
  )

instance Crypto C where
  type HASH C = ShortHash
  type ADDRHASH C = ShortHash
  type DSIGN C = MockDSIGN
  type KES C = MockKES 10
  type VRF C = FakeVRF
