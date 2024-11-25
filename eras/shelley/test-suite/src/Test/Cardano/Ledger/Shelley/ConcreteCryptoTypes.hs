{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (
  Mock,
  ExMock,
  C_Crypto,
  C,
  TestCrypto,
  StandardCrypto,
)
where

import Cardano.Crypto.DSIGN (VerKeyDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES (MockKES)
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (Seed)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Test.Cardano.Protocol.Crypto.VRF.Fake (FakeVRF)

-- | Mocking constraints used in generators
type Mock c =
  ( PraosCrypto c
  , KES.Signable (KES c) ~ SignableRepresentation
  , DSIGN.Signable DSIGN ~ SignableRepresentation
  , VRF.Signable (VRF c) Seed
  )

-- | Additional mocking constraints used in examples.
type ExMock c =
  ( Mock c
  , Num (DSIGN.SignKeyDSIGN DSIGN)
  , Num (VerKeyDSIGN DSIGN)
  , VRF c ~ FakeVRF
  )

type C = ShelleyEra

type TestCrypto = C_Crypto

data C_Crypto

instance Cardano.Ledger.Crypto.Crypto C_Crypto where
  type KES C_Crypto = MockKES 10
  type VRF C_Crypto = FakeVRF

instance PraosCrypto C_Crypto
