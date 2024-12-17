{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (
  MockCrypto,
  C,
)
where

import Cardano.Crypto.KES (MockKES)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Test.Cardano.Protocol.Crypto.VRF.Fake (FakeVRF)

type C = ShelleyEra

data MockCrypto

instance Crypto MockCrypto where
  type KES MockCrypto = MockKES 10
  type VRF MockCrypto = FakeVRF

instance PraosCrypto MockCrypto
