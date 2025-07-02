{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (
  MockCrypto,
) where

import Cardano.Crypto.KES (MockKES)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.API (PraosCrypto)
import Test.Cardano.Protocol.Crypto.VRF.Fake (FakeVRF)

data MockCrypto

instance Crypto MockCrypto where
  type KES MockCrypto = MockKES 10
  type VRF MockCrypto = FakeVRF

instance PraosCrypto MockCrypto
