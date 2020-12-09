{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module exports pieces one may use to make a new Era.
--   The pieces come in 3 flavors:
--   1) Crypto
--   2) Era's
--   3) Type families
--   Some of these pieces are pre-specialized to the Shelley,
--   Allegra, and Mary Eras. Some are trivial datatypes that one
--   may use as type instances for the type families when nothing
--   more specific is needed. One may make your own Era or just use
--   some of the pieces, which have been collected here for convenience.
--   To make a new Era, import this file, then write something like this:
--   data MyEra
--   instance Era MyEra where
--   type Crypto MyEra = TestCrypto
--   type instance Value MyEra = ConcreteValue.Value MyEra
--   type instance Script MyEra = TestScript
--   type instance TxBody MyEra = Mary.TxBody MyEra
module Test.Cardano.Ledger.EraBuffet
  ( TestCrypto, -- These are two crypto versions
    StandardCrypto,
    ShelleyEra, -- These are the crypto parameterized Eras re-exported for convenience.
    MaryEra, -- one needs to apply these to a crypto be be concrete
    AllegraEra,
    Value, -- These are the type families re-exported for convenience.
    Script,
    TxBody,
    AuxiliaryData,
    Era (..), -- The Era class re-exported
  )
where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, MockDSIGN)
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, MD5Prefix)
import Cardano.Crypto.KES (MockKES, Sum6KES)
import Cardano.Crypto.VRF.Praos
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core (Script, TxBody, Value, AuxiliaryData)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Shelley.Spec.Ledger.API (PraosCrypto)
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

{------------------------------------------------------------------------------
 First construct concrete versions of Crypto where the Hashing
 is concrete. Without this we won't be able to Hash things
------------------------------------------------------------------------------}
data TestCrypto

instance CryptoClass.Crypto TestCrypto where
  type HASH TestCrypto = MD5Prefix 10
  type ADDRHASH TestCrypto = MD5Prefix 8
  type DSIGN TestCrypto = MockDSIGN
  type KES TestCrypto = MockKES 10
  type VRF TestCrypto = FakeVRF

instance PraosCrypto TestCrypto

data StandardCrypto

instance CryptoClass.Crypto StandardCrypto where
  type DSIGN StandardCrypto = Ed25519DSIGN
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF
  type HASH StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224

instance PraosCrypto StandardCrypto

{------------------------------------------------------------------------------
 Example concrete Eras:

  type ShelleyTest = ShelleyEra TestCrypto

  type ShelleyStandard = ShelleyEra StandardCrypto

  type MaryTest = MaryEra TestCrypto

  type MaryStandard = MaryEra StandardCrypto

  type AllegraTest = AllegraEra TestCrypto

  type AllegraStandard = AllegraEra StandardCrypto
------------------------------------------------------------------------------}
