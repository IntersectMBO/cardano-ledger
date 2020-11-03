{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


-- | This module sets up a concrete Era for serialisation Tests.
--   Users must make there own instances of the Era specific type families.
--   The module provides some simple data types they can use, if
--    their testing needs do not need complicated ones.
--   Users make instances like these in the file that imports this file
--
--   data MyEra
--   instance Era MyEra where
--   type Crypto MyEra = TestCrypto
--   type instance Value MyEra = ConcreteValue.Value MyEra
--   type instance Script MyEra = TestScript
--   type instance TxBody MyEra = Mary.TxBody MyEra
--
--   They may also skip the first three lines and Just use TestEra
--   which is predefined.

module Test.Cardano.Ledger.ShelleyMA.TestEra
  ( TestEra,
    TestCrypto,
    TestValue,
    TestScript,
    TestTxBody,
    Value,  -- These are the type families re-exported for convenience.
    Script,
    TxBody,
  ) where

import Shelley.Spec.Ledger.API (PraosCrypto)
import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import Cardano.Crypto.KES
import Cardano.Crypto.VRF.Praos
import Cardano.Ledger.Core (Script, TxBody, Value)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Mary.Value ()
import qualified Cardano.Ledger.Mary.Value as ConcreteValue

-- First make a fully concrete Era where the Hashing is concrete
-- without this we won't be able to Serialize or Hash TxID.

data TestCrypto

instance CryptoClass.Crypto TestCrypto where
  type DSIGN TestCrypto = Ed25519DSIGN
  type KES TestCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF TestCrypto = PraosVRF
  type HASH TestCrypto = Blake2b_256
  type ADDRHASH TestCrypto = Blake2b_224

instance PraosCrypto TestCrypto

-- Now make a new example Era instance. Users will need to choose type family instances
-- for Value, Script, and TxBody. They may also make their own Era instances by cloning
-- the following 3 lines. Replacing TestEra with something else.

data TestEra

instance Era TestEra where
  type Crypto TestEra = TestCrypto

-- ===============================================================
-- Now some simple types users may choose as type family instances
-- if they don't need something more specific, they may always choose
-- other types if they want to

type TestValue = ConcreteValue.Value

data TestScript = TestScript

data TestTxBody = TestTxBody

instance FromCBOR TestScript where fromCBOR = pure TestScript

instance ToCBOR TestScript where toCBOR TestScript = mempty

instance FromCBOR (Annotator TestScript) where fromCBOR = pure <$> fromCBOR

instance FromCBOR TestTxBody where fromCBOR = pure TestTxBody

instance ToCBOR TestTxBody where toCBOR TestTxBody = mempty

instance FromCBOR (Annotator TestTxBody) where fromCBOR = pure <$> fromCBOR
