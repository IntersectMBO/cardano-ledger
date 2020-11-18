{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-}

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
--
module Test.Cardano.Ledger.EraBuffet
  ( TestCrypto,   -- These are two crypto versions
    StandardCrypto,
    ShelleyEra,   -- These are the crypto parameterized Eras re-exported for convenience.
    MaryEra,      -- one needs to apply these to a crypto be be concrete
    AllegraEra,
    ShelleyTest,      -- These are concrete Era's for Shelley, Allegra, and Mary
    ShelleyStandard,  -- fixed on one of the concrete crypto pieces
    MaryTest,
    MaryStandard,
    AllegraTest,
    AllegraStandard,
    Value,        -- These are the type families re-exported for convenience.
    Script,
    TxBody,
    TestValue,    -- These are trivial types useable for type families when they don't matter
    TestScript,
    TestTxBody,
    ShelleyTxBody, -- These are the concrete types used by Eras Shelley, Mary and Allegra
    ShelleyValue,  -- for type family instances. These need to be applied to a concrete
    ShelleyScript, -- Era to be fully concrete.
    AllegraValue,
    AllegraTxBody,
    AllegraScript,
    MaryValue,
    MaryTxBody,
    MaryScript,
    Era(..),       -- The Era class re-exported
  ) where


import Shelley.Spec.Ledger.API (PraosCrypto)
import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..),serializeEncoding')
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import Cardano.Crypto.VRF.Praos
import Cardano.Ledger.Core (Script, TxBody, Value)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Mary.Value ()
import qualified Cardano.Ledger.Mary.Value as MaryValue
import Cardano.Crypto.Hash (MD5Prefix)
import Cardano.Crypto.DSIGN (MockDSIGN, Ed25519DSIGN)
import Cardano.Crypto.KES (MockKES, Sum6KES)
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)
import Shelley.Spec.Ledger.Coin(Coin)
import qualified Shelley.Spec.Ledger.TxBody as ShelleyBody
import qualified Shelley.Spec.Ledger.Scripts as ShelleyScript
import qualified Cardano.Ledger.ShelleyMA.TxBody as MABody
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MAScript
import Shelley.Spec.Ledger.Keys(KeyHash,KeyRole(..))
import Cardano.Ledger.Mary(MaryEra)
import Cardano.Ledger.Allegra(AllegraEra)
import Cardano.Ledger.Shelley(ShelleyEra)

import Data.Hashable
import Test.Shelley.Spec.Ledger.Generator.Scripts(ScriptClass(..),Quantifier(..))
import Data.Sequence.Strict (fromList)
import Cardano.Slotting.Slot (SlotNo (..))

-- ===========================================================
-- First construct concrete versions of Crypto where the Hashing
-- is concrete. Without this we won't be able to Hash things

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

-- ==================================================================
-- These are the concrete Eras. Two each for Shelley, Mary, Allegra
-- In each pair a particular kind of crypto is chosen.

type ShelleyTest     = ShelleyEra TestCrypto
type ShelleyStandard = ShelleyEra StandardCrypto
type MaryTest        = MaryEra TestCrypto
type MaryStandard    = MaryEra StandardCrypto
type AllegraTest     = AllegraEra TestCrypto
type AllegraStandard = AllegraEra StandardCrypto

-- ==================================================================
-- These are the concrete types used to instantiate the type families
-- Value, Script, and TxBody for Eras Shelley, Allegra, and Mary

type ShelleyTxBody era = ShelleyBody.TxBody era
type ShelleyValue era = Coin
type ShelleyScript era = ShelleyScript.MultiSig era

type AllegraValue era = Coin
type AllegraTxBody era = MABody.TxBody era
type AllegraScript era = MAScript.Timelock era

type MaryValue era = MaryValue.Value era
type MaryTxBody era = MABody.TxBody era
type MaryScript era = MAScript.Timelock era

-- ===============================================================
-- Now some trivial types users may choose as type family instances
-- if they don't need something more specific, they may always choose
-- other types if they want to.

type TestValue = MaryValue.Value

data TestScript = TestScript

data TestTxBody = TestTxBody

instance FromCBOR TestScript where fromCBOR = pure TestScript

instance ToCBOR TestScript where toCBOR TestScript = mempty

instance FromCBOR (Annotator TestScript) where fromCBOR = pure <$> fromCBOR

instance FromCBOR TestTxBody where fromCBOR = pure TestTxBody

instance ToCBOR TestTxBody where toCBOR TestTxBody = mempty

instance FromCBOR (Annotator TestTxBody) where fromCBOR = pure <$> fromCBOR

-- =====================================================================
-- An example of how one might use this to make their own Era
--This exaple is not meant to make any semantic sense, but it illustrates
-- how one may choose pieces from the buffet to make an Era.
{-
data TestEra
instance Era TestEra where
  type Crypto TestEra = TestCrypto
type instance Value TestEra = Coin
type instance TxBody TestEra = MaryTxBody TestEra
type instance Script TestEra = TestScript
-}

-- ==========================================================================================
-- I am not sure that this stuff belongs here. But it does have one thing in common with the
-- other stuff here. Both groups have to be able to see all the source code of every Era. and
-- that the tests of every Era (if they are Era generic) have to be able to see this code.

-- =====================================
-- EraGen instaces for the MaryEra
-- =====================================

instance (CryptoClass.Crypto c) => ScriptClass (MaryEra c) where
  isKey _ (MAScript.RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

-- =====================================
-- EraGen instaces for the AllegraEra
-- =====================================

instance ( CryptoClass.Crypto c) => ScriptClass (AllegraEra c) where
  isKey _ (MAScript.RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

-- ========================================================
-- Reusable pieces for both Mary and Allegra

quantifyTL:: Era era => MAScript.Timelock era -> Quantifier (MAScript.Timelock era)
quantifyTL (MAScript.RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (MAScript.RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (MAScript.RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL:: Era era => Quantifier (MAScript.Timelock era) -> MAScript.Timelock era
unQuantifyTL (AllOf xs) = (MAScript.RequireAllOf (fromList xs))
unQuantifyTL (AnyOf xs) = (MAScript.RequireAnyOf (fromList xs))
unQuantifyTL (MOf n xs) = (MAScript.RequireMOf n (fromList xs))
unQuantifyTL (Leaf t) = t

-- | Generate some Leaf Timelock (i.e. a Signature or TimeStart or TimeExpire)
someLeaf :: Era era => KeyHash 'Witness (Crypto era) -> MAScript.Timelock era
someLeaf x =
    let n = mod (hash(serializeEncoding' (toCBOR x))) 200
    in if n <= 50
          then MAScript.RequireTimeStart (SlotNo (fromIntegral n))
          else if n > 150
                  then MAScript.RequireTimeExpire (SlotNo (fromIntegral n))
                  else MAScript.RequireSignature x