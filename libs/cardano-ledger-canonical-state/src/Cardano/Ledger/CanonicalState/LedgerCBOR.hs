{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Its a helper module, that is used to write canonical instances that
-- are lucky to match the current ledger implementation. There is no guarantee
-- that current ledger implementation will never diverge from the canonical
-- one. So it's important to run the scls conformance test for such instances.
--
-- If you use this method to derive canonical instances, make sure to add
-- a proper conformance test that covers the instance, and canonicity tests.
--
-- Example usage:
-- @
--
-- import Cardano.Ledger.Hashes ( ScriptHash )
--
-- deriving via LedgerCBOR v ScriptHash instance ToCanonicalCBOR v ScriptHash
--
-- deriving via LedgerCBOR v ScriptHash instance FromCanonicalCBOR v ScriptHash
--
-- tests = do
--    describe "GovConstitution ScriptHash canonical encoding" $ do
--      validateType @"gov/constitution/v0" @(ScriptHash) "script_hash"
-- --                  ^^^^^^^^^^^^^^^^^^                 ^^^^^^^^^^^^
-- --                   SCLS namespace                    CDDL rule name, to verify against
--      isCanonical @"common" @ScriptHash
-- --               ^^^^^^^
-- --               Namespace
-- @
module Cardano.Ledger.CanonicalState.LedgerCBOR (
  LedgerCBOR (..),
  LedgerSafeCBOR (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.CanonicalState.Namespace
import Cardano.Ledger.Core (fromEraCBOR, toEraCBOR)
import Cardano.SCLS.CBOR.Canonical (assumeCanonicalDecoder, assumeCanonicalEncoding)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), forceCanonical)
import Cardano.SCLS.Versioned
import GHC.TypeLits

-- | Helper that allows us to deriving instances via decodeTermToken CBOR representation.
--
-- Such newtype simply reuses existing EncCBOR and DecCBOR instances and does not guarantee
-- neither canonicity nor conformance with the specification, so it's important to add
-- conformance tests (@validateType@) and canonicity tests (@isCanonical@) for such instances.
--
-- Canonical ledger state uses so called deterministic cbor a canonical encoding with a few additional rules:
--  * sets (some tag) are sorted
--  * keys are sorted in lexicographical order of their byte encoding
--  * lists and byteareas use fixed length structures
--
-- It's needed for alignment of the binary encoding of the same data structure within
-- different scls implementations (different languages).
--
-- cborg library on it's own does use canonical encoding
-- (minimal size for integral values etc). If helpers from the scls-cbor library are used
-- then result encoding will satisfy all the properties, but sometimes you know that
-- existing cbor encoding already satisfies required properties, it happens when only
-- basic types and tuples are used, in this case it's possible to use 'assumeCanonicalEncoding'.
--
-- 'assumeCanonicalEncoding' means that we do not check or reencode cbor and
-- assume that whatever encoding was passed to that is canonical. But in this case
-- it would be nice to add a test that verifies that the encoding is actually canonical,
-- e.g.  'isCanonical @"gov/constitution/v0" @GovConstitution.V0.CanonicalConstitution'
-- is an example of such test.
newtype LedgerCBOR (v :: Symbol) a = LedgerCBOR {unLedgerCBOR :: a}
  deriving (Eq, Show)

instance (EncCBOR a, Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (LedgerCBOR v a) where
  toCanonicalCBOR _v (LedgerCBOR a) = assumeCanonicalEncoding $ toEraCBOR @era a

instance (DecCBOR a, Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v (LedgerCBOR v a) where
  fromCanonicalCBOR =
    Versioned . LedgerCBOR <$> assumeCanonicalDecoder (fromEraCBOR @era)

-- | Helper that allows us to deriving instances via decodeTermToken CBOR representation
newtype LedgerSafeCBOR (v :: Symbol) a = LedgerSafeCBOR {unLedgerSafeCBOR :: a}
  deriving (Eq, Show)

-- | A safer version of 'LedgerCBOR' that forces canonical encoding by re-encoding the produced value,
-- this instance is slower but guarantees canonicity of the type.
--
-- This instance does not guarantee conformance with the specification, so it's important to add
-- conformance tests (@validateType@) for such instances as well.
instance
  (EncCBOR a, Era era, NamespaceEra v ~ era) =>
  ToCanonicalCBOR v (LedgerSafeCBOR v a)
  where
  toCanonicalCBOR v (LedgerSafeCBOR a) = forceCanonical v $ toEraCBOR @era a

instance
  (DecCBOR a, Era era, NamespaceEra v ~ era) =>
  FromCanonicalCBOR v (LedgerSafeCBOR v a)
  where
  fromCanonicalCBOR =
    Versioned . LedgerSafeCBOR <$> assumeCanonicalDecoder (fromEraCBOR @era)
