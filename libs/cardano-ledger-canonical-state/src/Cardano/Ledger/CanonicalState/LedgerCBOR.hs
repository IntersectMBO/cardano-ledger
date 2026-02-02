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
--    descripe "GovConstitution ScriptHash canonical encoding" $ do
--      validateType @"gov/constitution/v0" @(ScriptHash) "script_hash"
-- --                  ^^^^^^^^^^^^^^^^^^                 ^^^^^^^^^^^^
-- --                   SCLS namespace                    CDDL rule name, to verify against
--     isCanonical @"common" @ScriptHash
--                  ^^^^^^^
--                   Namespace
-- @
module Cardano.Ledger.CanonicalState.LedgerCBOR (
  LedgerCBOR (..),
  LedgerCBORSafe (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.CanonicalState.Namespace
import Cardano.Ledger.Core (eraProtVerLow)
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
newtype LedgerCBOR (v :: Symbol) a = LedgerCBOR {unLedgerCBOR :: a}
  deriving (Eq, Show)

instance (EncCBOR a, Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (LedgerCBOR v a) where
  toCanonicalCBOR _v (LedgerCBOR a) = assumeCanonicalEncoding $ toPlainEncoding (eraProtVerLow @era) (encCBOR a)

instance (DecCBOR a, Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v (LedgerCBOR v a) where
  fromCanonicalCBOR =
    Versioned . LedgerCBOR
      <$> (assumeCanonicalDecoder $ toPlainDecoder Nothing (eraProtVerLow @era) decCBOR)

-- | Helper that allows us to deriving instances via decodeTermToken CBOR representation
newtype LedgerCBORSafe (v :: Symbol) a = LedgerCBORSafe {unLedgerCBORSafe :: a}
  deriving (Eq, Show)

-- | A safer version of 'LedgerCBOR' that forces canonical encoding by re-encoding the produced value,
-- this instance is slower but guarantees canonicity of the type.
--
-- This instance do not guarantee conformance with the specification, so it's important to add
-- conformance tests (@validateType@) for such instances as well.
instance
  (EncCBOR a, Era era, NamespaceEra v ~ era) =>
  ToCanonicalCBOR v (LedgerCBORSafe v a)
  where
  toCanonicalCBOR v (LedgerCBORSafe a) = forceCanonical v $ toPlainEncoding (eraProtVerLow @era) (encCBOR a)

instance
  (DecCBOR a, Era era, NamespaceEra v ~ era) =>
  FromCanonicalCBOR v (LedgerCBORSafe v a)
  where
  fromCanonicalCBOR =
    Versioned . LedgerCBORSafe
      <$> (assumeCanonicalDecoder $ toPlainDecoder Nothing (eraProtVerLow @era) decCBOR)
