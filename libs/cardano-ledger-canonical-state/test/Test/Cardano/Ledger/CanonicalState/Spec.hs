{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.CanonicalState.Spec (spec) where

import Cardano.Ledger.CanonicalState.Conway ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.Nonces.V0 as Nonces.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 as UTxO.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Testlib
import Data.Typeable
import GHC.TypeLits
import Test.Cardano.Ledger.CanonicalState.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary ()

spec :: Spec
spec = do
  describe "types" $ do
    describe "blocks/v0" $ do
      isCanonical @"blocks/v0" @Blocks.V0.BlockOut
      validateType @"blocks/v0" @Blocks.V0.BlockOut "record_entry"
    describe "utxo/v0" $ do
      isCanonical @"utxo/v0" @(UTxO.V0.UtxoOut ConwayEra)
      validateType @"utxo/v0" @(UTxO.V0.UtxoOut ConwayEra) "record_entry"
    describe "nonces/v0" $ do
      isCanonical @"nonces/v0" @Nonces.V0.NoncesState
      validateType @"nonces/v0" @Nonces.V0.NoncesState "record_entry"
    describe "gov/constitution/v0" $ do
      isCanonical @"gov/constitution/v0" @GovConstitution.V0.CanonicalConstitution
      validateType @"gov/constitution/v0" @GovConstitution.V0.CanonicalConstitution "record_entry"
  describe "namespaces" $ do
    testNS @"blocks/v0"
    testNS @"utxo/v0"
    testNS @"gov/constitution/v0"
    testNS @"nonces/v0"

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = prop propName $ propTypeIsCanonical @ns @a
  where
    propName = showsTypeRep (typeRep (Proxy @a)) " is canonical"
