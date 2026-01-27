{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.CanonicalState.Spec (spec) where

import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import Cardano.Ledger.Conway.CanonicalState.Namespace.UTxO.V0 ()
import qualified Cardano.Ledger.Conway.CanonicalState.Namespace.UTxO.V0 as UTxO.V0
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
      isCanonical @"utxo/v0" @(UTxO.V0.UtxoOut (UTxO.V0.BabbageTxOut UTxO.V0.ConwayEra))
      validateType @"utxo/v0" @(UTxO.V0.UtxoOut (UTxO.V0.BabbageTxOut UTxO.V0.ConwayEra)) "record_entry"
  describe "namespaces" $ do
    testNS @"blocks/v0"
    testNS @"utxo/v0"

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = prop propName $ propTypeIsCanonical @ns @a
  where
    propName = showsTypeRep (typeRep (Proxy @a)) " is canonical"
