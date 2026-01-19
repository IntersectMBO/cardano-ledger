{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Cardano.Ledger.SCLS.Spec (spec) where

-- import Cardano.Ledger.Address (CompactAddr)
-- import Cardano.Ledger.Allegra.Scripts (Timelock (..))
-- import Cardano.Ledger.Alonzo.Scripts ({-AlonzoScript,-} Prices)
-- import Cardano.Ledger.Conway (ConwayEra)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.SCLS.Arbitrary ()
import Cardano.Ledger.SCLS.BaseTypes
-- import Cardano.Ledger.Core
-- import Cardano.Ledger.Credential (Credential (..))
-- import qualified Cardano.Ledger.Hashes as H
-- import Cardano.Ledger.Mary.Value
-- import Cardano.Ledger.Plutus.CostModels
-- import Cardano.Ledger.Plutus.Data (BinaryData)
-- import Cardano.Ledger.Shelley.TxOut qualified as Shelley
import Cardano.SCLS.Testlib
import Data.Typeable
import GHC.TypeLits
import Test.Cardano.Ledger.Common

spec :: SpecWith ()
spec = do
    describe "BaseTypes" $ do
        isCanonical @"base" @Anchor
        validateType @"gov/committee/v0" @(Anchor) "anchor"
        isCanonical @"base" @EpochNo
        validateType @"gov/proposals/v0" @(EpochNo) "epoch_no"
        isCanonical @"base" @EpochInterval
        validateType @"gov/proposals/v0" @(EpochInterval) "epoch_interval"
        isCanonical @"base" @(NonNegativeInterval)
        validateType @"gov/proposals/v0" @(NonNegativeInterval) "nonnegative_interval"
        isCanonical @"base" @(Port)
        validateType @"snapshots/v0" @(Port) "port"
        isCanonical @"base" @(ProtVer)
        validateType @"gov/pparams/v0" @(ProtVer) "protocol_version"
        isCanonical @"base" @(SlotNo)
        validateType @"nonces/v0" @(SlotNo) "slot_no"
        isCanonical @"base" @(UnitInterval)
        validateType @"gov/pparams/v0" @(UnitInterval) "unit_interval"
        isCanonical @"base" @(Url)
        validateType @"snapshots/v0" @(Url) "url"
        isCanonical @"base" @(DnsName)
        validateType @"snapshots/v0" @(DnsName) "dns_name"

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = go
  where
    typ = showsTypeRep (typeRep (Proxy @a))
    go = prop (typ " is canonical") $ propTypeIsCanonical @ns @a