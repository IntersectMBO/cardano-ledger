{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.CanonicalState.Spec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval, NonNegativeInterval, UnitInterval)
import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalExUnits (..))
import Cardano.Ledger.CanonicalState.Conway ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 as Blocks.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 as Committee.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 as GovPParams.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.PoolStake.V0 as PoolStake.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.Snapshots.V0 as Snapshots.V0 ()
import qualified Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 as UTxO.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (PParams)
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
    describe "gov/committee/v0" $ do
      isCanonical @"gov/committee/v0" @Committee.V0.CanonicalCommitteeState
      validateType @"gov/committee/v0" @Committee.V0.CanonicalCommitteeState "committee"
      isCanonical @"gov/committee/v0" @Committee.V0.CanonicalCommitteeAuthorization
      validateType @"gov/committee/v0" @Committee.V0.CanonicalCommitteeAuthorization
        "committee_authorization"
    describe "gov/constitution/v0" $ do
      isCanonical @"gov/constitution/v0" @GovConstitution.V0.CanonicalConstitution
      validateType @"gov/constitution/v0" @GovConstitution.V0.CanonicalConstitution "record_entry"
    describe "gov/pparams/v0" $ do
      isCanonical @"gov/pparams/v0" @EpochInterval
      validateType @"gov/pparams/v0" @EpochInterval "epoch_interval"
      isCanonical @"gov/pparams/v0" @NonNegativeInterval
      validateType @"gov/pparams/v0" @NonNegativeInterval "nonnegative_interval"
      isCanonical @"gov/pparams/v0" @UnitInterval
      validateType @"gov/pparams/v0" @UnitInterval "unit_interval"
      isCanonical @"gov/pparams/v0" @CanonicalExUnits
      validateType @"gov/pparams/v0" @CanonicalExUnits "ex_units"
      isCanonical @"gov/pparams/v0" @(PParams ConwayEra)
      validateType @"gov/pparams/v0" @(GovPParams.V0.GovPParamsOut ConwayEra) "gov_pparams_out"
  describe "namespaces" $ do
    testNS @"blocks/v0"
    testNS @"utxo/v0"
    testNS @"gov/constitution/v0"
    testNS @"gov/committee/v0"
    testNS @"gov/pparams/v0"
    testNS @"gov/proposals/v0"
    testNS @"pool_stake/v0"
    testNS @"snapshots/v0"

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = prop propName $ propTypeIsCanonical @ns @a
  where
    propName = showsTypeRep (typeRep (Proxy @a)) " is canonical"
