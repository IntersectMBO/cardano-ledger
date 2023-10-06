{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Api.Arbitrary () where

import Cardano.Ledger.Api.State.Query (MemberStatus)
import Test.Cardano.Ledger.Common

instance Arbitrary MemberStatus where
  arbitrary = arbitraryBoundedEnum
