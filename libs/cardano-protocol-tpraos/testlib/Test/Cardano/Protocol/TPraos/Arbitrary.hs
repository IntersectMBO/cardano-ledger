{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.TPraos.Arbitrary () where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Core (Era, EraSegWits, Tx, toTxSeq)
import Cardano.Ledger.Crypto (Crypto (KES, VRF))
import Cardano.Ledger.Keys (signedKES)
import Cardano.Protocol.TPraos.BHeader (
  BHBody (BHBody),
  BHeader (BHeader),
  HashHeader (HashHeader),
  PrevHash (BlockHash, GenesisHash),
 )
import Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod), OCert (..))
import Cardano.Protocol.TPraos.Rules.Overlay (OBftSlot)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState)
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.QuickCheck (Arbitrary, arbitrary, frequency, genericShrink, shrink)

instance Crypto c => Arbitrary (HashHeader c) where
  arbitrary = HashHeader <$> arbitrary

instance Arbitrary TicknState where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (PrtclState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Crypto c => Arbitrary (OBftSlot c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , KES.Signable (KES c) ~ SignableRepresentation
  ) =>
  Arbitrary (BHeader c)
  where
  arbitrary = do
    bhBody <- arbitrary
    hotKey <- arbitrary
    let sig = signedKES () 1 bhBody hotKey
    pure $ BHeader bhBody sig

instance
  ( Crypto c
  , VRF.Signable (VRF c) ~ SignableRepresentation
  ) =>
  Arbitrary (BHBody c)
  where
  arbitrary =
    BHBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (PrevHash c) where
  arbitrary = do
    hash <- arbitrary
    frequency [(1, pure GenesisHash), (9999, pure (BlockHash hash))]

instance Crypto c => Arbitrary (OCert c) where
  arbitrary =
    OCert
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary KESPeriod

instance
  ( Crypto c
  , Era era
  , EraSegWits era
  , KES.Signable (KES c) ~ SignableRepresentation
  , VRF.Signable (VRF c) ~ SignableRepresentation
  , Arbitrary (Tx era)
  ) =>
  Arbitrary (Block (BHeader c) era)
  where
  arbitrary =
    Block
      <$> arbitrary
      <*> (toTxSeq @era <$> arbitrary)
