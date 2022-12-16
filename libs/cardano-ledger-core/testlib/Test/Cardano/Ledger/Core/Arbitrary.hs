{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ < 920
-- Workaroud a ghc bug:
{-# OPTIONS_GHC -Wno-name-shadowing #-}
#endif
module Test.Cardano.Ledger.Core.Arbitrary
  ( module Test.Cardano.Ledger.Binary.Arbitrary,
  )
where

import Cardano.Crypto.DSIGN.Class
  ( DSIGNAlgorithm (deriveVerKeyDSIGN, genKeyDSIGN),
    seedSizeDSIGN,
  )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Address
import Cardano.Ledger.CompactAddress
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BlocksMade (..),
    CertIx,
    DnsName,
    Network (..),
    NonNegativeInterval,
    Nonce (..),
    Port (..),
    PositiveInterval,
    PositiveUnitInterval,
    ProtVer (..),
    SlotNo (..),
    TxIx,
    UnitInterval,
    Url,
    mkActiveSlotCoeff,
    mkCertIxPartial,
    mkNonceFromNumber,
    mkTxIxPartial,
    promoteRatio,
    textToDns,
    textToUrl,
  )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core (EraTxOut (TxOut))
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (DSIGN))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyPair (..),
    VKey (..),
  )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams
  ( PoolMetadata (..),
    PoolParams (..),
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
    StakePoolRelay (..),
  )
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Data.GenValidity
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Typeable
import Data.Word (Word16, Word32, Word64)
import GHC.Stack
import Generic.Random (genericArbitraryU)
import qualified Test.Cardano.Chain.Common.Gen as Byron
import Test.Cardano.Ledger.Binary.Arbitrary
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.BaseTypes --------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary ActiveSlotCoeff where
  arbitrary = mkActiveSlotCoeff <$> arbitrary

instance Validity ActiveSlotCoeff where
  validate _ = mempty

instance GenValid ActiveSlotCoeff where
  genValid = mkActiveSlotCoeff <$> genValid

instance Crypto c => Arbitrary (BlocksMade c) where
  arbitrary = BlocksMade <$> arbitrary

instance Arbitrary Network where
  arbitrary = arbitraryBoundedEnum

genDnsName :: Int -> Gen T.Text
genDnsName n = do
  str <- vectorOf (n - 4) $ elements $ '.' : '-' : ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']
  pure $ T.pack str <> ".com"

guardLength :: HasCallStack => Int -> T.Text -> Maybe a -> a
guardLength n txt = \case
  Nothing -> error $ "Unexpected! Generated length: (" ++ show n ++ ") " ++ show txt
  Just t -> t

instance Arbitrary DnsName where
  arbitrary = do
    n <- chooseInt (5, 64)
    txt <- genDnsName n
    pure $! guardLength n txt $ textToDns txt

instance Arbitrary Url where
  arbitrary = do
    let prefix = "https://"
    n <- chooseInt (5, 64 - T.length prefix)
    txt <- genDnsName n
    pure $! guardLength n txt $ textToUrl (prefix <> txt)

instance Arbitrary Port where
  arbitrary = fromIntegral @Word16 @Port <$> arbitrary

-- JSON instances can't roundtrip, unless these are decimal.

-- | Decimal numbers only
instance Arbitrary UnitInterval where
  arbitrary = do
    p <- chooseInt (0, 19)
    let y = 10 ^ p :: Word64
    x <- choose (0, y)
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary PositiveUnitInterval where
  arbitrary = do
    p <- chooseInt (0, 19)
    let y = 10 ^ p :: Word64
    x <- choose (1, y)
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary PositiveInterval where
  arbitrary = do
    p <- chooseInt (0, 19)
    let y = 10 ^ p :: Word64
    x <- choose (1, 10 ^ (19 :: Int))
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary NonNegativeInterval where
  arbitrary = do
    p <- chooseInt (0, 19)
    let y = 10 ^ p :: Word64
    x <- choose (0, 10 ^ (19 :: Int))
    pure $ unsafeBoundRational $ promoteRatio (x % y)

instance Validity UnitInterval where
  validate _ = mempty

instance GenValid UnitInterval where
  genValid = do
    x :: Word64 <- genValid
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (if x > y then y % x else x % y)
  shrinkValid _ = []

instance Validity PositiveUnitInterval where
  validate _ = mempty

instance GenValid PositiveUnitInterval where
  genValid = do
    Positive (x :: Word64) <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (if x > y then y % x else x % y)
  shrinkValid _ = []

instance Validity PositiveInterval where
  validate _ = mempty

instance GenValid PositiveInterval where
  genValid = do
    Positive (x :: Word64) <- arbitrary
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (x % y)
  shrinkValid _ = []

instance Validity NonNegativeInterval where
  validate _ = mempty

instance GenValid NonNegativeInterval where
  genValid = do
    x :: Word64 <- genValid
    Positive (y :: Word64) <- arbitrary
    pure $ unsafeBoundRational $ promoteRatio (x % y)
  shrinkValid _ = []

instance Arbitrary TxIx where
  arbitrary = mkTxIxPartial . toInteger <$> (arbitrary :: Gen Word16)

instance Arbitrary CertIx where
  arbitrary = mkCertIxPartial . toInteger <$> (arbitrary :: Gen Word16)

instance Arbitrary ProtVer where
  arbitrary = ProtVer <$> arbitrary <*> arbitrary

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce,
        mkNonceFromNumber <$> arbitrary
      ]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.TxIn -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (TxId c) where
  arbitrary = TxId <$> arbitrary

instance Crypto c => Arbitrary (TxIn c) where
  arbitrary = TxIn <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Credential --------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary Ptr where
  arbitrary = Ptr <$> genSlotNo <*> arbitrary <*> arbitrary
    where
      -- We are only allowing 32bit large slot numbers in Ptrs
      genSlotNo = SlotNo . (fromIntegral :: Word32 -> Word64) <$> arbitrary

instance Crypto c => Arbitrary (Credential r c) where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> arbitrary,
        KeyHashObj <$> arbitrary
      ]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Hashes -----------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (SafeHash c i) where
  arbitrary = unsafeMakeSafeHash <$> arbitrary

instance Crypto c => Arbitrary (ScriptHash c) where
  arbitrary = ScriptHash <$> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.AuxiliaryDataHash ------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (AuxiliaryDataHash c) where
  arbitrary = AuxiliaryDataHash <$> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Keys -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (KeyHash a c) where
  arbitrary = KeyHash <$> arbitrary

instance DSIGNAlgorithm (DSIGN c) => Arbitrary (VKey kd c) where
  arbitrary = VKey <$> arbitrary

instance (Typeable kr, Crypto c) => Arbitrary (WitVKey kr c) where
  arbitrary = WitVKey <$> arbitrary <*> arbitrary

instance Arbitrary ChainCode where
  arbitrary = ChainCode <$> arbitrary

instance Crypto c => Arbitrary (BootstrapWitness c) where
  arbitrary = do
    bwKey <- arbitrary
    bwSig <- arbitrary
    bwChainCode <- arbitrary
    bwAttributes <- arbitrary
    pure $ BootstrapWitness {bwKey, bwSig, bwChainCode, bwAttributes}

instance Crypto c => Arbitrary (GenDelegPair c) where
  arbitrary = GenDelegPair <$> arbitrary <*> arbitrary

deriving instance Crypto c => Arbitrary (GenDelegs c)

instance DSIGNAlgorithm (DSIGN c) => Arbitrary (KeyPair kd c) where
  arbitrary = do
    seed <- mkSeedFromBytes <$> genByteString (fromIntegral (seedSizeDSIGN (Proxy @(DSIGN c))))
    let signKey = genKeyDSIGN seed
    pure $
      KeyPair
        { vKey = VKey $ deriveVerKeyDSIGN signKey,
          sKey = signKey
        }

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Coin -------------------------------------------------------------------
------------------------------------------------------------------------------------------
deriving instance Arbitrary (CompactForm Coin)

instance Arbitrary Coin where
  -- Cannot be negative even though it is an 'Integer'
  arbitrary = Coin <$> choose (0, 1000000)
  shrink (Coin i) = Coin <$> shrink i

instance Arbitrary DeltaCoin where
  arbitrary = DeltaCoin <$> choose (-1000000, 1000000)
  shrink (DeltaCoin i) = DeltaCoin <$> shrink i

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Address ----------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary (BootstrapAddress c) where
  arbitrary = BootstrapAddress <$> hedgehog Byron.genAddress

instance Crypto c => Arbitrary (Addr c) where
  arbitrary =
    frequency
      [ (8, Addr <$> arbitrary <*> arbitrary <*> arbitrary),
        (2, AddrBootstrap <$> arbitrary)
      ]
  shrink = genericShrink

instance Crypto c => Arbitrary (CompactAddr c) where
  arbitrary = compactAddr <$> arbitrary

instance Crypto c => Arbitrary (StakeReference c) where
  arbitrary =
    frequency
      [ (80, StakeRefBase <$> arbitrary),
        (5, StakeRefPtr <$> arbitrary),
        (15, pure StakeRefNull)
      ]
  shrink = genericShrink

instance Crypto c => Arbitrary (RewardAcnt c) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Reward -----------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary RewardType where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Crypto c => Arbitrary (Reward c) where
  arbitrary = Reward <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.PoolParams -------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (PoolParams c) where
  arbitrary =
    PoolParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PoolMetadata where
  arbitrary = PoolMetadata <$> arbitrary <*> arbitrary

instance Arbitrary StakePoolRelay where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary SizeOfPoolRelays where
  arbitrary = pure SizeOfPoolRelays

instance Arbitrary SizeOfPoolOwners where
  arbitrary = pure SizeOfPoolOwners

------------------------------------------------------------------------------------------
-- Cardano.Ledger.PoolDistr --------------------------------------------------------------
------------------------------------------------------------------------------------------

deriving instance Crypto c => Arbitrary (PoolDistr c)

instance Crypto c => Arbitrary (IndividualPoolStake c) where
  arbitrary = IndividualPoolStake <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.UTxO --------------------------------------------------------------
------------------------------------------------------------------------------------------

deriving instance (EraTxOut era, Arbitrary (TxOut era)) => Arbitrary (UTxO era)
