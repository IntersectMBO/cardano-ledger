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

module Test.Cardano.Ledger.Core.Arbitrary (
  module Test.Cardano.Ledger.Binary.Arbitrary,
  genAddrBadPtr,
  genCompactAddrBadPtr,
  genBadPtr,
  genValidUMap,
  genValidUMapNonEmpty,
  genValidTriples,
  genValidTriplesNonEmpty,
  genInvariantNonEmpty,
  genRightPreferenceUMap,
  genInsertDeleteRoundtripRDPair,
  genInsertDeleteRoundtripDelegation,
  genInsertDeleteRoundtripPtr,
)
where

import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm (deriveVerKeyDSIGN, genKeyDSIGN),
  seedSizeDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Address
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  CertIx (..),
  DnsName,
  Network (..),
  NonNegativeInterval,
  Nonce (..),
  Port (..),
  PositiveInterval,
  PositiveUnitInterval,
  ProtVer (..),
  SlotNo (..),
  StrictMaybe,
  TxIx (..),
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
import Cardano.Ledger.Binary (EncCBOR, Sized, mkSized)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (DSIGN), StandardCrypto)
import Cardano.Ledger.DPState
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRole (StakePool, Staking),
  VKey (..),
 )
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..), ChainCode (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (
  PoolMetadata (..),
  PoolParams (..),
  SizeOfPoolOwners (..),
  SizeOfPoolRelays (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (RDPair (..), Trip (Triple), UMap (UMap), View (RewardDeposits), unUnify, unify)
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Monad.Identity (Identity)
import Data.GenValidity
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Typeable
import qualified Data.VMap as VMap
import Data.Word (Word16, Word32, Word64)
import GHC.Stack
import Generic.Random (genericArbitraryU)
import qualified Test.Cardano.Chain.Common.Gen as Byron
import Test.Cardano.Ledger.Binary.Arbitrary
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)

instance (Era era, EncCBOR (f era), Arbitrary (f era)) => Arbitrary (Sized (f era)) where
  arbitrary = mkSized (eraProtVerHigh @era) <$> arbitrary

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
  arbitrary = TxIx <$> arbitrary

instance Arbitrary CertIx where
  arbitrary = CertIx <$> arbitrary

instance Arbitrary ProtVer where
  arbitrary = ProtVer <$> arbitrary <*> arbitrary

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce
      , mkNonceFromNumber <$> arbitrary
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
  arbitrary = genValidPtr

-- | Generate a Ptr that contains values that are allowed on the wire
genValidPtr :: Gen Ptr
genValidPtr =
  Ptr
    <$> (SlotNo . (fromIntegral :: Word32 -> Word64) <$> arbitrary)
    <*> (mkTxIxPartial . toInteger <$> (arbitrary :: Gen Word16))
    <*> (mkCertIxPartial . toInteger <$> (arbitrary :: Gen Word16))

-- | Generate a Ptr with full 64bit range for values. Not allowed starting in Babbage
genBadPtr :: Gen Ptr
genBadPtr = Ptr <$> arbitrary <*> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (Credential r c) where
  arbitrary =
    oneof
      [ ScriptHashObj . ScriptHash <$> arbitrary
      , KeyHashObj <$> arbitrary
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
        { vKey = VKey $ deriveVerKeyDSIGN signKey
        , sKey = signKey
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
  arbitrary = genAddrWith arbitrary
  shrink = genericShrink

genAddrWith :: Crypto c => Gen Ptr -> Gen (Addr c)
genAddrWith genPtr =
  frequency
    [ (8, Addr <$> arbitrary <*> arbitrary <*> genStakeRefWith genPtr)
    , (2, AddrBootstrap <$> arbitrary)
    ]

genAddrBadPtr :: Crypto c => Gen (Addr c)
genAddrBadPtr = genAddrWith genBadPtr

genCompactAddrBadPtr :: Crypto c => Gen (CompactAddr c)
genCompactAddrBadPtr = compactAddr <$> genAddrBadPtr

instance Crypto c => Arbitrary (CompactAddr c) where
  arbitrary = compactAddr <$> arbitrary

instance Crypto c => Arbitrary (StakeReference c) where
  arbitrary = genStakeRefWith arbitrary
  shrink = genericShrink

genStakeRefWith :: Crypto c => Gen Ptr -> Gen (StakeReference c)
genStakeRefWith genPtr =
  frequency
    [ (80, StakeRefBase <$> arbitrary)
    , (5, StakeRefPtr <$> genPtr)
    , (15, pure StakeRefNull)
    ]

instance Crypto c => Arbitrary (RewardAcnt c) where
  arbitrary = RewardAcnt <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (Withdrawals c) where
  arbitrary = genericArbitraryU
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
-- Cardano.Ledger.UTxO -------------------------------------------------------------------
------------------------------------------------------------------------------------------

deriving instance (EraTxOut era, Arbitrary (TxOut era)) => Arbitrary (UTxO era)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Core.PParams -----------------------------------------------------------
------------------------------------------------------------------------------------------

deriving instance (Era era, Arbitrary (PParamsHKD Identity era)) => Arbitrary (PParams era)

deriving instance (Era era, Arbitrary (PParamsHKD StrictMaybe era)) => Arbitrary (PParamsUpdate era)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.UMap -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary RDPair where
  arbitrary = RDPair <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (Trip c) where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (UMap c) where
  arbitrary = UMap <$> arbitrary <*> arbitrary

-- | Used for testing UMap operations
genValidTriples ::
  Gen
    ( Map.Map (Credential 'Staking StandardCrypto) RDPair
    , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
    , Map.Map Ptr (Credential 'Staking StandardCrypto)
    )
genValidTriples = scale (* 2) $ do
  creds :: [Credential 'Staking StandardCrypto] <- arbitrary
  let nCreds = length creds
  rdPairs :: [RDPair] <- vectorOf nCreds arbitrary
  delegs :: [KeyHash 'StakePool StandardCrypto] <- vectorOf nCreds arbitrary
  ptrs :: [Ptr] <- arbitrary
  pure
    ( Map.fromList $ zip creds rdPairs
    , Map.fromList $ zip creds delegs
    , Map.fromList $ zip ptrs creds
    )

genValidTriplesNonEmpty ::
  Gen
    ( Map.Map (Credential 'Staking StandardCrypto) RDPair
    , Map.Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
    , Map.Map Ptr (Credential 'Staking StandardCrypto)
    )
genValidTriplesNonEmpty = scale (* 2) $ do
  Positive nCreds <- arbitrary
  nPtrs <- chooseInt (1, nCreds)
  creds :: [Credential 'Staking StandardCrypto] <- vectorOf nCreds arbitrary
  rdPairs :: [RDPair] <- vectorOf nCreds arbitrary
  delegs :: [KeyHash 'StakePool StandardCrypto] <- vectorOf nCreds arbitrary
  ptrs :: [Ptr] <- vectorOf nPtrs arbitrary
  pure
    ( Map.fromList $ zip creds rdPairs
    , Map.fromList $ zip creds delegs
    , Map.fromList $ zip ptrs creds
    )

genValidUMap :: Gen (UMap StandardCrypto)
genValidUMap = do
  (rdPairs, delegs, ptrs) <- genValidTriples
  pure $ unify rdPairs delegs ptrs

genValidUMapNonEmpty :: Gen (UMap StandardCrypto)
genValidUMapNonEmpty = do
  (rdPairs, delegs, ptrs) <- genValidTriplesNonEmpty
  pure $ unify rdPairs delegs ptrs

genExcludingKey :: (Ord k, Arbitrary k) => Map.Map k a -> Gen k
genExcludingKey ks = do
  k <- arbitrary
  if k `Map.member` ks
    then genExcludingKey ks
    else pure k

genInsertDeleteRoundtripRDPair :: Gen (UMap StandardCrypto, Credential 'Staking StandardCrypto, RDPair)
genInsertDeleteRoundtripRDPair = do
  umap@(UMap tripmap _prtmap) <- genValidUMap
  k <- genExcludingKey tripmap
  v <- arbitrary
  pure (umap, k, v)

genInsertDeleteRoundtripDelegation ::
  Gen
    ( UMap StandardCrypto
    , Credential 'Staking StandardCrypto
    , KeyHash 'StakePool StandardCrypto
    )
genInsertDeleteRoundtripDelegation = do
  umap@(UMap tripmap _prtmap) <- genValidUMap
  k <- genExcludingKey tripmap
  v <- arbitrary
  pure (umap, k, v)

genInsertDeleteRoundtripPtr :: Gen (UMap StandardCrypto, Ptr, Credential 'Staking StandardCrypto)
genInsertDeleteRoundtripPtr = do
  umap@(UMap _tripmap ptrmap) <- genValidUMap
  k <- genExcludingKey ptrmap
  v <- arbitrary
  pure (umap, k, v)

genInvariantNonEmpty :: Gen (Credential 'Staking StandardCrypto, Ptr, UMap StandardCrypto)
genInvariantNonEmpty = do
  umap@(UMap tripmap ptrmap) <- genValidUMapNonEmpty
  cred <-
    oneof
      [ elements $ Map.keys tripmap
      , genExcludingKey tripmap
      ]
  ptr <-
    oneof
      [ elements $ Map.keys ptrmap
      , genExcludingKey ptrmap
      ]
  pure (cred, ptr, umap)

genRightPreferenceUMap :: Gen (UMap StandardCrypto, Map.Map (Credential 'Staking StandardCrypto) RDPair)
genRightPreferenceUMap = do
  umap <- genValidUMap
  let rdMap = unUnify $ RewardDeposits umap
  subdomain <- sublistOf $ Map.keys rdMap
  coins <- vectorOf (length subdomain) arbitrary
  pure (umap, Map.fromList $ zip subdomain coins)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.DPState -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Crypto c => Arbitrary (DPState c) where
  arbitrary = DPState <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (DState c) where
  arbitrary = DState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (PState c) where
  arbitrary = PState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (InstantaneousRewards c) where
  arbitrary = InstantaneousRewards <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Crypto c => Arbitrary (FutureGenDeleg c) where
  arbitrary = FutureGenDeleg <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.EpochBoundary ----------------------------------------------------------
------------------------------------------------------------------------------------------

-- | In the system, Stake never contains more than the sum of all Ada (which is constant).
-- This makes it safe to store individual Coins (in CompactForm) as Word64. But we must
-- be careful that we never generate Stake where the sum of all the coins exceeds (maxBound :: Word64)
-- There will never be a real Stake in the system with that many Ada, because total Ada is constant.
-- So using a restricted Arbitrary Generator is OK.
instance Crypto c => Arbitrary (Stake c) where
  arbitrary = Stake <$> (VMap.fromMap <$> theMap)
    where
      genWord64 :: Int -> Gen Word64
      genWord64 n =
        frequency
          [ (3, choose (1, 100))
          , (2, choose (101, 10000))
          , (1, choose (1, maxBound `div` fromIntegral n))
          ]
      theMap = do
        n <- frequency [(3, chooseInt (1, 20)), (2, chooseInt (21, 150)), (1, chooseInt (151, 1000))]
        let pair = (,) <$> arbitrary <*> (CompactCoin <$> genWord64 n)
        list <- frequency [(1, pure []), (99, vectorOf n pair)]
        pure (Map.fromList list)
