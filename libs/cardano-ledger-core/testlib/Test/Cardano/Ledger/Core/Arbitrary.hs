{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  genValidUMapWithCreds,
  genValidTuples,
  genValidTuplesNonEmpty,
  genInvariantNonEmpty,
  genRightPreferenceUMap,
  genInsertDeleteRoundtripRDPair,
  genInsertDeleteRoundtripPtr,
  genInsertDeleteRoundtripSPool,
  genInsertDeleteRoundtripDRep,

  -- * Plutus
  genValidAndUnknownCostModels,
  genValidCostModel,
  genValidCostModels,

  -- * Utils

  -- | Will need to find a better home in the future
  uniformSubSet,
  uniformSubMap,
  uniformSubMapElems,
)
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  CertIx (..),
  DnsName,
  EpochInterval (..),
  HasZero,
  Mismatch (..),
  Network (..),
  NonNegativeInterval,
  Nonce (..),
  Port (..),
  PositiveInterval,
  PositiveUnitInterval,
  ProtVer (..),
  StrictMaybe,
  TxIx (..),
  UnitInterval,
  Url,
  mkActiveSlotCoeff,
  mkNonceFromNumber,
  natVersion,
  promoteRatio,
  textToDns,
  textToUrl,
 )
import qualified Cardano.Ledger.BaseTypes as NZ
import Cardano.Ledger.Binary (EncCBOR, Sized, mkSized)
import Cardano.Ledger.CertState (
  Anchor (..),
  CommitteeAuthorization (..),
  CommitteeState (..),
  DState (..),
  FutureGenDeleg (..),
  InstantaneousRewards (..),
  PState (..),
  VState (..),
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr (..), SlotNo32 (..), StakeReference (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..), unsafeMakeSafeHash)
import Cardano.Ledger.Keys (BootstrapWitness (..), ChainCode (..), VKey (..), WitVKey (..))
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  CostModels,
  costModelParamsCount,
  mkCostModel,
  mkCostModels,
  mkCostModelsLenient,
  updateCostModels,
 )
import Cardano.Ledger.Plutus.Data (BinaryData, Data (..), Datum (..), dataToBinaryData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), Prices (..))
import Cardano.Ledger.Plutus.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.PoolParams (
  PoolMetadata (..),
  PoolParams (..),
  SizeOfPoolOwners (..),
  SizeOfPoolRelays (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (
  RDPair (..),
  UMElem (UMElem),
  UMap (UMap, umElems, umPtrs),
  UView (RewDepUView),
  unUnify,
  unify,
 )
import Control.Monad (replicateM)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.GenValidity
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Typeable
import qualified Data.VMap as VMap
import Data.Word (Word16, Word64, Word8)
import GHC.Stack
import Generic.Random (genericArbitraryU)
import System.Random.Stateful (StatefulGen, uniformRM)
import qualified Test.Cardano.Chain.Common.Gen as Byron
import Test.Cardano.Ledger.Binary.Arbitrary
import Test.Cardano.Ledger.Binary.Random (QC (..))
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)

maxDecimalsWord64 :: Int
maxDecimalsWord64 = 19

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

instance Arbitrary BlocksMade where
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
    pure $! guardLength n txt $ textToDns 64 txt

instance Arbitrary Url where
  arbitrary = do
    let prefix = "https://"
    n <- chooseInt (5, 64 - T.length prefix)
    txt <- genDnsName n
    pure $! guardLength n txt $ textToUrl 64 (prefix <> txt)

instance Arbitrary Port where
  arbitrary = fromIntegral @Word16 @Port <$> arbitrary

-- JSON instances can't roundtrip, unless these are decimal.

-- | Decimal numbers only
instance Arbitrary UnitInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, y)
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary PositiveUnitInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (1, y)
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary PositiveInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (1, 10 ^ (maxDecimalsWord64 :: Int))
    pure $ unsafeBoundRational $ promoteRatio (x % y)

-- | Decimal numbers only
instance Arbitrary NonNegativeInterval where
  arbitrary = do
    p <- chooseInt (0, maxDecimalsWord64)
    let y = 10 ^ p :: Word64
    x <- choose (0, 10 ^ (maxDecimalsWord64 :: Int))
    pure $ unsafeBoundRational $ promoteRatio (x % y)

instance Arbitrary (NoUpdate a) where
  arbitrary = pure NoUpdate

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
  -- starting with Conway, we only deserialize TxIx within Word16 range
  arbitrary = TxIx . fromIntegral <$> arbitrary @Word16

instance Arbitrary CertIx where
  -- starting with Conway, we only deserialize CertIx within Word16 range
  arbitrary = CertIx . fromIntegral <$> arbitrary @Word16

instance Arbitrary ProtVer where
  arbitrary = ProtVer <$> arbitrary <*> arbitrary

instance Arbitrary Nonce where
  arbitrary =
    oneof
      [ return NeutralNonce
      , mkNonceFromNumber <$> arbitrary
      ]

instance Arbitrary EpochInterval where
  arbitrary = EpochInterval <$> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.TxIn -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary TxId where
  arbitrary = TxId <$> arbitrary

instance Arbitrary TxIn where
  arbitrary = TxIn <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Credential --------------------------------------------------------------
------------------------------------------------------------------------------------------

deriving instance Arbitrary SlotNo32

instance Arbitrary Ptr where
  arbitrary = Ptr <$> arbitrary <*> arbitrary <*> arbitrary

-- | Generate a Ptr with full 64bit range for values. Not allowed starting in Babbage
genBadPtr :: Gen Ptr
genBadPtr = Ptr <$> arbitrary <*> arbitrary <*> arbitrary
{-# DEPRECATED genBadPtr "Bad pointers are no longer possible" #-}

instance Arbitrary (Credential r) where
  arbitrary =
    oneof
      [ ScriptHashObj <$> arbitrary
      , KeyHashObj <$> arbitrary
      ]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Hashes -----------------------------------------------------------------
------------------------------------------------------------------------------------------

genHash :: forall h a. HashAlgorithm h => Gen (Hash h a)
genHash = UnsafeHash <$> genShortByteString (fromIntegral (sizeHash (Proxy @h)))

instance Arbitrary (SafeHash i) where
  arbitrary = unsafeMakeSafeHash <$> genHash

instance Arbitrary ScriptHash where
  arbitrary = ScriptHash <$> genHash

------------------------------------------------------------------------------------------
-- Cardano.Ledger.AuxiliaryDataHash ------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary TxAuxDataHash where
  arbitrary = TxAuxDataHash <$> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Keys -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary (KeyHash r) where
  arbitrary = KeyHash <$> genHash

instance Arbitrary (VRFVerKeyHash r) where
  arbitrary = VRFVerKeyHash <$> genHash

instance Arbitrary (VKey kd) where
  arbitrary = VKey <$> arbitrary

instance Typeable kr => Arbitrary (WitVKey kr) where
  arbitrary = WitVKey <$> arbitrary <*> arbitrary

instance Arbitrary ChainCode where
  arbitrary = ChainCode <$> arbitrary

instance Arbitrary BootstrapWitness where
  arbitrary = do
    bwKey <- arbitrary
    bwSig <- arbitrary
    bwChainCode <- arbitrary
    bwAttributes <- arbitrary
    pure $ BootstrapWitness {bwKey, bwSig, bwChainCode, bwAttributes}

instance Arbitrary GenDelegPair where
  arbitrary = GenDelegPair <$> arbitrary <*> arbitrary

deriving instance Arbitrary GenDelegs

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

instance Arbitrary BootstrapAddress where
  arbitrary = BootstrapAddress <$> hedgehog Byron.genAddress

instance Arbitrary Byron.Address where
  arbitrary = hedgehog Byron.genAddress

instance Arbitrary Byron.AddrAttributes where
  arbitrary = hedgehog Byron.genAddrAttributes

instance Arbitrary (Byron.Attributes Byron.AddrAttributes) where
  arbitrary = hedgehog $ Byron.genAttributes Byron.genAddrAttributes

instance Arbitrary Addr where
  arbitrary = genAddrWith arbitrary
  shrink = genericShrink

genAddrWith :: Gen Ptr -> Gen Addr
genAddrWith genPtr =
  frequency
    [ (8, Addr <$> arbitrary <*> arbitrary <*> genStakeRefWith genPtr)
    , (2, AddrBootstrap <$> arbitrary)
    ]

genAddrBadPtr :: Gen Addr
genAddrBadPtr = genAddrWith genBadPtr
{-# DEPRECATED genAddrBadPtr "Addresses with bad pointers are no longer possible" #-}

genCompactAddrBadPtr :: Gen CompactAddr
genCompactAddrBadPtr = compactAddr <$> genAddrBadPtr
{-# DEPRECATED genCompactAddrBadPtr "Addresses with bad pointers are no longer possible" #-}

instance Arbitrary CompactAddr where
  arbitrary = compactAddr <$> arbitrary

instance Arbitrary StakeReference where
  arbitrary = genStakeRefWith arbitrary
  shrink = genericShrink

genStakeRefWith :: Gen Ptr -> Gen StakeReference
genStakeRefWith genPtr =
  frequency
    [ (80, StakeRefBase <$> arbitrary)
    , (5, StakeRefPtr <$> genPtr)
    , (15, pure StakeRefNull)
    ]

instance Arbitrary RewardAccount where
  arbitrary = RewardAccount <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Withdrawals where
  arbitrary = genericArbitraryU
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Reward -----------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary RewardType where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance Arbitrary Reward where
  arbitrary = Reward <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.PoolParams -------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary PoolParams where
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
-- Cardano.Ledger.State ------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary AccountState where
  arbitrary = AccountState <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.State.PoolDistr --------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary PoolDistr where
  arbitrary = do
    Positive denominator <- arbitrary
    PoolDistr <$> arbitrary <*> pure (CompactCoin denominator)

instance Arbitrary IndividualPoolStake where
  arbitrary = IndividualPoolStake <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.DRepState --------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary DRepState where
  arbitrary = DRepState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------------------------
-- Cardano.Ledger.State.UTxO -------------------------------------------------------------
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

instance Arbitrary UMElem where
  arbitrary = UMElem <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary UMap where
  arbitrary = UMap <$> arbitrary <*> arbitrary

instance Arbitrary DRep where
  arbitrary =
    oneof
      [ DRepCredential <$> arbitrary
      , pure DRepAlwaysAbstain
      , pure DRepAlwaysNoConfidence
      ]

instance (Arbitrary a, HasZero a) => Arbitrary (NZ.NonZero a) where
  arbitrary = arbitrary `suchThatMap` NZ.nonZero

-- | Used for testing UMap operations
genValidTuples ::
  Gen
    ( Map (Credential 'Staking) RDPair
    , Map Ptr (Credential 'Staking)
    , Map (Credential 'Staking) (KeyHash 'StakePool)
    , Map (Credential 'Staking) DRep
    )
genValidTuples = scale (* 2) $ do
  creds :: [Credential 'Staking] <- arbitrary
  let nCreds = length creds
  rdPairs :: [RDPair] <- vectorOf nCreds arbitrary
  ptrs :: [Ptr] <- arbitrary
  sPools :: [KeyHash 'StakePool] <- vectorOf nCreds arbitrary
  dReps :: [DRep] <- vectorOf nCreds arbitrary
  pure
    ( Map.fromList $ zip creds rdPairs
    , Map.fromList $ zip ptrs creds
    , Map.fromList $ zip creds sPools
    , Map.fromList $ zip creds dReps
    )

genValidTuplesNonEmpty ::
  Gen
    ( Map (Credential 'Staking) RDPair
    , Map Ptr (Credential 'Staking)
    , Map (Credential 'Staking) (KeyHash 'StakePool)
    , Map (Credential 'Staking) DRep
    )
genValidTuplesNonEmpty = scale (* 2) $ do
  Positive nCreds <- arbitrary
  nPtrs <- chooseInt (1, nCreds)
  creds :: [Credential 'Staking] <- vectorOf nCreds arbitrary
  rdPairs :: [RDPair] <- vectorOf nCreds arbitrary
  ptrs :: [Ptr] <- vectorOf nPtrs arbitrary
  sPools :: [KeyHash 'StakePool] <- vectorOf nCreds arbitrary
  dReps :: [DRep] <- vectorOf nCreds arbitrary
  pure
    ( Map.fromList $ zip creds rdPairs
    , Map.fromList $ zip ptrs creds
    , Map.fromList $ zip creds sPools
    , Map.fromList $ zip creds dReps
    )

genValidUMap :: Gen UMap
genValidUMap = do
  (rdPairs, ptrs, sPools, dReps) <- genValidTuples
  pure $ unify rdPairs ptrs sPools dReps

genValidUMapNonEmpty :: Gen UMap
genValidUMapNonEmpty = do
  (rdPairs, ptrs, sPools, dReps) <- genValidTuplesNonEmpty
  pure $ unify rdPairs ptrs sPools dReps

-- | Either clamp requested size to the range of @[0, actualSize]@ or generate at random
-- in the same range when requested size is not supplied.
uniformSubSize ::
  StatefulGen g m =>
  -- | Requested size
  Maybe Int ->
  -- | Actual size
  Int ->
  g ->
  m Int
uniformSubSize mReqSize actualSize gen =
  case mReqSize of
    Nothing -> uniformRM (0, actualSize) gen
    Just reqSize -> pure $ max 0 $ min actualSize reqSize

uniformSubSet ::
  (StatefulGen g m, Ord k) =>
  -- | Size of the subset. If supplied will be clamped to @[0, Set.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Set k ->
  g ->
  m (Set k)
uniformSubSet mSubSetSize inputSet gen = do
  subSetSize <- uniformSubSize mSubSetSize (Set.size inputSet) gen
  if subSetSize < Set.size inputSet `div` 2
    then
      goAdd inputSet Set.empty subSetSize
    else
      goDelete inputSet (Set.size inputSet - subSetSize)
  where
    -- Constructing a new Set is faster when less then a half of original Set will be used
    goAdd !s !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Set.size s - 1) gen
          goAdd (Set.deleteAt ix s) (Set.insert (Set.elemAt ix s) acc) (i - 1)
    -- Deleting is faster when more items need to be retained in the Set
    goDelete !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Set.size acc - 1) gen
          goDelete (Set.deleteAt ix acc) (i - 1)

uniformSubMap ::
  (StatefulGen g m, Ord k) =>
  -- | Size of the subMap. If supplied will be clamped to @[0, Map.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Map k v ->
  g ->
  m (Map k v)
uniformSubMap mSubMapSize inputMap gen = do
  subMapSize <- uniformSubSize mSubMapSize (Map.size inputMap) gen
  if subMapSize < Map.size inputMap `div` 2
    then
      -- Constructing a new Map is faster when less then a half of original Map will be used
      uniformSubMapElems Map.insert (Just subMapSize) inputMap gen
    else
      -- Deleting is faster when more items need to be retained in the Map
      goDelete inputMap (Map.size inputMap - subMapSize)
  where
    goDelete !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Map.size acc - 1) gen
          goDelete (Map.deleteAt ix acc) (i - 1)

uniformSubMapElems ::
  (StatefulGen g m, Monoid f) =>
  (k -> v -> f -> f) ->
  -- | Size of the subMap. If supplied will be clamped to @[0, Map.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Map k v ->
  g ->
  m f
uniformSubMapElems insert mSubMapSize inputMap gen = do
  subMapSize <- uniformSubSize mSubMapSize (Map.size inputMap) gen
  go inputMap mempty subMapSize
  where
    go !s !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Map.size s - 1) gen
          let (k, v) = Map.elemAt ix s
          go (Map.deleteAt ix s) (insert k v acc) (i - 1)

genValidUMapWithCreds :: Gen (UMap, Set (Credential 'Staking))
genValidUMapWithCreds = do
  umap <- genValidUMap
  creds <- uniformSubSet Nothing (Map.keysSet $ umElems umap) QC
  pure (umap, creds)

genExcludingKey :: (Ord k, Arbitrary k) => Map k a -> Gen k
genExcludingKey ks = do
  k <- arbitrary
  if k `Map.member` ks
    then genExcludingKey ks
    else pure k

genInsertDeleteRoundtripRDPair ::
  Gen (UMap, Credential 'Staking, RDPair)
genInsertDeleteRoundtripRDPair = do
  umap@UMap {umElems} <- genValidUMap
  k <- genExcludingKey umElems
  v <- arbitrary
  pure (umap, k, v)

genInsertDeleteRoundtripPtr :: Gen (UMap, Ptr, Credential 'Staking)
genInsertDeleteRoundtripPtr = do
  umap@UMap {umPtrs} <- genValidUMap
  k <- genExcludingKey umPtrs
  v <- arbitrary
  pure (umap, k, v)

genInsertDeleteRoundtripSPool :: Gen (UMap, Credential 'Staking, KeyHash 'StakePool)
genInsertDeleteRoundtripSPool = do
  umap@UMap {umElems} <- genValidUMap
  k <- genExcludingKey umElems
  v <- arbitrary
  pure (umap, k, v)

genInsertDeleteRoundtripDRep :: Gen (UMap, Credential 'Staking, DRep)
genInsertDeleteRoundtripDRep = do
  umap@UMap {umElems} <- genValidUMap
  k <- genExcludingKey umElems
  v <- arbitrary
  pure (umap, k, v)

genInvariantNonEmpty :: Gen (Credential 'Staking, Ptr, UMap)
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

genRightPreferenceUMap :: Gen (UMap, Map (Credential 'Staking) RDPair)
genRightPreferenceUMap = do
  umap <- genValidUMap
  let rdMap = unUnify $ RewDepUView umap
  subdomain <- sublistOf $ Map.keys rdMap
  coins <- vectorOf (length subdomain) arbitrary
  pure (umap, Map.fromList $ zip subdomain coins)

------------------------------------------------------------------------------------------
-- Cardano.Ledger.CertState -------------------------------------------------------------------
------------------------------------------------------------------------------------------

instance Era era => Arbitrary (DState era) where
  arbitrary =
    if eraProtVerLow @era >= natVersion @9
      then DState <$> genConwayUMap <*> arbitrary <*> arbitrary <*> arbitrary
      else DState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genConwayUMap :: Gen UMap
genConwayUMap = UMap <$> genElems <*> pure mempty
  where
    genElems :: Gen (Map (Credential 'Staking) UMElem)
    genElems = Map.fromList <$> listOf ((,) <$> arbitrary <*> genElem)
    genElem :: Gen UMElem
    genElem = UMElem <$> arbitrary <*> pure mempty <*> arbitrary <*> arbitrary

instance Arbitrary (PState era) where
  arbitrary = PState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Anchor where
  arbitrary = Anchor <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Mismatch r a) where
  arbitrary = Mismatch <$> arbitrary <*> arbitrary

instance Arbitrary CommitteeAuthorization where
  arbitrary =
    oneof
      [ CommitteeHotCredential <$> arbitrary
      , CommitteeMemberResigned <$> arbitrary
      ]

deriving instance Arbitrary (CommitteeState era)

instance Arbitrary (VState era) where
  arbitrary = VState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InstantaneousRewards where
  arbitrary = InstantaneousRewards <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary FutureGenDeleg where
  arbitrary = FutureGenDeleg <$> arbitrary <*> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.EpochBoundary ----------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary SnapShot where
  arbitrary =
    SnapShot
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary SnapShots where
  arbitrary = do
    ssStakeMark <- arbitrary
    ssStakeSet <- arbitrary
    ssStakeGo <- arbitrary
    ssFee <- arbitrary
    let ssStakeMarkPoolDistr = calculatePoolDistr ssStakeMark
    pure $ SnapShots {..}

-- | In the system, Stake never contains more than the sum of all Ada (which is constant).
-- This makes it safe to store individual Coins (in CompactForm) as Word64. But we must
-- be careful that we never generate Stake where the sum of all the coins exceeds (maxBound :: Word64)
-- There will never be a real Stake in the system with that many Ada, because total Ada is constant.
-- So using a restricted Arbitrary Generator is OK.
instance Arbitrary Stake where
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

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Core.TxCert ----------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary PoolCert where
  arbitrary =
    oneof
      [ RegPool <$> arbitrary
      , RetirePool <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Plutus ----------------------------------------------------------
------------------------------------------------------------------------------------------

instance Arbitrary Language where
  arbitrary = elements nonNativeLanguages

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> genUnit <*> genUnit
    where
      genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

instance Arbitrary CostModel where
  arbitrary = elements nonNativeLanguages >>= genValidCostModel

instance Era era => Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

instance Era era => Arbitrary (BinaryData era) where
  arbitrary = dataToBinaryData <$> arbitrary

instance Era era => Arbitrary (Datum era) where
  arbitrary =
    oneof
      [ pure NoDatum
      , DatumHash <$> arbitrary
      , Datum . dataToBinaryData <$> arbitrary
      ]

genValidCostModel :: Language -> Gen CostModel
genValidCostModel lang = do
  newParamValues <- vectorOf (costModelParamsCount lang) arbitrary
  either (\err -> error $ "Corrupt cost model: " ++ show err) pure $
    mkCostModel lang newParamValues

genValidCostModels :: Set.Set Language -> Gen CostModels
genValidCostModels = fmap mkCostModels . sequence . Map.fromSet genValidCostModel

genValidAndUnknownCostModels :: Gen CostModels
genValidAndUnknownCostModels = do
  langs <- sublistOf nonNativeLanguages
  validCms <- genValidCostModels $ Set.fromList langs
  unknownCms <- errorFail . mkCostModelsLenient <$> genUnknownCostModels
  pure $ updateCostModels validCms unknownCms

-- | This Arbitrary instance assumes the inflexible deserialization
-- scheme prior to version 9.
instance Arbitrary CostModels where
  arbitrary = do
    known <- genKnownCostModels
    unknown <- genUnknownCostModels
    let cms = known `Map.union` unknown
    pure . errorFail $ mkCostModelsLenient cms

genUnknownCostModels :: Gen (Map Word8 [Int64])
genUnknownCostModels = Map.fromList <$> listOf genUnknownCostModelValues

genKnownCostModels :: Gen (Map Word8 [Int64])
genKnownCostModels = do
  langs <- sublistOf nonNativeLanguages
  cms <- mapM genCostModelValues langs
  return $ Map.fromList cms

genUnknownCostModelValues :: Gen (Word8, [Int64])
genUnknownCostModelValues = do
  lang <- chooseInt (firstInvalid, fromIntegral (maxBound :: Word8))
  vs <- arbitrary
  return (fromIntegral . fromEnum $ lang, vs)
  where
    firstInvalid = fromEnum (maxBound :: Language) + 1

genCostModelValues :: Language -> Gen (Word8, [Int64])
genCostModelValues lang = do
  Positive sub <- arbitrary
  (,) lang'
    <$> oneof
      [ listAtLeast (costModelParamsCount lang) -- Valid Cost Model for known language
      , take (tooFew sub) <$> arbitrary -- Invalid Cost Model for known language
      ]
  where
    lang' = fromIntegral (fromEnum lang)
    tooFew sub = costModelParamsCount lang - sub
    listAtLeast :: Int -> Gen [Int64]
    listAtLeast x = do
      NonNegative y <- arbitrary
      replicateM (x + y) arbitrary
