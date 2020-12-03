{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Rewards (rewardTests,C,defaultMain) where

import Cardano.Binary (toCBOR)
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (MD5, hashToBytes)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as Crypto
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Crypto (VRF)
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad (replicateM)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Ratio (Ratio, (%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    StrictMaybe (..),
    UnitInterval,
    mkActiveSlotCoeff,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    Stake (..),
  )
import Shelley.Spec.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
    hashWithSerialiser,
    vKey,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards (reward)
import Shelley.Spec.Ledger.TxBody (PoolParams (..), RewardAcnt (..))
import Test.Shelley.Spec.Ledger.Generator.Core (genCoin, genNatural)
import Test.Shelley.Spec.Ledger.Utils
  ( testGlobals,
    unsafeMkUnitInterval,
  )
import Test.Tasty (TestTree, testGroup,defaultMain)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    choose,
    counterexample,
    elements,
    property,
    testProperty,
    withMaxSuccess,
    generate,
  )
import Test.Tasty.HUnit(testCaseInfo)
import Control.Monad.Identity(Identity(..))
import Data.Default.Class(Default(def))
import Control.Provenance(runProvM,runWithProvM)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)

-- Bounds and Constants --

maxNumPools :: Int
maxNumPools = 100

maxNumMembers :: Int
maxNumMembers = 100

maxMemberLovelace :: Integer
maxMemberLovelace = 100000

maxOwnerLovelaceAbovePledge :: Integer
maxOwnerLovelaceAbovePledge = 100000

maxPoolPledeg :: Integer
maxPoolPledeg = 1000000

maxPoolCost :: Integer
maxPoolCost = 1000000

maxPoolBlocks :: Natural
maxPoolBlocks = 1000000

numberOfTests :: Int
numberOfTests = 500

decentralizationRange :: [Ratio Word64]
decentralizationRange = [0, 0.1 .. 1]

tauRange :: [Ratio Word64]
tauRange = [0, 0.05 .. 0.3]

rhoRange :: [Ratio Word64]
rhoRange = [0, 0.05 .. 0.3]

-- Helpers --

keyPair :: CC.Crypto crypto => Int -> KeyPair r crypto
keyPair seed = KeyPair vk sk
  where
    vk = VKey (Crypto.deriveVerKeyDSIGN sk)
    sk =
      Crypto.genKeyDSIGN $
        mkSeedFromBytes . hashToBytes $
          hashWithSerialiser @MD5 toCBOR seed

vrfKeyPair :: forall v. Crypto.VRFAlgorithm v => Int -> (Crypto.SignKeyVRF v, Crypto.VerKeyVRF v)
vrfKeyPair seed = (sk, vk)
  where
    vk = Crypto.deriveVerKeyVRF sk
    sk =
      Crypto.genKeyVRF $
        mkSeedFromBytes . hashToBytes $
          hashWithSerialiser @MD5 toCBOR seed

data PoolSetUpArgs crypto f = PoolSetUpArgs
  { poolPledge :: f Coin,
    poolCost :: f Coin,
    poolMargin :: f UnitInterval,
    poolMembers :: f (Map (Credential 'Staking crypto) Coin)
  }

emptySetupArgs :: PoolSetUpArgs crypto Maybe
emptySetupArgs =
  PoolSetUpArgs
    { poolPledge = Nothing,
      poolCost = Nothing,
      poolMargin = Nothing,
      poolMembers = Nothing
    }

data PoolInfo crypto = PoolInfo
  { params :: PoolParams crypto,
    coldKey :: KeyPair 'StakePool crypto,
    ownerKey :: KeyPair 'Staking crypto,
    ownerStake :: Coin,
    rewardKey :: KeyPair 'Staking crypto,
    members :: Map (Credential 'Staking crypto) Coin
  }

-- Generators --

genNonOwnerMembers :: CC.Crypto crypto => Gen (Map (Credential 'Staking crypto) Coin)
genNonOwnerMembers = do
  numMembers <- choose (0, maxNumMembers)
  fmap Map.fromList . replicateM numMembers $ do
    credential <- KeyHashObj . hashKey . vKey . keyPair <$> arbitrary
    coins <- genCoin 0 maxMemberLovelace
    pure (credential, coins)

getOrGen :: Maybe a -> Gen a -> Gen a
getOrGen (Just x) _ = pure x
getOrGen Nothing g = g

genMargin :: Gen UnitInterval
genMargin = do
  let denom = 10
  numer <- choose (0, denom)
  pure $ unsafeMkUnitInterval (numer % denom)

genPoolInfo :: forall crypto. CC.Crypto crypto => PoolSetUpArgs crypto Maybe -> Gen (PoolInfo crypto)
genPoolInfo PoolSetUpArgs {poolPledge, poolCost, poolMargin, poolMembers} = do
  pledge <- getOrGen poolPledge $ genCoin 0 maxPoolPledeg
  cost <- getOrGen poolCost $ genCoin 0 maxPoolCost
  margin <- getOrGen poolMargin genMargin
  vrfKey <- vrfKeyPair @(VRF crypto) <$> arbitrary
  coldKey <- keyPair <$> arbitrary
  ownerKey <- keyPair <$> arbitrary
  rewardKey <- keyPair <$> arbitrary
  members' <- getOrGen poolMembers genNonOwnerMembers
  ownerStake <- (pledge <>) <$> genCoin 0 maxOwnerLovelaceAbovePledge
  -- here we are forcing the pool to meet the pledeg, later we may want flexibility
  let members = Map.insert (KeyHashObj . hashKey . vKey $ ownerKey) ownerStake members'
      params =
        PoolParams
          { _poolId = hashKey . vKey $ coldKey,
            _poolVrf = Crypto.hashVerKeyVRF . snd $ vrfKey,
            _poolPledge = pledge,
            _poolCost = cost,
            _poolMargin = margin,
            _poolRAcnt = RewardAcnt Testnet . KeyHashObj . hashKey . vKey $ rewardKey,
            _poolOwners = Set.fromList [hashKey $ vKey ownerKey],
            _poolRelays = StrictSeq.empty,
            _poolMD = SNothing
          }
  pure $ PoolInfo {params, coldKey, ownerKey, ownerStake, rewardKey, members}

genRewardPPs :: Gen (PParams era)
genRewardPPs = do
  d <- g decentralizationRange
  t <- g tauRange
  r <- g rhoRange
  pure $ emptyPParams {_d = d, _tau = t, _rho = r}
  where
    g xs = unsafeMkUnitInterval <$> elements xs

genBlocksMade :: [PoolParams crypto] -> Gen (BlocksMade crypto)
genBlocksMade pools = BlocksMade . Map.fromList <$> mapM f pools
  where
    f p = (_poolId p,) <$> genNatural 0 maxPoolBlocks

-- Properties --

rewardsBoundedByPot :: forall era. Era era => Proxy era -> Property
rewardsBoundedByPot _ = property $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo @(Crypto era) <$> replicate numPools emptySetupArgs
  pp <- genRewardPPs
  rewardPot <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  undelegatedLovelace <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  asc <- mkActiveSlotCoeff . unsafeMkUnitInterval <$> elements [0.1, 0.2, 0.3]
  bs@(BlocksMade blocks) <- genBlocksMade (fmap params pools)
  let totalBlocks = sum blocks
  silentSlots <- genNatural 0 (3 * totalBlocks) -- the '3 * sum blocks' is pretty arbitrary
  let stake = fold (members <$> pools)
      delegs = fold $
        flip fmap pools $
          \PoolInfo {params, members} ->
            Map.fromList $ (,_poolId params) <$> Map.keys members
      rewardAcnts = Set.fromList $ Map.keys delegs
      poolParams =
        Map.fromList $
          fmap
            ( \PoolInfo {params} ->
                (_poolId params, params)
            )
            pools
      totalLovelace = undelegatedLovelace <> fold stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      Identity rs = runProvM $
        reward @Identity @era
          pp
          bs
          rewardPot
          rewardAcnts
          poolParams
          (Stake stake)
          delegs
          totalLovelace
          asc
          slotsPerEpoch
  pure $
    counterexample
      ( mconcat
          [ "pp\n",
            show pp,
            "\nrewardPot\n",
            show rewardPot,
            "\nrewardAcnts\n",
            show rewardAcnts,
            "\npoolParams\n",
            show poolParams,
            "\nstake\n",
            show stake,
            "\ndelegs\n",
            show delegs,
            "\ntotalLovelace\n",
            show totalLovelace,
            "\nasc\n",
            show asc,
            "\nslotsPerEpoch\n",
            show slotsPerEpoch
          ]
      )
      (fold (fst rs) < rewardPot)


rewardsProvenance :: forall era. Era era => Proxy era -> IO String
rewardsProvenance _ = generate $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo @(Crypto era) <$> replicate numPools emptySetupArgs
  pp <- genRewardPPs
  rewardPot <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  undelegatedLovelace <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  asc <- mkActiveSlotCoeff . unsafeMkUnitInterval <$> elements [0.1, 0.2, 0.3]
  bs@(BlocksMade blocks) <- genBlocksMade (fmap params pools)
  let totalBlocks = sum blocks
  silentSlots <- genNatural 0 (3 * totalBlocks) -- the '3 * sum blocks' is pretty arbitrary
  let stake = fold (members <$> pools)
      delegs = fold $
        flip fmap pools $
          \PoolInfo {params, members} ->
            Map.fromList $ (,_poolId params) <$> Map.keys members
      rewardAcnts = Set.fromList $ Map.keys delegs
      poolParams =
        Map.fromList $
          fmap
            ( \PoolInfo {params} ->
                (_poolId params, params)
            )
            pools
      totalLovelace = undelegatedLovelace <> fold stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      Identity (_,prov) = runWithProvM def $
        reward @Identity @era
          pp
          bs
          rewardPot
          rewardAcnts
          poolParams
          (Stake stake)
          delegs
          totalLovelace
          asc
          slotsPerEpoch
  pure (show(snd(Map.findMin prov)))


rewardTests :: forall era. Era era => Proxy era -> TestTree
rewardTests proxy =
  testGroup
    "Reward Tests"
    [ testProperty
        "Sum of rewards is bounded by reward pot"
        (withMaxSuccess numberOfTests (rewardsBoundedByPot proxy))
    , testCaseInfo "Reward Provenance works" (rewardsProvenance proxy)
    ]
