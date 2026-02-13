{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Currently this uses the trace mechanism to check that computing rewards has
-- a required set of properties. It works only in the Shelley Era. It could be
-- generalized, and then moved to the Generator/Trace/ directory which computes
-- property tests in all eras.
module Test.Cardano.Ledger.Shelley.Rewards (
  tests,
  defaultMain,
  newEpochProp,
  newEpochEventsProp,
  RewardUpdateOld (..),
  createRUpdOld,
  createRUpdOld_,
  mkSnapShot,
) where

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (Blake2b_256, hashToBytes)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as Crypto
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BlocksMade (..),
  BoundedRational (..),
  Globals (..),
  Network (..),
  ProtVer (..),
  ShelleyBase,
  StrictMaybe (..),
  UnitInterval,
  activeSlotVal,
  epochInfoPure,
  mkActiveSlotCoeff,
  nonZeroOr,
  (%?),
 )
import Cardano.Ledger.Binary (encCBOR, hashWithEncoder, natVersion, shelleyProtVer)
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
  knownNonZeroCoin,
  rationalToCoinViaFloor,
  toDeltaCoin,
 )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Rewards (Reward (..))
import Cardano.Ledger.Shelley (
  ShelleyEra,
  hardforkAllegraAggregatedRewards,
  hardforkBabbageForgoRewardPrefilter,
 )
import Cardano.Ledger.Shelley.API (NonMyopic)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  FilteredRewards (..),
  NewEpochState (..),
  RewardUpdate (..),
  circulation,
  completeRupd,
  createRUpd,
  filterAllRewards,
  lsCertStateL,
  prevPParamsEpochStateL,
  updateNonMyopic,
 )
import Cardano.Ledger.Shelley.PoolRank (Likelihood, leaderProbability, likelihood)
import Cardano.Ledger.Shelley.RewardUpdate (
  FreeVars (..),
  Pulser,
  RewardAns (..),
  RewardEvent,
  RewardPulser (RSLP),
 )
import Cardano.Ledger.Shelley.Rewards (
  StakeShare (..),
  aggregateRewards,
  calcStakePoolMemberReward,
  calcStakePoolOperatorReward,
  mkApparentPerformance,
  mkPoolRewardInfo,
 )
import Cardano.Ledger.Shelley.Rules (
  PulsingRewUpdate (..),
  RupdEvent (RupdEvent),
  ShelleyNewEpochEvent (DeltaRewardEvent, TotalRewardEvent),
  ShelleyTickEvent (TickNewEpochEvent, TickRupdEvent),
 )
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (epochInfoSize)
import Cardano.Ledger.Val (Val (..), invert, (<+>), (<->))
import Cardano.Protocol.Crypto (VRF, hashVerKeyVRF)
import Cardano.Slotting.Slot (EpochSize (..))
import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (asks, runReader)
import Data.Foldable as F (fold, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy
import Data.Pulse (Pulsable (..))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff (ansiWlEditExprCompact, ediff)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Stack
import Lens.Micro ((&), (.~), (^.))
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (genCoin, genNatural)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainEvent (..), ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (forAllChainTrace, forEachEpochTrace)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils (
  runShelleyBase,
  testGlobals,
  unsafeBoundRational,
 )
import Test.Cardano.Ledger.TerseTools (Terse (..), tersemapdiffs)
import Test.Control.State.Transition.Trace (SourceSignalTarget (..), getEvents, sourceSignalTargets)
import Test.QuickCheck ((===))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (
  Gen,
  Property,
  arbitrary,
  choose,
  counterexample,
  elements,
  noShrinking,
  property,
  testProperty,
  withMaxSuccess,
 )

-- ========================================================================
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

decentralizationRange :: [Rational]
decentralizationRange = [0, 0.1 .. 1]

tauRange :: [Rational]
tauRange = [0, 0.05 .. 0.3]

rhoRange :: [Rational]
rhoRange = [0, 0.05 .. 0.3]

-- Helpers --

keyPair :: Int -> KeyPair r
keyPair seed = KeyPair vk sk
  where
    vk = VKey (Crypto.deriveVerKeyDSIGN sk)
    sk =
      Crypto.genKeyDSIGN $
        mkSeedFromBytes . hashToBytes $
          hashWithEncoder @Blake2b_256 shelleyProtVer encCBOR seed

vrfKeyPair :: forall v. Crypto.VRFAlgorithm v => Int -> (Crypto.SignKeyVRF v, Crypto.VerKeyVRF v)
vrfKeyPair seed = (sk, vk)
  where
    vk = Crypto.deriveVerKeyVRF sk
    sk =
      Crypto.genKeyVRF $
        mkSeedFromBytes . hashToBytes $
          hashWithEncoder @Blake2b_256 shelleyProtVer encCBOR seed

data PoolSetUpArgs f = PoolSetUpArgs
  { poolPledge :: f Coin
  , poolCost :: f Coin
  , poolMargin :: f UnitInterval
  , poolMembers :: f (Map (Credential Staking) Coin)
  }

emptySetupArgs :: PoolSetUpArgs Maybe
emptySetupArgs =
  PoolSetUpArgs
    { poolPledge = Nothing
    , poolCost = Nothing
    , poolMargin = Nothing
    , poolMembers = Nothing
    }

data PoolInfo = PoolInfo
  { params :: StakePoolParams
  , coldKey :: KeyPair StakePool
  , ownerKey :: KeyPair Staking
  , ownerStake :: Coin
  , rewardKey :: KeyPair Staking
  , members :: Map (Credential Staking) Coin
  }

-- Generators --

genNonOwnerMembers :: Gen (Map (Credential Staking) Coin)
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
  pure $ unsafeBoundRational (numer % denom)

genPoolInfo :: PoolSetUpArgs Maybe -> Gen PoolInfo
genPoolInfo PoolSetUpArgs {poolPledge, poolCost, poolMargin, poolMembers} = do
  pledge <- getOrGen poolPledge $ genCoin 0 maxPoolPledeg
  cost <- getOrGen poolCost $ genCoin 0 maxPoolCost
  margin <- getOrGen poolMargin genMargin
  vrfKey <- vrfKeyPair @(VRF MockCrypto) <$> arbitrary
  coldKey <- keyPair <$> arbitrary
  ownerKey <- keyPair <$> arbitrary
  rewardKey <- keyPair <$> arbitrary
  members' <- getOrGen poolMembers genNonOwnerMembers
  ownerStake <- (pledge <>) <$> genCoin 0 maxOwnerLovelaceAbovePledge
  -- here we are forcing the pool to meet the pledeg, later we may want flexibility
  let members = Map.insert (KeyHashObj . hashKey . vKey $ ownerKey) ownerStake members'
      params =
        StakePoolParams
          { sppId = hashKey . vKey $ coldKey
          , sppVrf = hashVerKeyVRF @MockCrypto $ snd vrfKey
          , sppPledge = pledge
          , sppCost = cost
          , sppMargin = margin
          , sppAccountAddress = AccountAddress Testnet . AccountId . KeyHashObj . hashKey . vKey $ rewardKey
          , sppOwners = Set.fromList [hashKey $ vKey ownerKey]
          , sppRelays = StrictSeq.empty
          , sppMetadata = SNothing
          }
  pure $ PoolInfo {params, coldKey, ownerKey, ownerStake, rewardKey, members}

genRewardPPs :: (EraPParams era, AtMostEra "Alonzo" era) => Gen (PParams era)
genRewardPPs = do
  d <- g decentralizationRange
  tau <- g tauRange
  rho <- g rhoRange
  pure $
    emptyPParams
      & ppDL .~ d
      & ppTauL .~ tau
      & ppRhoL .~ rho
  where
    g xs = unsafeBoundRational <$> elements xs

genBlocksMade :: [StakePoolParams] -> Gen BlocksMade
genBlocksMade pools = BlocksMade . Map.fromList <$> mapM f pools
  where
    f p = (sppId p,) <$> genNatural 0 maxPoolBlocks

-- Properties --

toCompactCoinError :: HasCallStack => Coin -> CompactForm Coin
toCompactCoinError c =
  fromMaybe (error $ "Invalid Coin: " <> show c) $ toCompact c

rewardsBoundedByPot ::
  forall era.
  (EraPParams era, AtMostEra "Alonzo" era) =>
  Proxy era ->
  Property
rewardsBoundedByPot _ = property $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo <$> replicate numPools emptySetupArgs
  pp <- genRewardPPs
  rewardPot <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  undelegatedLovelace <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  asc <- mkActiveSlotCoeff . unsafeBoundRational <$> elements [0.1, 0.2, 0.3]
  bs@(BlocksMade blocks) <- genBlocksMade (fmap params pools)
  let totalBlocks = sum blocks
  silentSlots <- genNatural 0 (3 * totalBlocks) -- the '3 * sum blocks' is pretty arbitrary
  let stake = fold (members <$> pools)
      delegs = fold $
        flip fmap pools $
          \PoolInfo {params, members} ->
            Map.fromList $ (,sppId params) <$> Map.keys members
      rewardAccounts = Set.fromList $ Map.keys delegs
      stakePoolParams =
        VMap.fromList
          [(sppId params, params) | PoolInfo {params} <- pools]
      totalLovelace = undelegatedLovelace <> fold stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      (RewardAns rs _) =
        runShelleyBase $
          mkRewardAns @era
            pp
            bs
            rewardPot
            rewardAccounts
            stakePoolParams
            (Stake (VMap.fromMap (toCompactCoinError <$> stake)))
            (VMap.fromMap delegs)
            totalLovelace
  pure $
    counterexample
      ( mconcat
          [ "pp\n"
          , show pp
          , "\nrewardPot\n"
          , show rewardPot
          , "\nrewardAccounts\n"
          , show rewardAccounts
          , "\nstakePoolParams\n"
          , show stakePoolParams
          , "\nstake\n"
          , show stake
          , "\ndelegs\n"
          , show delegs
          , "\ntotalLovelace\n"
          , show totalLovelace
          , "\nasc\n"
          , show asc
          , "\nslotsPerEpoch\n"
          , show slotsPerEpoch
          ]
      )
      (foldMap rewardAmount rs < rewardPot)

-- ====================================================================================
-- To demonstrate that the code we wrote that uses pulsing does not
-- change the result of reward calculation. we reproduce the old style functions here.

rewardOnePool ::
  EraPParams era =>
  PParams era ->
  Coin ->
  Natural ->
  Natural ->
  StakePoolParams ->
  Stake ->
  Rational ->
  Rational ->
  Coin ->
  Set.Set (Credential Staking) ->
  Map (Credential Staking) Coin
rewardOnePool
  pp
  r
  blocksN
  blocksTotal
  pool
  (Stake stake)
  sigma
  sigmaA
  (Coin totalStake)
  addrsRew =
    rewards'
    where
      Coin ostake =
        Set.foldl'
          (\c o -> maybe c (mappend c . fromCompact) $ VMap.lookup (KeyHashObj o) stake)
          mempty
          (sppOwners pool)
      Coin pledge = sppPledge pool
      pr = fromIntegral pledge % fromIntegral totalStake
      Coin maxP =
        if pledge <= ostake
          then maxPool pp r sigma pr
          else mempty
      appPerf = mkApparentPerformance (pp ^. ppDG) sigmaA blocksN blocksTotal
      poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
      tot = fromIntegral totalStake
      pv = pp ^. ppProtocolVersionL
      mRewards =
        Map.fromList
          [ ( hk
            , calcStakePoolMemberReward
                poolR
                (sppCost pool)
                (sppMargin pool)
                (StakeShare (unCoin (fromCompact c) % tot))
                (StakeShare sigma)
            )
          | (hk, c) <- VMap.toAscList stake
          , notPoolOwner hk
          ]
      notPoolOwner (KeyHashObj hk) = hk `Set.notMember` sppOwners pool
      notPoolOwner (ScriptHashObj _) = True
      lReward =
        calcStakePoolOperatorReward
          poolR
          (sppCost pool)
          (sppMargin pool)
          (StakeShare $ fromIntegral ostake % tot)
          (StakeShare sigma)
      f =
        if hardforkAllegraAggregatedRewards pv
          then Map.insertWith (<>)
          else Map.insert
      potentialRewards =
        f (unAccountId (aaAccountId $ sppAccountAddress pool)) lReward mRewards
      potentialRewards' =
        if hardforkBabbageForgoRewardPrefilter pv
          then potentialRewards
          else Map.restrictKeys potentialRewards addrsRew
      rewards' = Map.filter (/= Coin 0) potentialRewards'

-- | Get stake of one pool
poolStake ::
  KeyHash StakePool ->
  VMap.VMap VMap.VB VMap.VB (Credential Staking) (KeyHash StakePool) ->
  Stake ->
  Stake
poolStake hk delegs (Stake stake) =
  -- Stake $ (eval (dom (delegs ▷ setSingleton hk) ◁ stake))
  Stake $ VMap.filter (\cred _ -> VMap.lookup cred delegs == Just hk) stake

rewardOld ::
  forall era.
  EraPParams era =>
  PParams era ->
  BlocksMade ->
  Coin ->
  Set.Set (Credential Staking) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash StakePool) StakePoolParams ->
  Stake ->
  VMap.VMap VMap.VB VMap.VB (Credential Staking) (KeyHash StakePool) ->
  Coin ->
  ActiveSlotCoeff ->
  EpochSize ->
  ( Map (Credential Staking) Coin
  , Map (KeyHash StakePool) Likelihood
  )
rewardOld
  pp
  (BlocksMade b)
  r
  addrsRew
  stakePoolParams
  stake
  delegs
  (Coin totalStake)
  asc
  slotsPerEpoch = (rewards', hs)
    where
      totalBlocks = sum b
      Coin activeStake = sumAllStake stake
      results ::
        [ ( KeyHash StakePool
          , Maybe (Map (Credential Staking) Coin)
          , Likelihood
          )
        ]
      results = do
        (hk, spparams) <- VMap.toAscList stakePoolParams
        let sigma = fromIntegral pstake %? fromIntegral totalStake
            sigmaA = fromIntegral pstake %? fromIntegral activeStake
            blocksProduced = Map.lookup hk b
            actgr = poolStake hk delegs stake
            Coin pstake = sumAllStake actgr
            rewardMap = case blocksProduced of
              Nothing -> Nothing -- This is equivalent to calling rewarOnePool with n = 0
              Just n ->
                Just $
                  rewardOnePool
                    pp
                    r
                    n
                    totalBlocks
                    spparams
                    actgr
                    sigma
                    sigmaA
                    (Coin totalStake)
                    addrsRew
            ls =
              likelihood
                (fromMaybe 0 blocksProduced)
                (leaderProbability asc sigma (pp ^. ppDG))
                slotsPerEpoch
        pure (hk, rewardMap, ls)
      pv = pp ^. ppProtocolVersionL
      f =
        if hardforkAllegraAggregatedRewards pv
          then Map.unionsWith (<>)
          else Map.unions
      rewards' = f $ mapMaybe (\(_, x, _) -> x) results
      hs = Map.fromList $ fmap (\(hk, _, l) -> (hk, l)) results

data RewardUpdateOld = RewardUpdateOld
  { deltaTOld :: !DeltaCoin
  , deltaROld :: !DeltaCoin
  , rsOld :: !(Map (Credential Staking) Coin)
  , deltaFOld :: !DeltaCoin
  , nonMyopicOld :: !NonMyopic
  }
  deriving (Show, Eq)

createRUpdOld ::
  forall era.
  (EraGov era, EraCertState era) =>
  EpochSize ->
  BlocksMade ->
  EpochState era ->
  Coin ->
  ShelleyBase RewardUpdateOld
createRUpdOld slotsPerEpoch b es@(EpochState acnt ls ss nm) maxSupply =
  createRUpdOld_ @era slotsPerEpoch b ss reserves pr totalStake rs nm
  where
    ds = ls ^. lsCertStateL . certDStateL
    rs = Map.keysSet (ds ^. accountsL . accountsMapL)
    reserves = casReserves acnt
    totalStake = circulation es maxSupply
    pr = es ^. prevPParamsEpochStateL

createRUpdOld_ ::
  forall era.
  EraPParams era =>
  EpochSize ->
  BlocksMade ->
  SnapShots ->
  Coin ->
  PParams era ->
  Coin ->
  Set.Set (Credential Staking) ->
  NonMyopic ->
  ShelleyBase RewardUpdateOld
createRUpdOld_ slotsPerEpoch b@(BlocksMade b') ss (Coin reserves) pr totalStake rs nm = do
  asc <- asks activeSlotCoeff
  let SnapShot stake' _ delegs' poolParams _ = ssStakeGo ss
      -- reserves and rewards change
      deltaR1 =
        rationalToCoinViaFloor $
          min 1 eta
            * unboundRational (pr ^. ppRhoL)
            * fromIntegral reserves
      d = unboundRational (pr ^. ppDG)
      expectedBlocks =
        floor $
          (1 - d) * unboundRational (activeSlotVal asc) * fromIntegral slotsPerEpoch
      -- TODO asc is a global constant, and slotsPerEpoch should not change often at all,
      -- it would be nice to not have to compute expectedBlocks every epoch
      eta
        | unboundRational (pr ^. ppDG) >= 0.8 = 1
        | otherwise = blocksMade % expectedBlocks
      Coin rPot = ssFee ss <> deltaR1
      deltaT1 = floor $ unboundRational (pr ^. ppTauL) * fromIntegral rPot
      _R = Coin $ rPot - deltaT1
      (rs_, newLikelihoods) =
        rewardOld
          pr
          b
          _R
          rs
          poolParams
          stake'
          delegs'
          totalStake
          asc
          slotsPerEpoch
      deltaR2 = _R <-> Map.foldr (<+>) mempty rs_
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
  pure $
    RewardUpdateOld
      { deltaTOld = DeltaCoin deltaT1
      , deltaROld = invert (toDeltaCoin deltaR1) <> toDeltaCoin deltaR2
      , rsOld = rs_
      , deltaFOld = invert (toDeltaCoin $ ssFee ss)
      , nonMyopicOld = updateNonMyopic nm _R $ VMap.fromMap newLikelihoods
      }

overrideProtocolVersionUsedInRewardCalc ::
  EraGov era =>
  ProtVer ->
  EpochState era ->
  EpochState era
overrideProtocolVersionUsedInRewardCalc pv es =
  es & prevPParamsEpochStateL . ppProtocolVersionL .~ pv

oldEqualsNew ::
  forall era.
  (EraGov era, Show (NewEpochState era), EraCertState era) =>
  ProtVer ->
  NewEpochState era ->
  Property
oldEqualsNew pv newepochstate =
  counterexample
    (show newepochstate ++ show (ansiWlEditExprCompact $ ediff old new))
    (old === new)
  where
    globals = testGlobals
    epochstate = overrideProtocolVersionUsedInRewardCalc pv $ nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade
    blocksmade = nesBprev newepochstate
    epochNumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = epochInfoSize (epochInfoPure globals) epochNumber
    unAggregated =
      runReader (createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    newWithZeros = aggregateRewards pv (rs unAggregated)
    new = Map.filter (/= Coin 0) newWithZeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

oldEqualsNewOn ::
  forall era.
  (EraGov era, EraCertState era) =>
  ProtVer ->
  NewEpochState era ->
  Property
oldEqualsNewOn pv newepochstate = old === new
  where
    globals = testGlobals
    epochstate = overrideProtocolVersionUsedInRewardCalc pv $ nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade
    blocksmade = nesBprev newepochstate
    epochNumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = epochInfoSize (epochInfoPure globals) epochNumber
    unAggregated =
      runReader (createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old :: Map (Credential Staking) Coin
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    newWithZeros = aggregateRewards pv (rs unAggregated)
    new = Map.filter (/= Coin 0) newWithZeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

lastElem :: [a] -> Maybe a
lastElem [a] = Just a
lastElem [] = Nothing
lastElem (_ : xs) = lastElem xs

-- | Provide a legitimate NewEpochState to make an test Property
newEpochProp :: Word64 -> (NewEpochState ShelleyEra -> Property) -> Property
newEpochProp tracelen propf = withMaxSuccess 100 $
  forAllChainTrace @ShelleyEra tracelen defaultConstants $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} -> propf (chainNes target)
      _ -> property True

-- | Given a NewEpochState and [ChainEvent], test a Property at every Epoch Boundary
newEpochEventsProp ::
  Word64 -> ([ChainEvent ShelleyEra] -> NewEpochState ShelleyEra -> Property) -> Property
newEpochEventsProp tracelen propf = withMaxSuccess 10 $
  forEachEpochTrace @ShelleyEra 10 tracelen defaultConstants $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} ->
        propf (concat (runShelleyBase $ getEvents tr)) (chainNes target)
      _ -> property True

aggIncrementalRewardEvents ::
  [ChainEvent ShelleyEra] ->
  Map (Credential Staking) (Set Reward)
aggIncrementalRewardEvents = F.foldl' accum Map.empty
  where
    accum ans (TickEvent (TickRupdEvent (RupdEvent _ m))) = Map.unionWith Set.union m ans
    accum ans (TickEvent (TickNewEpochEvent (DeltaRewardEvent (RupdEvent _ m)))) =
      Map.unionWith Set.union m ans
    accum ans _ = ans

getMostRecentTotalRewardEvent ::
  [ChainEvent ShelleyEra] ->
  Map (Credential Staking) (Set Reward)
getMostRecentTotalRewardEvent = F.foldl' accum Map.empty
  where
    accum ans (TickEvent (TickNewEpochEvent (TotalRewardEvent _ m))) = Map.unionWith Set.union m ans
    accum ans _ = ans

complete :: PulsingRewUpdate -> (RewardUpdate, RewardEvent)
complete (Complete r) = (r, mempty)
complete (Pulsing rewsnap pulser) = runShelleyBase $ (completeRupd (Pulsing rewsnap pulser))

eventsMirrorRewards :: [ChainEvent ShelleyEra] -> NewEpochState ShelleyEra -> Property
eventsMirrorRewards events nes = same eventRew compRew
  where
    (compRew, eventRew) =
      case nesRu nes of
        SNothing -> (total, aggFilteredEvent)
        SJust pulser ->
          ( Map.unionWith Set.union (rs completed) total
          , Map.unionWith Set.union lastevent aggevent
          )
          where
            (completed, lastevent) = complete pulser
    total = getMostRecentTotalRewardEvent events
    aggevent = aggIncrementalRewardEvents events
    FilteredRewards aggFilteredEvent _ _ _ = filterAllRewards aggevent (nesEs nes)
    same x y = withMaxSuccess 1 $ counterexample message (x === y)
      where
        message =
          "events don't mirror rewards "
            ++ tersemapdiffs
              "Map differences: aggregated filtered events on the left, computed on the right."
              x
              y

instance Terse Reward where
  terse (Reward ty pl (Coin n)) = "Reward{" ++ show ty ++ ", #" ++ take 9 (show pl) ++ ", " ++ show n ++ "}"

instance Terse x => Terse (Set x) where
  terse x = unlines (Set.toList (Set.map terse x))

-- ================================================================

mkRewardAns ::
  forall era.
  EraPParams era =>
  PParams era ->
  BlocksMade ->
  Coin ->
  Set (Credential Staking) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash StakePool) StakePoolParams ->
  Stake ->
  VMap.VMap VMap.VB VMap.VB (Credential Staking) (KeyHash StakePool) ->
  Coin ->
  ShelleyBase RewardAns
mkRewardAns
  pp
  (BlocksMade b)
  r
  addrsRew
  stakePools
  stake
  delegs
  totalStake = completeM pulser
    where
      totalBlocks = sum b
      totalActiveStake = sumAllStake stake `nonZeroOr` knownNonZeroCoin @1
      delegatorsPerStakePool =
        VMap.foldlWithKey
          (\acc cred poolId -> Map.insertWith (<>) poolId (Set.singleton cred) acc)
          mempty
          delegs

      mkPoolRewardInfo' stakePoolParams =
        -- lehins: why is this restriction necessary? :o
        -- ensure mkPoolRewardInfo does not use stake that doesn't belong to the pool
        let stakeRestrictedToPool = poolStake (sppId stakePoolParams) delegs stake
            stakePoolId = sppId stakePoolParams
            delegators = Map.findWithDefault mempty stakePoolId delegatorsPerStakePool
            stakePoolState = mkStakePoolState (pp ^. ppPoolDepositCompactL) delegators stakePoolParams
            stakePoolSnapShot = mkStakePoolSnapShot stakeRestrictedToPool totalActiveStake stakePoolState
         in mkPoolRewardInfo
              pp
              r
              (BlocksMade b)
              totalBlocks
              totalStake
              totalActiveStake
              stakePoolId
              stakePoolSnapShot
      free =
        FreeVars
          { fvAddrsRew = addrsRew
          , fvTotalStake = totalStake
          , fvPoolRewardInfo =
              VMap.mapMaybe (either (const Nothing) Just . mkPoolRewardInfo') stakePools
          , fvDelegs = delegs
          , fvProtVer = pp ^. ppProtocolVersionL
          }
      pulser :: Pulser
      pulser = RSLP 2 free (unStake stake) (RewardAns Map.empty Map.empty)

-- | Adaptor that can construct new SnapShot representation from the old one
mkSnapShot ::
  Stake ->
  VMap.VMap VMap.VB VMap.VB (Credential Staking) (KeyHash StakePool) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash StakePool) StakePoolParams ->
  SnapShot
mkSnapShot activeStake delegs stakePools =
  SnapShot
    { ssStake = activeStake
    , ssTotalActiveStake = totalActiveStake
    , ssDelegations = delegs
    , ssPoolParams = stakePools
    , ssStakePoolsSnapShot = force $ VMap.map snapShotFromStakePoolParams stakePools
    }
  where
    snapShotFromStakePoolParams stakePoolParams =
      let delegations = Map.findWithDefault mempty (sppId stakePoolParams) delegatorsPerStakePool
       in mkStakePoolSnapShot activeStake totalActiveStake $
            mkStakePoolState mempty delegations stakePoolParams
    totalActiveStake = sumAllStake activeStake `nonZeroOr` knownNonZeroCoin @1
    delegatorsPerStakePool =
      VMap.foldlWithKey
        (\acc cred poolId -> Map.insertWith (<>) poolId (Set.singleton cred) acc)
        mempty
        delegs

-- ==================================================================

-- | Note that chainlen must be set high enough so that enough epochs
-- have passed to get non-trivial rewards.
chainlen :: Word64
chainlen = 200

tests :: TestTree
tests =
  testGroup
    "Reward Tests"
    [ testProperty "Sum of rewards is bounded by reward pot" $
        withMaxSuccess numberOfTests (rewardsBoundedByPot (Proxy @ShelleyEra))
    , testProperty "compare with reference impl, no provenance, v3" . noShrinking $
        newEpochProp chainlen (oldEqualsNew @ShelleyEra (ProtVer (natVersion @3) 0))
    , testProperty "compare with reference impl, no provenance, v7" . noShrinking $
        newEpochProp chainlen (oldEqualsNew @ShelleyEra (ProtVer (natVersion @7) 0))
    , testProperty "compare with reference impl, with provenance" . noShrinking $
        newEpochProp chainlen (oldEqualsNewOn @ShelleyEra (ProtVer (natVersion @3) 0))
    , testProperty "delta events mirror reward updates" $
        newEpochEventsProp chainlen eventsMirrorRewards
    ]
