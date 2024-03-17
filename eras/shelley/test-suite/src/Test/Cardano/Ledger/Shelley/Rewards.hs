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
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Currently this uses the trace mechanism to check that computing rewards has
-- a required set of properties. It works only in the Shelley Era. It could be
-- generalized, and then moved to the Generator/Trace/ directory which computes
-- property tests in all eras.
module Test.Cardano.Ledger.Shelley.Rewards (
  tests,
  C,
  defaultMain,
  newEpochProp,
  newEpochEventsProp,
  RewardUpdateOld (..),
  createRUpdOld,
  createRUpdOld_,
)
where

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
 )
import Cardano.Ledger.Binary (encCBOR, hashWithEncoder, natVersion, shelleyProtVer)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), rationalToCoinViaFloor, toDeltaCoin)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (VRF)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.EpochBoundary (
  Stake (..),
  maxPool,
  poolStake,
  sumAllStake,
  sumStakePerPool,
 )
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  VKey (..),
  hashKey,
 )
import Cardano.Ledger.Shelley.API (NonMyopic, SnapShot (..), SnapShots (..))
import Cardano.Ledger.Shelley.API.Types (PoolParams (..))
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  CertState (..),
  EpochState (..),
  FilteredRewards (..),
  LedgerState (..),
  NewEpochState (..),
  RewardUpdate (..),
  circulation,
  completeRupd,
  createRUpd,
  filterAllRewards,
  lsCertState,
  prevPParamsEpochStateL,
  rewards,
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
  leaderRew,
  memberRew,
  mkApparentPerformance,
  mkPoolRewardInfo,
 )
import Cardano.Ledger.Shelley.Rules (
  PulsingRewUpdate (..),
  RupdEvent (RupdEvent),
  ShelleyNewEpochEvent (DeltaRewardEvent, TotalRewardEvent),
  ShelleyTickEvent (TickNewEpochEvent, TickRupdEvent),
 )
import Cardano.Ledger.Shelley.TxBody (RewardAccount (..))
import Cardano.Ledger.Slot (epochInfoSize)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (Val (..), invert, (<+>), (<->))
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (asks, runReader)
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (fold, foldl')
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
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C)
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
  (===),
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

keyPair :: CC.Crypto c => Int -> KeyPair r c
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

data PoolSetUpArgs c f = PoolSetUpArgs
  { poolPledge :: f Coin
  , poolCost :: f Coin
  , poolMargin :: f UnitInterval
  , poolMembers :: f (Map (Credential 'Staking c) Coin)
  }

emptySetupArgs :: PoolSetUpArgs c Maybe
emptySetupArgs =
  PoolSetUpArgs
    { poolPledge = Nothing
    , poolCost = Nothing
    , poolMargin = Nothing
    , poolMembers = Nothing
    }

data PoolInfo c = PoolInfo
  { params :: PoolParams c
  , coldKey :: KeyPair 'StakePool c
  , ownerKey :: KeyPair 'Staking c
  , ownerStake :: Coin
  , rewardKey :: KeyPair 'Staking c
  , members :: Map (Credential 'Staking c) Coin
  }

-- Generators --

genNonOwnerMembers :: CC.Crypto c => Gen (Map (Credential 'Staking c) Coin)
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

genPoolInfo :: forall c. CC.Crypto c => PoolSetUpArgs c Maybe -> Gen (PoolInfo c)
genPoolInfo PoolSetUpArgs {poolPledge, poolCost, poolMargin, poolMembers} = do
  pledge <- getOrGen poolPledge $ genCoin 0 maxPoolPledeg
  cost <- getOrGen poolCost $ genCoin 0 maxPoolCost
  margin <- getOrGen poolMargin genMargin
  vrfKey <- vrfKeyPair @(VRF c) <$> arbitrary
  coldKey <- keyPair <$> arbitrary
  ownerKey <- keyPair <$> arbitrary
  rewardKey <- keyPair <$> arbitrary
  members' <- getOrGen poolMembers genNonOwnerMembers
  ownerStake <- (pledge <>) <$> genCoin 0 maxOwnerLovelaceAbovePledge
  -- here we are forcing the pool to meet the pledeg, later we may want flexibility
  let members = Map.insert (KeyHashObj . hashKey . vKey $ ownerKey) ownerStake members'
      params =
        PoolParams
          { ppId = hashKey . vKey $ coldKey
          , ppVrf = Crypto.hashVerKeyVRF . snd $ vrfKey
          , ppPledge = pledge
          , ppCost = cost
          , ppMargin = margin
          , ppRewardAccount = RewardAccount Testnet . KeyHashObj . hashKey . vKey $ rewardKey
          , ppOwners = Set.fromList [hashKey $ vKey ownerKey]
          , ppRelays = StrictSeq.empty
          , ppMetadata = SNothing
          }
  pure $ PoolInfo {params, coldKey, ownerKey, ownerStake, rewardKey, members}

genRewardPPs :: (EraPParams era, ProtVerAtMost era 6) => Gen (PParams era)
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

genBlocksMade :: [PoolParams c] -> Gen (BlocksMade c)
genBlocksMade pools = BlocksMade . Map.fromList <$> mapM f pools
  where
    f p = (ppId p,) <$> genNatural 0 maxPoolBlocks

-- Properties --

toCompactCoinError :: HasCallStack => Coin -> CompactForm Coin
toCompactCoinError c =
  fromMaybe (error $ "Invalid Coin: " <> show c) $ toCompact c

rewardsBoundedByPot ::
  forall era.
  (EraPParams era, ProtVerAtMost era 6) =>
  Proxy era ->
  Property
rewardsBoundedByPot _ = property $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo @(EraCrypto era) <$> replicate numPools emptySetupArgs
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
            Map.fromList $ (,ppId params) <$> Map.keys members
      rewardAccounts = Set.fromList $ Map.keys delegs
      poolParams =
        VMap.fromList
          [(ppId params, params) | PoolInfo {params} <- pools]
      totalLovelace = undelegatedLovelace <> fold stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      (RewardAns rs _) =
        runShelleyBase $
          reward @era
            pp
            bs
            rewardPot
            rewardAccounts
            poolParams
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
          , "\npoolParams\n"
          , show poolParams
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
  PoolParams (EraCrypto era) ->
  Stake (EraCrypto era) ->
  Rational ->
  Rational ->
  Coin ->
  Set.Set (Credential 'Staking (EraCrypto era)) ->
  Map (Credential 'Staking (EraCrypto era)) Coin
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
          (ppOwners pool)
      Coin pledge = ppPledge pool
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
            , memberRew
                poolR
                pool
                (StakeShare (unCoin (fromCompact c) % tot))
                (StakeShare sigma)
            )
          | (hk, c) <- VMap.toAscList stake
          , notPoolOwner hk
          ]
      notPoolOwner (KeyHashObj hk) = hk `Set.notMember` ppOwners pool
      notPoolOwner (ScriptHashObj _) = True
      lReward =
        leaderRew
          poolR
          pool
          (StakeShare $ fromIntegral ostake % tot)
          (StakeShare sigma)
      f =
        if HardForks.aggregatedRewards pv
          then Map.insertWith (<>)
          else Map.insert
      potentialRewards =
        f (raCredential $ ppRewardAccount pool) lReward mRewards
      potentialRewards' =
        if HardForks.forgoRewardPrefilter pv
          then potentialRewards
          else eval (addrsRew ◁ potentialRewards)
      rewards' = Map.filter (/= Coin 0) potentialRewards'

rewardOld ::
  forall era.
  EraPParams era =>
  PParams era ->
  BlocksMade (EraCrypto era) ->
  Coin ->
  Set.Set (Credential 'Staking (EraCrypto era)) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
  Stake (EraCrypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Coin ->
  ActiveSlotCoeff ->
  EpochSize ->
  ( Map
      (Credential 'Staking (EraCrypto era))
      Coin
  , Map (KeyHash 'StakePool (EraCrypto era)) Likelihood
  )
rewardOld
  pp
  (BlocksMade b)
  r
  addrsRew
  poolParams
  stake
  delegs
  (Coin totalStake)
  asc
  slotsPerEpoch = (rewards', hs)
    where
      totalBlocks = sum b
      Coin activeStake = sumAllStake stake
      results ::
        [ ( KeyHash 'StakePool (EraCrypto era)
          , Maybe (Map (Credential 'Staking (EraCrypto era)) Coin)
          , Likelihood
          )
        ]
      results = do
        (hk, pparams) <- VMap.toAscList poolParams
        let sigma = if totalStake == 0 then 0 else fromIntegral pstake % fromIntegral totalStake
            sigmaA = if activeStake == 0 then 0 else fromIntegral pstake % fromIntegral activeStake
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
                    pparams
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
        if HardForks.aggregatedRewards pv
          then Map.unionsWith (<>)
          else Map.unions
      rewards' = f $ mapMaybe (\(_, x, _) -> x) results
      hs = Map.fromList $ fmap (\(hk, _, l) -> (hk, l)) results

data RewardUpdateOld c = RewardUpdateOld
  { deltaTOld :: !DeltaCoin
  , deltaROld :: !DeltaCoin
  , rsOld :: !(Map (Credential 'Staking c) Coin)
  , deltaFOld :: !DeltaCoin
  , nonMyopicOld :: !(NonMyopic c)
  }
  deriving (Show, Eq)

createRUpdOld ::
  forall era.
  EraGov era =>
  EpochSize ->
  BlocksMade (EraCrypto era) ->
  EpochState era ->
  Coin ->
  ShelleyBase (RewardUpdateOld (EraCrypto era))
createRUpdOld slotsPerEpoch b es@(EpochState acnt ls ss nm) maxSupply =
  createRUpdOld_ @era slotsPerEpoch b ss reserves pr totalStake rs nm
  where
    ds = certDState $ lsCertState ls
    rs = UM.domain $ rewards ds
    reserves = asReserves acnt
    totalStake = circulation es maxSupply
    pr = es ^. prevPParamsEpochStateL

createRUpdOld_ ::
  forall era.
  EraPParams era =>
  EpochSize ->
  BlocksMade (EraCrypto era) ->
  SnapShots (EraCrypto era) ->
  Coin ->
  PParams era ->
  Coin ->
  Set.Set (Credential 'Staking (EraCrypto era)) ->
  NonMyopic (EraCrypto era) ->
  ShelleyBase (RewardUpdateOld (EraCrypto era))
createRUpdOld_ slotsPerEpoch b@(BlocksMade b') ss (Coin reserves) pr totalStake rs nm = do
  asc <- asks activeSlotCoeff
  let SnapShot stake' delegs' poolParams = ssStakeGo ss
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
      , nonMyopicOld = updateNonMyopic nm _R newLikelihoods
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
  era ~ C =>
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
    blocksmade :: BlocksMade (EraCrypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    unAggregated =
      runReader (createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    newWithZeros = aggregateRewards @(EraCrypto era) pv (rs unAggregated)
    new = Map.filter (/= Coin 0) newWithZeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

oldEqualsNewOn ::
  forall era.
  era ~ C =>
  ProtVer ->
  NewEpochState era ->
  Property
oldEqualsNewOn pv newepochstate = old === new
  where
    globals = testGlobals
    epochstate = overrideProtocolVersionUsedInRewardCalc pv $ nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (EraCrypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    unAggregated =
      runReader (createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old :: Map (Credential 'Staking (EraCrypto era)) Coin
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    newWithZeros = aggregateRewards @(EraCrypto era) pv (rs unAggregated)
    new = Map.filter (/= Coin 0) newWithZeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

lastElem :: [a] -> Maybe a
lastElem [a] = Just a
lastElem [] = Nothing
lastElem (_ : xs) = lastElem xs

-- | Provide a legitimate NewEpochState to make an test Property
newEpochProp :: Word64 -> (NewEpochState C -> Property) -> Property
newEpochProp tracelen propf = withMaxSuccess 100 $
  forAllChainTrace @C tracelen defaultConstants $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} -> propf (chainNes target)
      _ -> property True

-- | Given a NewEpochState and [ChainEvent], test a Property at every Epoch Boundary
newEpochEventsProp :: Word64 -> ([ChainEvent C] -> NewEpochState C -> Property) -> Property
newEpochEventsProp tracelen propf = withMaxSuccess 10 $
  forEachEpochTrace @C 10 tracelen defaultConstants $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} ->
        propf (concat (runShelleyBase $ getEvents tr)) (chainNes target)
      _ -> property True

aggIncrementalRewardEvents ::
  [ChainEvent C] ->
  Map (Credential 'Staking (EraCrypto C)) (Set (Reward (EraCrypto C)))
aggIncrementalRewardEvents = foldl' accum Map.empty
  where
    accum ans (TickEvent (TickRupdEvent (RupdEvent _ m))) = Map.unionWith Set.union m ans
    accum ans (TickEvent (TickNewEpochEvent (DeltaRewardEvent (RupdEvent _ m)))) =
      Map.unionWith Set.union m ans
    accum ans _ = ans

getMostRecentTotalRewardEvent ::
  [ChainEvent C] ->
  Map (Credential 'Staking (EraCrypto C)) (Set (Reward (EraCrypto C)))
getMostRecentTotalRewardEvent = foldl' accum Map.empty
  where
    accum ans (TickEvent (TickNewEpochEvent (TotalRewardEvent _ m))) = Map.unionWith Set.union m ans
    accum ans _ = ans

complete :: PulsingRewUpdate c -> (RewardUpdate c, RewardEvent c)
complete (Complete r) = (r, mempty)
complete (Pulsing rewsnap pulser) = runShelleyBase $ (completeRupd (Pulsing rewsnap pulser))

eventsMirrorRewards :: [ChainEvent C] -> NewEpochState C -> Property
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

instance Terse (Reward c) where
  terse (Reward ty pl (Coin n)) = "Reward{" ++ show ty ++ ", #" ++ take 9 (show pl) ++ ", " ++ show n ++ "}"

instance Terse x => Terse (Set x) where
  terse x = unlines (Set.toList (Set.map terse x))

-- ================================================================

reward ::
  forall era.
  EraPParams era =>
  PParams era ->
  BlocksMade (EraCrypto era) ->
  Coin ->
  Set (Credential 'Staking (EraCrypto era)) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)) ->
  Stake (EraCrypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)) ->
  Coin ->
  ShelleyBase (RewardAns (EraCrypto era))
reward
  pp
  (BlocksMade b)
  r
  addrsRew
  poolParams
  stake
  delegs
  totalStake = completeM pulser
    where
      totalBlocks = sum b
      stakePerPool = sumStakePerPool delegs stake
      activeStake = sumAllStake stake
      -- ensure mkPoolRewardInfo does not use stake that doesn't belong to the pool
      stakeForPool pool = poolStake (ppId pool) delegs stake
      mkPoolRewardInfo' pool =
        mkPoolRewardInfo
          pp
          r
          (BlocksMade b)
          totalBlocks
          (stakeForPool pool)
          delegs
          stakePerPool
          totalStake
          activeStake
          pool
      free =
        FreeVars
          { fvAddrsRew = addrsRew
          , fvTotalStake = totalStake
          , fvPoolRewardInfo =
              VMap.toMap $ VMap.mapMaybe (either (const Nothing) Just . mkPoolRewardInfo') poolParams
          , fvDelegs = delegs
          , fvProtVer = pp ^. ppProtocolVersionL
          }
      pulser :: Pulser (EraCrypto era)
      pulser = RSLP 2 free (unStake stake) (RewardAns Map.empty Map.empty)

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
        withMaxSuccess numberOfTests (rewardsBoundedByPot (Proxy @C))
    , testProperty "compare with reference impl, no provenance, v3" . noShrinking $
        newEpochProp chainlen (oldEqualsNew @C (ProtVer (natVersion @3) 0))
    , testProperty "compare with reference impl, no provenance, v7" . noShrinking $
        newEpochProp chainlen (oldEqualsNew @C (ProtVer (natVersion @7) 0))
    , testProperty "compare with reference impl, with provenance" . noShrinking $
        newEpochProp chainlen (oldEqualsNewOn @C (ProtVer (natVersion @3) 0))
    , testProperty "delta events mirror reward updates" $
        newEpochEventsProp chainlen eventsMirrorRewards
    ]
