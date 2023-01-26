{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | How to compute the reward update compuation. Also, how to spread the
--     compuation over many blocks, once the chain reaches a stability point.
module Cardano.Ledger.Shelley.RewardUpdate where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase)
import Cardano.Ledger.Binary.Plain (
  DecCBOR (..),
  EncCBOR (..),
  decNoShareCBOR,
  decodeListLen,
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm, DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (fromCompact))
import Cardano.Ledger.Core (Reward (..), RewardType (MemberReward))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.PoolRank (Likelihood, NonMyopic)
import Cardano.Ledger.Shelley.Rewards (
  PoolRewardInfo (..),
  rewardOnePoolMember,
 )
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (toExpr))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (when)
import Data.Default.Class (def)
import Data.Group (invert)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pulse (Pulsable (..), completeM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.VMap as VMap
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), allNoThunks)

-- ===============================================================

type RewardEvent c = (Map (Credential 'Staking c) (Set (Reward c)))

-- | The result of reward calculation is a pair of aggregate Maps.
--   One for the accumulated answer, and one for the answer since the last pulse
data RewardAns c = RewardAns
  { accumRewardAns :: !(Map (Credential 'Staking c) (Reward c))
  , recentRewardAns :: !(RewardEvent c)
  }
  deriving (Show, Eq, Generic)
  deriving (NFData)

instance NoThunks (RewardAns c)

instance Crypto c => EncCBOR (RewardAns c) where
  encCBOR RewardAns {accumRewardAns, recentRewardAns} =
    encCBOR (accumRewardAns, recentRewardAns)

instance Crypto c => DecCBOR (RewardAns c) where
  decCBOR = do
    (accumRewardAns, recentRewardAns) <- decCBOR
    pure RewardAns {accumRewardAns, recentRewardAns}

-- | The type of RewardPulser we pulse on.
type Pulser c = RewardPulser c ShelleyBase (RewardAns c)

-- =====================================

-- | The ultiate goal of a reward update computation.
--     Aggregating rewards for each staking credential.
data RewardUpdate c = RewardUpdate
  { deltaT :: !DeltaCoin
  , deltaR :: !DeltaCoin
  , rs :: !(Map (Credential 'Staking c) (Set (Reward c)))
  , deltaF :: !DeltaCoin
  , nonMyopic :: !(NonMyopic c)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (RewardUpdate c)

instance NFData (RewardUpdate c)

instance Crypto c => EncCBOR (RewardUpdate c) where
  encCBOR RewardUpdate {deltaT, deltaR, rs, deltaF, nonMyopic} =
    encCBOR (deltaT, deltaR, rs, deltaF, nonMyopic)

instance Crypto c => DecCBOR (RewardUpdate c) where
  decCBOR = do
    (deltaT, deltaR, rs, deltaF, nonMyopic) <- decCBOR
    pure $ RewardUpdate {deltaT, deltaR, rs, deltaF, nonMyopic}

emptyRewardUpdate :: RewardUpdate c
emptyRewardUpdate =
  RewardUpdate (DeltaCoin 0) (DeltaCoin 0) Map.empty (DeltaCoin 0) def

-- ===================================================

-- | To complete the reward update, we need a snap shot of the EpochState particular to this computation
data RewardSnapShot c = RewardSnapShot
  { rewFees :: !Coin
  , rewProtocolVersion :: !ProtVer
  , rewNonMyopic :: !(NonMyopic c)
  , rewDeltaR1 :: !Coin -- deltaR1
  , rewR :: !Coin -- r
  , rewDeltaT1 :: !Coin -- deltaT1
  , rewLikelihoods :: !(Map (KeyHash 'StakePool c) Likelihood)
  , rewLeaders :: !(Map (Credential 'Staking c) (Set (Reward c)))
  }
  deriving (Show, Eq, Generic)

instance Typeable c => NoThunks (RewardSnapShot c)

instance NFData (RewardSnapShot c)

instance Crypto c => EncCBOR (RewardSnapShot c) where
  encCBOR
    RewardSnapShot
      { rewFees
      , rewProtocolVersion
      , rewNonMyopic
      , rewDeltaR1
      , rewR
      , rewDeltaT1
      , rewLikelihoods
      , rewLeaders
      } =
      encCBOR
        ( rewFees
        , rewProtocolVersion
        , rewNonMyopic
        , rewDeltaR1
        , rewR
        , rewDeltaT1
        , rewLikelihoods
        , rewLeaders
        )

instance Crypto c => DecCBOR (RewardSnapShot c) where
  decCBOR = do
    ( rewFees
      , rewProtocolVersion
      , rewNonMyopic
      , rewDeltaR1
      , rewR
      , rewDeltaT1
      , rewLikelihoods
      , rewLeaders
      ) <-
      decCBOR
    pure RewardSnapShot {..}

-- ========================================================
-- FreeVars is the set of variables needed to compute
-- rewardStakePool, so that it can be made into a serializable
-- Pulsable function.

data FreeVars c = FreeVars
  { fvDelegs :: !(VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c))
  , fvAddrsRew :: !(Set (Credential 'Staking c))
  , fvTotalStake :: !Coin
  , fvProtVer :: !ProtVer
  , fvPoolRewardInfo :: !(Map (KeyHash 'StakePool c) (PoolRewardInfo c))
  }
  deriving (Eq, Show, Generic)
  deriving (NoThunks)

instance NFData (FreeVars c)

instance Crypto c => EncCBOR (FreeVars c) where
  encCBOR
    FreeVars
      { fvDelegs
      , fvAddrsRew
      , fvTotalStake
      , fvProtVer
      , fvPoolRewardInfo
      } =
      encCBOR
        ( fvDelegs
        , fvAddrsRew
        , fvTotalStake
        , fvProtVer
        , fvPoolRewardInfo
        )

instance Crypto c => DecCBOR (FreeVars c) where
  decCBOR = do
    ( fvDelegs
      , fvAddrsRew
      , fvTotalStake
      , fvProtVer
      , fvPoolRewardInfo
      ) <-
      decCBOR
    pure FreeVars {..}

-- =====================================================================

-- | The function to call on each reward update pulse. Called by the pulser.
rewardStakePoolMember ::
  FreeVars c ->
  RewardAns c ->
  Credential 'Staking c ->
  CompactForm Coin ->
  RewardAns c
rewardStakePoolMember
  FreeVars
    { fvDelegs
    , fvAddrsRew
    , fvTotalStake
    , fvPoolRewardInfo
    , fvProtVer
    }
  inputanswer@(RewardAns accum recent)
  cred
  c = fromMaybe inputanswer $ do
    poolID <- VMap.lookup cred fvDelegs
    poolRI <- Map.lookup poolID fvPoolRewardInfo
    r <- rewardOnePoolMember fvProtVer fvTotalStake fvAddrsRew poolRI cred (fromCompact c)
    let ans = Reward MemberReward poolID r
    -- There is always just 1 member reward, so Set.singleton is appropriate
    pure $ RewardAns (Map.insert cred ans accum) (Map.insert cred (Set.singleton ans) recent)

-- ================================================================

-- | The type of a Pulser which uses 'rewardStakePoolMember' as its underlying function.
--     'rewardStakePool' will be partially applied to the component of type
--     (FreeVars c) when pulsing. Note that we use two type equality (~) constraints
--     to fix both the monad 'm' and the 'ans' type, to the context where we will use
--     the type as a Pulser. The type must have 'm' and 'ans' as its last two
--     parameters so we can make a Pulsable instance.
--     RSPL = Reward Serializable Listbased Pulser
data RewardPulser c (m :: Type -> Type) ans where
  RSLP ::
    (ans ~ RewardAns c, m ~ ShelleyBase) =>
    !Int ->
    !(FreeVars c) ->
    !(VMap.VMap VMap.VB VMap.VP (Credential 'Staking c) (CompactForm Coin)) ->
    !ans ->
    RewardPulser c m ans

-- Because of the constraints on the Constructor RSLP, there is really only one inhabited
-- type:  (RewardPulser c ShelleyBase (RewardAns c))
-- All of the instances are at that type. Though only the CBOR instances need make that explicit.

clearRecent :: RewardAns c -> RewardAns c
clearRecent (RewardAns accum _) = RewardAns accum Map.empty

instance Pulsable (RewardPulser c) where
  done (RSLP _n _free zs _ans) = VMap.null zs
  current (RSLP _ _ _ ans) = ans
  pulseM p@(RSLP n free balance (clearRecent -> ans)) =
    if VMap.null balance
      then pure p
      else do
        let !(steps, !balance') = VMap.splitAt n balance
            ans' = VMap.foldlWithKey (rewardStakePoolMember free) ans steps
        pure $! RSLP n free balance' ans'
  completeM (RSLP _ free balance (clearRecent -> ans)) =
    pure $ VMap.foldlWithKey (rewardStakePoolMember free) ans balance

deriving instance Eq ans => Eq (RewardPulser c m ans)

deriving instance Show ans => Show (RewardPulser c m ans)

instance Typeable c => NoThunks (Pulser c) where
  showTypeOf _ = "RewardPulser"
  wNoThunks ctxt (RSLP n free balance ans) =
    allNoThunks
      [ noThunks ctxt n
      , noThunks ctxt free
      , noThunks ctxt balance
      , noThunks ctxt ans
      ]

instance NFData (Pulser c) where
  rnf (RSLP n free balance ans) = n `deepseq` free `deepseq` balance `deepseq` rnf ans

instance Crypto c => EncCBOR (Pulser c) where
  encCBOR (RSLP n free balance ans) = encCBOR (n, free, balance, ans)

instance Crypto c => DecCBOR (Pulser c) where
  decCBOR = do
    (n, free, balance, ans) <- decCBOR
    pure $ RSLP n free balance ans

-- =========================================================================

-- | The state used in the STS rules
data PulsingRewUpdate c
  = Pulsing !(RewardSnapShot c) !(Pulser c) -- Pulsing work still to do
  | Complete !(RewardUpdate c) -- Pulsing work completed, ultimate goal reached
  deriving (Eq, Show, Generic, NoThunks)

instance Crypto c => EncCBOR (PulsingRewUpdate c) where
  encCBOR (Pulsing s p) = encodeListLen 3 <> encCBOR (0 :: Word) <> encCBOR s <> encCBOR p
  encCBOR (Complete r) = encodeListLen 2 <> encCBOR (1 :: Word) <> encCBOR r

instance Crypto c => DecCBOR (PulsingRewUpdate c) where
  decCBOR = do
    n <- decodeListLen
    when (n < 1) $ fail $ "<PulsingRewUpdate> Unexpected list length: " ++ show n
    decCBOR >>= \case
      0 | n == 3 -> Pulsing <$> decCBOR <*> decCBOR
      1 | n == 2 -> Complete <$> decCBOR
      t ->
        fail $
          "<PulsingRewUpdate> Unexpected combination of key: "
            ++ show t
            ++ " and list length: "
            ++ show n

instance NFData (PulsingRewUpdate c)

-- ===============================================================

-- | You really don't want to see what is inside this.
instance ToExpr (PulsingRewUpdate c) where
  toExpr _ = App "PulsingRewUpdate..." []
