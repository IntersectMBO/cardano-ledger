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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | How to compute the reward update compuation. Also, how to spread the
--     compuation over many blocks, once the chain reaches a stability point.
module Cardano.Ledger.Shelley.RewardUpdate (
  RewardEvent,
  RewardAns (..),
  Pulser,
  RewardUpdate (..),
  emptyRewardUpdate,
  RewardSnapShot (..),
  FreeVars (..),
  rewardStakePoolMember,
  RewardPulser (..),
  clearRecent,
  PulsingRewUpdate (..),
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), ShelleyBase)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decNoShareCBOR,
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
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
import Control.DeepSeq (NFData (..))
import Data.Aeson (KeyValue, ToJSON (..), Value (Null), object, pairs, (.=))
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

type RewardEvent c = Map (Credential 'Staking c) (Set (Reward c))

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
  encCBOR (RewardAns accum recent) = encodeListLen 2 <> encCBOR accum <> encCBOR recent

instance Crypto c => DecCBOR (RewardAns c) where
  decCBOR = decodeRecordNamed "RewardAns" (const 2) (RewardAns <$> decCBOR <*> decCBOR)

-- | The type of RewardPulser we pulse on.
type Pulser c = RewardPulser c ShelleyBase (RewardAns c)

-- =====================================

-- | The ultimate goal of a reward update computation.
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
  encCBOR (RewardUpdate dt dr rw df nm) =
    encodeListLen 5
      <> encCBOR dt
      <> encCBOR (invert dr) -- TODO change Coin serialization to use integers?
      <> encCBOR rw
      <> encCBOR (invert df) -- TODO change Coin serialization to use integers?
      <> encCBOR nm

instance Crypto c => DecCBOR (RewardUpdate c) where
  decCBOR = do
    decodeRecordNamed "RewardUpdate" (const 5) $ do
      dt <- decCBOR
      dr <- decCBOR -- TODO change Coin serialization to use integers?
      rw <- decCBOR
      df <- decCBOR -- TODO change Coin serialization to use integers?
      nm <- decNoShareCBOR
      pure $ RewardUpdate dt (invert dr) rw (invert df) nm

instance Crypto c => ToJSON (RewardUpdate c) where
  toJSON = object . toRewardUpdatePair
  toEncoding = pairs . mconcat . toRewardUpdatePair

toRewardUpdatePair :: (KeyValue e a, Crypto c) => RewardUpdate c -> [a]
toRewardUpdatePair ru@(RewardUpdate _ _ _ _ _) =
  let RewardUpdate {..} = ru
   in [ "deltaT" .= deltaT
      , "deltaR" .= deltaR
      , "rs" .= rs
      , "deltaF" .= deltaF
      , "nonMyopic" .= nonMyopic
      ]

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
  encCBOR (RewardSnapShot fees ver nm dr1 r dt1 lhs lrs) =
    encode
      ( Rec RewardSnapShot
          !> To fees
          !> To ver
          !> To nm
          !> To dr1
          !> To r
          !> To dt1
          !> To lhs
          !> To lrs
      )

instance Crypto c => DecCBOR (RewardSnapShot c) where
  decCBOR =
    decode
      ( RecD RewardSnapShot
          <! From
          <! From
          <! D decNoShareCBOR
          <! From
          <! From
          <! From
          <! From
          <! From
      )

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
      encode
        ( Rec FreeVars
            !> To fvDelegs
            !> To fvAddrsRew
            !> To fvTotalStake
            !> To fvProtVer
            !> To fvPoolRewardInfo
        )

instance Crypto c => DecCBOR (FreeVars c) where
  decCBOR =
    decode
      ( RecD FreeVars
          <! From {- fvDelegs -}
          <! From {- fvAddrsRew -}
          <! From {- fvTotalStake -}
          <! From {- fvProtver -}
          <! From {- fvPoolRewardInfo -}
      )

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
--     RSLP = Reward Serializable Listbased Pulser
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
  rnf (RSLP n1 c1 b1 a1) = seq (rnf n1) (seq (rnf c1) (seq (rnf b1) (rnf a1)))

instance Crypto c => EncCBOR (Pulser c) where
  encCBOR (RSLP n free balance ans) =
    encode (Rec RSLP !> To n !> To free !> To balance !> To ans)

instance Crypto c => DecCBOR (Pulser c) where
  decCBOR =
    decode (RecD RSLP <! From <! From <! From <! From)

-- =========================================================================

-- | The state used in the STS rules
data PulsingRewUpdate c
  = Pulsing !(RewardSnapShot c) !(Pulser c) -- Pulsing work still to do
  | Complete !(RewardUpdate c) -- Pulsing work completed, ultimate goal reached
  deriving (Eq, Show, Generic, NoThunks)

instance Crypto c => EncCBOR (PulsingRewUpdate c) where
  encCBOR (Pulsing s p) = encode (Sum Pulsing 0 !> To s !> To p)
  encCBOR (Complete r) = encode (Sum Complete 1 !> To r)

instance Crypto c => DecCBOR (PulsingRewUpdate c) where
  decCBOR = decode (Summands "PulsingRewUpdate" decPS)
    where
      decPS 0 = SumD Pulsing <! From <! From
      decPS 1 = SumD Complete <! From
      decPS n = Invalid n

instance NFData (PulsingRewUpdate c)

instance Crypto c => ToJSON (PulsingRewUpdate c) where
  toJSON = \case
    Pulsing _ _ -> Null
    Complete ru -> toJSON ru
  toEncoding = \case
    Pulsing _ _ -> toEncoding Null
    Complete ru -> toEncoding ru
