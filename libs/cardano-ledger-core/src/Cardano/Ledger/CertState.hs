{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.CertState (
  CertState (..),
  CommitteeAuthorization (..),
  DState (..),
  PState (..),
  VState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  Anchor (..),
  DRepState (..),
  DRep (..),
  CommitteeState (..),
  AnchorData,
  lookupDepositDState,
  lookupRewardDState,
  rewards,
  delegations,
  ptrsMap,
  payPoolDeposit,
  refundPoolDeposit,
  obligationCertState,
  Obligations (..),
  sumObligation,
  certsTotalDepositsTxBody,
  certsTotalRefundsTxBody,
  -- Lenses
  certDStateL,
  certPStateL,
  certVStateL,
  dsUnifiedL,
  dsGenDelegsL,
  dsIRewardsL,
  dsFutureGenDelegsL,
  psStakePoolParamsL,
  psFutureStakePoolParamsL,
  psRetiringL,
  psDepositsL,
  vsDRepsL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
  csCommitteeCredsL,
  lookupDepositVState,
)
where

import Cardano.Ledger.BaseTypes (Anchor (..), AnchorData, StrictMaybe)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR (..),
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr, StakeCredential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Sharing
import Cardano.Ledger.Slot (
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.UMap (RDPair (..), UMap (UMap), UView (RewDepUView, SPoolUView))
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData (..))

-- import Control.Monad.Trans (lift)

import Control.Monad.Trans.State.Strict (modify)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (def))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))

-- ======================================

data FutureGenDeleg c = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo
  , fGenDelegGenKeyHash :: !(KeyHash 'Genesis c)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureGenDeleg c)

instance NFData (FutureGenDeleg c)

instance Crypto c => EncCBOR (FutureGenDeleg c) where
  encCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> encCBOR a <> encCBOR b

instance Crypto c => DecCBOR (FutureGenDeleg c) where
  decCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> decCBOR <*> decCBOR

instance Crypto c => ToJSON (FutureGenDeleg c) where
  toJSON fGenDeleg =
    object
      [ "fGenDelegSlot" .= fGenDelegSlot fGenDeleg
      , "fGenDelegGenKeyHash" .= fGenDelegGenKeyHash fGenDeleg
      ]

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards c = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking c) Coin)
  , iRTreasury :: !(Map (Credential 'Staking c) Coin)
  , deltaReserves :: !DeltaCoin
  , deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c)

instance NFData (InstantaneousRewards c)

instance Crypto c => ToJSON (InstantaneousRewards c) where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair :: (KeyValue e a, Crypto c) => InstantaneousRewards c -> [a]
toInstantaneousRewardsPair InstantaneousRewards {..} =
  [ "iRReserves" .= iRReserves
  , "iRTreasury" .= iRTreasury
  , "deltaReserves" .= deltaReserves
  , "deltaTreasury" .= deltaTreasury
  ]

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState era = DState
  { dsUnified :: !(UMap (EraCrypto era))
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !(GenDelegs (EraCrypto era))
  -- ^ Genesis key delegations
  , dsIRewards :: !(InstantaneousRewards (EraCrypto era))
  -- ^ Instantaneous Rewards
  }
  deriving (Show, Eq, Generic)

instance NoThunks (DState era)

instance NFData (DState era)

instance Era era => EncCBOR (DState era) where
  encCBOR = encode . encDState

encDState :: Era era => DState era -> Encode ('Closed 'Dense) (DState era)
encDState (DState unified fgs gs ir) =
  Rec DState
    !> To unified
    !> To fgs
    !> To gs
    !> To ir

instance Era era => DecCBOR (DState era) where
  decCBOR = decode $ RecD DState <! From <! From <! From <! From

instance Era era => DecShareCBOR (DState era) where
  type
    Share (DState era) =
      DShare era
  decSharePlusCBOR = decodeRecordNamedT "DState" (const 4) $ do
    umap <-
      decSharePlusLensCBOR
        ( pairL
            (dShareToPShareL . credentialStakingL)
            (dShareToPShareL . keyHashStakePoolL)
        )
    -- (Interns (Credential 'Staking c),Interns (KeyHash 'StakePool c'))
    fdeleg <- lift decCBOR
    deleg <- lift decCBOR
    insta <- decSharePlusLensCBOR (dShareToPShareL . credentialStakingL) -- Interns (Credential 'Staking c)
    pure (DState umap fdeleg deleg insta)

instance Era era => ToJSON (DState era) where
  toJSON = object . toDStatePair
  toEncoding = pairs . mconcat . toDStatePair

toDStatePair :: (KeyValue e a, Era era) => DState era -> [a]
toDStatePair DState {..} =
  [ "unified" .= dsUnified
  , "fGenDelegs" .= Map.toList dsFutureGenDelegs
  , "genDelegs" .= dsGenDelegs
  , "irwd" .= dsIRewards
  ]

-- | Function that looks up the deposit for currently delegated staking credential
lookupDepositDState :: DState era -> (StakeCredential (EraCrypto era) -> Maybe Coin)
lookupDepositDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair _ deposit <- UM.lookup k currentRewardDeposits
        Just $! fromCompact deposit

-- | Function that looks up curret reward for the delegated staking credential.
lookupRewardDState :: DState era -> (StakeCredential (EraCrypto era) -> Maybe Coin)
lookupRewardDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair reward _ <- UM.lookup k currentRewardDeposits
        Just $! fromCompact reward

-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  -- ^ A map of the deposits for each pool
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState era)

instance NFData (PState era)

instance Era era => EncCBOR (PState era) where
  encCBOR = encode . encodePState

encodePState :: Era era => PState era -> Encode ('Closed 'Dense) (PState era)
encodePState (PState a b c d) = Rec PState !> To a !> To b !> To c !> To d

-- | Note that all components have type (Map (KeyHash 'StakePool c) _)
--   So we want to intern only the domain of the Map, Since Maps always
--   take a pair of Shares, we genup a bogus Share for the range
--   using the Lens (toMemptyLens fstL id)
instance Era era => DecShareCBOR (PState era) where
  type Share (PState era) = PShare (EraCrypto era)
  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    poolparams0 <- decSharePlusLensCBOR (shareMapL keyHashStakePoolL)
    futurepool0 <- decSharePlusLensCBOR (shareMapL keyHashStakePoolL)
    -- Construct the sharing from the PoolParams (the range of the maps)
    let (poolparams, share1) = mapShare poolparams0 mempty
        (futurepool, share2) = mapShare futurepool0 share1
    -- add that PoolParam sharing to the underlying PShare
    modify (merge share2)
    retiring <- decSharePlusLensCBOR (shareMapL keyHashStakePoolL)
    deposits <- decSharePlusLensCBOR (shareMapL keyHashStakePoolL)
    pure (PState poolparams futurepool retiring deposits)

instance (Era era, DecCBOR (PState era)) => DecCBOR (PState era) where
  decCBOR = decode $ RecD PState <! From <! From <! From <! From

{-
instance (Era era, DecShareCBOR (PState era)) => DecCBOR (PState era) where
  decCBOR = decNoShareCBOR
-}

instance Era era => ToJSON (PState era) where
  toJSON = object . toPStatePair
  toEncoding = pairs . mconcat . toPStatePair

toPStatePair :: (KeyValue e a, Era era) => PState era -> [a]
toPStatePair PState {..} =
  [ "stakePoolParams" .= psStakePoolParams
  , "futureStakePoolParams" .= psFutureStakePoolParams
  , "retiring" .= psRetiring
  , "deposits" .= psDeposits
  ]

data CommitteeAuthorization c
  = -- | Member authorized with a Hot credential acting on behalf of their Cold credential
    CommitteeHotCredential !(Credential 'HotCommitteeRole c)
  | -- | Member resigned with a potential explanation in Anchor
    CommitteeMemberResigned !(StrictMaybe (Anchor c))
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance NoThunks (CommitteeAuthorization c)
instance Crypto c => NFData (CommitteeAuthorization c)

instance Crypto c => EncCBOR (CommitteeAuthorization c) where
  encCBOR =
    encode . \case
      CommitteeHotCredential cred -> Sum CommitteeHotCredential 0 !> To cred
      CommitteeMemberResigned anchor -> Sum CommitteeMemberResigned 1 !> To anchor

instance Crypto c => DecCBOR (CommitteeAuthorization c) where
  decCBOR =
    decode $ Summands "CommitteeAuthorization" $ \case
      0 -> SumD CommitteeHotCredential <! From
      1 -> SumD CommitteeMemberResigned <! From
      k -> Invalid k

newtype CommitteeState era = CommitteeState
  { csCommitteeCreds ::
      Map
        (Credential 'ColdCommitteeRole (EraCrypto era))
        (CommitteeAuthorization (EraCrypto era))
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (CommitteeState era)
instance Default (CommitteeState era)

instance Era era => NFData (CommitteeState era)

-- deriving newtype instance Era era => EncCBOR (CommitteeState era)
instance Era era => EncCBOR (CommitteeState era) where
  encCBOR (CommitteeState x) = encode $ Newtype (CommitteeState @era) !> To x

instance Era era => DecShareCBOR (CommitteeState era) where
  type Share (CommitteeState era) = Interns (Credential 'ColdCommitteeRole (EraCrypto era))
  decShareCBOR share =
    decShareFromDecode share $
      (Pure (NewtypeD CommitteeState) <!> WriteShare (toMemptyLens fstL id))

-- Map (Credential 'ColdCommitteeRole c) (CommitteeAuthorization c)

instance Era era => DecCBOR (CommitteeState era) where
  decCBOR = decNoShareCBOR

deriving newtype instance Era era => ToJSON (CommitteeState era)

instance Era era => ToCBOR (CommitteeState era) where
  toCBOR = toEraCBOR @era

-- | The state that tracks the voting entities (DReps and Constitutional Committee members)
data VState era = VState
  { vsDReps ::
      !( Map
          (Credential 'DRepRole (EraCrypto era))
          (DRepState (EraCrypto era))
       )
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }
  deriving (Show, Eq, Generic)

-- | Function that looks up the deposit for currently registered DRep
lookupDepositVState :: VState era -> Credential 'DRepRole (EraCrypto era) -> Maybe Coin
lookupDepositVState vstate = fmap drepDeposit . flip Map.lookup (vstate ^. vsDRepsL)

instance Default (VState era) where
  def = VState def def (EpochNo 0)

instance Typeable (EraCrypto era) => NoThunks (VState era)

instance Era era => NFData (VState era)

{-
instance Era era => DecCBOR (VState era) where
  decCBOR = decNoShareCBOR
-}

instance Era era => DecCBOR (VState era) where
  decCBOR = decode $ RecD (VState @era) <! From <! From <! From

-- | Note that credL :: Lens' (Credential 'Staking c) (Credential 'Voting c)
--   This lets us intern  (Credential 'Voting c) in the (Credential 'Staking c) Share
--   because they have the same representation, and only differ in their indexe
instance Era era => DecShareCBOR (VState era) where
  type Share (VState era) = VShare era
  decSharePlusCBOR = decodeRecordNamedT "VState" (const 3) $ do
    reps <- decSharePlusLensCBOR (shareMapL credentialDRepRoleL)
    comm <- decSharePlusLensCBOR credentialColdCommitteeRoleL
    dorm <- lift decCBOR
    pure (VState reps comm dorm)

instance Era era => EncCBOR (VState era) where
  encCBOR VState {..} =
    encode $
      Rec (VState @era)
        !> To vsDReps
        !> To vsCommitteeState
        !> To vsNumDormantEpochs

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data CertState era = CertState
  { certVState :: !(VState era)
  , certPState :: !(PState era)
  , certDState :: !(DState era)
  }
  deriving (Show, Eq, Generic)

instance Typeable (EraCrypto era) => NoThunks (CertState era)

instance Era era => NFData (CertState era)

instance Crypto c => EncCBOR (InstantaneousRewards c) where
  encCBOR (InstantaneousRewards irR irT dR dT) =
    encode $
      (Rec InstantaneousRewards !> To irR !> To irT !> To dR !> To dT)

instance Crypto c => DecCBOR (InstantaneousRewards c) where
  decCBOR = decode $ (RecD InstantaneousRewards <! From <! From <! From <! From)

-- | Note for the iRResrves and iRTreasury fields which are both maps
--   we need to expand the Share from (Interns cred) to (Interns cred,Interns coin).
--   This is how Map works. That is the purpose of the lens (toMemptyLens fstL id)
instance Crypto c => DecShareCBOR (InstantaneousRewards c) where
  type Share (InstantaneousRewards c) = Interns (Credential 'Staking c)
  decSharePlusCBOR = decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
    reserve <- decSharePlusLensCBOR (toMemptyLens fstL id)
    treas <- decSharePlusLensCBOR (toMemptyLens fstL id)
    deltaReserve <- lift decCBOR
    deltaTreas <- lift decCBOR
    pure (InstantaneousRewards reserve treas deltaReserve deltaTreas)

instance Era era => EncCBOR (CertState era) where
  encCBOR CertState {certPState, certDState, certVState} =
    encode $
      (Rec CertState !> To certVState !> To certPState !> To certDState)

instance Era era => DecCBOR (CertState era) where
  decCBOR = decode $ (RecD CertState <! From <! From <! From)

instance Era era => DecShareCBOR (CertState era) where
  type
    Share (CertState era) =
      CertShare era
  decSharePlusCBOR = decodeRecordNamedT "CertState" (const 3) $ do
    vstate <- decSharePlusLensCBOR certShareToVShareL -- VState needs: VShare
    pstate <- decSharePlusLensCBOR (certShareToDShareL . dShareToPShareL) -- PState needs: PShare
    dstate <- decSharePlusLensCBOR certShareToDShareL -- DState needs: DShare
    pure (CertState vstate pstate dstate)

instance Default (CertState era) where
  def = CertState def def def

instance Era era => ToJSON (CertState era) where
  toJSON = object . toCertStatePairs
  toEncoding = pairs . mconcat . toCertStatePairs

toCertStatePairs :: (KeyValue e a, Era era) => CertState era -> [a]
toCertStatePairs CertState {..} =
  [ "dstate" .= certDState
  , "pstate" .= certPState
  ]

instance Default (InstantaneousRewards c) where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState era) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState c) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState era -> UView (EraCrypto era) (Credential 'Staking (EraCrypto era)) RDPair
rewards = RewDepUView . dsUnified

delegations ::
  DState era ->
  UView (EraCrypto era) (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
delegations = SPoolUView . dsUnified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState era -> Map Ptr (Credential 'Staking (EraCrypto era))
ptrsMap (DState {dsUnified = UMap _ ptrmap}) = ptrmap

-- ==========================================================
-- Functions that handle Deposits

-- | One only pays a deposit on the initial pool registration. So return the
--   the Deposits unchanged if the keyhash already exists. There are legal
--   situations where a pool may be registered multiple times.
payPoolDeposit ::
  EraPParams era =>
  KeyHash 'StakePool (EraCrypto era) ->
  PParams era ->
  PState era ->
  PState era
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    !deposit = pp ^. ppPoolDepositL
    newpool
      | Map.notMember keyhash pool = Map.insert keyhash deposit pool
      | otherwise = pool

refundPoolDeposit :: KeyHash 'StakePool (EraCrypto era) -> PState era -> (Coin, PState era)
refundPoolDeposit keyhash pstate = (coin, pstate {psDeposits = newpool})
  where
    pool = psDeposits pstate
    (coin, newpool) = case Map.lookup keyhash pool of
      Just c -> (c, Map.delete keyhash pool)
      Nothing -> (mempty, pool)

-- | A composite of all the Deposits the system is obligated to eventually pay back.
data Obligations = Obligations
  { oblStake :: !Coin
  , oblPool :: !Coin
  , oblDRep :: !Coin
  , oblProposal :: !Coin
  }
  deriving (Eq, Ord, Generic)

instance NFData Obligations

-- | Calculate total possible refunds in the system that are related to certificates
--
-- There is an invariant that the sum of all the fields should be the same as the
-- utxosDeposited field of the UTxOState. Note that this does not depend upon the current
-- values of the Key and Pool deposits of the PParams.
obligationCertState :: CertState era -> Obligations
obligationCertState (CertState VState {vsDReps} PState {psDeposits} DState {dsUnified}) =
  let accum ans drepState = ans <> drepDeposit drepState
   in Obligations
        { oblStake = UM.fromCompact (UM.sumDepositUView (RewDepUView dsUnified))
        , oblPool = foldl' (<>) (Coin 0) psDeposits
        , oblDRep = foldl' accum (Coin 0) vsDReps
        , oblProposal = Coin 0
        }

sumObligation :: Obligations -> Coin
sumObligation x = oblStake x <> oblPool x <> oblDRep x <> oblProposal x

instance Semigroup Obligations where
  x <> y =
    Obligations
      { oblStake = oblStake x <> oblStake y
      , oblPool = oblPool x <> oblPool y
      , oblDRep = oblDRep x <> oblDRep y
      , oblProposal = oblProposal x <> oblProposal y
      }

instance Monoid Obligations where
  mempty = Obligations {oblStake = Coin 0, oblPool = Coin 0, oblDRep = Coin 0, oblProposal = Coin 0}

instance Show Obligations where
  show x =
    unlines
      [ "Total Obligations = " ++ show (sumObligation x)
      , "   Stake deposits = " ++ show (oblStake x)
      , "   Pool deposits = " ++ show (oblPool x)
      , "   DRep deposits = " ++ show (oblDRep x)
      , "   Proposal deposits = " ++ show (oblProposal x)
      ]

-- | Compute the total deposits from the Certs of a TxBody.
--
-- This is the contribution of a TxBody towards the deposit pot (utxosDeposit field of
-- the UTxOState) of the system
certsTotalDepositsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin
certsTotalDepositsTxBody pp CertState {certPState} =
  getTotalDepositsTxBody pp (`Map.member` psStakePoolParams certPState)

-- | Compute the total refunds from the Certs of a TxBody.
--
-- This is the contribution of a TxBody towards the total 'Obligations' of the system
-- See `Obligations` and `obligationCertState` for more information.
certsTotalRefundsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin
certsTotalRefundsTxBody pp CertState {certDState, certVState} =
  getTotalRefundsTxBody pp (lookupDepositDState certDState) (lookupDepositVState certVState)

-- =======================================================
-- Lenses for CertState and its subsidiary types

-- ========================================
-- CertState

certDStateL :: Lens' (CertState era) (DState era)
certDStateL = lens certDState (\ds u -> ds {certDState = u})

certPStateL :: Lens' (CertState era) (PState era)
certPStateL = lens certPState (\ds u -> ds {certPState = u})

certVStateL :: Lens' (CertState era) (VState era)
certVStateL = lens certVState (\ds u -> ds {certVState = u})

-- ===================================
-- DState

dsUnifiedL :: Lens' (DState era) (UMap (EraCrypto era))
dsUnifiedL = lens dsUnified (\ds u -> ds {dsUnified = u})

dsGenDelegsL :: Lens' (DState era) (GenDelegs (EraCrypto era))
dsGenDelegsL = lens dsGenDelegs (\ds u -> ds {dsGenDelegs = u})

dsIRewardsL :: Lens' (DState era) (InstantaneousRewards (EraCrypto era))
dsIRewardsL = lens dsIRewards (\ds u -> ds {dsIRewards = u})

dsFutureGenDelegsL ::
  Lens' (DState era) (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
dsFutureGenDelegsL = lens dsFutureGenDelegs (\ds u -> ds {dsFutureGenDelegs = u})

-- ===================================
-- PState

psStakePoolParamsL ::
  Lens' (PState era) (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
psStakePoolParamsL = lens psStakePoolParams (\ds u -> ds {psStakePoolParams = u})

psFutureStakePoolParamsL ::
  Lens' (PState era) (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
psFutureStakePoolParamsL = lens psFutureStakePoolParams (\ds u -> ds {psFutureStakePoolParams = u})

psRetiringL :: Lens' (PState era) (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
psRetiringL = lens psRetiring (\ds u -> ds {psRetiring = u})

psDepositsL :: Lens' (PState era) (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
psDepositsL = lens psDeposits (\ds u -> ds {psDeposits = u})

-- ===================================
-- VState

vsDRepsL ::
  Lens' (VState era) (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
vsDRepsL = lens vsDReps (\vs u -> vs {vsDReps = u})

vsCommitteeStateL :: Lens' (VState era) (CommitteeState era)
vsCommitteeStateL = lens vsCommitteeState (\vs u -> vs {vsCommitteeState = u})

vsNumDormantEpochsL :: Lens' (VState era) EpochNo
vsNumDormantEpochsL = lens vsNumDormantEpochs (\vs u -> vs {vsNumDormantEpochs = u})

csCommitteeCredsL ::
  Lens'
    (CommitteeState era)
    ( Map
        (Credential 'ColdCommitteeRole (EraCrypto era))
        (CommitteeAuthorization (EraCrypto era))
    )
csCommitteeCredsL = lens csCommitteeCreds (\cs u -> cs {csCommitteeCreds = u})
