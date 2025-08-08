{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.State.CertState (
  EraCertState (..),
  CommitteeAuthorization (..),
  DState (..),
  PState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  Anchor (..),
  DRepState (..),
  DRep (..),
  CommitteeState (..),
  authorizedHotCommitteeCredentials,
  AnchorData,
  lookupDepositDState,
  lookupRewardDState,
  payPoolDeposit,
  refundPoolDeposit,
  Obligations (..),
  sumObligation,
  -- Lenses
  dsGenDelegsL,
  dsIRewardsL,
  dsFutureGenDelegsL,
  psStakePoolStateL,
  psFutureStakePoolStateL,
  psRetiringL,
  psDepositsL,
  psDepositsCompactL,
) where

import Cardano.Ledger.BaseTypes (
  Anchor (..),
  AnchorData,
  KeyValuePairs (..),
  StrictMaybe,
  ToKeyValuePairs (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
  decSharePlusCBOR,
  decSharePlusLensCBOR,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeListLen,
  internsFromSet,
  toMemptyLens,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), compactCoinOrError)
import Cardano.Ledger.Compactible (Compactible (..), fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.StakePool (StakePoolState)
import Control.DeepSeq (NFData (..))
import Control.Monad.Trans
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Default (Default (def))
import qualified Data.Foldable as F
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.), _1)
import NoThunks.Class (NoThunks (..))

-- ======================================

data FutureGenDeleg = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo
  , fGenDelegGenKeyHash :: !(KeyHash 'Genesis)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks FutureGenDeleg

instance NFData FutureGenDeleg

instance EncCBOR FutureGenDeleg where
  encCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> encCBOR a <> encCBOR b

instance DecCBOR FutureGenDeleg where
  decCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> decCBOR <*> decCBOR

instance ToJSON FutureGenDeleg where
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
data InstantaneousRewards = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking) Coin)
  , iRTreasury :: !(Map (Credential 'Staking) Coin)
  , deltaReserves :: !DeltaCoin
  , deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs InstantaneousRewards

instance NoThunks InstantaneousRewards

instance NFData InstantaneousRewards

instance ToKeyValuePairs InstantaneousRewards where
  toKeyValuePairs InstantaneousRewards {..} =
    [ "iRReserves" .= iRReserves
    , "iRTreasury" .= iRTreasury
    , "deltaReserves" .= deltaReserves
    , "deltaTreasury" .= deltaTreasury
    ]

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState era = DState
  { dsAccounts :: !(Accounts era)
  -- ^ Keep track of the account state (eg. balance, deposit, stake-pool delegation, etc.) for all registered stake credentials.
  , dsFutureGenDelegs :: !(Map FutureGenDeleg GenDelegPair)
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !GenDelegs
  -- ^ Genesis key delegations
  , dsIRewards :: !InstantaneousRewards
  -- ^ Instantaneous Rewards
  }
  deriving (Generic)

instance CanGetAccounts DState

instance CanSetAccounts DState where
  accountsL =
    lens dsAccounts (\dState accounts -> dState {dsAccounts = accounts})
  {-# INLINE accountsL #-}

deriving instance Eq (Accounts era) => Eq (DState era)

deriving instance Show (Accounts era) => Show (DState era)

deriving via
  KeyValuePairs (DState era)
  instance
    ToJSON (Accounts era) => ToJSON (DState era)

instance NoThunks (Accounts era) => NoThunks (DState era)

instance NFData (Accounts era) => NFData (DState era)

instance (Era era, EncCBOR (Accounts era)) => EncCBOR (DState era) where
  encCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> encCBOR unified
      <> encCBOR fgs
      <> encCBOR gs
      <> encCBOR ir

instance EraAccounts era => DecShareCBOR (DState era) where
  type
    Share (DState era) =
      (Interns (Credential 'Staking), Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decSharePlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- decSharePlusCBOR
      fgs <- lift decCBOR
      gs <- lift decCBOR
      ir <- decSharePlusLensCBOR _1
      pure $ DState unified fgs gs ir

instance ToJSON (Accounts era) => ToKeyValuePairs (DState era) where
  toKeyValuePairs DState {..} =
    [ "accounts" .= dsAccounts
    , "fGenDelegs" .= Map.toList dsFutureGenDelegs
    , "genDelegs" .= dsGenDelegs
    , "irwd" .= dsIRewards
    ]

-- | Function that looks up the deposit for currently delegated staking credential
lookupDepositDState :: EraAccounts era => DState era -> (StakeCredential -> Maybe Coin)
lookupDepositDState DState {dsAccounts} cred = do
  accountState <- Map.lookup cred (dsAccounts ^. accountsMapL)
  Just $! fromCompact (accountState ^. depositAccountStateL)

-- | Function that looks up curret reward for the delegated staking credential.
lookupRewardDState :: EraAccounts era => DState era -> (StakeCredential -> Maybe Coin)
lookupRewardDState DState {dsAccounts} cred = do
  accountState <- Map.lookup cred (dsAccounts ^. accountsMapL)
  Just $! fromCompact (accountState ^. balanceAccountStateL)

-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolState :: !(Map (KeyHash 'StakePool) StakePoolState)
  -- ^ The stake pool state.
  , psFutureStakePoolState :: !(Map (KeyHash 'StakePool) StakePoolState)
  -- ^ The future stake pool state.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool) (CompactForm Coin))
  -- ^ A map of the deposits for each pool
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs (PState era)

instance NoThunks (PState era)

instance NFData (PState era)

instance Era era => EncCBOR (PState era) where
  encCBOR (PState a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance DecShareCBOR (PState era) where
  type Share (PState era) = Interns (KeyHash 'StakePool)
  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolState <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolState <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- decSharePlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolState, psFutureStakePoolState, psRetiring, psDeposits}

instance (Era era, DecShareCBOR (PState era)) => DecCBOR (PState era) where
  decCBOR = decNoShareCBOR

instance ToKeyValuePairs (PState era) where
  toKeyValuePairs PState {..} =
    [ "stakePoolState" .= psStakePoolState
    , "futureStakePoolState" .= psFutureStakePoolState
    , "retiring" .= psRetiring
    , "deposits" .= psDeposits
    ]

data CommitteeAuthorization
  = -- | Member authorized with a Hot credential acting on behalf of their Cold credential
    CommitteeHotCredential !(Credential 'HotCommitteeRole)
  | -- | Member resigned with a potential explanation in Anchor
    CommitteeMemberResigned !(StrictMaybe Anchor)
  deriving (Eq, Ord, Show, Generic)

instance NoThunks CommitteeAuthorization

instance NFData CommitteeAuthorization

instance ToJSON CommitteeAuthorization

instance EncCBOR CommitteeAuthorization where
  encCBOR =
    encode . \case
      CommitteeHotCredential cred -> Sum CommitteeHotCredential 0 !> To cred
      CommitteeMemberResigned anchor -> Sum CommitteeMemberResigned 1 !> To anchor

instance DecCBOR CommitteeAuthorization where
  decCBOR =
    decode $ Summands "CommitteeAuthorization" $ \case
      0 -> SumD CommitteeHotCredential <! From
      1 -> SumD CommitteeMemberResigned <! From
      k -> Invalid k

newtype CommitteeState era = CommitteeState
  { csCommitteeCreds :: Map (Credential 'ColdCommitteeRole) CommitteeAuthorization
  }
  deriving (Eq, Ord, Show, Generic, EncCBOR, NFData, Default, NoThunks)

instance ToJSON (CommitteeState era)

-- | Extract all unique hot credential authorizations for the current committee.  Note
-- that there is no unique mapping from Hot to Cold credential, therefore we produce a
-- Set, instead of a Map.
authorizedHotCommitteeCredentials :: CommitteeState era -> Set.Set (Credential 'HotCommitteeRole)
authorizedHotCommitteeCredentials CommitteeState {csCommitteeCreds} =
  let toHotCredSet acc = \case
        CommitteeHotCredential hotCred -> Set.insert hotCred acc
        CommitteeMemberResigned {} -> acc
   in F.foldl' toHotCredSet Set.empty csCommitteeCreds

instance Era era => DecShareCBOR (CommitteeState era) where
  type Share (CommitteeState era) = Interns (Credential 'HotCommitteeRole)
  getShare = internsFromSet . authorizedHotCommitteeCredentials
  decShareCBOR _ = CommitteeState <$> decCBOR

instance Era era => DecCBOR (CommitteeState era) where
  decCBOR = decNoShareCBOR

instance Era era => ToCBOR (CommitteeState era) where
  toCBOR = toEraCBOR @era

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
class
  ( EraAccounts era
  , ToJSON (CertState era)
  , EncCBOR (CertState era)
  , DecShareCBOR (CertState era)
  , Share (CertState era)
      ~ ( Interns (Credential 'Staking)
        , Interns (KeyHash 'StakePool)
        , Interns (Credential 'DRepRole)
        , Interns (Credential 'HotCommitteeRole)
        )
  , Default (CertState era)
  , NoThunks (CertState era)
  , NFData (CertState era)
  , Show (CertState era)
  , Eq (CertState era)
  ) =>
  EraCertState era
  where
  type CertState era = (r :: Type) | r -> era

  certDStateL :: Lens' (CertState era) (DState era)

  certPStateL :: Lens' (CertState era) (PState era)

  -- | Calculate total possible refunds in the system that are related to certificates
  --
  -- There is an invariant that the sum of all the fields should be the same as the
  -- utxosDeposited field of the UTxOState. Note that this does not depend upon the current
  -- values of the Key and Pool deposits of the PParams.
  obligationCertState :: CertState era -> Obligations

  -- | Compute the total deposits from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards the deposit pot (utxosDeposit field of
  -- the UTxOState) of the system
  certsTotalDepositsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin

  -- | Compute the total refunds from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards the total 'Obligations' of the system
  -- See `Obligations` and `obligationCertState` for more information.
  certsTotalRefundsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin

instance EncCBOR InstantaneousRewards where
  encCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> encCBOR irR <> encCBOR irT <> encCBOR dR <> encCBOR dT

instance DecShareCBOR InstantaneousRewards where
  type Share InstantaneousRewards = Interns (Credential 'Staking)
  decSharePlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- decSharePlusLensCBOR (toMemptyLens _1 id)
      irT <- decSharePlusLensCBOR (toMemptyLens _1 id)
      dR <- lift decCBOR
      dT <- lift decCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance Default InstantaneousRewards where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (Accounts era) => Default (DState era) where
  def =
    DState
      def
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState era) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

-- ==========================================================
-- Functions that handle Deposits

-- | One only pays a deposit on the initial pool registration. So return the
--   the Deposits unchanged if the keyhash already exists. There are legal
--   situations where a pool may be registered multiple times.
payPoolDeposit ::
  EraPParams era =>
  KeyHash 'StakePool ->
  PParams era ->
  PState era ->
  PState era
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    !deposit = pp ^. ppPoolDepositCompactL
    newpool
      | Map.notMember keyhash pool = Map.insert keyhash deposit pool
      | otherwise = pool

refundPoolDeposit :: KeyHash 'StakePool -> PState era -> (CompactForm Coin, PState era)
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

-- =======================================================
-- Lenses for CertState and its subsidiary types

-- ===================================
-- DState

dsGenDelegsL :: Lens' (DState era) GenDelegs
dsGenDelegsL = lens dsGenDelegs (\ds u -> ds {dsGenDelegs = u})

dsIRewardsL :: Lens' (DState era) InstantaneousRewards
dsIRewardsL = lens dsIRewards (\ds u -> ds {dsIRewards = u})

dsFutureGenDelegsL ::
  Lens' (DState era) (Map FutureGenDeleg GenDelegPair)
dsFutureGenDelegsL = lens dsFutureGenDelegs (\ds u -> ds {dsFutureGenDelegs = u})

-- ===================================
-- PState

psStakePoolStateL :: Lens' (PState era) (Map (KeyHash 'StakePool) StakePoolState)
psStakePoolStateL = lens psStakePoolState (\ds u -> ds {psStakePoolState = u})

psFutureStakePoolStateL :: Lens' (PState era) (Map (KeyHash 'StakePool) StakePoolState)
psFutureStakePoolStateL = lens psFutureStakePoolState (\ds u -> ds {psFutureStakePoolState = u})

psRetiringL :: Lens' (PState era) (Map (KeyHash 'StakePool) EpochNo)
psRetiringL = lens psRetiring (\ds u -> ds {psRetiring = u})

psDepositsL :: Lens' (PState era) (Map (KeyHash 'StakePool) Coin)
psDepositsL = psDepositsCompactL . lens (fmap fromCompact) (\_ -> fmap compactCoinOrError)

psDepositsCompactL :: Lens' (PState era) (Map (KeyHash 'StakePool) (CompactForm Coin))
psDepositsCompactL = lens psDeposits (\ds u -> ds {psDeposits = u})
