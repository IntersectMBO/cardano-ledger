{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
  Obligations (..),
  sumObligation,
  unDelegReDelegStakePool,
  -- Lenses
  iRReservesL,
  dsIRewardsL,
  dsGenDelegsL,
  iRTreasuryL,
  iRDeltaReservesL,
  iRDeltaTreasuryL,
  dsFutureGenDelegsL,
  psStakePoolsL,
  psFutureStakePoolsL,
  psRetiringL,
  psVRFKeyHashesL,
) where

import Cardano.Ledger.BaseTypes (
  Anchor (..),
  AnchorData,
  KeyValuePairs (..),
  NonZero,
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
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (..), fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.State.Account
import Cardano.Ledger.State.StakePool (StakePoolState (..), spsDelegatorsL)
import Control.DeepSeq (NFData (..))
import Control.Monad.Trans
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Default (Default (def))
import qualified Data.Foldable as F
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

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
  encCBOR dState@(DState _ _ _ _) =
    let DState {..} = dState
     in encodeListLen 4
          <> encCBOR dsAccounts
          <> encCBOR dsFutureGenDelegs
          <> encCBOR dsGenDelegs
          <> encCBOR dsIRewards

instance EraAccounts era => DecShareCBOR (DState era) where
  type
    Share (DState era) =
      (Interns (Credential 'Staking), Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decSharePlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      dsAccounts <- decSharePlusCBOR
      dsFutureGenDelegs <- lift decCBOR
      dsGenDelegs <- lift decCBOR
      dsIRewards <- decSharePlusLensCBOR _1
      pure DState {..}

instance ToJSON (Accounts era) => ToKeyValuePairs (DState era) where
  toKeyValuePairs dState@(DState _ _ _ _) =
    let DState {..} = dState
     in [ "accounts" .= dsAccounts
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
  { psVRFKeyHashes :: !(Map (VRFVerKeyHash 'StakePoolVRF) (NonZero Word64))
  -- ^ VRF key hashes that have been registered via PoolParams
  , psStakePools :: !(Map (KeyHash 'StakePool) StakePoolState)
  -- ^ The state of current stake pools.
  , psFutureStakePools :: !(Map (KeyHash 'StakePool) StakePoolState)
  -- ^ The state of future stake pools.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs (PState era)

instance NoThunks (PState era)

instance NFData (PState era)

instance Era era => EncCBOR (PState era) where
  encCBOR (PState a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance DecShareCBOR (PState era) where
  type Share (PState era) = (Interns (VRFVerKeyHash 'StakePoolVRF), Interns (KeyHash 'StakePool))

  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psVRFKeyHashes <- decSharePlusLensCBOR (toMemptyLens _1 _1)
    psStakePools <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    psFutureStakePools <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    psRetiring <- decSharePlusLensCBOR (toMemptyLens _1 _2)
    pure PState {psVRFKeyHashes, psStakePools, psFutureStakePools, psRetiring}

instance (Era era
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , DecShareCBOR (PState era)
#endif
  ) => DecCBOR (PState era) where
  decCBOR = decNoShareCBOR

instance ToKeyValuePairs (PState era) where
  toKeyValuePairs PState {..} =
    [ "vrfKeyHashes" .= psVRFKeyHashes
    , "stakePools" .= psStakePools
    , "futureStakePools" .= psFutureStakePools
    , "retiring" .= psRetiring
    ]

-- | Reverses stake pool delegation.
-- To be called when a stake credential is unregistered or its delegation target changes.
-- If the new delegation matches the previous one, this is a noop.
unDelegReDelegStakePool ::
  EraAccounts era =>
  Credential 'Staking ->
  -- | Account that is losing its current delegation and/or acquiring a new one
  AccountState era ->
  -- | Optional new delegation target. Use 'Nothing' when the stake credential unregisters.
  Maybe (KeyHash 'StakePool) ->
  PState era ->
  PState era
unDelegReDelegStakePool stakeCred accountState mNewStakePool =
  fromMaybe (psStakePoolsL %~ addNewDelegation) $ do
    curStakePool <- accountState ^. stakePoolDelegationAccountStateL
    pure $
      -- no need to update the set of delegations if the delegation is unchanged
      if Just curStakePool == mNewStakePool
        then id
        else
          psStakePoolsL %~ addNewDelegation . Map.adjust (spsDelegatorsL %~ Set.delete stakeCred) curStakePool
  where
    addNewDelegation = maybe id (Map.adjust (spsDelegatorsL %~ Set.insert stakeCred)) mNewStakePool

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
  certsTotalDepositsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody t era -> Coin

  -- | Compute the total refunds from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards the total 'Obligations' of the system
  -- See `Obligations` and `obligationCertState` for more information.
  certsTotalRefundsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody t era -> Coin

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
  def = DState def Map.empty (GenDelegs Map.empty) def

instance Default (PState era) where
  def = PState Map.empty Map.empty Map.empty Map.empty

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

dsGenDelegsL :: Lens' (DState era) GenDelegs
dsGenDelegsL = lens dsGenDelegs (\ds u -> ds {dsGenDelegs = u})

dsIRewardsL :: Lens' (DState era) InstantaneousRewards
dsIRewardsL = lens dsIRewards (\ds u -> ds {dsIRewards = u})

iRReservesL :: Lens' InstantaneousRewards (Map (Credential 'Staking) Coin)
iRReservesL = lens iRReserves (\ir m -> ir {iRReserves = m})

iRTreasuryL :: Lens' InstantaneousRewards (Map (Credential 'Staking) Coin)
iRTreasuryL = lens iRTreasury (\ir m -> ir {iRTreasury = m})

iRDeltaReservesL :: Lens' InstantaneousRewards DeltaCoin
iRDeltaReservesL = lens deltaReserves (\ir d -> ir {deltaReserves = d})

iRDeltaTreasuryL :: Lens' InstantaneousRewards DeltaCoin
iRDeltaTreasuryL = lens deltaTreasury (\ir d -> ir {deltaTreasury = d})

dsFutureGenDelegsL ::
  Lens' (DState era) (Map FutureGenDeleg GenDelegPair)
dsFutureGenDelegsL = lens dsFutureGenDelegs (\ds u -> ds {dsFutureGenDelegs = u})

psStakePoolsL :: Lens' (PState era) (Map (KeyHash 'StakePool) StakePoolState)
psStakePoolsL = lens psStakePools (\ps u -> ps {psStakePools = u})

psFutureStakePoolsL :: Lens' (PState era) (Map (KeyHash 'StakePool) StakePoolState)
psFutureStakePoolsL = lens psFutureStakePools (\ps u -> ps {psFutureStakePools = u})

psRetiringL :: Lens' (PState era) (Map (KeyHash 'StakePool) EpochNo)
psRetiringL = lens psRetiring (\ps u -> ps {psRetiring = u})

psVRFKeyHashesL :: Lens' (PState era) (Map (VRFVerKeyHash 'StakePoolVRF) (NonZero Word64))
psVRFKeyHashesL = lens psVRFKeyHashes (\ps u -> ps {psVRFKeyHashes = u})
