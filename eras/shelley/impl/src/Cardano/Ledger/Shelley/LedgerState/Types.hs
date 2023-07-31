{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Shelley.LedgerState.Types where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  EpochNo,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecShareCBOR (Share, decShareCBOR, decSharePlusCBOR),
  EncCBOR (encCBOR),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
  decShareLensCBOR,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeListLen,
  toPlainDecoder,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), Ptr (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary (
  SnapShots (..),
 )
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyPair, -- deprecated
  KeyRole (..),
 )
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.UTxO (UTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default, def)
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, _1, _2)
import NoThunks.Class (NoThunks (..))

-- ==================================

type KeyPairs c = [(KeyPair 'Payment c, KeyPair 'Staking c)]

{-# DEPRECATED KeyPairs "Use `Test.Cardano.Ledger.Core.KeyPair (KeyPairs)` instead" #-}

type RewardAccounts c =
  Map (Credential 'Staking c) Coin

data AccountState = AccountState
  { asTreasury :: !Coin
  , asReserves :: !Coin
  }
  deriving (Show, Eq, Generic)

instance EncCBOR AccountState where
  encCBOR (AccountState t r) =
    encodeListLen 2 <> encCBOR t <> encCBOR r

instance DecCBOR AccountState where
  decCBOR =
    decodeRecordNamed "AccountState" (const 2) $ AccountState <$> decCBOR <*> decCBOR

instance ToJSON AccountState where
  toJSON = object . toAccountStatePairs
  toEncoding = pairs . mconcat . toAccountStatePairs

toAccountStatePairs :: KeyValue a => AccountState -> [a]
toAccountStatePairs as@(AccountState _ _) =
  let AccountState {asTreasury, asReserves} = as
   in [ "treasury" .= asTreasury
      , "reserves" .= asReserves
      ]

instance NoThunks AccountState

instance NFData AccountState

data EpochState era = EpochState
  { esAccountState :: !AccountState
  , esLState :: !(LedgerState era)
  , esSnapshots :: !(SnapShots (EraCrypto era))
  , esNonMyopic :: !(NonMyopic (EraCrypto era))
  -- ^ This field, esNonMyopic, does not appear in the formal spec
  -- and is not a part of the protocol. It is only used for providing
  -- data to the stake pool ranking calculation @getNonMyopicMemberRewards@.
  -- See https://github.com/input-output-hk/cardano-ledger/releases/latest/download/pool-ranking.pdf
  }
  deriving (Generic)

esAccountStateL :: Lens' (EpochState era) AccountState
esAccountStateL = lens esAccountState (\x y -> x {esAccountState = y})

esSnapshotsL :: Lens' (EpochState era) (SnapShots (EraCrypto era))
esSnapshotsL = lens esSnapshots (\x y -> x {esSnapshots = y})

esLStateL :: Lens' (EpochState era) (LedgerState era)
esLStateL = lens esLState (\x y -> x {esLState = y})

esNonMyopicL :: Lens' (EpochState era) (NonMyopic (EraCrypto era))
esNonMyopicL = lens esNonMyopic (\x y -> x {esNonMyopic = y})

curPParamsEpochStateL :: EraGovernance era => Lens' (EpochState era) (PParams era)
curPParamsEpochStateL = esLStateL . lsUTxOStateL . utxosGovernanceL . curPParamsGovStateL

prevPParamsEpochStateL :: EraGovernance era => Lens' (EpochState era) (PParams era)
prevPParamsEpochStateL = esLStateL . lsUTxOStateL . utxosGovernanceL . prevPParamsGovStateL

deriving stock instance
  ( EraTxOut era
  , Show (GovernanceState era)
  ) =>
  Show (EpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovernanceState era)
  ) =>
  Eq (EpochState era)

instance
  ( EraTxOut era
  , NoThunks (GovernanceState era)
  ) =>
  NoThunks (EpochState era)

instance
  ( EraTxOut era
  , NFData (GovernanceState era)
  ) =>
  NFData (EpochState era)

instance
  ( EraTxOut era
  , EncCBOR (GovernanceState era)
  ) =>
  EncCBOR (EpochState era)
  where
  encCBOR EpochState {esAccountState, esLState, esSnapshots, esNonMyopic} =
    encode $
      Rec EpochState
        !> To esAccountState
        !> To esSnapshots
        !> To esLState -- We get better sharing when encoding ledger state before snaphots
        !> To esNonMyopic

instance
  ( EraTxOut era
  , EraGovernance era
  ) =>
  DecCBOR (EpochState era)
  where
  decCBOR =
    decodeRecordNamed "EpochState" (const 4) $
      flip evalStateT mempty $ do
        esAccountState <- lift decCBOR
        esSnapshots <- decSharePlusCBOR
        esLState <- decSharePlusCBOR
        esNonMyopic <- decShareLensCBOR _2
        pure EpochState {esAccountState, esSnapshots, esLState, esNonMyopic}

instance (EraTxOut era, EraGovernance era) => ToCBOR (EpochState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGovernance era) => FromCBOR (EpochState era) where
  fromCBOR = fromEraCBOR @era

instance (EraTxOut era, EraGovernance era) => ToJSON (EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( EraTxOut era
  , EraGovernance era
  , KeyValue a
  ) =>
  EpochState era ->
  [a]
toEpochStatePairs es@(EpochState _ _ _ _) =
  let EpochState {..} = es
   in [ "esAccountState" .= esAccountState
      , "esSnapshots" .= esSnapshots
      , "esLState" .= esLState
      , "esNonMyopic" .= esNonMyopic
      ]

-- =============================

-- | Incremental Stake, Stake along with possible missed coins from danging Ptrs.
--   Transactions can use Ptrs to refer to a stake credential in a TxOut. The Ptr
--   does not have to point to anything until the epoch boundary, when we compute
--   rewards and aggregate staking information for ranking. This is unusual but legal.
--   In a non incremental system, we use whatever 'legal' Ptrs exist at the epoch
--   boundary. Here we are computing things incrementally, so we need to remember Ptrs
--   that might point to something by the time the epoch boundary is reached. When
--   the epoch boundary is reached we 'resolve' these pointers, to see if any have
--   become non-dangling since the time they were first used in the incremental computation.
data IncrementalStake c = IStake
  { credMap :: !(Map (Credential 'Staking c) Coin)
  , ptrMap :: !(Map Ptr Coin)
  }
  deriving (Generic, Show, Eq, Ord, NoThunks, NFData)

instance Crypto c => EncCBOR (IncrementalStake c) where
  encCBOR (IStake st dangle) =
    encodeListLen 2 <> encCBOR st <> encCBOR dangle

instance Crypto c => DecShareCBOR (IncrementalStake c) where
  type Share (IncrementalStake c) = Interns (Credential 'Staking c)
  decShareCBOR credInterns =
    decodeRecordNamed "Stake" (const 2) $ do
      stake <- decShareCBOR (credInterns, mempty)
      IStake stake <$> decCBOR

instance Semigroup (IncrementalStake c) where
  (IStake a b) <> (IStake c d) = IStake (Map.unionWith (<>) a c) (Map.unionWith (<>) b d)

instance Monoid (IncrementalStake c) where
  mempty = IStake Map.empty Map.empty

instance Data.Group.Group (IncrementalStake c) where
  invert (IStake m1 m2) = IStake (Map.map invert m1) (Map.map invert m2)

instance Default (IncrementalStake c) where
  def = IStake Map.empty Map.empty

instance Crypto c => ToJSON (IncrementalStake c) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs ::
  (KeyValue a, Crypto crypto) => IncrementalStake crypto -> [a]
toIncrementalStakePairs iStake@(IStake _ _) =
  let IStake {..} = iStake -- guard against addition or removal of fields
   in [ "credentials" .= credMap
      , "pointers" .= ptrMap
      ]

-- =============================

-- | There is a serious invariant that we must maintain in the UTxOState.
--   Given (UTxOState utxo _ _ _ istake) it must be the case that
--   istake == (updateStakeDistribution (UTxO Map.empty) (UTxO Map.empty) utxo)
--   Of course computing the RHS of the above equality can be very expensive, so we only
--   use this route in the testing function smartUTxO. But we are very careful, wherever
--   we update the UTxO, we carefully make INCREMENTAL changes to istake to maintain
--   this invariant. This happens in the UTxO rule.
data UTxOState era = UTxOState
  { utxosUtxo :: !(UTxO era)
  , utxosDeposited :: !Coin
  , utxosFees :: !Coin
  , utxosGovernance :: !(GovernanceState era)
  , utxosStakeDistr :: !(IncrementalStake (EraCrypto era))
  }
  deriving (Generic)

utxosUtxoL :: Lens' (UTxOState era) (UTxO era)
utxosUtxoL = lens utxosUtxo (\x y -> x {utxosUtxo = y})

utxosDepositedL :: Lens' (UTxOState era) Coin
utxosDepositedL = lens utxosDeposited (\x y -> x {utxosDeposited = y})

utxosFeesL :: Lens' (UTxOState era) Coin
utxosFeesL = lens utxosFees (\x y -> x {utxosFees = y})

utxosGovernanceL :: Lens' (UTxOState era) (GovernanceState era)
utxosGovernanceL = lens utxosGovernance (\x y -> x {utxosGovernance = y})

utxosStakeDistrL :: Lens' (UTxOState era) (IncrementalStake (EraCrypto era))
utxosStakeDistrL = lens utxosStakeDistr (\x y -> x {utxosStakeDistr = y})

instance
  ( EraTxOut era
  , NFData (GovernanceState era)
  ) =>
  NFData (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Show (GovernanceState era)
  ) =>
  Show (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovernanceState era)
  ) =>
  Eq (UTxOState era)

instance
  ( NoThunks (UTxO era)
  , NoThunks (Value era)
  , NoThunks (GovernanceState era)
  ) =>
  NoThunks (UTxOState era)

instance
  ( EraTxOut era
  , EncCBOR (GovernanceState era)
  ) =>
  EncCBOR (UTxOState era)
  where
  encCBOR (UTxOState ut dp fs us sd) =
    encodeListLen 5 <> encCBOR ut <> encCBOR dp <> encCBOR fs <> encCBOR us <> encCBOR sd

instance
  ( EraTxOut era
  , EraGovernance era
  ) =>
  DecShareCBOR (UTxOState era)
  where
  type
    Share (UTxOState era) =
      Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credInterns =
    decodeRecordNamed "UTxOState" (const 5) $ do
      utxosUtxo <- decShareCBOR credInterns
      utxosDeposited <- decCBOR
      utxosFees <- decCBOR
      utxosGovernance <- decCBOR
      utxosStakeDistr <- decShareCBOR credInterns
      pure UTxOState {..}

instance (EraTxOut era, EraGovernance era) => ToCBOR (UTxOState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGovernance era) => FromCBOR (UTxOState era) where
  fromCBOR = toPlainDecoder (eraProtVerLow @era) decNoShareCBOR

instance (EraTxOut era, EraGovernance era) => ToJSON (UTxOState era) where
  toJSON = object . toUTxOStatePairs
  toEncoding = pairs . mconcat . toUTxOStatePairs

toUTxOStatePairs ::
  (EraTxOut era, EraGovernance era, KeyValue a) => UTxOState era -> [a]
toUTxOStatePairs utxoState@(UTxOState _ _ _ _ _) =
  let UTxOState {..} = utxoState
   in [ "utxo" .= utxosUtxo
      , "deposited" .= utxosDeposited
      , "fees" .= utxosFees
      , "ppups" .= utxosGovernance
      , "stake" .= utxosStakeDistr
      ]

-- | New Epoch state and environment
data NewEpochState era = NewEpochState
  { nesEL :: !EpochNo
  -- ^ Last epoch
  , nesBprev :: !(BlocksMade (EraCrypto era))
  -- ^ Blocks made before current epoch
  , nesBcur :: !(BlocksMade (EraCrypto era))
  -- ^ Blocks made in current epoch
  , nesEs :: !(EpochState era)
  -- ^ Epoch state before current
  , nesRu :: !(StrictMaybe (PulsingRewUpdate (EraCrypto era)))
  -- ^ Possible reward update
  , nesPd :: !(PoolDistr (EraCrypto era))
  -- ^ Stake distribution within the stake pool
  , stashedAVVMAddresses :: !(StashedAVVMAddresses era)
  -- ^ AVVM addresses to be removed at the end of the Shelley era. Note that
  -- the existence of this field is a hack, related to the transition of UTxO
  -- to disk. We remove AVVM addresses from the UTxO on the Shelley/Allegra
  -- boundary. However, by this point the UTxO will be moved to disk, and
  -- hence doing a scan of the UTxO for AVVM addresses will be expensive. Our
  -- solution to this is to do a scan of the UTxO on the Byron/Shelley
  -- boundary (since Byron UTxO are still on disk), stash the results here,
  -- and then remove them at the Shelley/Allegra boundary.
  --
  -- This is very much an awkward implementation hack, and hence we hide it
  -- from as many places as possible.
  }
  deriving (Generic)

nesEpochStateL :: Lens' (NewEpochState era) (EpochState era)
nesEpochStateL = lens nesEs $ \x y -> x {nesEs = y}

type family StashedAVVMAddresses era where
  StashedAVVMAddresses (ShelleyEra c) = UTxO (ShelleyEra c)
  StashedAVVMAddresses _ = ()

deriving stock instance
  ( EraTxOut era
  , Show (StashedAVVMAddresses era)
  , Show (GovernanceState era)
  ) =>
  Show (NewEpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (StashedAVVMAddresses era)
  , Eq (GovernanceState era)
  ) =>
  Eq (NewEpochState era)

instance
  ( EraTxOut era
  , NFData (StashedAVVMAddresses era)
  , NFData (GovernanceState era)
  ) =>
  NFData (NewEpochState era)

instance
  ( EraTxOut era
  , EncCBOR (StashedAVVMAddresses era)
  , EncCBOR (GovernanceState era)
  ) =>
  EncCBOR (NewEpochState era)
  where
  encCBOR (NewEpochState e bp bc es ru pd av) =
    encodeListLen 7
      <> encCBOR e
      <> encCBOR bp
      <> encCBOR bc
      <> encCBOR es
      <> encCBOR ru
      <> encCBOR pd
      <> encCBOR av

instance
  ( EraTxOut era
  , EraGovernance era
  , DecCBOR (StashedAVVMAddresses era)
  ) =>
  DecCBOR (NewEpochState era)
  where
  decCBOR = do
    decode $
      RecD NewEpochState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance
  (EraTxOut era, EraGovernance era, EncCBOR (StashedAVVMAddresses era)) =>
  ToCBOR (NewEpochState era)
  where
  toCBOR = toEraCBOR @era

instance
  (EraTxOut era, EraGovernance era, DecCBOR (StashedAVVMAddresses era)) =>
  FromCBOR (NewEpochState era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( Era era
  , NoThunks (BlocksMade (EraCrypto era))
  , NoThunks (EpochState era)
  , NoThunks (PulsingRewUpdate (EraCrypto era))
  , NoThunks (StashedAVVMAddresses era)
  ) =>
  NoThunks (NewEpochState era)

-- | The state associated with a 'Ledger'.
data LedgerState era = LedgerState
  { lsUTxOState :: !(UTxOState era)
  -- ^ The current unspent transaction outputs.
  , lsCertState :: !(CertState era)
  }
  deriving (Generic)

lsUTxOStateL :: Lens' (LedgerState era) (UTxOState era)
lsUTxOStateL = lens lsUTxOState (\x y -> x {lsUTxOState = y})

lsCertStateL :: Lens' (LedgerState era) (CertState era)
lsCertStateL = lens lsCertState (\x y -> x {lsCertState = y})

deriving stock instance
  ( EraTxOut era
  , Show (GovernanceState era)
  ) =>
  Show (LedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovernanceState era)
  ) =>
  Eq (LedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovernanceState era)
  ) =>
  NoThunks (LedgerState era)

instance
  ( EraTxOut era
  , NFData (GovernanceState era)
  ) =>
  NFData (LedgerState era)

instance
  ( EraTxOut era
  , EncCBOR (GovernanceState era)
  ) =>
  EncCBOR (LedgerState era)
  where
  encCBOR LedgerState {lsUTxOState, lsCertState} =
    encodeListLen 2
      <> encCBOR lsCertState -- encode delegation state first to improve sharing
      <> encCBOR lsUTxOState

instance
  ( EraTxOut era
  , EraGovernance era
  ) =>
  DecShareCBOR (LedgerState era)
  where
  type
    Share (LedgerState era) =
      (Interns (Credential 'Staking (EraCrypto era)), Interns (KeyHash 'StakePool (EraCrypto era)))
  decSharePlusCBOR =
    decodeRecordNamedT "LedgerState" (const 2) $ do
      lsCertState <- decSharePlusCBOR
      lsUTxOState <- decShareLensCBOR _1
      pure LedgerState {lsUTxOState, lsCertState}

instance (EraTxOut era, EraGovernance era) => ToCBOR (LedgerState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGovernance era) => FromCBOR (LedgerState era) where
  fromCBOR = toPlainDecoder (eraProtVerLow @era) decNoShareCBOR

instance (EraTxOut era, EraGovernance era) => ToJSON (LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  (EraTxOut era, EraGovernance era, KeyValue a) => LedgerState era -> [a]
toLedgerStatePairs ls@(LedgerState _ _) =
  let LedgerState {..} = ls
   in [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsCertState
      ]

-- ====================================================

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

instance EraGovernance era => Default (UTxOState era) where
  def = UTxOState mempty mempty mempty def mempty

instance
  Default (LedgerState era) =>
  Default (EpochState era)
  where
  def = EpochState def def def def

instance Default (UTxOState era) => Default (LedgerState era) where
  def = LedgerState def def

instance Default AccountState where
  def = AccountState (Coin 0) (Coin 0)

-- =============================================================
-- TreeDiff ToExpr instances

instance ToExpr AccountState

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovernanceState era)
  ) =>
  ToExpr (NewEpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (GovernanceState era)
  ) =>
  ToExpr (EpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovernanceState era)
  ) =>
  ToExpr (LedgerState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovernanceState era)
  ) =>
  ToExpr (UTxOState era)

instance ToExpr (IncrementalStake c)
