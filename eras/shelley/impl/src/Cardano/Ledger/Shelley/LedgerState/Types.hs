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
{-# LANGUAGE RankNTypes #-}
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
  DecCBOR (decCBOR, dropCBOR),
  DecShareCBOR (Share, decShareCBOR, decSharePlusCBOR),
  EncCBOR (encCBOR),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
  decShareLensCBOR,
  decSharePlusLensCBOR,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeListLen,
  encodeMap,
  encodeMemPack,
  enforceDecoderVersion,
  ifDecoderVersionAtLeast,
  natVersion,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.CertState (
  CertState,
  DRepState,
  EraCertState (..),
  Obligations (..),
  dsUnifiedL,
  psStakePoolParamsL,
  sumObligation,
  vsDRepsL,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Credential (Credential (..), Ptr (..))
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PoolRank (NonMyopic (..))
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..))
import Cardano.Ledger.State
import Cardano.Ledger.UMap (UMap (..))
import Control.DeepSeq (NFData)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default, def)
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.VMap (VB, VMap, VP)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- ==================================

type RewardAccounts =
  Map (Credential 'Staking) Coin
{-# DEPRECATED RewardAccounts "In favor of `Map` (`Credential` `Staking`) `Coin`" #-}

data EpochState era = EpochState
  { esAccountState :: !AccountState
  , esLState :: !(LedgerState era)
  , esSnapshots :: !SnapShots
  , esNonMyopic :: !NonMyopic
  -- ^ This field, esNonMyopic, does not appear in the formal spec
  -- and is not a part of the protocol. It is only used for providing
  -- data to the stake pool ranking calculation @getNonMyopicMemberRewards@.
  -- See https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf
  }
  deriving (Generic)

instance CanGetUTxO EpochState
instance CanSetUTxO EpochState where
  utxoL = (lens esLState $ \s ls -> s {esLState = ls}) . utxoL
  {-# INLINE utxoL #-}

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (CertState era)
  ) =>
  Show (EpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (CertState era)
  ) =>
  Eq (EpochState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  , NoThunks (CertState era)
  ) =>
  NoThunks (EpochState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (CertState era)
  ) =>
  NFData (EpochState era)

instance
  ( EraTxOut era
  , EncCBOR (GovState era)
  , EncCBOR (CertState era)
  ) =>
  EncCBOR (EpochState era)
  where
  encCBOR EpochState {esAccountState, esLState, esSnapshots, esNonMyopic} =
    encode $
      Rec EpochState
        !> To esAccountState
        !> To esLState -- We get better sharing when encoding ledger state before snaphots
        !> To esSnapshots
        !> To esNonMyopic

instance
  ( EraTxOut era
  , EraGov era
  , EraCertState era
  ) =>
  DecCBOR (EpochState era)
  where
  decCBOR =
    decodeRecordNamed "EpochState" (const 4) $
      flip evalStateT mempty $ do
        esAccountState <- lift decCBOR
        esLState <- decSharePlusCBOR
        esSnapshots <-
          decSharePlusLensCBOR $
            lens (\(cs, ks, _, _) -> (cs, ks)) (\(_, _, cd, ch) (cs, ks) -> (cs, ks, cd, ch))
        esNonMyopic <- decShareLensCBOR _2
        pure EpochState {esAccountState, esSnapshots, esLState, esNonMyopic}

instance (EraTxOut era, EraGov era, EraCertState era) => ToCBOR (EpochState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraCertState era) => FromCBOR (EpochState era) where
  fromCBOR = fromEraCBOR @era

instance (EraTxOut era, EraGov era, EraCertState era) => ToJSON (EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( EraTxOut era
  , EraGov era
  , KeyValue e a
  , EraCertState era
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
data IncrementalStake = IStake
  { credMap :: !(Map (Credential 'Staking) (CompactForm Coin))
  , ptrMap :: !(Map Ptr (CompactForm Coin))
  }
  deriving (Generic, Show, Eq, Ord, NoThunks, NFData)

instance EncCBOR IncrementalStake where
  encCBOR (IStake st dangle) =
    encodeListLen 2 <> encCBOR st <> encCBOR dangle

instance DecShareCBOR IncrementalStake where
  type Share IncrementalStake = Interns (Credential 'Staking)
  decShareCBOR credInterns =
    decodeRecordNamed "Stake" (const 2) $ do
      stake <- decShareCBOR (credInterns, mempty)
      let dropPtrs =
            mempty
              <$ enforceDecoderVersion (natVersion @8) (dropCBOR (Proxy @(Map Ptr (CompactForm Coin))))
      ptrs <- ifDecoderVersionAtLeast (natVersion @9) dropPtrs decCBOR
      pure $ IStake stake ptrs

instance Semigroup IncrementalStake where
  (IStake a b) <> (IStake c d) = IStake (Map.unionWith (<>) a c) (Map.unionWith (<>) b d)

instance Monoid IncrementalStake where
  mempty = IStake Map.empty Map.empty

instance Data.Group.Group IncrementalStake where
  invert (IStake m1 m2) = IStake (Map.map invert m1) (Map.map invert m2)

instance Default IncrementalStake where
  def = IStake Map.empty Map.empty

instance ToJSON IncrementalStake where
  toJSON = object . toIncrementalStakePairs
  toEncoding = pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs :: KeyValue e a => IncrementalStake -> [a]
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
  , utxosGovState :: !(GovState era)
  , utxosStakeDistr :: !IncrementalStake
  , utxosDonation :: !Coin
  }
  deriving (Generic)

instance CanGetUTxO UTxOState
instance CanSetUTxO UTxOState where
  utxoL = lens utxosUtxo $ \s u -> s {utxosUtxo = u}
  {-# INLINE utxoL #-}

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (UTxOState era)

instance (NoThunks (UTxO era), NoThunks (GovState era)) => NoThunks (UTxOState era)

instance
  ( EraTxOut era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (UTxOState era)
  where
  encCBOR (UTxOState utxo dp fs us sd don) =
    encode $
      Rec UTxOState
        -- We need to define encoder with MemPack manually here instead of changing the `EncCBOR`
        -- instance for `UTxO` in order to not affect some of the ledger state queries.
        !> E (encodeMap encodeMemPack encodeMemPack . unUTxO) utxo
        !> To dp
        !> To fs
        !> To us
        !> To sd
        !> To don

instance (EraTxOut era, EraGov era) => DecShareCBOR (UTxOState era) where
  type
    Share (UTxOState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is@(cs, _, _, _) =
    decodeRecordNamed "UTxOState" (const 6) $ do
      utxosUtxo <- decShareCBOR cs
      utxosDeposited <- decCBOR
      utxosFees <- decCBOR
      utxosGovState <- decShareCBOR is
      utxosStakeDistr <- decShareCBOR cs
      utxosDonation <- decCBOR
      pure UTxOState {..}

instance (EraTxOut era, EraGov era) => ToCBOR (UTxOState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era) => FromCBOR (UTxOState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era) => ToJSON (UTxOState era) where
  toJSON = object . toUTxOStatePairs
  toEncoding = pairs . mconcat . toUTxOStatePairs

toUTxOStatePairs ::
  (EraTxOut era, EraGov era, KeyValue e a) => UTxOState era -> [a]
toUTxOStatePairs utxoState@(UTxOState _ _ _ _ _ _) =
  let UTxOState {..} = utxoState
   in [ "utxo" .= utxosUtxo
      , "deposited" .= utxosDeposited
      , "fees" .= utxosFees
      , "ppups" .= utxosGovState
      , "stake" .= utxosStakeDistr
      ]

-- | New Epoch state and environment
data NewEpochState era = NewEpochState
  { nesEL :: !EpochNo
  -- ^ Number of the epoch when this NewEpochState was modified last. With respect to
  -- block and transactions validation this will always be the current epoch
  -- number. However, when it comes to the TICK rule, it will be the epoch number of the
  -- previous epoch whenever we are crossing the epoch boundary.
  , nesBprev :: !BlocksMade
  -- ^ Blocks made before current epoch
  , nesBcur :: !BlocksMade
  -- ^ Blocks made in current epoch
  , nesEs :: !(EpochState era)
  -- ^ Epoch state
  , nesRu :: !(StrictMaybe PulsingRewUpdate)
  -- ^ Possible reward update
  , nesPd :: !PoolDistr
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

instance CanGetUTxO NewEpochState
instance CanSetUTxO NewEpochState where
  utxoL = (lens nesEs $ \s es -> s {nesEs = es}) . utxoL
  {-# INLINE utxoL #-}

type family StashedAVVMAddresses era where
  StashedAVVMAddresses ShelleyEra = UTxO ShelleyEra
  StashedAVVMAddresses _ = ()

deriving stock instance
  ( EraTxOut era
  , Show (StashedAVVMAddresses era)
  , Show (GovState era)
  , Show (CertState era)
  ) =>
  Show (NewEpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (StashedAVVMAddresses era)
  , Eq (GovState era)
  , Eq (CertState era)
  ) =>
  Eq (NewEpochState era)

instance
  ( EraTxOut era
  , NFData (StashedAVVMAddresses era)
  , NFData (GovState era)
  , NFData (CertState era)
  ) =>
  NFData (NewEpochState era)

instance
  ( EraTxOut era
  , EncCBOR (StashedAVVMAddresses era)
  , EncCBOR (GovState era)
  , EncCBOR (CertState era)
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
  , EraGov era
  , DecCBOR (StashedAVVMAddresses era)
  , EraCertState era
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
  (EraTxOut era, EraGov era, EncCBOR (StashedAVVMAddresses era), EraCertState era) =>
  ToCBOR (NewEpochState era)
  where
  toCBOR = toEraCBOR @era

instance
  (EraTxOut era, EraGov era, DecCBOR (StashedAVVMAddresses era), EraCertState era) =>
  FromCBOR (NewEpochState era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( Era era
  , NoThunks (EpochState era)
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

instance CanGetUTxO LedgerState
instance CanSetUTxO LedgerState where
  utxoL = (lens lsUTxOState $ \s us -> s {lsUTxOState = us}) . utxoL
  {-# INLINE utxoL #-}

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (CertState era)
  ) =>
  Show (LedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (CertState era)
  ) =>
  Eq (LedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  , NoThunks (CertState era)
  ) =>
  NoThunks (LedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (CertState era)
  ) =>
  NFData (LedgerState era)

instance
  ( EraTxOut era
  , EncCBOR (GovState era)
  , EncCBOR (CertState era)
  ) =>
  EncCBOR (LedgerState era)
  where
  encCBOR LedgerState {lsUTxOState, lsCertState} =
    encodeListLen 2
      <> encCBOR lsCertState -- encode delegation state first to improve sharing
      <> encCBOR lsUTxOState

instance
  ( EraTxOut era
  , EraGov era
  , EraCertState era
  ) =>
  DecShareCBOR (LedgerState era)
  where
  type
    Share (LedgerState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decSharePlusCBOR =
    decodeRecordNamedT "LedgerState" (const 2) $ do
      lsCertState <- decSharePlusCBOR
      lsUTxOState <- decSharePlusCBOR
      pure LedgerState {lsUTxOState, lsCertState}

instance (EraTxOut era, EraGov era, EraCertState era) => ToCBOR (LedgerState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraCertState era) => FromCBOR (LedgerState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era, EraCertState era) => ToJSON (LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  (EraTxOut era, EraGov era, KeyValue e a, EraCertState era) => LedgerState era -> [a]
toLedgerStatePairs ls@(LedgerState _ _) =
  let LedgerState {..} = ls
   in [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsCertState
      ]

-- ====================================================

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

instance EraGov era => Default (UTxOState era) where
  def = UTxOState mempty mempty mempty def mempty mempty

instance
  Default (LedgerState era) =>
  Default (EpochState era)
  where
  def = EpochState def def def def

instance (Default (UTxOState era), Default (CertState era)) => Default (LedgerState era) where
  def = LedgerState def def

-- =============================================================
-- Lenses for types found inside NewEpochState and its fields

-- ==========================================
-- NewEpochState

nesPdL :: Lens' (NewEpochState era) PoolDistr
nesPdL = lens nesPd (\ds u -> ds {nesPd = u})

{- Called nesEpochStateL elsewhere -}
nesEsL :: Lens' (NewEpochState era) (EpochState era)
nesEsL = lens nesEs (\ds u -> ds {nesEs = u})

unifiedL :: EraCertState era => Lens' (NewEpochState era) UMap
unifiedL = nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL

nesELL :: Lens' (NewEpochState era) EpochNo
nesELL = lens nesEL (\ds u -> ds {nesEL = u})

nesBprevL :: Lens' (NewEpochState era) (Map (KeyHash 'StakePool) Natural)
nesBprevL = lens (unBlocksMade . nesBprev) (\ds u -> ds {nesBprev = BlocksMade u})

nesBcurL :: Lens' (NewEpochState era) (Map (KeyHash 'StakePool) Natural)
nesBcurL = lens (unBlocksMade . nesBcur) (\ds u -> ds {nesBcur = BlocksMade u})

nesRuL :: Lens' (NewEpochState era) (StrictMaybe PulsingRewUpdate)
nesRuL = lens nesRu (\ds u -> ds {nesRu = u})

nesStashedAVVMAddressesL :: Lens' (NewEpochState era) (StashedAVVMAddresses era)
nesStashedAVVMAddressesL = lens stashedAVVMAddresses (\ds u -> ds {stashedAVVMAddresses = u})

-- For backward compatibility
nesEpochStateL :: Lens' (NewEpochState era) (EpochState era)
nesEpochStateL = lens nesEs $ \x y -> x {nesEs = y}

-- ===================================================
-- EpochState

esAccountStateL :: Lens' (EpochState era) AccountState
esAccountStateL = lens esAccountState (\x y -> x {esAccountState = y})

esSnapshotsL :: Lens' (EpochState era) SnapShots
esSnapshotsL = lens esSnapshots (\x y -> x {esSnapshots = y})

esLStateL :: Lens' (EpochState era) (LedgerState era)
esLStateL = lens esLState (\x y -> x {esLState = y})

esNonMyopicL :: Lens' (EpochState era) NonMyopic
esNonMyopicL = lens esNonMyopic (\x y -> x {esNonMyopic = y})

curPParamsEpochStateL :: EraGov era => Lens' (EpochState era) (PParams era)
curPParamsEpochStateL = epochStateGovStateL . curPParamsGovStateL

prevPParamsEpochStateL :: EraGov era => Lens' (EpochState era) (PParams era)
prevPParamsEpochStateL = epochStateGovStateL . prevPParamsGovStateL

futurePParamsEpochStateL :: EraGov era => Lens' (EpochState era) (FuturePParams era)
futurePParamsEpochStateL = epochStateGovStateL . futurePParamsGovStateL

-- ==========================================
-- AccountState

asTreasuryL :: Lens' AccountState Coin
asTreasuryL = lens asTreasury (\ds u -> ds {asTreasury = u})

asReservesL :: Lens' AccountState Coin
asReservesL = lens asReserves (\ds u -> ds {asReserves = u})

-- ====================================================
-- LedgerState

lsUTxOStateL :: Lens' (LedgerState era) (UTxOState era)
lsUTxOStateL = lens lsUTxOState (\x y -> x {lsUTxOState = y})

lsCertStateL :: Lens' (LedgerState era) (CertState era)
lsCertStateL = lens lsCertState (\x y -> x {lsCertState = y})

-- ================ UTxOState ===========================

utxosUtxoL :: Lens' (UTxOState era) (UTxO era)
utxosUtxoL = lens utxosUtxo (\x y -> x {utxosUtxo = y})
{-# DEPRECATED utxosUtxoL "In favor of `utxoL`" #-}

utxosDepositedL :: Lens' (UTxOState era) Coin
utxosDepositedL = lens utxosDeposited (\x y -> x {utxosDeposited = y})

utxosFeesL :: Lens' (UTxOState era) Coin
utxosFeesL = lens utxosFees (\x y -> x {utxosFees = y})

utxosGovStateL :: Lens' (UTxOState era) (GovState era)
utxosGovStateL = lens utxosGovState (\x y -> x {utxosGovState = y})

utxosStakeDistrL :: Lens' (UTxOState era) IncrementalStake
utxosStakeDistrL = lens utxosStakeDistr (\x y -> x {utxosStakeDistr = y})

utxosDonationL :: Lens' (UTxOState era) Coin
utxosDonationL = lens utxosDonation (\x y -> x {utxosDonation = y})

-- ================ IncremetalStake ===========================

credMapL :: Lens' IncrementalStake (Map (Credential 'Staking) (CompactForm Coin))
credMapL = lens credMap (\x y -> x {credMap = y})

ptrMapL :: Lens' IncrementalStake (Map Ptr (CompactForm Coin))
ptrMapL = lens ptrMap (\x y -> x {ptrMap = y})

-- ====================  Compound Lenses =======================

newEpochStateGovStateL :: Lens' (NewEpochState era) (GovState era)
newEpochStateGovStateL = nesEsL . epochStateGovStateL

epochStateGovStateL :: Lens' (EpochState era) (GovState era)
epochStateGovStateL = esLStateL . lsUTxOStateL . utxosGovStateL

epochStateDonationL :: Lens' (EpochState era) Coin
epochStateDonationL = esLStateL . lsUTxOStateL . utxosDonationL

epochStateTreasuryL :: Lens' (EpochState era) Coin
epochStateTreasuryL = esAccountStateL . asTreasuryL

epochStateIncrStakeDistrL ::
  Lens' (EpochState era) (Map (Credential 'Staking) (CompactForm Coin))
epochStateIncrStakeDistrL = esLStateL . lsUTxOStateL . utxosStakeDistrL . credMapL

epochStateRegDrepL ::
  EraCertState era => Lens' (EpochState era) (Map (Credential 'DRepRole) DRepState)
epochStateRegDrepL = esLStateL . lsCertStateL . certVStateL . vsDRepsL

epochStatePoolParamsL ::
  EraCertState era => Lens' (EpochState era) (Map (KeyHash 'StakePool) PoolParams)
epochStatePoolParamsL = esLStateL . lsCertStateL . certPStateL . psStakePoolParamsL

epochStateUMapL :: EraCertState era => Lens' (EpochState era) UMap
epochStateUMapL = esLStateL . lsCertStateL . certDStateL . dsUnifiedL

epochStateStakeDistrL ::
  Lens' (EpochState era) (VMap VB VP (Credential 'Staking) (CompactForm Coin))
epochStateStakeDistrL = esSnapshotsL . ssStakeMarkL . ssStakeDistrL

potEqualsObligation ::
  (EraGov era, EraCertState era) =>
  CertState era ->
  UTxOState era ->
  Bool
potEqualsObligation certState utxoSt = obligations == pot
  where
    obligations = totalObligation certState (utxoSt ^. utxosGovStateL)
    pot = utxoSt ^. utxosDepositedL

allObligations :: (EraGov era, EraCertState era) => CertState era -> GovState era -> Obligations
allObligations certState govState =
  obligationCertState certState <> obligationGovState govState

totalObligation :: (EraGov era, EraCertState era) => CertState era -> GovState era -> Coin
totalObligation certState govState = sumObligation (allObligations certState govState)
