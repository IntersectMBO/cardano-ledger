{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
  DecCBOR (decCBOR),
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
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Credential (Credential (..))
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
import Data.Map.Strict (Map)
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
  { esChainAccountState :: !ChainAccountState
  , esLState :: !(LedgerState era)
  , esSnapshots :: !SnapShots
  , esNonMyopic :: !NonMyopic
  -- ^ This field, esNonMyopic, does not appear in the formal spec
  -- and is not a part of the protocol. It is only used for providing
  -- data to the stake pool ranking calculation @getNonMyopicMemberRewards@.
  -- See https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf
  }
  deriving (Generic)

--emptyEpochState :: EpochState era
--emptyEpochState = 
--  EpochState
--    emptyChainAccountState
--    emptyLedgerState
--    emptySnapShots
--    emptyNonMyopic

instance CanGetUTxO EpochState

instance CanSetUTxO EpochState where
  utxoL = lens esLState (\es ls -> es {esLState = ls}) . utxoL
  {-# INLINE utxoL #-}

instance CanGetInstantStake EpochState

instance CanSetInstantStake EpochState where
  instantStakeL = lens esLState (\es ls -> es {esLState = ls}) . instantStakeL
  {-# INLINE instantStakeL #-}

instance CanGetChainAccountState EpochState

instance CanSetChainAccountState EpochState where
  chainAccountStateL = lens esChainAccountState $ \es cas -> es {esChainAccountState = cas}
  {-# INLINE chainAccountStateL #-}

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (CertState era)
  , Show (InstantStake era)
  ) =>
  Show (EpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (EpochState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  , NoThunks (CertState era)
  , NoThunks (InstantStake era)
  ) =>
  NoThunks (EpochState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (CertState era)
  , NFData (InstantStake era)
  ) =>
  NFData (EpochState era)

instance
  ( EraTxOut era
  , EraStake era
  , EncCBOR (GovState era)
  , EncCBOR (CertState era)
  ) =>
  EncCBOR (EpochState era)
  where
  encCBOR EpochState {esChainAccountState, esLState, esSnapshots, esNonMyopic} =
    encode $
      Rec EpochState
        !> To esChainAccountState
        !> To esLState -- We get better sharing when encoding ledger state before snaphots
        !> To esSnapshots
        !> To esNonMyopic

instance
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraCertState era
  ) =>
  DecCBOR (EpochState era)
  where
  decCBOR =
    decodeRecordNamed "EpochState" (const 4) $
      flip evalStateT mempty $ do
        esChainAccountState <- lift decCBOR
        esLState <- decSharePlusCBOR
        esSnapshots <-
          decSharePlusLensCBOR $
            lens (\(cs, ks, _, _) -> (cs, ks)) (\(_, _, cd, ch) (cs, ks) -> (cs, ks, cd, ch))
        esNonMyopic <- decShareLensCBOR _2
        pure EpochState {esChainAccountState, esSnapshots, esLState, esNonMyopic}

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => ToCBOR (EpochState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => FromCBOR (EpochState era) where
  fromCBOR = fromEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => ToJSON (EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , KeyValue e a
  , EraCertState era
  ) =>
  EpochState era ->
  [a]
toEpochStatePairs es@(EpochState _ _ _ _) =
  let EpochState {..} = es
   in [ "esChainAccountState" .= esChainAccountState
      , "esSnapshots" .= esSnapshots
      , "esLState" .= esLState
      , "esNonMyopic" .= esNonMyopic
      ]

-- =============================

-- | There is a serious invariant that we must maintain in the UTxOState.
--   Given (UTxOState utxo _ _ _ istake) it must be the case that
--   Of course computing the RHS of the above equality can be very expensive, so we only
--   use this route in the testing function smartUTxO. But we are very careful, wherever
--   we update the UTxO, we carefully make INCREMENTAL changes to istake to maintain
--   this invariant. This happens in the UTxO rule.
data UTxOState era = UTxOState
  { utxosUtxo :: !(UTxO era)
  , utxosDeposited :: !Coin
  , utxosFees :: !Coin
  , utxosGovState :: !(GovState era)
  , utxosInstantStake :: !(InstantStake era)
  , utxosDonation :: !Coin
  }
  deriving (Generic)

instance CanGetUTxO UTxOState

instance CanSetUTxO UTxOState where
  utxoL = lens utxosUtxo $ \s u -> s {utxosUtxo = u}
  {-# INLINE utxoL #-}

instance CanGetInstantStake UTxOState

instance CanSetInstantStake UTxOState where
  instantStakeL = lens utxosInstantStake $ \s is -> s {utxosInstantStake = is}
  {-# INLINE instantStakeL #-}

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (InstantStake era)
  ) =>
  NFData (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (InstantStake era)
  ) =>
  Show (UTxOState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (InstantStake era)
  ) =>
  Eq (UTxOState era)

instance
  ( NoThunks (UTxO era)
  , NoThunks (GovState era)
  , NoThunks (InstantStake era)
  ) =>
  NoThunks (UTxOState era)

instance
  ( EraTxOut era
  , EraStake era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (UTxOState era)
  where
  encCBOR utxos@(UTxOState _ _ _ _ _ _) =
    let UTxOState {..} = utxos
     in encode $
          Rec UTxOState
            -- We need to define encoder with MemPack manually here instead of changing the `EncCBOR`
            -- instance for `UTxO` in order to not affect some of the ledger state queries.
            !> E (encodeMap encodeMemPack encodeMemPack . unUTxO) utxosUtxo
            !> To utxosDeposited
            !> To utxosFees
            !> To utxosGovState
            !> To utxosInstantStake
            !> To utxosDonation

instance (EraTxOut era, EraGov era, EraStake era) => DecShareCBOR (UTxOState era) where
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
      utxosInstantStake <- decShareCBOR cs
      utxosDonation <- decCBOR
      pure UTxOState {..}

instance (EraTxOut era, EraGov era, EraStake era) => ToCBOR (UTxOState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => FromCBOR (UTxOState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => ToJSON (UTxOState era) where
  toJSON = object . toUTxOStatePairs
  toEncoding = pairs . mconcat . toUTxOStatePairs

toUTxOStatePairs ::
  (EraTxOut era, EraGov era, EraStake era, KeyValue e a) => UTxOState era -> [a]
toUTxOStatePairs utxoState@(UTxOState _ _ _ _ _ _) =
  let UTxOState {..} = utxoState
   in [ "utxo" .= utxosUtxo
      , "deposited" .= utxosDeposited
      , "fees" .= utxosFees
      , "ppups" .= utxosGovState
      , "stake" .= utxosInstantStake
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
  utxoL = lens nesEs (\s es -> s {nesEs = es}) . utxoL
  {-# INLINE utxoL #-}

instance CanGetInstantStake NewEpochState

instance CanSetInstantStake NewEpochState where
  instantStakeL = lens nesEs (\s es -> s {nesEs = es}) . instantStakeL
  {-# INLINE instantStakeL #-}

instance CanGetChainAccountState NewEpochState

instance CanSetChainAccountState NewEpochState where
  chainAccountStateL = lens nesEs (\s es -> s {nesEs = es}) . chainAccountStateL
  {-# INLINE chainAccountStateL #-}

type family StashedAVVMAddresses era where
  StashedAVVMAddresses ShelleyEra = UTxO ShelleyEra
  StashedAVVMAddresses _ = ()

deriving stock instance
  ( EraTxOut era
  , Show (StashedAVVMAddresses era)
  , Show (GovState era)
  , Show (CertState era)
  , Show (InstantStake era)
  ) =>
  Show (NewEpochState era)

deriving stock instance
  ( EraTxOut era
  , Eq (StashedAVVMAddresses era)
  , Eq (GovState era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (NewEpochState era)

instance
  ( EraTxOut era
  , NFData (StashedAVVMAddresses era)
  , NFData (GovState era)
  , NFData (CertState era)
  , NFData (InstantStake era)
  ) =>
  NFData (NewEpochState era)

instance
  ( EraTxOut era
  , EraStake era
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
  , EraStake era
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
  (EraTxOut era, EraGov era, EraStake era, EraCertState era, EncCBOR (StashedAVVMAddresses era)) =>
  ToCBOR (NewEpochState era)
  where
  toCBOR = toEraCBOR @era

instance
  (EraTxOut era, EraGov era, EraStake era, EraCertState era, DecCBOR (StashedAVVMAddresses era)) =>
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
  utxoL = lens lsUTxOState (\s us -> s {lsUTxOState = us}) . utxoL
  {-# INLINE utxoL #-}

instance CanGetInstantStake LedgerState

instance CanSetInstantStake LedgerState where
  instantStakeL = lens lsUTxOState (\s us -> s {lsUTxOState = us}) . instantStakeL
  {-# INLINE instantStakeL #-}

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (CertState era)
  , Show (InstantStake era)
  ) =>
  Show (LedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (CertState era)
  , Eq (InstantStake era)
  ) =>
  Eq (LedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  , NoThunks (CertState era)
  , NoThunks (InstantStake era)
  ) =>
  NoThunks (LedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (CertState era)
  , NFData (InstantStake era)
  ) =>
  NFData (LedgerState era)

instance
  ( EraTxOut era
  , EraStake era
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
  , EraStake era
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

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => ToCBOR (LedgerState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => FromCBOR (LedgerState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era, EraStake era, EraCertState era) => ToJSON (LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  (EraTxOut era, EraGov era, KeyValue e a, EraStake era, EraCertState era) => LedgerState era -> [a]
toLedgerStatePairs ls@(LedgerState _ _) =
  let LedgerState {..} = ls
   in [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsCertState
      ]

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

esAccountStateL :: Lens' (EpochState era) ChainAccountState
esAccountStateL = lens esChainAccountState (\x y -> x {esChainAccountState = y})
{-# DEPRECATED esAccountStateL "In favor of `chainAccountStateL`" #-}

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
-- ChainAccountState

asTreasuryL :: Lens' ChainAccountState Coin
asTreasuryL = lens casTreasury (\ds u -> ds {casTreasury = u})
{-# DEPRECATED asTreasuryL "In favor of `casTreasuryL`" #-}

asReservesL :: Lens' ChainAccountState Coin
asReservesL = lens casReserves (\ds u -> ds {casReserves = u})
{-# DEPRECATED asReservesL "In favor of `casReservesL`" #-}

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

utxosDonationL :: Lens' (UTxOState era) Coin
utxosDonationL = lens utxosDonation (\x y -> x {utxosDonation = y})

-- ====================  Compound Lenses =======================

newEpochStateGovStateL :: Lens' (NewEpochState era) (GovState era)
newEpochStateGovStateL = nesEsL . epochStateGovStateL

epochStateGovStateL :: Lens' (EpochState era) (GovState era)
epochStateGovStateL = esLStateL . lsUTxOStateL . utxosGovStateL

epochStateDonationL :: Lens' (EpochState era) Coin
epochStateDonationL = esLStateL . lsUTxOStateL . utxosDonationL

epochStateTreasuryL :: Lens' (EpochState era) Coin
epochStateTreasuryL = treasuryL
{-# DEPRECATED epochStateTreasuryL "In favor of `treasuryL`" #-}

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
