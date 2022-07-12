{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    DState (..),
    PState (..),
    InstantaneousRewards (..),
    FutureGenDeleg (..),
    rewards,
    delegations,
    ptrsMap,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
  )
import Cardano.Ledger.Credential (Credential (..), Ptr)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
  )
import Cardano.Ledger.Serialization (decodeRecordNamedT, mapToCBOR)
import Cardano.Ledger.Shelley.TxBody
  ( PoolParams (..),
  )
import Cardano.Ledger.Slot
  ( EpochNo (..),
    SlotNo (..),
  )
import Cardano.Ledger.UnifiedMap (UMap (UnifiedMap), UnifiedMap, View (Delegations, Rewards), ViewMap)
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Coders
  ( decodeRecordNamed,
  )
import Data.Default.Class (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sharing
import qualified Data.UMap as UM
import GHC.Generics (Generic)
import Lens.Micro (_1, _2)
import NoThunks.Class (NoThunks (..))

data FutureGenDeleg crypto = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo,
    fGenDelegGenKeyHash :: !(KeyHash 'Genesis crypto)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureGenDeleg crypto)

instance NFData (FutureGenDeleg crypto)

instance CC.Crypto crypto => ToCBOR (FutureGenDeleg crypto) where
  toCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance CC.Crypto crypto => FromCBOR (FutureGenDeleg crypto) where
  fromCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> fromCBOR <*> fromCBOR

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards crypto = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking crypto) Coin),
    iRTreasury :: !(Map (Credential 'Staking crypto) Coin),
    deltaReserves :: !DeltaCoin,
    deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards crypto)

instance NFData (InstantaneousRewards crypto)

-- | State of staking pool delegations and rewards
data DState crypto = DState
  { -- | Unified Reward Maps
    _unified :: !(UnifiedMap crypto),
    -- | Future genesis key delegations
    _fGenDelegs :: !(Map (FutureGenDeleg crypto) (GenDelegPair crypto)),
    -- | Genesis key delegations
    _genDelegs :: !(GenDelegs crypto),
    -- | Instantaneous Rewards
    _irwd :: !(InstantaneousRewards crypto)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards crypto) => NoThunks (DState crypto)

instance NFData (InstantaneousRewards crypto) => NFData (DState crypto)

instance (CC.Crypto crypto, ToCBOR (InstantaneousRewards crypto)) => ToCBOR (DState crypto) where
  toCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> toCBOR unified
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir

instance (CC.Crypto crypto, FromSharedCBOR (InstantaneousRewards crypto)) => FromSharedCBOR (DState crypto) where
  type
    Share (DState crypto) =
      (Interns (Credential 'Staking crypto), Interns (KeyHash 'StakePool crypto))
  fromSharedPlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- fromSharedPlusCBOR
      fgs <- lift fromCBOR
      gs <- lift fromCBOR
      ir <- fromSharedPlusLensCBOR _1
      pure $ DState unified fgs gs ir

-- | Current state of staking pools and their certificate counters.
data PState crypto = PState
  { -- | The pool parameters.
    _pParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | The future pool parameters.
    _fPParams :: !(Map (KeyHash 'StakePool crypto) (PoolParams crypto)),
    -- | A map of retiring stake pools to the epoch when they retire.
    _retiring :: !(Map (KeyHash 'StakePool crypto) EpochNo)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState crypto)

instance NFData (PState crypto)

instance CC.Crypto crypto => ToCBOR (PState crypto) where
  toCBOR (PState a b c) =
    encodeListLen 3 <> toCBOR a <> toCBOR b <> toCBOR c

instance CC.Crypto crypto => FromSharedCBOR (PState crypto) where
  type
    Share (PState crypto) =
      Interns (KeyHash 'StakePool crypto)
  fromSharedPlusCBOR = decodeRecordNamedT "PState" (const 3) $ do
    _pParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    _fPParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    _retiring <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    pure PState {_pParams, _fPParams, _retiring}

instance (CC.Crypto crypto, FromSharedCBOR (PState crypto)) => FromCBOR (PState crypto) where
  fromCBOR = fromNotSharedCBOR

-- | The state associated with the current stake delegation.
data DPState crypto = DPState
  { dpsDState :: !(DState crypto),
    dpsPState :: !(PState crypto)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards crypto) => NoThunks (DPState crypto)

instance NFData (InstantaneousRewards crypto) => NFData (DPState crypto)

instance CC.Crypto crypto => ToCBOR (InstantaneousRewards crypto) where
  toCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> mapToCBOR irR <> mapToCBOR irT <> toCBOR dR <> toCBOR dT

instance CC.Crypto crypto => FromSharedCBOR (InstantaneousRewards crypto) where
  type Share (InstantaneousRewards crypto) = Interns (Credential 'Staking crypto)
  fromSharedPlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      irT <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      dR <- lift fromCBOR
      dT <- lift fromCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance
  CC.Crypto crypto =>
  ToCBOR (DPState crypto)
  where
  toCBOR DPState {dpsPState, dpsDState} =
    encodeListLen 2
      <> toCBOR dpsPState -- We get better sharing when encoding pstate before dstate
      <> toCBOR dpsDState

instance CC.Crypto crypto => FromSharedCBOR (DPState crypto) where
  type
    Share (DPState crypto) =
      ( Interns (Credential 'Staking crypto),
        Interns (KeyHash 'StakePool crypto)
      )
  fromSharedPlusCBOR = decodeRecordNamedT "DPState" (const 2) $ do
    dpsPState <- fromSharedPlusLensCBOR _2
    dpsDState <- fromSharedPlusCBOR
    pure DPState {dpsPState, dpsDState}

instance Default (DPState crypto) where
  def = DPState def def

instance Default (InstantaneousRewards crypto) where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState crypto) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState crypto) where
  def =
    PState Map.empty Map.empty Map.empty

rewards :: DState crypto -> ViewMap crypto (Credential 'Staking crypto) Coin
rewards (DState unified _ _ _) = Rewards unified

delegations ::
  DState crypto ->
  ViewMap crypto (Credential 'Staking crypto) (KeyHash 'StakePool crypto)
delegations (DState unified _ _ _) = Delegations unified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState crypto -> Map Ptr (Credential 'Staking crypto)
ptrsMap (DState (UnifiedMap _ ptrmap) _ _ _) = ptrmap
