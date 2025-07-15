{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Orphans () where

import Cardano.Crypto.Hash (Hash (..))
import qualified Cardano.Crypto.Hash as HS
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (Anchor (..), Network (..), UnitInterval, textToUrl)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (
  EraPParams,
  KeyHash (..),
  PParams,
  PParamsUpdate,
  SafeHash,
  VRFVerKeyHash (..),
  emptyPParams,
  emptyPParamsUpdate,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (GenDelegs (..), unsafeMakeSafeHash)
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.State (
  ChainAccountState (..),
  CommitteeState (..),
  DState (..),
  FuturePParams (..),
  InstantaneousRewards (..),
  PState (..),
  SnapShots,
  UTxO (..),
  emptySnapShots,
 )
import qualified Cardano.Ledger.UMap as UM
import qualified Data.ByteString as Long
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import Data.Data (Proxy (..))
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as SS

instance Default (SS.StrictSeq t) where
  def = SS.Empty

instance Default Short.ShortByteString where
  def = Short.empty

instance Default Long.ByteString where
  def = Long.empty

instance Default Lazy.ByteString where
  def = Lazy.empty

instance HS.HashAlgorithm h => Default (Hash h b) where
  def =
    UnsafeHash $
      Short.pack $
        replicate (fromIntegral (Hash.sizeHash (Proxy :: Proxy h))) 0

deriving instance Default (KeyHash r)

instance Default (SafeHash i) where
  def = unsafeMakeSafeHash def

instance Default UnitInterval where
  def = minBound

instance Default Anchor where
  def = Anchor (fromJust $ textToUrl 0 "") def

instance Default Network where
  def = Mainnet

instance Default (Credential r) where
  def = KeyHashObj def

instance Default ChainAccountState where
  def = ChainAccountState (Coin 0) (Coin 0)

instance EraPParams era => Default (PParams era) where
  def = emptyPParams

instance EraPParams era => Default (PParamsUpdate era) where
  def = emptyPParamsUpdate

instance Default RewardAccount where
  def = RewardAccount def def

deriving instance Default (VRFVerKeyHash k)

instance Default PoolParams where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

deriving instance Default (CommitteeState era)

instance Default InstantaneousRewards where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState era) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState era) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

instance Default (FuturePParams era) where
  def = NoPParamsUpdate

instance Default SnapShots where
  def = emptySnapShots

deriving instance Default (UTxO era)
