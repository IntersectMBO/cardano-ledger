{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A Monomorphic version of UMap specialized to the needs of Cardano
module Cardano.Ledger.UnifiedMap
  ( ViewMap,
    UnifiedMap,
    Triple,
    View (..),
    UMap (..),
    Trip (..),
    UnifiedView (..),
    Tag (..),
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Data.UMap (Tag (..), Trip (..), UMap (..), UnifiedView (..), View (..))

-- ====================================================
-- Recall
-- data UMap coin cred pool ptr = UnifiedMap !(Map cred (Trip coin ptr pool)) !(Map ptr cred)

type UnifiedMap c = UMap Coin (Credential 'Staking c) (KeyHash 'StakePool c) Ptr

type Triple c = Trip Coin Ptr (KeyHash 'StakePool c)

type ViewMap c = View Coin (Credential 'Staking c) (KeyHash 'StakePool c) Ptr

instance
  UnifiedView
    Coin
    (Credential 'Staking c)
    (KeyHash 'StakePool c)
    Ptr
    (Credential 'Staking c)
    Coin
  where
  tag = Rew

instance
  UnifiedView
    Coin
    (Credential 'Staking c)
    (KeyHash 'StakePool c)
    Ptr
    (Credential 'Staking c)
    (KeyHash 'StakePool c)
  where
  tag = Del

instance
  UnifiedView
    Coin
    (Credential 'Staking c)
    (KeyHash 'StakePool c)
    Ptr
    Ptr
    (Credential 'Staking c)
  where
  tag = Ptr
