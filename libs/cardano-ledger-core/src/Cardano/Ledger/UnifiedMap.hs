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

type UnifiedMap crypto = UMap Coin (Credential 'Staking crypto) (KeyHash 'StakePool crypto) Ptr

type Triple crypto = Trip Coin Ptr (KeyHash 'StakePool crypto)

type ViewMap crypto = View Coin (Credential 'Staking crypto) (KeyHash 'StakePool crypto) Ptr

instance
  UnifiedView
    Coin
    (Credential 'Staking crypto)
    (KeyHash 'StakePool crypto)
    Ptr
    (Credential 'Staking crypto)
    Coin
  where
  tag = Rew

instance
  UnifiedView
    Coin
    (Credential 'Staking crypto)
    (KeyHash 'StakePool crypto)
    Ptr
    (Credential 'Staking crypto)
    (KeyHash 'StakePool crypto)
  where
  tag = Del

instance
  UnifiedView
    Coin
    (Credential 'Staking crypto)
    (KeyHash 'StakePool crypto)
    Ptr
    Ptr
    (Credential 'Staking crypto)
  where
  tag = Ptr
