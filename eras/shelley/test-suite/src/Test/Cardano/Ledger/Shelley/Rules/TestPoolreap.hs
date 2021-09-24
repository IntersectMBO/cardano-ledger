{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Shelley.Rules.TestPoolreap
  ( removedAfterPoolreap,
  )
where

import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.LedgerState
  ( PState (..),
    _pParams,
  )
import Cardano.Ledger.Slot (EpochNo (..))
import Control.SetAlgebra (dom, eval, setSingleton, (∩), (⊆), (▷))
import qualified Data.Set as Set (Set, null)
import Test.QuickCheck (Property, property)

-----------------------------
-- Properties for POOLREAP --
-----------------------------

-- | Check that after a POOLREAP certificate transition the pool is removed from
-- the stake pool and retiring maps.
removedAfterPoolreap ::
  forall crypto.
  PState crypto ->
  PState crypto ->
  EpochNo ->
  Property
removedAfterPoolreap p p' e =
  property $
    eval (retire ⊆ dom stp)
      && Set.null (eval (retire ∩ dom stp'))
      && Set.null (eval (retire ∩ dom retiring'))
  where
    stp = _pParams p
    stp' = _pParams p'
    retiring = _retiring p
    retiring' = _retiring p'
    retire :: Set.Set (KeyHash 'StakePool crypto) -- This declaration needed to disambiguate 'eval'
    retire = eval (dom (retiring ▷ setSingleton e))
