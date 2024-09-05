{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Core.Fuzzer () where

import Test.Cardano.Ledger.Common (Gen)
import Control.Monad.State.Strict (StateT, MonadState (..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Type.Reflection (TypeRep, typeRep)
import qualified Data.Set as Set

data Mutate a where
  Pure :: a -> Mutate a
  Ap :: Mutate (a -> b) -> Mutate a -> Mutate b

instance Functor Mutate where
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap g a) = Ap ((f .) <$> g) a

instance Applicative Mutate where
  pure = Pure
  (<*>) = Ap

class Mutatable a where
  mutate :: a -> Mutate a

data MutateState = MutateState
  { msNMutations :: Int
  }

countTips :: Mutate a -> Int
countTips (Pure _) = 1
countTips (Ap f a) = countTips f + countTips a
