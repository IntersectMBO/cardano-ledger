{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides variables (V era t), and mappings of them to objects of type 't'
module Test.Cardano.Ledger.Constrained.Env (
  Env,
  emptyEnv,
  findVar,
  storeVar,
  Dyn (..),
  Field,
)
where

import Test.Cardano.Ledger.Constrained.Monad (Dyn (..))
import Test.Cardano.Ledger.Constrained.TypeRep

import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Lens.Micro
import Data.Functor.Identity (Identity)
import Type.Reflection (typeRep)
import Data.Type.Equality (TestEquality(..))

-- ================================================================

type Field era x = Lens' (NewEpochState era) x

data Payload where
  Payload :: Rep t -> t -> (Maybe (Lens' s t)) -> Payload

-- We are ignoring the Accessfield on purpose

type Env env = Rec Identity env

emptyEnv :: Env '[]
emptyEnv = RNil

findVar :: forall (n :: String) {env}. Env env -> Lookup n env
findVar RNil = error $ "Failed to find variable " ++ show (typeRep @n)
findVar (Field n v :& t) = case testEquality n (typeRep @n) of
  Just Refl -> v
  Nothing -> findVar t

storeVar :: forall (n :: String) t env. t -> Env env -> (Env (n .: t ': env))
storeVar t env = t :& env
