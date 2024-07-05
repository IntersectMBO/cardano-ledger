{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}

module Constrained.Env where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable

import Constrained.Core
import Constrained.GenT

-- | Typed environments for mapping `Var a` to `a`
newtype Env = Env {unEnv :: Map EnvKey EnvValue}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Show)

data EnvValue where
  EnvValue :: (Typeable a, Show a) => !a -> EnvValue

deriving instance Show EnvValue

data EnvKey where
  EnvKey :: !(Var a) -> EnvKey

instance Eq EnvKey where
  EnvKey v == EnvKey v' = nameOf v == nameOf v'

instance Ord EnvKey where
  compare (EnvKey v) (EnvKey v') = compare (nameOf v) (nameOf v')

instance Show EnvKey where
  show (EnvKey var) = show var

extendEnv :: (Typeable a, Show a) => Var a -> a -> Env -> Env
extendEnv v a (Env m) = Env $ Map.insert (EnvKey v) (EnvValue a) m

removeVar :: Var a -> Env -> Env
removeVar v (Env m) = Env $ Map.delete (EnvKey v) m

singletonEnv :: (Typeable a, Show a) => Var a -> a -> Env
singletonEnv v a = Env $ Map.singleton (EnvKey v) (EnvValue a)

lookupEnv :: Typeable a => Env -> Var a -> Maybe a
lookupEnv (Env m) v = do
  EnvValue val <- Map.lookup (EnvKey v) m
  cast val

findEnv :: (Typeable a, MonadGenError m) => Env -> Var a -> m a
findEnv env var = do
  case lookupEnv env var of
    Just a -> pure a
    Nothing -> genError (pure ("Couldn't find " ++ show var ++ " in " ++ show env))
