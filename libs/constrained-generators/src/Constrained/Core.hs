{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Constrained.Core where

import Control.Applicative
import Data.Function
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable

import Constrained.List

-- Variables --------------------------------------------------------------

data Var a = Var {nameOf :: Int, nameHint :: String}

instance Ord (Var a) where
  compare = compare `on` nameOf

instance Eq (Var a) where
  (==) = (==) `on` nameOf

instance Show (Var a) where
  show v = nameHint v ++ "_" ++ show (nameOf v)

eqVar :: forall a a'. (Typeable a, Typeable a') => Var a -> Var a' -> Maybe (a :~: a')
eqVar v v' | nameOf v == nameOf v' = eqT @a @a'
eqVar _ _ = Nothing

-- Variable renaming ------------------------------------------------------

class Rename a where
  rename :: Typeable x => Var x -> Var x -> a -> a

instance Typeable a => Rename (Var a) where
  rename v v' v''
    | Just Refl <- eqVar v v'' = v'
    | otherwise = v''

instance Rename () where
  rename _ _ _ = ()

instance (Rename a, Rename b) => Rename (a, b) where
  rename x x' (a, b) = (rename x x' a, rename x x' b)

instance {-# OVERLAPPABLE #-} (Functor t, Rename a) => Rename (t a) where
  rename v v'
    | v == v' = id
    | otherwise = fmap (rename v v')

instance (Ord a, Rename a) => Rename (Set a) where
  rename v v'
    | v == v' = id
    | otherwise = Set.map (rename v v')

instance (forall a. Rename (f a)) => Rename (List f as) where
  rename v v' = mapList (rename v v')

freshVar :: Var a -> Set Int -> Var a
freshVar (Var n nh) ns
  | Set.member n ns = Var (1 + Set.findMax ns) nh
  | otherwise = Var n nh

freshen :: (Typeable a, Rename t) => Var a -> t -> Set Int -> (Var a, t)
freshen v t nms
  | nameOf v `Set.member` nms = let v' = freshVar v nms in (v', rename v v' t)
  | otherwise = (v, t)

-- Values -----------------------------------------------------------------

data Value a where
  Value :: Show a => !a -> Value a

deriving instance Eq a => Eq (Value a)
deriving instance Ord a => Ord (Value a)
instance Show (Value a) where
  showsPrec p (Value a) = showsPrec p a

unValue :: Value a -> a
unValue (Value v) = v

-- Cruft ------------------------------------------------------------------

data Evidence c where
  Evidence :: c => Evidence c

unionWithMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWithMaybe f ma ma' = (f <$> ma <*> ma') <|> ma <|> ma'
