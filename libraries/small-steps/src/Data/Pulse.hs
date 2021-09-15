{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Pulse
  ( -- * The class that defines operations on pulsers.
    Pulsable (..),

    -- * Two reusable types that have Pulsable instances
    PulseMapM (..),
    PulseListM (..),

    -- * Virtual versions of PulseMapM and PulseListM specialized to be non-monadic.
    PulseMap,
    PulseList,
    pulseList,
    pulseMap,
    pulse,
    complete,

    -- * Monadic folds designed to be used inside pulsers.
    foldlM',
    foldlWithKeyM',
  )
where

import Control.Monad.Identity (Identity (..))
import qualified Data.Foldable as Foldable
import Data.Kind
import qualified Data.List as List
import Data.Map (Map)
import Data.Map.Internal (Map (..))
import qualified Data.Map.Strict as Map

-- ====================================================

-- | let T be a Pulse structure. A Pulse struture
--   is abstracted over a monad: m, and an answer type: t,
--   so the concrete type of a pulse structure is written: (T m a).
--   The Pulsable class supplies operations on the structure
--   that allow its computation to be split into many discrete
--   steps. One does this by running: "pulse p" or "pulseM p",
--   depending upon whether the computation is monadic or not,
--   to run a discrete step.  The scheduling infrastructure needs
--   to know nothing about what is going on inside the pulse structure.
class Pulsable (pulse :: (Type -> Type) -> Type -> Type) where
  done :: pulse m ans -> Bool
  current :: pulse m ans -> ans
  pulseM :: Monad m => pulse m ans -> m (pulse m ans)
  completeM :: Monad m => pulse m ans -> m ans
  completeM p =
    if done p
      then pure (current p)
      else do p' <- pulseM p; completeM p'

-- =================================
-- Pulse structure for List in an arbitray monad

-- | A List based pulser
data PulseListM m ans where
  PulseList :: !Int -> !(ans -> a -> m ans) -> ![a] -> !ans -> PulseListM m ans

instance Show ans => Show (PulseListM m ans) where
  show (PulseList n _ t a) = "(Pulse " ++ show n ++ status t ++ show a ++ ")"

status :: [a] -> String
status x = if null x then " Done " else " More "

-- =================================
-- Pulse structure for Map in an arbitray monad

-- | A Map based pulser.
data PulseMapM m ans where
  PulseMap :: !Int -> !(ans -> k -> v -> m ans) -> !(Map k v) -> !ans -> PulseMapM m ans

instance Show ans => Show (PulseMapM m ans) where
  show (PulseMap n _ t a) =
    "(Pulse " ++ show n ++ (if Map.null t then " Done " else " More ") ++ show a ++ ")"

-- ===============================================================
-- Pulse structures can be Specialize to the Identity Monad

-- | Type of a List based pulser in the Identity monad.
type PulseList ans = PulseListM Identity ans

-- | Type of a Map based pulser in the Identity monad.
type PulseMap ans = PulseListM Identity ans

-- Use these 'pseudo constructors' to construct Pulse structures in
-- the identity monad. They automatically lift the accumulating function

-- | Create List pulser structure in the Identity monad, a pure accumulator is lifted to a monadic one.
pulseList :: Int -> (t1 -> t2 -> t1) -> [t2] -> t1 -> PulseListM Identity t1
pulseList n accum xs zero =
  PulseList n (\ans x -> Identity (accum ans x)) xs zero

-- | Create Map pulser structure in the Identity monad, a pure accumulator is lifted to a monadic one.
pulseMap :: Int -> (a -> k -> v -> a) -> Map k v -> a -> PulseMapM Identity a
pulseMap n accum ts zero = PulseMap n (\ans k v -> Identity (accum ans k v)) ts zero

-- run Pulse structures in the Identity monad.

-- | Pulse a structure in the Identity monad
pulse :: Pulsable p => p Identity ans -> p Identity ans
pulse p = runIdentity (pulseM p)

-- | Complete a structure in the Identity monad
complete :: Pulsable p => p Identity ans -> ans
complete p = runIdentity (completeM p)

-- =================================================
-- Some instances

instance Pulsable PulseListM where
  done (PulseList _ _ zs _) = null zs
  current (PulseList _ _ _ ans) = ans
  pulseM (ll@(PulseList _ _ balance _)) | null balance = pure ll
  pulseM (PulseList n accum balance ans) = do
    let (steps, balance') = List.splitAt n balance
    ans' <- foldlM' accum ans steps
    pure (PulseList n accum balance' ans')
  completeM (PulseList _ accum balance ans) = foldlM' accum ans balance

instance Pulsable PulseMapM where
  done (PulseMap _ _ m _) = Map.null m
  current (PulseMap _ _ _ ans) = ans
  pulseM (ll@(PulseMap _ _ balance _)) | Map.null balance = pure ll
  pulseM (PulseMap n accum balance ans) = do
    let (steps, balance') = Map.splitAt n balance
    ans' <- foldlWithKeyM' accum ans steps
    pure (PulseMap n accum balance' ans')

-- ================================================================
-- Special monadic folds for use with PulseListM and PulseMapM
-- They are strict, monadic, and their arguments are in the right order.
-- These functions should appear somewhere in Data.List or Data.List or
-- Data.Foldable or Data.Traversable, or Control.Monad, but they don't.

-- | A strict, monadic, version of 'foldl'. It  associates to the left.
foldlM' :: (Foldable t, Monad m) => (ans -> k -> m ans) -> ans -> t k -> m ans
foldlM' accum !ans acc = case Foldable.toList acc of
  [] -> pure ans
  (k : more) -> do ans1 <- accum ans k; foldlM' accum ans1 more

-- | /O(n)/. A strict, monadic, version of 'foldlWithKey'. Each application of the
--   operator is evaluated before using the result in the next application. This
--   function is strict in the starting value. Associates to the left.
foldlWithKeyM' :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
foldlWithKeyM' f z = go z
  where
    go !z' Tip = pure z'
    go z' (Bin _ kx x l r) =
      do
        !ans1 <- (go z' l)
        !ans2 <- (f ans1 kx x)
        go ans2 r

-- ===================================
-- We could probably generalise this to PulseFoldableM over any
-- foldable structure. We would have to devise a way to break a Foldable
-- structure into small pieces. Lets leave this to another day.
