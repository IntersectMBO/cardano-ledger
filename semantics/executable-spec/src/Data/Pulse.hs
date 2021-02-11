{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DerivingVia #-}

module Data.Pulse where

import qualified Data.List as List
import Data.Kind
import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Map.Internal (Map (..))
import Control.Monad.Identity(Identity(..))
import Data.Coders
import Cardano.Binary(ToCBOR(..),FromCBOR(..))
import Data.Typeable
import Data.Closure(Closure(..),apply,rootName)
import NoThunks.Class(NoThunks,InspectHeapNamed(..))
import Control.DeepSeq (NFData,rnf)


-- ====================================================


{- | let T be a Pulse structure. A Pulse struture
   is abstracted over a monad: m, and an answer type: t,
   so the concrete type of a pulse structure is written: (T m a).
   The Pulsable class supplies operations on the structure
   that allow its computation to be split into many discrete
   steps. One does this by running: "pulse p" or "pulseM p",
   depending upon whether the computation is monadic or not,
   to run a discrete step.  The scheduling infrastructure needs
   to know nothing about what is going on inside the pulse structure.
-}
class Pulsable (pulse :: (Type -> Type) -> Type -> Type) where
   done ::  pulse m ans -> Bool
   current :: pulse m ans -> ans
   pulseM :: Monad m => pulse m ans -> m(pulse m ans)
   completeM :: Monad m => pulse m ans -> m ans
   completeM p = if done p
                    then pure (current p)
                    else  do p' <- pulseM p; completeM p'

-- =================================
-- Pulse structure for List in an arbitray monad

data PulseListM m ans where
  PulseList:: Assoc -> !Int -> !(ans -> a -> m ans) -> ![a] -> !ans -> PulseListM m ans

instance Show ans => Show (PulseListM m ans) where
  show(PulseList ass n _ t a) = "(Pulse "++assoc ass++show n++status t++show a++")"
    where assoc LeftA = "left "
          assoc RightA = "right "

isNil :: [a] -> Bool
isNil [] = True
isNil (_ : _) = False

status :: [a] -> String
status x = if isNil x then " Done " else " More "

-- =================================
-- Pulse structure for Map in an arbitray monad

data PulseMapM m ans where
  PulseMap:: !Int ->  !(ans -> k -> v -> m ans) -> !(Map k v) -> !ans -> PulseMapM m ans

instance Show ans => Show (PulseMapM m ans) where
  show(PulseMap n _ t a) =
    "(Pulse "++show n++(if Map.null t then " Done " else " More ")++show a++")"

-- ===============================================================
-- Pulse structures can be Specialize to the Identity Monad

type PulseList ans = PulseListM Identity ans
type PulseMap ans = PulseListM Identity ans

-- Use these 'pseudo constructors' to construct Pulse structures in
-- the identity monad. They automatically lift the accumulating function

pulseList:: Assoc -> Int -> (t1 -> t2 -> t1) -> [t2] -> t1 -> PulseListM Identity t1
pulseList LeftA n accum xs zero =
   PulseList LeftA n (\ ans x -> Identity(accum ans x)) xs zero
pulseList RightA n accum xs zero =
   PulseList RightA n (\ ans x -> Identity(accum ans x)) (reverse xs) zero


pulseMap:: Int -> (a -> k -> v -> a) -> Map k v -> a -> PulseMapM Identity a
pulseMap n accum ts zero = PulseMap n  (\ ans k v -> Identity(accum ans k v)) ts zero

-- run Pulse structures in the Identity monad.

pulse :: Pulsable p => p Identity ans -> p Identity ans
pulse p = runIdentity (pulseM p)

complete :: Pulsable p => p Identity ans -> ans
complete p = runIdentity (completeM p)

-- =================================================
-- Some instances

instance Pulsable PulseListM where
   done (PulseList _ _ _ zs _) = isNil zs
   current (PulseList _ _ _ _ ans) = ans
   pulseM (ll@(PulseList _ _ _ balance _)) | isNil balance = pure ll
   pulseM (PulseList ass n accum balance ans) = do
       let (steps, balance') = List.splitAt n balance
       ans' <- (foldM' ass) accum ans steps
       pure (PulseList ass n accum balance' ans')

instance Pulsable PulseMapM where
   done (PulseMap _ _ m _) = Map.null m
   current (PulseMap _ _ _ ans) = ans
   pulseM (ll@(PulseMap _ _ balance _)) | Map.null balance = pure ll
   pulseM (PulseMap n accum balance ans) = do
      let (steps, balance') = Map.splitAt n balance
      ans' <-  foldlWithKeyM' accum ans steps
      pure (PulseMap n accum balance' ans')

-- ================================================================
-- Special monadic folds for use with PulseListM and PulseMapM
-- They are strict, monadic, and their arguments are in the right order.
-- These functions should appear somewhere in Data.List or Data.List or
-- Data.Foldable or Data.Traversable, or Control.Monad, but they don't.

-- | A strict, monadic, version of 'foldl' for lists. It  associates to the left.
foldlM' :: Monad m => (ans -> k -> m ans) -> ans -> [k] -> m ans
foldlM' _accum !ans [] = pure ans
foldlM' accum !ans (k:more) = do { ans1 <- accum ans k; foldlM' accum ans1 more }


-- | A strict, monadic, version of 'foldlr' for lists. It  associates to the right.
foldrM' :: Monad m => (ans -> k -> m ans) -> ans -> [k] -> m ans
foldrM' _accum !ans [] = pure ans
foldrM' accum !ans (k : more) = do !ans1 <- foldrM' accum ans more; accum ans1 k

data Assoc = LeftA | RightA

-- | Choose either asscociating to the left or right.
foldM' :: Monad m => Assoc -> (ans -> k -> m ans) -> ans -> [k] -> m ans
foldM' LeftA  = foldlM'
foldM' RightA = foldrM'

-- | /O(n)/. A strict, monadic, version of 'foldlWithKey'. Each application of the
--   operator is evaluated before using the result in the next application. This
--   function is strict in the starting value. Associates to the left.
foldlWithKeyM' :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
foldlWithKeyM' f z = go z
  where
    go !z' Tip              = pure z'
    go z' (Bin _ kx x l r) =
      do !ans1 <- (go z' l)
         !ans2 <- (f ans1 kx x)
         go ans2 r


-- =========================================================

-- | Serializable List based Pulser (SLP)
data SLP name env i (m :: Type -> Type) ans where
  SLP:: !Int -> !(Closure name env (ans -> i -> m ans)) -> ![i] -> !ans -> SLP name env i m ans

deriving via InspectHeapNamed "SLP" (SLP n e i m t) instance NoThunks (SLP n e i m t)

instance (NFData ans, NFData i, NFData (Closure name '[] (ans -> i -> m ans))) => NFData (SLP name '[] i m ans) where
  rnf (SLP n1 c1 b1 a1) = seq (rnf n1) (seq (rnf c1) (seq (rnf b1) (rnf a1)))

instance (NFData ans, NFData i, NFData (Closure name (z ': e) (ans -> i -> m ans))) => NFData (SLP name (z ': e) i m ans) where
  rnf (SLP n1 c1 b1 a1) =  seq (rnf n1) (seq (rnf c1) (seq (rnf b1) (rnf a1)))

instance (Show ans, Show i) => Show (SLP name env i m ans) where
  show (SLP n cl cs ans) = "(SLP "++show n++" "++rootName cl++status cs++show ans++")"

-- we need a pair of Eq instances

instance (Eq ans, Eq i, Eq (Closure name '[] (ans -> i -> m ans))) => Eq (SLP name '[] i m ans) where
  (SLP n1 c1 b1 a1) == (SLP n2 c2 b2 a2) = (n1==n2) && (c1==c2) && (b1==b2) && (a1==a2)

instance (Eq ans, Eq i, Eq z, Eq (Closure name (z ': e) (ans -> i -> m ans))) => Eq (SLP name (z ': e) i m ans) where
  (SLP n1 c1 b1 a1) == (SLP n2 c2 b2 a2) = (n1==n2) && (c1==c2) && (b1==b2) && (a1==a2)

instance Pulsable (SLP name env i) where
   done (SLP _n _cl zs _ans) = isNil zs
   current (SLP _ _ _ ans) = ans
   pulseM (ll@(SLP _ _ [] _)) = pure ll
   pulseM (SLP n cl balance ans) = do
       let (steps, balance') = List.splitAt n balance
       ans' <- foldlM' (apply cl) ans steps
       pure (SLP n cl balance' ans')
   completeM (SLP _ cl balance ans) = foldlM' (apply cl) ans balance

instance
   ( ToCBOR ans,
     ToCBOR i,
     ToCBOR (Closure name env (ans -> i -> m ans)),
     Typeable m,
     Typeable name,
     Typeable env
   ) => ToCBOR (SLP name env i m ans) where
   toCBOR (SLP n cl balance ans) = encode (Rec SLP !> To n !> To cl !> To balance !> To ans)

instance
   ( FromCBOR ans,
     FromCBOR i,
     FromCBOR (Closure name env (ans -> i -> m ans)),
     Typeable m,
     Typeable name,
     Typeable env
   )
   => FromCBOR (SLP name env i m ans) where
   fromCBOR = decode(RecD SLP <! From <! From <! From <! From)
