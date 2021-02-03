{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Pulse where

import qualified Data.List as List
import Data.Kind
import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Data.Map.Internal (Map (..))
import Control.Monad.Identity(Identity(..))

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
   completeM p =
      do p' <- pulseM p
         if done p' then pure(current p') else completeM p'

-- =================================
-- Pulse structure for List in an arbitray monad

data PulseListM m ans where
  PulseList:: Assoc -> !Int -> !(ans -> a -> m ans) -> ![a] -> !ans -> PulseListM m ans

instance Show ans => Show (PulseListM m ans) where
  show(PulseList ass n _ t a) = "(Pulse "++assoc ass++show n++status t++show a++")"
    where status [] = " Done "
          status (_ : _) = " More "
          assoc LeftA = "left "
          assoc RightA = "right "

-- =================================
-- Pulse structure for Map in an arbitray monad

data PulseMapM m ans where
  PulseMap:: !Int ->  !(ans -> k -> v -> m ans) -> !(Map k v) -> !ans -> PulseMapM m ans

instance Show ans => Show (PulseMapM m ans) where
  show(PulseMap n _ t a) = "(Pulse "++show n++status t++show a++")"
    where status x = if Map.null x then " Done " else " More "

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
   done (PulseList _ _ _ [] _) = True
   done (PulseList _ _ _ (_: _) _) = False
   current (PulseList _ _ _ _ ans) = ans
   pulseM (PulseList ass n accum balance ans) = do
       let (steps, balance') = List.splitAt n balance
       ans' <- (foldM' ass) accum ans steps
       pure (PulseList ass n accum balance' ans')

instance Pulsable PulseMapM where
   done (PulseMap _ _ m _) = Map.null m
   current (PulseMap _ _ _ ans) = ans
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

-- ======================================================
-- Two examples

isum :: PulseListM Identity Integer
isum = pulseList RightA 10 (+) [1..33] 0

{-  Note how we start adding the last 10 elements
*Pulse> isum
(Pulse right 10 More 0)
*Pulse> pulse it
(Pulse right 10 More 285)
*Pulse> pulse it
(Pulse right 10 More 470)
*Pulse> pulse it
(Pulse right 10 More 555)
*Pulse> pulse it
(Pulse right 10 Done 561)
-}


jsum :: PulseListM Identity Integer
jsum = pulseList LeftA 10 (+) [1..33] 0

{- Here we are adding the first 10 elements
*Pulse> jsum
(Pulse left 10 More 0)
*Pulse> pulse it
(Pulse left 10 More 55)
*Pulse> pulse it
(Pulse left 10 More 210)
*Pulse> pulse it
(Pulse left 10 More 465)
*Pulse> pulse it
(Pulse left 10 Done 561)
-}

ksum :: PulseList Integer
ksum = pulseList RightA 1 (+) [1..5] 0
{-
*Pulse> ksum
(Pulse right 1 More 0)
*Pulse> pulse it
(Pulse right 1 More 5)
*Pulse> pulse it
(Pulse right 1 More 9)
*Pulse> pulse it
(Pulse right 1 More 12)
*Pulse> pulse it
(Pulse right 1 More 14)
*Pulse> pulse it
(Pulse right 1 Done 15)
-}

hsum :: PulseList Integer
hsum = pulseList LeftA 1 (+) [1..5] 0
{-
*Pulse> hsum
(Pulse left 1 More 0)
*Pulse> pulse it
(Pulse left 1 More 1)
*Pulse> pulse it
(Pulse left 1 More 3)
*Pulse> pulse it
(Pulse left 1 More 6)
*Pulse> pulse it
(Pulse left 1 More 10)
*Pulse> pulse it
(Pulse left 1 Done 15)
-}

msum :: PulseMapM Identity (Map Char Integer)
msum = pulseMap 2
          (\ a k n -> Map.insertWith (+) k n a)
          (Map.fromList [(c,1) | c <- "abcdefg"])
          (Map.fromList [('z',1),('d',1),('g',1)])

{-
Pulse> msum
(Pulse 2 More fromList [('d',1),('g',1),('z',1)])
*Pulse> pulse it
(Pulse 2 More fromList [('a',1),('b',1),('d',1),('g',1),('z',1)])
*Pulse> pulse it
(Pulse 2 More fromList [('a',1),('b',1),('c',1),('d',2),('g',1),('z',1)])
*Pulse> pulse it
(Pulse 2 More fromList [('a',1),('b',1),('c',1),('d',2),('e',1),('f',1),('g',1),('z',1)])
*Pulse> pulse it
(Pulse 2 Done fromList [('a',1),('b',1),('c',1),('d',2),('e',1),('f',1),('g',2),('z',1)])
-}


iosum :: PulseListM IO ()
iosum = PulseList LeftA 2 (\ () k -> putStrLn (show k)) [(1::Int)..5] ()
{-
*Pulse> iosum
(Pulse left 2 More ())
*Pulse> pulseM it
1
2
(Pulse left 2 More ())
*Pulse> pulseM it
3
4
(Pulse left 2 More ())
*Pulse> pulseM it
5
(Pulse left 2 Done ())
-}