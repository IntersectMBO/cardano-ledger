{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}


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

{-
Need to serialize

-}

-- =========================================================
-- Every instance of MAccum, refers to exactly one function

class MAccum unique (m :: Type -> Type) free item ans | unique -> m free item ans where
  maccum :: unique -> free -> ans -> item -> m ans

-- Here is an example instance

-- Make a Unique Unit type. (I.e. an enumeration with one constructor)
data XXX = XXX
  deriving Show

instance ToCBOR XXX where toCBOR XXX = encode(Rec XXX)
instance FromCBOR XXX where fromCBOR = decode(RecD XXX)

-- | The unique 'maccum' function of the (MAccum XXX _ _ _ _) instance

fooAccum :: [a] -> Int -> Int -> Identity Int
fooAccum bs ans v = Identity (v+ans + length bs)

instance MAccum XXX Identity [Bool] Int Int where
  maccum XXX = fooAccum

-- =========================================================
-- LL is a first order data type (no embedded functions)
-- that can be given a (Pulsable (LL name)) instance, We
--  can also make ToCBOR and FromCBOR instances for it.

data LL name (m :: Type -> Type) ans where
  LL:: (MAccum name m free v ans, ToCBOR v, ToCBOR free) =>
       name -> !Int -> !free -> ![v] -> !ans -> LL name m ans

instance (Show ans, Show name) => Show (LL name m ans) where
  show (LL name n _ vs ans) = "(LL "++show name++" "++show n++status vs++" "++show ans++")"


-- There is a single ToCBOR instance for (LL name m ans)
-- But because of the uniqueness of the name, which implies
-- the hidden types (free and v for LL), We must supply
-- a unique FromCBOR instance for each name. See the XXX example below.

instance (Typeable m, ToCBOR name, ToCBOR ans) => ToCBOR (LL name m ans) where
  toCBOR (LL name n free vs ans) = encode(Rec (LL name) !> To n !> To free !> To vs !> To ans)

instance Pulsable (LL name) where
   done (LL _name _n _free zs _ans) = isNil zs
   current (LL _ _ _ _ ans) = ans
   pulseM (ll@(LL _ _ _ [] _)) = pure ll
   pulseM (LL name n free balance ans) = do
       let (steps, balance') = List.splitAt n balance
       ans' <- foldlM' (maccum name free) ans steps
       pure (LL name n free balance' ans')
   completeM (LL name _ free balance ans) = foldlM' (maccum name free) ans balance

-- =================================================
-- To make a serializable type that has a (Pulsable (LL name)) instance,
-- first, define a Unit type (an enumeration with 1 constructor).
-- This will have a unique MAccum instance, which
-- will refer to a unique function with no free variables.
-- If we follow the pattern below, then the Pulsable instance
-- will refer to that (MAccum) instance, but will store only
-- first order data.

-- We must supply a unique FromCBOR instance for each 'name'. The 'name'
-- fixes the monad 'm' and 'ans' type, as well as the 'maccum' function
-- for XXX at the value level.

instance FromCBOR (LL XXX Identity Int) where
  fromCBOR = decode (RecD (LL XXX) <! From <! From <! From <! From)

foo :: LL XXX Identity Int
foo = LL XXX 3 [True] [1,2,3,5,6,7,8] 0
