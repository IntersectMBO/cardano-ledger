{-# LANGUAGE UndecidableInstances #-}

module Constrained.Examples.Tutorial where

import Constrained
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Test.QuickCheck.Gen

-- Here are the defined function symbols
-- You may also use the methods of Num, since there is a (Num (Term fn)) instance.
import Constrained (
  -- Function symbols
  disjoint_,
  dom_,
  elem_,
  length_,
  member_,
  not_,
  rng_,
  singleton_,
  sizeOf_,
  subset_,
  sum_,
  (/=.),
  (<.),
  (<=.),
  (==.),
  (>.),
  (>=.),
 )
import Constrained.Base (fromList_, null_, union_)

-- See the acompanying Google Doc
-- https://docs.google.com/presentation/d/1AN6YFJKrjQ8yLUHX8a2uAksrIBHlQ9hOfIjjdyyPWA4/edit?usp=sharing

-- =================================================

data State = State
  { utxo :: Map Integer Integer
  , maxInputs :: Integer
  , maxOutputs :: Integer
  , minValue :: Integer
  }
  deriving (Generic, Eq)

instance HasSimpleRep State
instance BaseUniverse fn => HasSpec fn State

instance Show State where
  show (State u i o v) =
    unlines
      [ "  utxo = " ++ show u
      , "  mapInputs = " ++ show i
      , "  maxOutputs = " ++ show o
      , "  minValue = " ++ show v
      ]

data Tx = Tx
  { inputs :: Set Integer
  , outputs :: [Integer]
  , txBodyHash :: Integer
  }
  deriving (Generic, Eq)

instance HasSimpleRep Tx
instance BaseUniverse fn => HasSpec fn Tx

instance Show Tx where
  show (Tx i o h) = unlines ["  Inputs = " ++ show i, "  Outputs = " ++ show o, "  txBodyHash = " ++ show h]

data System = System State Tx deriving (Generic, Eq)

instance HasSimpleRep System
instance BaseUniverse fn => HasSpec fn System

instance Show System where
  show (System s t) = unlines ["System", "State", show s, "Tx", show t]

s :: Specification BaseFn System
s = constrained $ \s ->
  match s $ \state tx ->
    match state $ \utxo maxO maxI minV ->
      match tx $ \inputs outputs hash ->
        [subset_ inputs (dom_ utxo)]

main = generate (genFromSpec s)
