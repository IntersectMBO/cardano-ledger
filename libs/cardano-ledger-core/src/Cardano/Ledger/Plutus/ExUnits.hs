{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Plutus.ExUnits (
  txscriptfee,
  pointWiseExUnits,
  zipSemiExUnits,
  ExUnits (ExUnits, exUnitsMem, exUnitsSteps, ..),
  ExUnits' (..),
  Prices (..),
)
where

import Cardano.Ledger.BaseTypes (
  BoundedRational (unboundRational),
  NonNegativeInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  cborError,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (D, From, RecD),
  Encode (Rec, To),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.DerivingVia (InstantiatedAt (..))
import Data.Int (Int64)
import Data.Measure (BoundedMeasure, Measure)
import Data.Semigroup (All (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Arbitrary execution unit in which we measure the cost of scripts in terms
-- of space in memory and execution time.
--
-- The ledger itself uses 'ExUnits' Natural' exclusively, but the flexibility here
-- allows the consensus layer to translate the execution units into something
-- equivalent to 'ExUnits (Inf Natural)'. This is needed in order to provide
-- a 'BoundedMeasure' instance, which itself is needed for the alonzo instance of
-- 'TxLimits' (in consensus).
data ExUnits' a = ExUnits'
  { exUnitsMem' :: !a
  , exUnitsSteps' :: !a
  }
  deriving (Eq, Generic, Show, Functor)
  -- It is deliberate that there is no Ord instance, use `pointWiseExUnits` instead.
  deriving
    (Measure, BoundedMeasure)
    via (InstantiatedAt Generic (ExUnits' a))
  deriving
    (Monoid, Semigroup)
    via (InstantiatedAt Measure (ExUnits' a))

instance NoThunks a => NoThunks (ExUnits' a)

instance NFData a => NFData (ExUnits' a)

deriving instance ToJSON a => ToJSON (ExUnits' a)

deriving instance FromJSON a => FromJSON (ExUnits' a)

-- | This newtype wrapper of ExUnits' is used to hide
--  an implementation detail inside the ExUnits pattern.
newtype ExUnits = WrapExUnits {unWrapExUnits :: ExUnits' Natural}
  deriving (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup)

instance NoThunks ExUnits

instance NFData ExUnits

instance ToJSON ExUnits where
  toJSON exUnits@(ExUnits _ _) =
    let ExUnits {exUnitsMem, exUnitsSteps} = exUnits
     in object
          [ "memory" .= toJSON exUnitsMem
          , "steps" .= toJSON exUnitsSteps
          ]

instance FromJSON ExUnits where
  parseJSON = withObject "exUnits" $ \o -> do
    exUnitsMem <- checkWord64Bounds =<< (o .: "memory" <|> o .: "exUnitsMem")
    exUnitsSteps <- checkWord64Bounds =<< (o .: "steps" <|> o .: "exUnitsSteps")
    pure $ ExUnits {exUnitsMem, exUnitsSteps}
    where
      checkWord64Bounds n =
        if n >= fromIntegral (minBound @Word64)
          && n <= fromIntegral (maxBound @Word64)
          then pure n
          else fail ("Unit out of bounds for Word64: " <> show n)

-- | Arbitrary execution unit in which we measure the cost of scripts in terms
-- of space in memory and execution time.
--
-- This pattern hides the fact that ExUnits' is parametric in the underlying type.
-- The ledger itself uses 'ExUnits' Natural' exclusively.
--
-- We would have preferred to use a type alias for 'ExUnits' Natural',
-- but this is not possible: https://gitlab.haskell.org/ghc/ghc/-/issues/19507.
pattern ExUnits :: Natural -> Natural -> ExUnits
pattern ExUnits {exUnitsMem, exUnitsSteps} <-
  WrapExUnits (ExUnits' exUnitsMem exUnitsSteps)
  where
    ExUnits m s = WrapExUnits (ExUnits' m s)

{-# COMPLETE ExUnits #-}

-- | It is deliberate that there is no `Ord` instance for `ExUnits`. Use this function to
--   compare if one `ExUnit` is pointwise compareable to another. In case when `Ord`
--   instance like comparison is necessary you can use @`zipSemiExUnits` `compare`@
pointWiseExUnits :: (Natural -> Natural -> Bool) -> ExUnits -> ExUnits -> Bool
pointWiseExUnits f ex1 ex2 = getAll (zipSemiExUnits (\x y -> All (f x y)) ex1 ex2)

-- | Pointwise combine units into a semigroup and mappened the results.
zipSemiExUnits :: Semigroup a => (Natural -> Natural -> a) -> ExUnits -> ExUnits -> a
zipSemiExUnits f (ExUnits m1 s1) (ExUnits m2 s2) = (m1 `f` m2) <> (s1 `f` s2)

-- ==================================

-- | Prices per execution unit
data Prices = Prices
  { prMem :: !NonNegativeInterval
  , prSteps :: !NonNegativeInterval
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Prices

instance NFData Prices

instance ToJSON Prices where
  toJSON Prices {prSteps, prMem} =
    object
      [ "priceSteps" .= prSteps
      , "priceMemory" .= prMem
      ]

instance FromJSON Prices where
  parseJSON =
    withObject "prices" $ \o -> do
      prSteps <- o .: "priceSteps" <|> o .: "prSteps"
      prMem <- o .: "priceMemory" <|> o .: "prMem"
      return Prices {prSteps, prMem}

-- | Compute the cost of a script based upon prices and the number of execution
-- units.
txscriptfee :: Prices -> ExUnits -> Coin
txscriptfee Prices {prMem, prSteps} ExUnits {exUnitsMem = m, exUnitsSteps = s} =
  Coin $
    ceiling $
      (fromIntegral m * unboundRational prMem)
        + (fromIntegral s * unboundRational prSteps)

instance EncCBOR ExUnits where
  encCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance DecCBOR ExUnits where
  decCBOR = decode $ RecD ExUnits <! D decNat <! D decNat
    where
      decNat :: Decoder s Natural
      decNat = do
        x <- decCBOR
        when
          (x > fromIntegral (Prelude.maxBound :: Int64))
          ( cborError $
              DecoderErrorCustom "ExUnits field" "values must not exceed maxBound :: Int64"
          )
        pure $ wordToNatural x
      {-# INLINE decNat #-}
      wordToNatural :: Word64 -> Natural
      wordToNatural = fromIntegral
      {-# INLINE wordToNatural #-}
  {-# INLINE decCBOR #-}

instance EncCBOR Prices where
  encCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance DecCBOR Prices where
  decCBOR = decode $ RecD Prices <! From <! From
  {-# INLINE decCBOR #-}
