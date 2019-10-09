{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module OCert
  ( OCert(..)
  , KESPeriod(..)
  , slotsPerKESPeriod
  , kesPeriod)
where

import           Cardano.Binary (FromCBOR(..), ToCBOR, encodeListLen
                                , enforceSize, toCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           GHC.Generics (Generic)

import           Keys (DSIGNAlgorithm, KESAlgorithm, Sig, VKey, VKeyES)
import           Slot (Slot (..))

import           Numeric.Natural (Natural)

newtype KESPeriod = KESPeriod Natural
  deriving (Show, Eq, Ord, NoUnexpectedThunks, FromCBOR, ToCBOR)

data OCert dsignAlgo kesAlgo = OCert
  { -- | The operational hot key
    ocertVkHot     :: VKeyES kesAlgo
    -- | The cold key
  , ocertVkCold    :: VKey dsignAlgo
    -- | counter
  , ocertN         :: Natural
    -- | Start of key evolving signature period
  , ocertKESPeriod :: KESPeriod
    -- | Signature of block operational certificate content
  , ocertSigma     :: Sig dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  } deriving (Show, Eq, Generic)

instance
  ( DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  ) => NoUnexpectedThunks (OCert dsignAlgo kesAlgo)

instance
  (DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => ToCBOR (OCert dsignAlgo kesAlgo)
 where
  toCBOR ocert =
    encodeListLen 5
      <> toCBOR (ocertVkHot ocert)
      <> toCBOR (ocertVkCold ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> toCBOR (ocertSigma ocert)

instance
  (DSIGNAlgorithm dsignAlgo, KESAlgorithm kesAlgo)
  => FromCBOR (OCert dsignAlgo kesAlgo)
 where
  fromCBOR = enforceSize "OCert should have 5 fields" 5 >>
    OCert
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

slotsPerKESPeriod :: Natural
slotsPerKESPeriod = 90

kesPeriod :: Slot -> KESPeriod
kesPeriod (Slot s) = KESPeriod $ s `div` slotsPerKESPeriod
