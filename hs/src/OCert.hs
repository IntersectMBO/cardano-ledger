module OCert
  ( OCert(..)
  , KESPeriod(..)
  , slotsPerKESPeriod
  , kesPeriod)
where

import qualified Keys         as Keys
import qualified Slot         as Slot

import           Numeric.Natural (Natural)

newtype KESPeriod = KESPeriod Natural
  deriving (Show, Eq, Ord)

data OCert = OCert
  { -- | The operational hot key
    ocertVkHot     :: Keys.VKey
    -- | counter
  , ocertN         :: Natural
    -- | Start of key evolving signature period
  , ocertKESPeriod :: KESPeriod
    -- | Signature of block operational certificate content
  , ocertSigma     :: Keys.Sig (Keys.VKey, Natural, KESPeriod)
  } deriving (Show, Eq)

slotsPerKESPeriod :: Natural
slotsPerKESPeriod = 90

kesPeriod :: Slot.Slot -> KESPeriod
kesPeriod (Slot.Slot s) = KESPeriod $ s `div` slotsPerKESPeriod
