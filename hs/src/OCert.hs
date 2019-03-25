module OCert
  ( OCert(..)
  , KESPeriod
  ) where

import qualified Keys            as Keys
import qualified Slot            as Slot

import           Numeric.Natural (Natural)

type KESPeriod = Slot.Slot

data OCert = OCert
    -- | The operational hot key
  { ocertVkHot     :: Keys.VKey
    -- | counter
  , ocertN         :: Natural
    -- | Start of key evolving signature period
  , ocertKESPeriod :: KESPeriod
    -- | Signature of block operational certificate content
  , ocertSigma     :: Keys.Sig (Keys.VKey, Natural, KESPeriod)
  } deriving (Show, Eq)
