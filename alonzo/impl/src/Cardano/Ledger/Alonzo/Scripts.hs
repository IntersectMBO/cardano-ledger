{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    Script,
    ExUnits (..),
    CostModel,
    Language,
    Prices (..)
  )
where

import Data.Map (Map)
import Data.ByteString (ByteString)
import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.ShelleyMA.Timelocks
import Data.Coders
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Control.DeepSeq (NFData (..))
import Shelley.Spec.Ledger.Coin (Coin (..))

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Input
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Wdrl
  deriving (Eq, Generic, Ord, Show)

instance NoThunks Tag

-- TODO Extend this to include Plutus scripts (CAD-1908)
-- data Script era
--   = NativeScript (Timelock era)
--   | NonNativeScript
type Script era = Timelock (Crypto era)

-- | Arbitrary execution unit in which we measure the cost of scripts.
data ExUnits = ExUnits
  { exUnitsMem :: !Word64,
    exUnitsSteps :: !Word64
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks ExUnits
instance NFData ExUnits

instance Semigroup ExUnits where
  ExUnits a c <> ExUnits b d = ExUnits (a + b) (c + d)

instance Monoid ExUnits where
  mempty = ExUnits 0 0

-- Script language
newtype Language = Language ByteString
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Language
instance NFData Language
deriving instance ToCBOR Language
deriving instance FromCBOR Language

-- Cost Model
newtype CostModel = CostModel (Map ByteString Integer)
  deriving (Eq, Generic, Show, Ord)

instance NoThunks CostModel
instance NFData CostModel
deriving instance ToCBOR CostModel
deriving instance FromCBOR CostModel

-- | Prices per execution unit
data Prices = Prices
  { prMem  :: !Coin,
    prSteps :: !Coin
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Prices
instance NFData Prices

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance ToCBOR Tag where
  toCBOR = encode . encodeTag
    where
      encodeTag Input = Sum Input 0
      encodeTag Mint = Sum Mint 1
      encodeTag Cert = Sum Cert 2
      encodeTag Wdrl = Sum Wdrl 3

instance FromCBOR Tag where
  fromCBOR = decode $ Summands "Tag" decodeTag
    where
      decodeTag 0 = SumD Input
      decodeTag 1 = SumD Mint
      decodeTag 2 = SumD Cert
      decodeTag 3 = SumD Wdrl
      decodeTag n = Invalid n

instance ToCBOR ExUnits where
  toCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance FromCBOR ExUnits where
  fromCBOR = decode $ RecD ExUnits <! From <! From

instance ToCBOR Prices where
  toCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance FromCBOR Prices where
  fromCBOR = decode $ RecD Prices <! From <! From
