{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Needed for FromCBOR(Annotator CostModel)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Alonzo.Scripts
  ( Tag (..),
    Script (..),
    ExUnits (..),
    CostModel (CostModel),
    Language (..),
    Prices (..),
    proxyhash,
    hashCostModel,
  )
where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.ShelleyMA.Timelocks
import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.Coders
import Data.Map (Map)
import Data.MemoBytes
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Coin (Coin (..))

type SafeHash e t = Hash.Hash (HASH (Crypto e)) t

proxyhash :: forall e t. (ToCBOR t, Era e) => Proxy e -> t -> SafeHash e t
proxyhash Proxy x = Hash.castHash (Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR x)

-- | Marker indicating the part of a transaction for which this script is acting
-- as a validator.
data Tag
  = -- | Validates spending a script-locked UTxO
    Spend
  | -- | Validates minting new tokens
    Mint
  | -- | Validates certificate transactions
    Cert
  | -- | Validates withdrawl from a reward account
    Rewrd
  deriving (Eq, Generic, Ord, Show)

instance NoThunks Tag

data Script era
  = NativeScript (Timelock (Crypto era))
  | PlutusScript
  deriving (Eq, Show, Generic, Ord)

instance Typeable (Crypto era) => NoThunks (Script era)

instance NFData (Script era)

-- type Script era = Timelock (Crypto era)

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

-- Non-Native Script language.
-- This is an open type. We will add values of this type
-- for each Non-Native scripting language as they are added.

newtype Language = Language ByteString
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Language

instance NFData Language

deriving instance ToCBOR Language

deriving instance FromCBOR Language

-- =====================================
-- Cost Model needs to preserve its serialization bytes as
-- it is going to be hashed. Thus we make it a newtype around a MemoBytes

newtype CostModel = CostModelConstr (MemoBytes (Map ByteString Integer))
  deriving (Eq, Generic, Show, Ord)

pattern CostModel :: (Map ByteString Integer) -> CostModel
pattern CostModel m <-
  CostModelConstr (Memo m _)
  where
    CostModel m = CostModelConstr (memoBytes (To m))

instance NoThunks CostModel

instance NFData CostModel

deriving instance ToCBOR CostModel

-- This is needed to derive the FromCBOR (Annotator CostModel) instance
instance FromCBOR (Annotator (Map ByteString Integer)) where
  fromCBOR = pure <$> fromCBOR

deriving via
  Mem (Map ByteString Integer)
  instance
    FromCBOR (Annotator CostModel)

hashCostModel :: forall e. Era e => Proxy e -> CostModel -> SafeHash e CostModel
hashCostModel Proxy x = Hash.castHash (Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR x)

-- ==================================

-- | Prices per execution unit
data Prices = Prices
  { prMem :: !Coin,
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
      encodeTag Spend = Sum Spend 0
      encodeTag Mint = Sum Mint 1
      encodeTag Cert = Sum Cert 2
      encodeTag Rewrd = Sum Rewrd 3

instance FromCBOR Tag where
  fromCBOR = decode $ Summands "Tag" decodeTag
    where
      decodeTag 0 = SumD Spend
      decodeTag 1 = SumD Mint
      decodeTag 2 = SumD Cert
      decodeTag 3 = SumD Rewrd
      decodeTag n = Invalid n

instance ToCBOR ExUnits where
  toCBOR (ExUnits m s) = encode $ Rec ExUnits !> To m !> To s

instance FromCBOR ExUnits where
  fromCBOR = decode $ RecD ExUnits <! From <! From

instance ToCBOR Prices where
  toCBOR (Prices m s) = encode $ Rec Prices !> To m !> To s

instance FromCBOR Prices where
  fromCBOR = decode $ RecD Prices <! From <! From

instance forall era. (Typeable (Crypto era), Typeable era) => ToCBOR (Script era) where
  toCBOR x = encode (encodeScript x)
    where
      encodeScript :: Script era -> Encode 'Open (Script era)
      encodeScript (NativeScript i) = Sum NativeScript 0 !> To i
      encodeScript PlutusScript = Sum PlutusScript 1

instance
  (CC.Crypto (Crypto era), Typeable (Crypto era), Typeable era) =>
  FromCBOR (Annotator (Script era))
  where
  fromCBOR = decode (Summands "Alonzo Script" decodeScript)
    where
      decodeScript :: Word -> Decode 'Open (Annotator (Script era))
      decodeScript 0 = Ann (SumD NativeScript) <*! From
      decodeScript 1 = Ann (SumD PlutusScript)
      decodeScript n = Invalid n

-- =================================================
-- Languages
-- =================================================
