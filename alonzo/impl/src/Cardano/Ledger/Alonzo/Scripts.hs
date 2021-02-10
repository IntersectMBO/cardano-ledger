{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
    Prices (..),
    hashCostModel,
    scriptfee,
    ppTag,
    ppScript,
    ppExUnits,
    ppCostModel,
    ppPrices,
  )
where

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppCoin,
    ppInteger,
    ppLong,
    ppMap,
    ppRecord,
    ppSexp,
    ppString,
    ppWord64,
  )
import Cardano.Ledger.SafeHash
  ( HashWithCrypto (..),
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Ledger.Val (Val ((<+>), (<×>)))
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

-- =====================================
-- Cost Model needs to preserve its serialization bytes as
-- it is going to be hashed. Thus we make it a newtype around a MemoBytes

newtype CostModel = CostModelConstr (MemoBytes (Map ByteString Integer))
  deriving (Eq, Generic, Show, Ord)
  deriving newtype (SafeToHash)

-- CostModel does not determine 'crypto' so make a HashWithCrypto
-- rather than a HashAnotated instance.

instance HashWithCrypto CostModel CostModel

pattern CostModel :: Map ByteString Integer -> CostModel
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

-- CostModel is not parameterized by Crypto or Era so we use the
-- hashWithCrypto function, rather than hashAnnotated

hashCostModel ::
  forall e.
  Era e =>
  Proxy e ->
  CostModel ->
  SafeHash (Crypto e) CostModel
hashCostModel _proxy = hashWithCrypto (Proxy @(Crypto e))

-- ==================================

-- | Prices per execution unit
data Prices = Prices
  { prMem :: !Coin,
    prSteps :: !Coin
  }
  deriving (Eq, Generic, Show, Ord)

instance NoThunks Prices

instance NFData Prices

-- | Compute the cost of a script based upon prices and the number of execution
-- units.
scriptfee :: Prices -> ExUnits -> Coin
scriptfee (Prices pr_mem pr_steps) (ExUnits mem steps) =
  (mem <×> pr_mem) <+> (steps <×> pr_steps)

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

-- ============================================================
-- Pretty printing versions

ppTag :: Tag -> PDoc
ppTag x = ppString (show x)

instance PrettyA Tag where prettyA = ppTag

ppScript :: Script era -> PDoc
ppScript PlutusScript = ppString "PlutusScript"
ppScript (NativeScript x) = ppTimelock x

instance PrettyA (Script era) where prettyA = ppScript

ppExUnits :: ExUnits -> PDoc
ppExUnits (ExUnits mem step) =
  ppRecord "ExUnits" [("memory", ppWord64 mem), ("steps", ppWord64 step)]

instance PrettyA ExUnits where prettyA = ppExUnits

ppCostModel :: CostModel -> PDoc
ppCostModel (CostModelConstr (Memo m _)) =
  ppSexp "CostModel" [ppMap ppLong ppInteger m]

instance PrettyA CostModel where prettyA = ppCostModel

ppPrices :: Prices -> PDoc
ppPrices (Prices mem step) =
  ppRecord "Prices" [("prMem", ppCoin mem), ("prSteps", ppCoin step)]

instance PrettyA Prices where prettyA = ppPrices
