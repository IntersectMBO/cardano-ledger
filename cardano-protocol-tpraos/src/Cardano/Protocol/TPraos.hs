{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Protocol.TPraos where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (..),
    encodeListLen,
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Cardano.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeRecordNamed,
  )
import Control.DeepSeq (NFData)
import Control.Iterate.SetAlgebra
  ( BaseRep (MapR),
    Embed (..),
    Exp (Base),
    HasExp (toExp),
  )
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Relation (Relation (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

data ProtVer = ProtVer {pvMajor :: !Natural, pvMinor :: !Natural}
  deriving (Show, Eq, Generic, Ord, NFData)
  deriving (ToCBOR) via (CBORGroup ProtVer)
  deriving (FromCBOR) via (CBORGroup ProtVer)

instance NoThunks ProtVer

instance ToJSON ProtVer where
  toJSON (ProtVer major minor) =
    Aeson.object
      [ "major" .= major,
        "minor" .= minor
      ]

instance FromJSON ProtVer where
  parseJSON =
    Aeson.withObject "ProtVer" $ \obj ->
      ProtVer
        <$> obj .: "major"
        <*> obj .: "minor"

instance ToCBORGroup ProtVer where
  toCBORGroup (ProtVer x y) = toCBOR x <> toCBOR y
  encodedGroupSizeExpr l proxy =
    encodedSizeExpr l ((\(ProtVer x _) -> toWord x) <$> proxy)
      + encodedSizeExpr l ((\(ProtVer _ y) -> toWord y) <$> proxy)
    where
      toWord :: Natural -> Word
      toWord = fromIntegral

  listLen _ = 2
  listLenBound _ = 2

instance FromCBORGroup ProtVer where
  fromCBORGroup = ProtVer <$> fromCBOR <*> fromCBOR

data IndividualPoolStake crypto = IndividualPoolStake
  { individualPoolStake :: !Rational,
    individualPoolStakeVrf :: !(Hash crypto (VerKeyVRF crypto))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

instance CC.Crypto crypto => ToCBOR (IndividualPoolStake crypto) where
  toCBOR (IndividualPoolStake stake vrf) =
    mconcat
      [ encodeListLen 2,
        toCBOR stake,
        toCBOR vrf
      ]

instance CC.Crypto crypto => FromCBOR (IndividualPoolStake crypto) where
  fromCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> fromCBOR
        <*> fromCBOR

newtype PoolDistr crypto = PoolDistr
  { unPoolDistr ::
      Map (KeyHash 'StakePool crypto) (IndividualPoolStake crypto)
  }
  deriving stock (Show, Eq)
  deriving newtype (ToCBOR, FromCBOR, NFData, NoThunks, Relation)

instance
  HasExp
    (PoolDistr crypto)
    ( Map
        (KeyHash 'StakePool crypto)
        (IndividualPoolStake crypto)
    )
  where
  toExp (PoolDistr x) = Base MapR x

instance
  Embed
    (PoolDistr crypto)
    ( Map
        (KeyHash 'StakePool crypto)
        (IndividualPoolStake crypto)
    )
  where
  toBase (PoolDistr x) = x
  fromBase = PoolDistr
