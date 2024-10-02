{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.TreeDiff (
  module Test.Cardano.Ledger.Binary.TreeDiff,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.HKD
import Cardano.Ledger.Keys
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash
import Cardano.Ledger.TxIn
import Cardano.Ledger.UMap
import Cardano.Ledger.UTxO
import Data.Functor.Identity
import Data.TreeDiff.OMap
import GHC.TypeLits
import Test.Cardano.Ledger.Binary.TreeDiff
import Test.Data.VMap.TreeDiff ()

-- Coin
instance ToExpr Coin

instance ToExpr DeltaCoin

instance ToExpr (CompactForm Coin) where
  toExpr x = toExpr (fromCompact x)

deriving newtype instance ToExpr (CompactForm DeltaCoin)

-- HKD
instance ToExpr (NoUpdate a)

-- Keys
instance Crypto c => ToExpr (VKey r c) where
  toExpr vk =
    Rec "VKey" $ fromList [("VKey (hashOf)", toExpr $ hashKey vk)]

instance ToExpr (GenDelegs c)

instance ToExpr (GenDelegPair c)

instance ToExpr (KeyHash keyrole c) where
  toExpr (KeyHash x) = App "KeyHash" [toExpr x]

-- PoolDist
instance ToExpr (PoolDistr c)

instance ToExpr (IndividualPoolStake c)

-- SafeHash
instance ToExpr (SafeHash c index) where
  toExpr x = App "SafeHash" [toExpr (extractHash x)]

-- Language
instance ToExpr (Plutus l)

instance ToExpr PlutusBinary

instance ToExpr Language

-- MemoBytes
instance ToExpr (t era) => ToExpr (MemoBytes t era)

-- Core

instance ToExpr (VoidEraRule (rule :: Symbol) era) where
  toExpr = absurdEraRule

instance ToExpr (ScriptHash c)

instance ToExpr CostModel where
  toExpr costModel =
    App
      "CostModel"
      [ toExpr (getCostModelLanguage costModel)
      , paramsExpr (getCostModelParams costModel)
      ]
    where
      paramsExpr [] = Lst []
      paramsExpr xs = App "concat" [Lst . map Lst . chunksOf 15 . map toExpr $ xs]
      chunksOf _ [] = []
      chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

instance ToExpr CostModels

-- Keys/WitVKey
instance Crypto c => ToExpr (WitVKey kr c)

-- Keys/Bootstrap
instance Crypto c => ToExpr (BootstrapWitness c)

instance ToExpr ChainCode

-- TxIn
instance ToExpr TxIx

-- BaseTypes
instance ToExpr CertIx where
  toExpr (CertIx x) = App "CertIx" [toExpr x]

instance ToExpr UnitInterval where
  toExpr = toExpr . unboundRational

instance ToExpr NonNegativeInterval where
  toExpr = toExpr . unboundRational

instance ToExpr Network

instance ToExpr Port

instance ToExpr Url

instance ToExpr Nonce where
  toExpr NeutralNonce = App "NeutralNonce" []
  toExpr (Nonce x) = App "Nonce" [trimExprViaShow 10 x]

instance ToExpr DnsName

instance ToExpr (BlocksMade c)

instance ToExpr ProtVer

instance ToExpr (Anchor c)

instance ToExpr a => ToExpr (Mismatch r a)

instance ToExpr EpochInterval

-- AuxiliaryData
instance ToExpr (AuxiliaryDataHash c)

-- Plutus/ExUnits
instance ToExpr Prices

instance ToExpr ExUnits where
  toExpr (WrapExUnits (ExUnits' x y)) = App "ExUnits" [defaultExprViaShow x, defaultExprViaShow y]

-- Credential
instance ToExpr (Credential keyrole c)

instance ToExpr (StakeReference c)

instance ToExpr Ptr

deriving newtype instance
  ToExpr (PParamsHKD Identity era) => ToExpr (PParams era)

deriving newtype instance
  ToExpr (PParamsHKD StrictMaybe era) => ToExpr (PParamsUpdate era)

instance ToExpr (TxIn c)

instance ToExpr (TxId c)

-- CertState
instance ToExpr (DRep c)

instance ToExpr (DRepState c)

-- Address
instance ToExpr (Addr c)

instance ToExpr (RewardAccount era)

instance ToExpr (BootstrapAddress c) where
  toExpr = defaultExprViaShow

instance ToExpr (Withdrawals c)

instance ToExpr (CompactAddr c)

-- PoolParams
instance ToExpr PoolMetadata

instance ToExpr (PoolParams era)

instance ToExpr StakePoolRelay

instance ToExpr (PoolCert c)

-- UMap
instance ToExpr RDPair

instance ToExpr (UMElem c)

instance ToExpr (UMap c)

instance ToExpr (PlutusData era) where
  toExpr = trimExprViaShow 30

instance ToExpr (Data era)

instance ToExpr (BinaryData era) where
  toExpr _ = App "BinaryData" []

instance ToExpr (Datum era) where
  toExpr NoDatum = App "NoDatum" []
  toExpr (DatumHash x) = App "DatumHash" [toExpr x]
  toExpr (Datum bd) = App "Datum" [toExpr bd]

-- EpochBoundary
instance ToExpr (SnapShots c)

instance ToExpr (SnapShot c)

deriving newtype instance ToExpr (Stake c)

instance ToExpr (CertState era)

instance ToExpr (PState era)

instance ToExpr (DState era)

instance ToExpr (VState era)

instance ToExpr (FutureGenDeleg c)

instance ToExpr (InstantaneousRewards c)

instance ToExpr (CommitteeAuthorization c)

instance ToExpr (CommitteeState era)

-- UTxO
deriving instance (Era era, ToExpr (Script era)) => ToExpr (ScriptsProvided era)

instance ToExpr (TxOut era) => ToExpr (UTxO era)

instance ToExpr (TxOutSource era)
