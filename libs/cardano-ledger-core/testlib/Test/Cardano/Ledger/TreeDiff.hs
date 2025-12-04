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
import Cardano.Ledger.BHeaderView
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.HKD
import Cardano.Ledger.Keys
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.State
import Cardano.Ledger.TxIn
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
instance ToExpr (VKey r) where
  toExpr vk =
    Rec "VKey" $ fromList [("VKey (hashOf)", toExpr $ hashKey vk)]

instance ToExpr GenDelegs

instance ToExpr GenDelegPair

instance ToExpr (KeyHash keyrole) where
  toExpr (KeyHash x) = App "KeyHash" [toExpr x]

instance ToExpr (VRFVerKeyHash keyrole) where
  toExpr (VRFVerKeyHash x) = App "VRFVerKeyHash" [toExpr x]

-- PoolDist
instance ToExpr PoolDistr

instance ToExpr IndividualPoolStake

-- SafeHash
instance ToExpr (SafeHash i) where
  toExpr x = App "SafeHash" [toExpr (extractHash x)]

-- Language
instance ToExpr (Plutus l)

instance ToExpr PlutusBinary

instance ToExpr Language

-- MemoBytes
instance ToExpr t => ToExpr (MemoBytes t)

-- Core

instance ToExpr (VoidEraRule (rule :: Symbol) era) where
  toExpr = absurdEraRule

instance ToExpr ScriptHash

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
instance ToExpr (WitVKey kr)

-- Keys/Bootstrap
instance ToExpr BootstrapWitness

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

instance ToExpr BlocksMade

instance ToExpr ProtVer

instance ToExpr Anchor

instance ToExpr a => ToExpr (Mismatch r a)

instance ToExpr EpochInterval

-- AuxiliaryData
instance ToExpr TxAuxDataHash

-- Plutus/ExUnits
instance ToExpr Prices

instance ToExpr ExUnits where
  toExpr (WrapExUnits (ExUnits' x y)) = App "ExUnits" [defaultExprViaShow x, defaultExprViaShow y]

-- Credential
instance ToExpr (Credential keyrole)

instance ToExpr StakeReference

deriving newtype instance ToExpr SlotNo32

instance ToExpr Ptr

deriving newtype instance
  ToExpr (PParamsHKD Identity era) => ToExpr (PParams era)

deriving newtype instance
  ToExpr (PParamsHKD StrictMaybe era) => ToExpr (PParamsUpdate era)

deriving newtype instance ToExpr CoinPerByte

instance ToExpr TxIn

instance ToExpr TxId

instance ToExpr ChainAccountState

-- CertState
instance ToExpr DRep

instance ToExpr DRepState

-- Address
instance ToExpr Addr

instance ToExpr RewardAccount

instance ToExpr BootstrapAddress where
  toExpr = defaultExprViaShow

instance ToExpr Withdrawals

instance ToExpr CompactAddr

-- PoolParams
instance ToExpr PoolMetadata

instance ToExpr StakePoolParams

instance ToExpr StakePoolState

instance ToExpr StakePoolRelay

instance ToExpr PoolCert

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
instance ToExpr SnapShots

instance ToExpr SnapShot

deriving newtype instance ToExpr Stake

instance ToExpr (PState era)

instance ToExpr (Accounts era) => ToExpr (DState era)

instance ToExpr FutureGenDeleg

instance ToExpr InstantaneousRewards

instance ToExpr CommitteeAuthorization

instance ToExpr (CommitteeState era)

-- UTxO
deriving instance (Era era, ToExpr (Script era)) => ToExpr (ScriptsProvided era)

instance ToExpr (TxOut era) => ToExpr (UTxO era)

instance ToExpr TxOutSource

instance ToExpr a => ToExpr (NonZero a) where
  toExpr x = App "NonZero" [toExpr $ unNonZero x]

instance ToExpr PositiveInterval where
  toExpr = toExpr . unboundRational

instance ToExpr BHeaderView

instance (ToExpr h, ToExpr (BlockBody era)) => ToExpr (Block h era)
