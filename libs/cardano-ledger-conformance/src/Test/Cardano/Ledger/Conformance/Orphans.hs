{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans where

import Cardano.Ledger.Hashes (standardAddrHashSize)
import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (OpaqueErrorString, SpecNormalize (..))
import Test.Cardano.Ledger.Conformance.Utils
import Test.Cardano.Ledger.Conway.TreeDiff (Expr (..), ToExpr (..))

deriving instance Generic HsRewardUpdate

deriving instance Ord DepositPurpose

deriving instance Ord Tag

deriving instance Ord Credential

deriving instance Ord GovRole

deriving instance Ord VDeleg

deriving instance Ord Vote

deriving instance Ord PoolThresholds

deriving instance Ord DrepThresholds

deriving instance Ord PParamsUpdate

deriving instance Ord RwdAddr

deriving instance Ord GovAction

deriving instance Ord GovActionState

instance (NFData k, NFData v) => NFData (HSMap k v)

instance NFData a => NFData (HSSet a)

instance NFData PParamsUpdate

instance NFData RwdAddr

instance NFData GovAction

instance NFData BaseAddr

instance NFData BootstrapAddr

instance NFData Timelock

instance NFData HSTimelock

instance NFData HSPlutusScript

instance NFData UTxOState

instance NFData Vote

instance NFData Credential

instance NFData GovRole

instance NFData GovActionState

instance NFData Anchor

instance NFData GovVote

instance NFData GovProposal

instance NFData DrepThresholds

instance NFData PoolThresholds

instance NFData PParams

instance NFData EnactState

instance NFData GovEnv

instance NFData VDeleg

instance NFData PoolParams

instance NFData DCert

instance NFData TxBody

instance NFData Tag

instance NFData HSVKey

instance NFData TxWitnesses

instance NFData Tx

instance NFData UTxOEnv

instance NFData DepositPurpose

instance NFData CertEnv

instance NFData PState

instance NFData DState

instance NFData GState

instance NFData CertState

instance NFData StakeDistrs

instance NFData RatifyEnv

instance NFData RatifyState

instance NFData EnactEnv

instance NFData DelegEnv

instance NFData EpochState

instance NFData Snapshots

instance NFData Snapshot

instance NFData Acnt

instance NFData LState

instance NFData HsRewardUpdate

instance NFData NewEpochState

instance NFData LEnv

instance ToExpr a => ToExpr (HSSet a)

instance ToExpr Credential where
  toExpr (KeyHashObj h) =
    App
      "KeyHashObj"
      [ agdaHashToExpr standardAddrHashSize h
      , toExpr h
      ]
  toExpr (ScriptObj h) =
    App
      "ScriptObj"
      [ agdaHashToExpr standardAddrHashSize h
      , toExpr h
      ]

instance (ToExpr k, ToExpr v) => ToExpr (HSMap k v)

instance ToExpr PParamsUpdate

instance ToExpr RwdAddr

instance ToExpr GovAction

instance ToExpr GovRole

instance ToExpr Vote

instance ToExpr GovActionState

instance ToExpr Anchor

instance ToExpr GovProposal

instance ToExpr GovVote

instance ToExpr PoolThresholds

instance ToExpr DrepThresholds

instance ToExpr PParams

instance ToExpr GovEnv

instance ToExpr EnactState

instance ToExpr VDeleg

instance ToExpr PoolParams

instance ToExpr DCert

instance ToExpr BaseAddr

instance ToExpr BootstrapAddr

instance ToExpr Timelock

instance ToExpr HSTimelock

instance ToExpr HSPlutusScript

instance ToExpr TxBody

instance ToExpr Tag

instance ToExpr HSVKey

instance ToExpr TxWitnesses

instance ToExpr Tx

instance ToExpr UTxOState

instance ToExpr UTxOEnv

instance ToExpr DepositPurpose

instance ToExpr CertEnv

instance ToExpr DState

instance ToExpr PState

instance ToExpr GState

instance ToExpr CertState

instance ToExpr StakeDistrs

instance ToExpr RatifyEnv

instance ToExpr RatifyState

instance ToExpr EnactEnv

instance ToExpr DelegEnv

instance ToExpr EpochState

instance ToExpr Snapshots

instance ToExpr Snapshot

instance ToExpr LState

instance ToExpr Acnt

instance ToExpr HsRewardUpdate

instance ToExpr NewEpochState

instance ToExpr LEnv

instance Default (HSMap k v)

instance SpecNormalize Void

instance SpecNormalize a => SpecNormalize (NonEmpty a)

instance SpecNormalize Text where
  specNormalize = id

instance SpecNormalize OpaqueErrorString

instance SpecNormalize a => SpecNormalize [a]

instance SpecNormalize Char where
  specNormalize = id

instance
  ( Eq v
  , Ord k
  , SpecNormalize k
  , SpecNormalize v
  ) =>
  SpecNormalize (HSMap k v)
  where
  specNormalize (MkHSMap l) = MkHSMap . sortOn fst $ bimap specNormalize specNormalize <$> nub l

instance (Ord a, SpecNormalize a) => SpecNormalize (HSSet a) where
  specNormalize (MkHSSet l) = MkHSSet . Set.toList . Set.fromList $ specNormalize <$> l

instance (SpecNormalize a, SpecNormalize b) => SpecNormalize (a, b)

instance SpecNormalize a => SpecNormalize (Maybe a)

instance (SpecNormalize a, SpecNormalize b) => SpecNormalize (Either a b)

instance SpecNormalize Bool

instance SpecNormalize TxId where
  specNormalize = id

instance SpecNormalize ()

instance SpecNormalize BaseAddr

instance SpecNormalize BootstrapAddr

instance SpecNormalize Timelock

instance SpecNormalize HSTimelock

instance SpecNormalize HSPlutusScript

instance SpecNormalize UTxOState

instance SpecNormalize Credential

instance SpecNormalize GovRole

instance SpecNormalize VDeleg

instance SpecNormalize DepositPurpose

instance SpecNormalize DState

instance SpecNormalize PoolParams

instance SpecNormalize PState

instance SpecNormalize GState

instance SpecNormalize CertState

instance SpecNormalize Vote

instance SpecNormalize Agda.Rational where
  specNormalize = id

instance SpecNormalize PParamsUpdate

instance SpecNormalize RwdAddr

instance SpecNormalize GovAction

instance SpecNormalize GovActionState

instance SpecNormalize StakeDistrs

instance SpecNormalize PoolThresholds

instance SpecNormalize DrepThresholds

instance SpecNormalize PParams

instance SpecNormalize EnactState

instance SpecNormalize RatifyEnv

instance SpecNormalize RatifyState

instance SpecNormalize EpochState

instance SpecNormalize Snapshots

instance SpecNormalize Snapshot

instance SpecNormalize Acnt

instance SpecNormalize LState

instance SpecNormalize HsRewardUpdate

instance SpecNormalize NewEpochState

deriving instance Semigroup (HSMap k v)

deriving instance Monoid (HSMap k v)
