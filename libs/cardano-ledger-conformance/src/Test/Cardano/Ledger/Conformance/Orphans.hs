{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans where

import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lib
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (FixupSpecRep (..), OpaqueErrorString)
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

instance FixupSpecRep Void

instance FixupSpecRep a => FixupSpecRep (NonEmpty a)

instance FixupSpecRep Text where
  fixup = id

instance FixupSpecRep OpaqueErrorString

instance FixupSpecRep a => FixupSpecRep [a]

instance FixupSpecRep Char where
  fixup = id

instance
  ( Eq v
  , Ord k
  , FixupSpecRep k
  , FixupSpecRep v
  ) =>
  FixupSpecRep (HSMap k v)
  where
  fixup (MkHSMap l) = MkHSMap . sortOn fst $ bimap fixup fixup <$> nub l

instance (Ord a, FixupSpecRep a) => FixupSpecRep (HSSet a) where
  fixup (MkHSSet l) = MkHSSet . Set.toList . Set.fromList $ fixup <$> l

instance (FixupSpecRep a, FixupSpecRep b) => FixupSpecRep (a, b)

instance FixupSpecRep a => FixupSpecRep (Maybe a)

instance (FixupSpecRep a, FixupSpecRep b) => FixupSpecRep (Either a b)

instance FixupSpecRep Bool

instance FixupSpecRep TxId where
  fixup = id

instance FixupSpecRep ()

instance FixupSpecRep BaseAddr

instance FixupSpecRep BootstrapAddr

instance FixupSpecRep Timelock

instance FixupSpecRep HSTimelock

instance FixupSpecRep HSPlutusScript

instance FixupSpecRep UTxOState

instance FixupSpecRep Credential

instance FixupSpecRep GovRole

instance FixupSpecRep VDeleg

instance FixupSpecRep DepositPurpose

instance FixupSpecRep DState

instance FixupSpecRep PoolParams

instance FixupSpecRep PState

instance FixupSpecRep GState

instance FixupSpecRep CertState

instance FixupSpecRep Vote

instance FixupSpecRep Lib.Rational where
  fixup = id

instance FixupSpecRep PParamsUpdate

instance FixupSpecRep RwdAddr

instance FixupSpecRep GovAction

instance FixupSpecRep GovActionState

instance FixupSpecRep StakeDistrs

instance FixupSpecRep PoolThresholds

instance FixupSpecRep DrepThresholds

instance FixupSpecRep PParams

instance FixupSpecRep EnactState

instance FixupSpecRep RatifyEnv

instance FixupSpecRep RatifyState

instance FixupSpecRep EpochState

instance FixupSpecRep Snapshots

instance FixupSpecRep Snapshot

instance FixupSpecRep Acnt

instance FixupSpecRep LState

instance FixupSpecRep HsRewardUpdate

instance FixupSpecRep NewEpochState
