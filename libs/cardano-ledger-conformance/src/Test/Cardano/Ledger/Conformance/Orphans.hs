{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans where

import GHC.Generics (Generic)
import Lib
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.Utils
import Test.Cardano.Ledger.Conway.TreeDiff (Expr (..), ToExpr (..))

deriving instance Generic GovActionState

deriving instance Generic Vote

deriving instance Generic GovProposal

deriving instance Generic GovAction

deriving instance Generic GovVote

deriving instance Generic GovSignal

deriving instance Generic GovEnv

deriving instance Generic EnactState

deriving instance Eq AgdaEmpty

deriving instance Eq TxBody

deriving instance Eq Tag

deriving instance Ord Tag

deriving instance Ord Credential

deriving instance Eq TxWitnesses

deriving instance Eq Tx

deriving instance Eq PParams

deriving instance Eq UTxOState

deriving instance Eq GovAction

deriving instance Eq GovVote

deriving instance Eq GovSignal

deriving instance Eq GovProposal

deriving instance Eq Vote

deriving instance Eq GovActionState

deriving instance Eq GovEnv

deriving instance Eq EnactState

deriving instance Eq UTxOEnv

instance NFData GovAction

instance NFData TxId

instance NFData UTxOState

instance NFData Vote

instance NFData Credential

instance NFData GovRole

instance NFData GovActionState

instance NFData AgdaEmpty

instance NFData GovVote

instance NFData GovProposal

instance NFData GovSignal

instance NFData PParams

instance NFData EnactState

instance NFData GovEnv

instance NFData VDeleg

instance NFData TxCert

instance NFData TxBody

instance NFData Tag

instance NFData TxWitnesses

instance NFData Tx

instance NFData UTxOEnv

instance ToExpr Credential where
  toExpr (KeyHashObj h) = App "KeyHashObj" [agdaHashToExpr h]
  toExpr (ScriptObj h) = App "ScriptObj" [agdaHashToExpr h]

instance ToExpr GovAction

instance ToExpr GovRole

instance ToExpr Vote

instance ToExpr TxId where
  toExpr (MkTxId x) = App "TxId" [agdaHashToExpr x]

instance ToExpr GovActionState

instance ToExpr GovProposal

instance ToExpr GovVote

instance ToExpr GovSignal

instance ToExpr PParams

instance ToExpr GovEnv

instance ToExpr EnactState

instance ToExpr VDeleg

instance ToExpr TxCert

instance ToExpr TxBody

instance ToExpr AgdaEmpty

instance ToExpr Tag

instance ToExpr TxWitnesses

instance ToExpr Tx

instance ToExpr UTxOState

instance ToExpr UTxOEnv
