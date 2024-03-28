{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Cardano.Ledger.Conformance.Orphans () where

import Lib
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import GHC.Generics (Generic)

deriving instance Generic GovActionState

deriving instance Generic Vote

deriving instance Generic GovAction

deriving instance Eq AgdaEmpty

deriving instance Eq TxBody

deriving instance Eq Tag

deriving instance Ord Tag

deriving instance Eq TxWitnesses

deriving instance Eq Tx

deriving instance Eq PParams

deriving instance Eq UTxOState

deriving instance Eq GovAction

deriving instance Eq Vote

deriving instance Eq GovActionState

instance NFData GovAction

instance NFData UTxOState

instance NFData Vote

instance NFData Credential

instance NFData GovRole

instance NFData GovActionState

instance ToExpr GovAction

instance ToExpr Vote

instance ToExpr GovActionState
