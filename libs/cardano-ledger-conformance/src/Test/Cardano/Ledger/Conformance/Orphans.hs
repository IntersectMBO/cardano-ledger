{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans () where

import Lib

deriving instance Eq AgdaEmpty

deriving instance Eq TxBody

deriving instance Eq TxWitnesses

deriving instance Eq Tx

deriving instance Eq PParams

deriving instance Eq UTxOState
