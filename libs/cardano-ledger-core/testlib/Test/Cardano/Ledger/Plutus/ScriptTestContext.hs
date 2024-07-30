{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Cardano.Ledger.Plutus.ScriptTestContext (
  PlutusArgs (..),
  ScriptTestContext (..),
)
where

import Cardano.Ledger.Plutus.Language (Plutus, PlutusLanguage)
import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (Data)

data PlutusArgs = PlutusArgs
  { paData :: Data
  , paSpendDatum :: Maybe Data
  }
  deriving (Generic, Show)

instance NFData PlutusArgs

data ScriptTestContext
  = forall l.
  PlutusLanguage l =>
  ScriptTestContext
  { stcScript :: Plutus l
  , stcArgs :: PlutusArgs
  }

deriving instance Show ScriptTestContext

instance NFData ScriptTestContext where
  rnf (ScriptTestContext script args) = rnf script `seq` rnf args
