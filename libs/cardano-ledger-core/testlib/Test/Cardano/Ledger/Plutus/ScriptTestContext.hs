{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

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
  deriving (Generic)

instance NFData PlutusArgs

data ScriptTestContext = forall l.
  PlutusLanguage l =>
  ScriptTestContext
  { stcScript :: Plutus l
  , stcArgs :: PlutusArgs
  }

instance NFData ScriptTestContext where
  rnf (ScriptTestContext script args) = rnf script `seq` rnf args
