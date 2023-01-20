{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Various identifiers in the ledger are hashes of particular structures.
-- While the structures may change from era to era, the hash will remain the
-- same, and we can refer to the hash of, say, a transaction, without knowing
-- the actual transaction type. As such, we define a number of these hashes
-- here.
module Cardano.Ledger.Hashes (
  -- * Era-independent hash type identifiers.
  -- $eraIndep
  EraIndependentTxBody,
  EraIndependentBlockHeader,
  EraIndependentBlockBody,
  EraIndependentMetadata,
  EraIndependentScript,
  EraIndependentData,
  EraIndependentScriptData,
  EraIndependentTxAuxData,
  EraIndependentPParamView,
  EraIndependentScriptIntegrity,

  -- * Script hashes
  ScriptHash (..),
  DataHash,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.Binary.Plain (DecCBOR, EncCBOR)
import Cardano.Ledger.Crypto (ADDRHASH, Crypto)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

instance ToExpr (ScriptHash c)

--   $eraIndep
--
--   Hashes carry around a phantom type parameter to identify the sort of thing
--   they are hashing. This is useful to allow us to distinguish, say, a place
--   where we expect the hash for a block from the hash for a script. However,
--   the exact structure that makes up a "block" will differ from era to era. We
--   still want to share the same namespace for the identifiers. Consequently we
--   define some era-independent indices here.

data EraIndependentTxBody

data EraIndependentBlockHeader

data EraIndependentBlockBody

data EraIndependentMetadata

data EraIndependentTxAuxData

data EraIndependentScript

data EraIndependentData

type DataHash c = SafeHash c EraIndependentData

data EraIndependentScriptData

data EraIndependentPParamView

data EraIndependentScriptIntegrity

newtype ScriptHash c
  = ScriptHash (Hash.Hash (ADDRHASH c) EraIndependentScript)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance Crypto c => EncCBOR (ScriptHash c)

deriving newtype instance Crypto c => DecCBOR (ScriptHash c)

deriving newtype instance Crypto c => ToCBOR (ScriptHash c)

deriving newtype instance Crypto c => FromCBOR (ScriptHash c)

deriving newtype instance Crypto c => ToJSON (ScriptHash c)

deriving newtype instance Crypto c => FromJSON (ScriptHash c)
