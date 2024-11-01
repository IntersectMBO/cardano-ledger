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
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Binary.Plain (FromCBOR, ToCBOR)
import Cardano.Ledger.Crypto (ADDRHASH, Crypto)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

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

type DataHash = SafeHash EraIndependentData

data EraIndependentScriptData

data EraIndependentPParamView

data EraIndependentScriptIntegrity

newtype ScriptHash
  = ScriptHash (Hash.Hash ADDRHASH EraIndependentScript)
  deriving (Show, Eq, Ord, Generic)
  deriving newtype
    ( NFData
    , NoThunks
    , ToCBOR
    , FromCBOR
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )
