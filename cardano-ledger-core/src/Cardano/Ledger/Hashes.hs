{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various identifiers in the ledger are hashes of particular structures.
-- While the structures may change from era to era, the hash will remain the
-- same, and we can refer to the hash of, say, a transaction, without knowing
-- the actual transaction type. As such, we define a number of these hashes
-- here.
module Cardano.Ledger.Hashes
  ( -- * Era-independent hash type identifiers.
    -- $eraIndep
    EraIndependentTxBody,
    EraIndependentBlockBody,
    EraIndependentMetadata,
    EraIndependentScript,
    EraIndependentData,
    EraIndependentScriptData,
    EraIndependentAuxiliaryData,
    EraIndependentPParamView,
    EraIndependentScriptIntegrity,
    -- $scriptHash
    ScriptHash (..),

    -- * deprecated
    EraIndependentWitnessPPData,
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Control.DeepSeq (NFData)
import Data.Aeson
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

data EraIndependentBlockBody

data EraIndependentMetadata

data EraIndependentAuxiliaryData

data EraIndependentScript

data EraIndependentData

data EraIndependentScriptData

data EraIndependentPParamView

data EraIndependentScriptIntegrity

{-# DEPRECATED EraIndependentWitnessPPData "Use EraIndependentScriptIntegrity instead" #-}

type EraIndependentWitnessPPData = EraIndependentScriptIntegrity

-- $scriptHash

newtype ScriptHash crypto
  = ScriptHash (Hash.Hash (ADDRHASH crypto) EraIndependentScript)
  deriving (Generic)

type HashConstraint crypto = Hash.HashAlgorithm (ADDRHASH crypto)

deriving instance HashConstraint crypto => Show (ScriptHash crypto)
deriving instance HashConstraint crypto => Eq (ScriptHash crypto)
deriving instance HashConstraint crypto => Ord (ScriptHash crypto)

deriving newtype instance HashConstraint crypto => NFData (ScriptHash crypto)
deriving newtype instance HashConstraint crypto => NoThunks (ScriptHash crypto)

deriving newtype instance
  CC.Crypto crypto =>
  ToCBOR (ScriptHash crypto)

deriving newtype instance
  CC.Crypto crypto =>
  FromCBOR (ScriptHash crypto)

deriving newtype instance CC.Crypto crypto => ToJSON (ScriptHash crypto)

deriving newtype instance CC.Crypto crypto => FromJSON (ScriptHash crypto)
