{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines core type families which we know to vary from era to
-- era.
--
-- Families in this module should be indexed on era.
--
-- It is intended for qualified import:
-- > import qualified Cardano.Ledger.Core as Core
module Cardano.Ledger.Core
  ( -- * Era-changing types
    TxOut,
    TxBody,
    Value,
    Script,
    AuxiliaryData,
    PParams,
    PParamsDelta,
    Witnesses,

    -- * Re-exported fixed Tx
    Tx,

    -- * Constraint synonyms
    ChainData,
    SerialisableData,
    AnnotatedData,

    -- * Era STS
    EraRule,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import NoThunks.Class (NoThunks)

-- | A transaction.
type family Tx era = (r :: Type) | r -> era

-- | A transaction output.
type family TxOut era = (r :: Type) | r -> era

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | The body of a transaction.
type family TxBody era = (r :: Type) | r -> era

-- | Scripts which may lock transaction outputs in this era
type family Script era :: Type

-- | AuxiliaryData which may be attached to a transaction
type family AuxiliaryData era = (r :: Type) | r -> era

-- | Protocol parameters
type family PParams era = (r :: Type) | r -> era

-- | The type of updates to Protocol parameters
type family PParamsDelta era = (r :: Type) | r -> era

-- | The set of witnesses in a Tx
type family Witnesses era = (r :: Type) | r -> era

-- | Common constraints
--
-- NOTE: 'Ord' is not included, as 'Ord' for a 'Block' or a 'NewEpochState'
-- doesn't make sense.
type ChainData t = (Eq t, Show t, NoThunks t, Typeable t)

-- | Constraints for serialising from/to CBOR
type SerialisableData t = (FromCBOR t, ToCBOR t)

-- | Constraints for serialising from/to CBOR using 'Annotator'
type AnnotatedData t = (FromCBOR (Annotator t), ToCBOR t)

-- | Era STS map
type family EraRule (k :: Symbol) era :: Type
