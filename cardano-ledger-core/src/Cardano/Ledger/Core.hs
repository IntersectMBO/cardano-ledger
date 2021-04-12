{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import {-# SOURCE #-} Cardano.Ledger.Tx (Tx)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import NoThunks.Class (NoThunks)

-- | A transaction output.
type family TxOut era :: Type

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | The body of a transaction.
type family TxBody era :: Type

-- | Scripts which may lock transaction outputs in this era
type family Script era :: Type

-- | AuxiliaryData which may be attached to a transaction
type family AuxiliaryData era :: Type

-- | Protocol parameters
type family PParams era :: Type

-- | The type of updates to Protocol parameters
type family PParamsDelta era :: Type

-- | The set of witnesses in a Tx
type family Witnesses era :: Type

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
