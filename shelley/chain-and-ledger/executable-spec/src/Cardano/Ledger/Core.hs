{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    TxBody,
    Value,
    Script,

    -- * Era-changing STS's
    UpdateSTS,
    -- * Constraint synonyms
    ChainData,
    SerialisableData,
    AnnotatedData,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)

-- TODO: import qualified.
import Control.State.Transition

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | The body of a transaction.
type family TxBody era :: Type

-- | Scripts which may lock transaction outputs in this era
type family Script era :: Type

-- | Update transition system for the era.
type family UpdateSTS era :: Type

-- | Common constraints
--
-- NOTE: 'Ord' is not included, as 'Ord' for a 'Block' or a 'NewEpochState'
-- doesn't make sense.
type ChainData t = (Eq t, Show t, NoThunks t, Typeable t)

-- | Constraints for serialising from/to CBOR
type SerialisableData t = (FromCBOR t, ToCBOR t)

-- | Constraints for serialising from/to CBOR using 'Annotator'
type AnnotatedData t = (FromCBOR (Annotator t), ToCBOR t)
