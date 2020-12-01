{-# LANGUAGE AllowAmbiguousTypes #-}
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
    AuxiliaryData,
    UpdateSTS,
    HasUpdateLogic
      ( initialUpdateState,
        registerProtocolParametersChange,
        votedValue
      ),

    -- * Constraint synonyms
    ChainData,
    SerialisableData,
    AnnotatedData,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Control.State.Transition (STS (State))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)
-- TODO: we need to make sure it is ok to postpone the abstraction of the era's
-- update parameters.
import Shelley.Spec.Ledger.PParams (PParams)

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | The body of a transaction.
type family TxBody era :: Type

-- | Scripts which may lock transaction outputs in this era
type family Script era :: Type

-- | AuxiliaryData which may be attached to a transaction
type family AuxiliaryData era :: Type

-- | Update transition system for the era.
type family UpdateSTS era :: Type

class HasUpdateLogic era where
  initialUpdateState :: State (UpdateSTS era)

  registerProtocolParametersChange ::
    State (UpdateSTS era) -> PParams era -> State (UpdateSTS era)

  votedValue ::
    State (UpdateSTS era) -> PParams era -> Int -> Maybe (PParams era)

-- | Common constraints
--
-- NOTE: 'Ord' is not included, as 'Ord' for a 'Block' or a 'NewEpochState'
-- doesn't make sense.
type ChainData t = (Eq t, Show t, NoThunks t, Typeable t)

-- | Constraints for serialising from/to CBOR
type SerialisableData t = (FromCBOR t, ToCBOR t)

-- | Constraints for serialising from/to CBOR using 'Annotator'
type AnnotatedData t = (FromCBOR (Annotator t), ToCBOR t)
