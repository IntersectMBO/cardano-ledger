{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( -- * Compactible
    Compactible (..),
    Compact (..),
    TxBody,
    Value,

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

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | The body of a transaction.
type family TxBody era :: Type

--------------------------------------------------------------------------------

-- * Compactible

--
-- Certain types may have a "presentation" form and a more compact
-- representation that allows for more efficient memory usage. In this case,
-- one should make instances of the 'Compactible' class for them.
--------------------------------------------------------------------------------

class Compactible a where
  data CompactForm a :: Type
  toCompact :: a -> CompactForm a
  fromCompact :: CompactForm a -> a

newtype Compact a = Compact {unCompact :: a}

instance
  (Typeable a, Compactible a, ToCBOR (CompactForm a)) =>
  ToCBOR (Compact a)
  where
  toCBOR = toCBOR . toCompact . unCompact

instance
  (Typeable a, Compactible a, FromCBOR (CompactForm a)) =>
  FromCBOR (Compact a)
  where
  fromCBOR = Compact . fromCompact <$> fromCBOR

-- TODO: consider if this is better the other way around
instance (Eq a, Compactible a) => Eq (CompactForm a) where
  a == b = fromCompact a == fromCompact b

-------------------------------------------------------------------------------

-- * Constraint synonyms

-------------------------------------------------------------------------------

-- | Common constraints
--
-- NOTE: 'Ord' is not included, as 'Ord' for a 'Block' or a 'NewEpochState'
-- doesn't make sense.
type ChainData t = (Eq t, Show t, NoThunks t, Typeable t)

-- | Constraints for serialising from/to CBOR
type SerialisableData t = (FromCBOR t, ToCBOR t)

-- | Constraints for serialising from/to CBOR using 'Annotator'
type AnnotatedData t = (FromCBOR (Annotator t), ToCBOR t)
