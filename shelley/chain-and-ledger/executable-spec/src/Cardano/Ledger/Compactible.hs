{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Compactible
  ( -- * Compactible
    Compactible (..),
    Compact (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Kind (Type)
import Data.Typeable (Typeable)

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
