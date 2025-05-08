{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Compactible (
  -- * Compactible
  Compactible (..),
  partialCompactFL,
  toCompactPartial,
) where

import Cardano.Ledger.Binary.Encoding (EncCBOR)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------

-- * Compactible

--
-- Certain types may have a "presentation" form and a more compact
-- representation that allows for more efficient memory usage. In this case,
-- one should make instances of the 'Compactible' class for them.
--------------------------------------------------------------------------------
class
  ( Show (CompactForm a)
  , Eq (CompactForm a)
  , EncCBOR (CompactForm a)
  , NoThunks (CompactForm a)
  ) =>
  Compactible a
  where
  data CompactForm a :: Type
  toCompact :: a -> Maybe (CompactForm a)
  fromCompact :: CompactForm a -> a

partialCompactFL :: (Functor f, Compactible c, HasCallStack) => Lens' (f (CompactForm c)) (f c)
partialCompactFL = lens (fmap fromCompact) $ \_ -> fmap toCompactPartial

toCompactPartial :: (HasCallStack, Compactible a) => a -> CompactForm a
toCompactPartial = fromMaybe err . toCompact
  where
    err = error "Failed to compact the value"
