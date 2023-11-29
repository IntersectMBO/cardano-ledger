{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Compactible (
  -- * Compactible
  Compactible (..),
)
where

import Cardano.Ledger.Binary.Encoding (EncCBOR)
import Data.Kind (Type)
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
