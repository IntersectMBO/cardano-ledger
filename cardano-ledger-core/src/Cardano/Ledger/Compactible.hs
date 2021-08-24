{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Compactible
  ( -- * Compactible
    Compactible (..),
  )
where

import Cardano.Binary (ToCBOR)
import Data.Kind (Type)

--------------------------------------------------------------------------------

-- * Compactible

--
-- Certain types may have a "presentation" form and a more compact
-- representation that allows for more efficient memory usage. In this case,
-- one should make instances of the 'Compactible' class for them.
--------------------------------------------------------------------------------
class
  ( Show (CompactForm a),
    Eq (CompactForm a),
    ToCBOR (CompactForm a)
  ) =>
  Compactible a
  where
  data CompactForm a :: Type
  toCompact :: a -> Maybe (CompactForm a)
  fromCompact :: CompactForm a -> a
