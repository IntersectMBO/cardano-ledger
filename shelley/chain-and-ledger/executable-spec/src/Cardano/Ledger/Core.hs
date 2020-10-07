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
  ( TxBody,
    Value (..),
    VALUE,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (NFData)
import Data.Group (Abelian, Group)
import Data.Kind (Type)
import Data.PartialOrd (PartialOrd (..))
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)

-- | A value is something which quantifies a transaction output.
type family VALUE era :: Type

-- | The body of a transaction.
type family TxBody era :: Type

-- Wrap the type family as a newtype because :
-- the genericShrink has something that
-- detects that the immediate subterms of a type are the same as the parent type
-- when there is a type family in that position, the instance resolution fails
newtype Value era = Value {unVl :: VALUE era}

deriving instance (Typeable (VALUE era)) => Typeable (Value era)

deriving instance (ToCBOR (VALUE era), Typeable era) => ToCBOR (Value era)

deriving instance (FromCBOR (VALUE era), Typeable era) => FromCBOR (Value era)

deriving instance (Eq (VALUE era)) => Eq (Value era)

deriving instance (Show (VALUE era)) => Show (Value era)

deriving instance (NoThunks (VALUE era)) => NoThunks (Value era)

deriving instance (NFData (VALUE era)) => NFData (Value era)

deriving instance (Val (VALUE era)) => Val (Value era)

deriving instance (Abelian (VALUE era)) => Abelian (Value era)

deriving instance (PartialOrd (VALUE era)) => PartialOrd (Value era)

deriving instance (Group (VALUE era)) => Group (Value era)

deriving instance (Monoid (VALUE era)) => Monoid (Value era)

deriving instance (Semigroup (VALUE era)) => Semigroup (Value era)

deriving instance (ToCBOR (CompactForm (VALUE era)), Typeable era) => ToCBOR (CompactForm (Value era))

deriving instance (FromCBOR (CompactForm (VALUE era)), Typeable era) => FromCBOR (CompactForm (Value era))

instance (Compactible (VALUE era)) => Compactible (Value era) where
  newtype CompactForm (Value era) = ValueC (CompactForm (VALUE era))
  toCompact (Value v) = ValueC (toCompact v)
  fromCompact (ValueC v) = Value (fromCompact v)
