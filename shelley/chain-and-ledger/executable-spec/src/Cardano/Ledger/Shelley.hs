{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Hashing (HashAnnotated)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data ShelleyEra c

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance VALUE (ShelleyEra c) = Coin

type TxBodyConstraints era =
  ( NoThunks (TxBody era),
    Eq (TxBody era),
    Show (TxBody era),
    FromCBOR (Annotator (TxBody era)),
    ToCBOR (TxBody era),
    HashAnnotated (TxBody era) era
  )

-- this is a type class rather than a constraint bundle to avoid having
-- to add the `UndecidableInstances` PRAGMA in modules which make use of this constraint.
class
  ( Era era,
    -- Value constraints
    Val (VALUE era),
    Compactible (VALUE era),
    Eq (VALUE era),
    FromCBOR (CompactForm (VALUE era)),
    FromCBOR (VALUE era),
    NFData (VALUE era),
    NoThunks (VALUE era),
    Show (VALUE era),
    ToCBOR (CompactForm (VALUE era)),
    ToCBOR (VALUE era),
    Typeable (VALUE era),
    TxBodyConstraints era
  ) =>
  ShelleyBased era

deriving instance (CryptoClass.Crypto c, TxBodyConstraints (ShelleyEra c)) => ShelleyBased (ShelleyEra c)
