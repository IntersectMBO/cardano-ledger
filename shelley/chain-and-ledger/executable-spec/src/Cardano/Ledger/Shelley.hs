{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
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

type instance Value (ShelleyEra c) = Coin

type TxBodyConstraints era =
  ( NoThunks (TxBody era),
    Eq (TxBody era),
    Show (TxBody era),
    FromCBOR (Annotator (TxBody era)),
    ToCBOR (TxBody era),
    HashAnnotated (TxBody era) era
  )

type ShelleyBased era =
  ( Era era,
    -- Value constraints
    Val (Value era),
    Compactible (Value era),
    Eq (Value era),
    FromCBOR (CompactForm (Value era)),
    FromCBOR (Value era),
    NFData (Value era),
    NoThunks (Value era),
    Show (Value era),
    ToCBOR (CompactForm (Value era)),
    ToCBOR (Value era),
    Typeable (Value era),
    TxBodyConstraints era
  )
