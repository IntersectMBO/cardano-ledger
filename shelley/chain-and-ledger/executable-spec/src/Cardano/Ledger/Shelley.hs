{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Core (Compactible (..), Value)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val)
import Control.DeepSeq (NFData)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.Coin (Coin)

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data ShelleyEra c

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance Value (ShelleyEra c) = Coin

type ShelleyBased era =
  ( Era era,
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
    Typeable (Value era)
  )
