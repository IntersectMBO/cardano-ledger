{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val)
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
  ( ChainData (TxBody era),
    AnnotatedData (TxBody era),
    HashAnnotated (TxBody era) era
  )

type ShelleyBased era =
  ( Era era,
    -- Value constraints
    Val (Value era),
    Compactible (Value era),
    ChainData (Value era),
    SerialisableData (Value era),
    SerialisableData (CompactForm (Value era)),
    -- TxBody constraints
    TxBodyConstraints era,
    -- Script constraints
    ChainData (Script era),
    AnnotatedData (Script era)
  )
