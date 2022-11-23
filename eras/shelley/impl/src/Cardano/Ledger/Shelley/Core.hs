{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Shelley.Core
  ( ShelleyEraTxBody (..),
    Wdrl (..),
    module Cardano.Ledger.Core,
  )
where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)

class EraTxBody era => ShelleyEraTxBody era where
  wdrlsTxBodyL :: Lens' (TxBody era) (Wdrl (EraCrypto era))

  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: Lens' (TxBody era) (StrictMaybe (Update era))

  certsTxBodyL :: Lens' (TxBody era) (StrictSeq (DCert (EraCrypto era)))

newtype Wdrl c = Wdrl {unWdrl :: Map (RewardAcnt c) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData)

instance Crypto c => ToCBOR (Wdrl c) where
  toCBOR = toCBOR . unWdrl

instance Crypto c => FromCBOR (Wdrl c) where
  fromCBOR = Wdrl <$> fromCBOR
