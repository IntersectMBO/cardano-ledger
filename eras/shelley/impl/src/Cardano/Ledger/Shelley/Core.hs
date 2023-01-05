{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Ledger.Shelley.Core (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  Wdrl,
  module Cardano.Ledger.Core,
  pattern Wdrl,
)
where

import Cardano.Ledger.Address
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Slot (SlotNo (..))
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro (Lens', SimpleGetter)

class EraTxBody era => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictMaybe (Update era))

  certsTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictSeq (DCert (EraCrypto era)))

  certsTxBodyG :: SimpleGetter (TxBody era) (StrictSeq (DCert (EraCrypto era)))
  default certsTxBodyG :: ProtVerAtMost era 8 => SimpleGetter (TxBody era) (StrictSeq (DCert (EraCrypto era)))
  certsTxBodyG = certsTxBodyL

type Wdrl c = Withdrawals c
{-# DEPRECATED Wdrl "In favor of `Cardano.Ledger.Address.Withdrawals`" #-}
pattern Wdrl :: Map (RewardAcnt c) Coin -> Withdrawals c
pattern Wdrl ws = Withdrawals ws
