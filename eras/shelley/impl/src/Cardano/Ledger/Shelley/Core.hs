{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Ledger.Shelley.Core (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  Wdrl,
  module Cardano.Ledger.Core,
  pattern Wdrl,
  module Cardano.Ledger.Shelley.Governance,
)
where

import Cardano.Ledger.Address
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Delegation.Certificates ( DelegCert )
import Cardano.Ledger.Slot (SlotNo (..))
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro (Lens', SimpleGetter)

class EraTxBody era => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictMaybe (Update era))

  updateTxBodyG :: SimpleGetter (TxBody era) (StrictMaybe (Update era))
  default updateTxBodyG :: ProtVerAtMost era 8 => SimpleGetter (TxBody era) (StrictMaybe (Update era))
  updateTxBodyG = updateTxBodyL

  getDCertDeleg :: DCert era -> Maybe (DelegCert (EraCrypto era))


pattern DCertDeleg :: ShelleyEraTxBody era => DelegCert (EraCrypto era) -> DCert era
pattern DCertDeleg d <- (getDCertDeleg -> Just d)

type Wdrl c = Withdrawals c
{-# DEPRECATED Wdrl "In favor of `Cardano.Ledger.Address.Withdrawals`" #-}
pattern Wdrl :: Map (RewardAcnt c) Coin -> Withdrawals c
pattern Wdrl ws = Withdrawals ws
