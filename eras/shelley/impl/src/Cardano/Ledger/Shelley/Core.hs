{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Shelley.Core (
  ShelleyEraTxBody (..),
  pattern MirTxCert,
  ShelleyEraTxCert (..),
  Withdrawals (..),
  Wdrl,
  module Cardano.Ledger.Core,
  pattern Wdrl,
  module Cardano.Ledger.Shelley.Governance,
)
where

import Cardano.Ledger.Address
import Cardano.Ledger.CertState (CertState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Slot (SlotNo (..))
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro (Lens', SimpleGetter)

{-# DEPRECATED updateTxBodyG "Use `updateTxBodyL` instead" #-}
class (ShelleyEraTxCert era, EraTxBody era) => ShelleyEraTxBody era where
  ttlTxBodyL :: ExactEra ShelleyEra era => Lens' (TxBody era) SlotNo

  updateTxBodyL :: ProtVerAtMost era 8 => Lens' (TxBody era) (StrictMaybe (Update era))

  updateTxBodyG :: SimpleGetter (TxBody era) (StrictMaybe (Update era))
  default updateTxBodyG :: ProtVerAtMost era 8 => SimpleGetter (TxBody era) (StrictMaybe (Update era))
  updateTxBodyG = updateTxBodyL

  -- | Compute the total deposits from the Certs of a TxBody
  --   This is the contribution of a TxBody towards the deposit pot (utxosDeposit field of the UTxOState) of the system
  getTotalDepositsTxBody :: PParams era -> CertState era -> TxBody era -> Coin

  -- | Compute the total refunds from the Certs of a TxBody.
  --   This is the contribution of a TxBody towards the total 'Obligations' of the system
  --   See the Types and Functions Cardano.Ledger.CertState(Obligations,obligationCertState) for more information.
  getTotalRefundsTxBody :: PParams era -> CertState era -> TxBody era -> Coin

type Wdrl c = Withdrawals c
{-# DEPRECATED Wdrl "In favor of `Cardano.Ledger.Address.Withdrawals`" #-}
pattern Wdrl :: Map (RewardAcnt c) Coin -> Withdrawals c
pattern Wdrl ws = Withdrawals ws
