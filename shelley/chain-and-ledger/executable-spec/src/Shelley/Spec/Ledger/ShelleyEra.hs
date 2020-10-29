{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.ShelleyEra where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (..))
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Scripts (MultiSig)
import Shelley.Spec.Ledger.Tx
  ( ValidateScript (..),
    hashMultiSigScript,
    validateNativeMultiSigScript,
  )
import Shelley.Spec.Ledger.TxBody (TxBody)

data ShelleyEra c

instance CryptoClass.Crypto c => Era (ShelleyEra c) where
  type Crypto (ShelleyEra c) = c

type instance Core.Value (ShelleyEra _) = Coin

type instance Core.Script (ShelleyEra c) = MultiSig (ShelleyEra c)

type instance Core.TxBody (ShelleyEra c) = TxBody (ShelleyEra c)

-- | instance of MultiSignatureScript type class
instance CryptoClass.Crypto c => ValidateScript (ShelleyEra c) where
  validateScript = validateNativeMultiSigScript
  hashScript = hashMultiSigScript
