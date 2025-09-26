{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Binary.Twiddle (
  module Test.Cardano.Ledger.Core.Binary.Twiddle,
) where

import Cardano.Ledger.Shelley (ShelleyEra, ShelleyTxAuxData, ShelleyTxOut)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert)
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits)
import Cardano.Ledger.Val (Val)
import Test.Cardano.Ledger.Core.Binary.Twiddle

instance (Era era, Val (Value era)) => Twiddle (ShelleyTxOut era) where
  twiddle = twiddleTerm

instance Era era => Twiddle (ShelleyTxCert era) where
  twiddle = twiddleTerm

instance Twiddle (PParams ShelleyEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate ShelleyEra) where
  twiddle = twiddleTerm

instance Era era => Twiddle (ShelleyTxWits era) where
  twiddle = twiddleTerm

instance Twiddle (MultiSig era)

instance Era era => Twiddle (ShelleyTxAuxData era) where
  twiddle = twiddleTerm

instance Twiddle (Tx ShelleyEra) where
  twiddle = twiddleTerm

instance Twiddle (TxBody ShelleyEra) where
  twiddle = twiddleTerm
