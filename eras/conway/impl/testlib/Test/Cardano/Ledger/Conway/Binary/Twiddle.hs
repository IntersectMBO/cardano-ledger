{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.Twiddle (
  module Test.Cardano.Ledger.Babbage.Binary.Twiddle,
) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Core
import Cardano.Ledger.Val (Val)
import Test.Cardano.Ledger.Babbage.Binary.Twiddle

instance Twiddle (PParams ConwayEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate ConwayEra) where
  twiddle = twiddleTerm

instance (Era era, Val (Value era)) => Twiddle (ConwayTxCert era) where
  twiddle = twiddleTerm

instance Twiddle (TxBody ConwayEra) where
  twiddle = twiddleTerm

instance Twiddle (Tx ConwayEra) where
  twiddle = twiddleTerm
