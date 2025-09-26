{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Binary.Twiddle (
  module Test.Cardano.Ledger.Allegra.Binary.Twiddle,
) where

import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Core
import Test.Cardano.Ledger.Allegra.Binary.Twiddle

instance Twiddle (CompactForm MaryValue) where
  twiddle = twiddleTerm

instance Twiddle (PParams MaryEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate MaryEra) where
  twiddle = twiddleTerm

instance Twiddle MaryValue where
  twiddle = twiddleTerm

instance Twiddle (Tx MaryEra) where
  twiddle = twiddleTerm

instance Twiddle (TxBody MaryEra) where
  twiddle = twiddleTerm
