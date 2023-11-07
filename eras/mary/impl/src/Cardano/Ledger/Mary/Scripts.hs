{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Scripts (
  module Cardano.Ledger.Allegra.Scripts,
)
where

import Cardano.Ledger.Allegra.Scripts (Timelock, translateTimelock)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance Crypto c => EraScript (MaryEra c) where
  type Script (MaryEra c) = Timelock (MaryEra c)
  type NativeScript (MaryEra c) = Timelock (MaryEra c)

  upgradeScript = translateTimelock

  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"

  getNativeScript = Just

  fromNativeScript = id
