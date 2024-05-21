{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Scripts (
  module Cardano.Ledger.Allegra.Scripts,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript (..),
  nativeMultiSigTag,
 )

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

instance Crypto c => ShelleyEraScript (MaryEra c) where
  {-# SPECIALIZE instance ShelleyEraScript (MaryEra StandardCrypto) #-}

  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance Crypto c => AllegraEraScript (MaryEra c) where
  {-# SPECIALIZE instance AllegraEraScript (MaryEra StandardCrypto) #-}

  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock
