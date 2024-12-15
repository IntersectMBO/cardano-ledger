{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Scripts (
  module Cardano.Ledger.Allegra.Scripts,
)
where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.Scripts (
  ShelleyEraScript (..),
  nativeMultiSigTag,
 )

-- | Since Timelock scripts are a strictly backwards compatible extension of
-- MultiSig scripts, we can use the same 'scriptPrefixTag' tag here as we did
-- for the ValidateScript instance in MultiSig
instance EraScript MaryEra where
  type Script MaryEra = Timelock MaryEra
  type NativeScript MaryEra = Timelock MaryEra

  upgradeScript = translateTimelock

  scriptPrefixTag _script = nativeMultiSigTag -- "\x00"

  getNativeScript = Just

  fromNativeScript = id

instance ShelleyEraScript MaryEra where
  mkRequireSignature = mkRequireSignatureTimelock
  getRequireSignature = getRequireSignatureTimelock

  mkRequireAllOf = mkRequireAllOfTimelock
  getRequireAllOf = getRequireAllOfTimelock

  mkRequireAnyOf = mkRequireAnyOfTimelock
  getRequireAnyOf = getRequireAnyOfTimelock

  mkRequireMOf = mkRequireMOfTimelock
  getRequireMOf = getRequireMOfTimelock

instance AllegraEraScript MaryEra where
  mkTimeStart = mkTimeStartTimelock
  getTimeStart = getTimeStartTimelock

  mkTimeExpire = mkTimeExpireTimelock
  getTimeExpire = getTimeExpireTimelock
