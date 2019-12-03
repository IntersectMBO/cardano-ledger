-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Cardano.Crypto.Dummy
  ( protocolMagic
  , protocolMagicId
  )
where

import Cardano.Crypto
  ( ProtocolMagic(..)
  , ProtocolMagicId(..)
  , RequiresNetworkMagic(..)
  )

protocolMagic :: ProtocolMagic
protocolMagic = ProtocolMagic protocolMagicId RequiresMagic

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 55550001
