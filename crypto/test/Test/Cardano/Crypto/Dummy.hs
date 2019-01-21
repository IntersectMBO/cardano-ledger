-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Cardano.Crypto.Dummy
  ( dummyProtocolMagic
  , dummyProtocolMagicId
  )
where

import Cardano.Crypto
  (ProtocolMagic(..), ProtocolMagicId(..), RequiresNetworkMagic(..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic dummyProtocolMagicId RequiresMagic

dummyProtocolMagicId :: ProtocolMagicId
dummyProtocolMagicId = ProtocolMagicId 55550001
