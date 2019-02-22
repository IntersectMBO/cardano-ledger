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

-- TODO: if we import this qualified we don't need to prepend the 'dummy' name.
dummyProtocolMagicId :: ProtocolMagicId
dummyProtocolMagicId = ProtocolMagicId 55550001
