-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Cardano.Crypto.Dummy
  ( dummyProtocolMagic
  )
where

import Cardano.Crypto (ProtocolMagic(..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic 55550001
