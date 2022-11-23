-- | Dummy values used in tests (replacing `configuration.yaml`)
module Test.Cardano.Crypto.Dummy
  ( annotatedProtocolMagicId,
    aProtocolMagic,
    protocolMagic,
    protocolMagicId,
  )
where

import Cardano.Crypto
  ( AProtocolMagic (..),
    ProtocolMagic,
    ProtocolMagicId (..),
    RequiresNetworkMagic (..),
  )
import Cardano.Ledger.Binary (Annotated (..), byronProtVer, serialize')
import Cardano.Prelude

aProtocolMagic :: AProtocolMagic ByteString
aProtocolMagic = AProtocolMagic annotatedProtocolMagicId RequiresMagic

protocolMagic :: ProtocolMagic
protocolMagic = AProtocolMagic (Annotated protocolMagicId ()) RequiresMagic

annotatedProtocolMagicId :: Annotated ProtocolMagicId ByteString
annotatedProtocolMagicId =
  Annotated protocolMagicId (serialize' byronProtVer protocolMagicId)

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 55550001
