{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Address.Bootstrap (
  aliceByronAddr,
)
where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Byron
import Data.ByteString (ByteString)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()

aliceSigningKey :: Byron.SigningKey
aliceSigningKey = Byron.SigningKey $ Byron.generate seed (mempty :: ByteString)
  where
    seed :: ByteString -- 32 bytes
    seed = "12345678901234567890123456789012"

aliceByronAddr :: Byron.Address
aliceByronAddr = Byron.makeAddress asd attrs
  where
    asd = Byron.VerKeyASD byronVerificationKey
    attrs =
      Byron.AddrAttributes
        (Just (Byron.HDAddressPayload "a compressed lenna.png"))
        (Byron.NetworkTestnet 0)
    byronVerificationKey = Byron.toVerification aliceSigningKey
