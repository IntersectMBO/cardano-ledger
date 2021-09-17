{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Address.CompactAddr where

import Cardano.Ledger.Address (Addr (..), serialiseAddr)
import qualified Cardano.Ledger.Address as Addr
import Cardano.Ledger.Credential
  ( PaymentCredential,
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import qualified Cardano.Ledger.Shelley.CompactAddr as CA
import qualified Data.ByteString.Short as SBS
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck

propCompactAddrRoundTrip :: CC.Crypto crypto => Addr crypto -> Bool
propCompactAddrRoundTrip addr =
  let compact = CA.compactAddr addr
      decompact = CA.decompactAddr compact
   in addr == decompact

propCompactSerializationAgree :: Addr crypto -> Bool
propCompactSerializationAgree addr =
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
   in sbs == SBS.toShort (serialiseAddr addr)

-- Test that we can tell whether this address is a bootstrap address
-- without decoding the rest of the address
propDecompactAddrLazy :: forall crypto. Mock crypto => Gen Bool
propDecompactAddrLazy = do
  addr <- arbitrary :: Gen (Addr crypto)
  let mangledAddr = CA.decompactAddr . mangle . CA.compactAddr $ addr
  case mangledAddr of
    Addr _ _ _ -> pure True
    AddrBootstrap _ -> pure True

-- This tests that reading the keyhash doesn't force the stake reference.
-- Testing that reading the stake reference doesn't force the keyhash in the
-- same way would involve constructing a binary representation of a keyhash with the
-- correct length that wasn't a valid hash, which doesn't seem possible.
propDecompactShelleyLazyAddr ::
  forall crypto.
  CC.Crypto crypto =>
  Gen Bool
propDecompactShelleyLazyAddr = do
  stakeRef <-
    oneof
      [ StakeRefBase <$> arbitrary,
        StakeRefPtr <$> arbitrary
      ] ::
      Gen (StakeReference crypto)
  addr <- Addr <$> arbitrary <*> arbitrary <*> pure stakeRef
  let keyHash0 = unsafeGetHash addr
      keyHash1 = unsafeGetHash . CA.decompactAddr . mangle . CA.compactAddr $ addr
   in pure $ keyHash0 == keyHash1

-- | TODO This property test is failing to find a discrepancy that was found on mainnet.
propIsBootstrapRedeemer :: Addr crypto -> Property
propIsBootstrapRedeemer addr =
  Addr.isBootstrapRedeemer addr === CA.isBootstrapRedeemer (CA.compactAddr addr)

unsafeGetHash :: Addr crypto -> PaymentCredential crypto
unsafeGetHash (Addr _ hash _) = hash
unsafeGetHash _ = error "this can't get a keyhash for a byron address"

mangle :: CA.CompactAddr crypto -> CA.CompactAddr crypto
mangle (CA.UnsafeCompactAddr bytes) =
  CA.UnsafeCompactAddr $
    CA.substring bytes 0 (SBS.length bytes - 1)
