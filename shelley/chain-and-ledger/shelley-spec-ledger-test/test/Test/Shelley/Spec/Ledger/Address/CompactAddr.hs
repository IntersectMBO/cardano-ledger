{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Address.CompactAddr where

import Cardano.Ledger.Era (Era (..))
import qualified Data.ByteString.Short as SBS
import Shelley.Spec.Ledger.Address (Addr (..), serialiseAddr)
import qualified Shelley.Spec.Ledger.CompactAddr as CA
import Shelley.Spec.Ledger.Credential
  ( PaymentCredential,
    StakeReference (..),
  )
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()

propCompactAddrRoundTrip :: forall era. Era era => Addr era -> Bool
propCompactAddrRoundTrip addr =
  let compact = CA.compactAddr addr
      decompact = CA.decompactAddr compact
   in addr == decompact

propCompactSerializationAgree :: Addr era -> Bool
propCompactSerializationAgree addr =
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
   in sbs == SBS.toShort (serialiseAddr addr)

-- Test that we can tell whether this address is a bootstrap address
-- without decoding the rest of the address
propDecompactAddrLazy :: forall era. (Era era, Mock (Crypto era)) => Gen Bool
propDecompactAddrLazy = do
  addr <- arbitrary :: Gen (Addr era)
  let mangledAddr = CA.decompactAddr . mangle . CA.compactAddr $ addr
  case mangledAddr of
    Addr _ _ _ -> pure True
    AddrBootstrap _ -> pure True

-- This tests that reading the keyhash doesn't force the stake reference.
-- Testing that reading the stake reference doesn't force the keyhash in the
-- same way would involve constructing a binary representation of a keyhash with the
-- correct length that wasn't a valid hash, which doesn't seem possible.
propDecompactShelleyLazyAddr ::
  forall era.
  Era era =>
  Gen Bool
propDecompactShelleyLazyAddr = do
  stakeRef <-
    oneof
      [ StakeRefBase <$> arbitrary,
        StakeRefPtr <$> arbitrary
      ] ::
      Gen (StakeReference era)
  addr <- Addr <$> arbitrary <*> arbitrary <*> pure stakeRef
  let keyHash0 = unsafeGetHash addr
      keyHash1 = unsafeGetHash . CA.decompactAddr . mangle . CA.compactAddr $ addr
   in pure $ keyHash0 == keyHash1

unsafeGetHash :: Addr era -> PaymentCredential era
unsafeGetHash (Addr _ hash _) = hash
unsafeGetHash _ = error "this can't get a keyhash for a byron address"

mangle :: CA.CompactAddr era -> CA.CompactAddr era
mangle (CA.UnsafeCompactAddr bytes) =
  CA.UnsafeCompactAddr $
    CA.substring bytes 0 (SBS.length bytes - 1)
