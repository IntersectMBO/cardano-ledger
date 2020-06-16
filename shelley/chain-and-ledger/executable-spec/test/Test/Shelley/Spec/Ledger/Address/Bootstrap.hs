{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapTest,
    genBootstrapAddress,
  )
where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import Cardano.Prelude (Proxy (..))
import Data.Maybe (fromJust)
import Data.String (fromString)
import Hedgehog ((===), Gen)
import qualified Hedgehog as H
import Shelley.Spec.Ledger.Address
  ( BootstrapAddress (..),
    bootstrapKeyHash,
  )
import Shelley.Spec.Ledger.Address.Bootstrap
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys (coerceKeyRole)
import qualified Test.Cardano.Chain.Common.Gen as Byron
import qualified Test.Cardano.Crypto.Gen as Byron
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Tasty (TestTree)
import qualified Test.Tasty.Hedgehog as T

-- Test that a BootstrapWitness properly reconstructs the AddrRoot of a
-- corresponding BootstrapAddress
bootstrapTest :: TestTree
bootstrapTest = T.testProperty "bootstrap addr root" $ H.property $ do
  (byronVKey, byronAddr) <- H.forAll genByronVKeyAddr
  let addr = BootstrapAddress byronAddr
      (shelleyVKey, chainCode) = unpackByronKey @C byronVKey
      witness =
        BootstrapWitness
          { bwKey = shelleyVKey,
            bwChainCode = chainCode,
            bwSig = dummySig,
            bwPadding = fromJust $ getPadding byronAddr
          }
  (coerceKeyRole $ bootstrapKeyHash addr) === bootstrapWitKeyHash witness

dummySig :: forall v a. DSIGN.DSIGNAlgorithm v => DSIGN.SignedDSIGN v a
dummySig =
  DSIGN.SignedDSIGN
    (fromJust $ DSIGN.rawDeserialiseSigDSIGN (fromString $ replicate size '0'))
  where
    size = fromIntegral $ DSIGN.sizeSigDSIGN (Proxy :: Proxy v)

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress . snd <$> genByronVKeyAddr

genByronVKeyAddr :: Gen (Byron.VerificationKey, Byron.Address)
genByronVKeyAddr = do
  vkey <- Byron.genVerificationKey
  addr <- Byron.makeAddress (Byron.VerKeyASD vkey) <$> Byron.genAddrAttributes
  pure (vkey, addr)

data C

instance Crypto C where
  type KES C = KES (ConcreteCrypto Hash.ShortHash)
  type VRF C = VRF (ConcreteCrypto Hash.ShortHash)
  type DSIGN C = DSIGN.Ed25519DSIGN
  type HASH C = HASH (ConcreteCrypto Hash.ShortHash)
  type ADDRHASH C = Hash.Blake2b_224
