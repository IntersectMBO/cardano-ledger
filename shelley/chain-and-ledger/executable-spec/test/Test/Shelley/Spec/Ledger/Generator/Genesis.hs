{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Generator.Genesis where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Crypto.VRF.Class
import Cardano.Prelude (Natural, Word32, Word64)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))
import Data.Fixed
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen ()
import Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes hiding (Seed)
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    Hash,
    IsKeyRole,
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.PParams
import Test.Cardano.Crypto.Gen (genProtocolMagicId)

genShelleyGenesis :: Crypto c => Gen (ShelleyGenesis c)
genShelleyGenesis =
  ShelleyGenesis
    <$> genUTCTime
    <*> genNetworkMagic
    <*> Gen.element [Mainnet, Testnet]
    <*> genProtocolMagicId
    <*> fmap realToFrac genSlotLength
    <*> Gen.word64 (Range.linear 1 1000000)
    <*> fmap EpochSize genSecurityParam
    <*> Gen.word64 (Range.linear 1 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> genSlotLength
    <*> Gen.word64 (Range.linear 1 100000)
    <*> Gen.word64 (Range.linear 1 100000)
    <*> genPParams
    <*> fmap Map.fromList genGenesisDelegationList
    <*> fmap Map.fromList genFundsList
    <*> pure emptyGenesisStaking

genPParams :: Gen PParams
genPParams =
  PParams
    <$> genNatural (Range.linear 0 1000)
    <*> genNatural (Range.linear 0 3)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> fmap fromIntegral (Gen.word $ Range.linear 100 1000000)
    <*> genCoin
    <*> genCoin
    <*> genEpochNo
    <*> genNatural (Range.linear 0 10)
    <*> genRational
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genNonce
    <*> genProtVer
    <*> genMinUTxOValue
    <*> (pure 0) -- TODO handle a min pool cost > 0

genNatural :: Range Natural -> Gen Natural
genNatural = Gen.integral

genRational :: Gen Rational
genRational = Gen.realFrac_ (Range.linearFrac 0 10000)

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 500)

genMinUTxOValue :: Gen Coin
genMinUTxOValue = Coin <$> Gen.integral (Range.linear 1 1000)

genNonce :: Gen Nonce
genNonce =
  Gen.choice
    [ mkNonce <$> genNatural (Range.linear 1 123),
      pure NeutralNonce
    ]

genProtVer :: Gen ProtVer
genProtVer =
  ProtVer
    <$> genNatural (Range.linear 0 1000)
    <*> genNatural (Range.linear 0 1000)

genUnitInterval :: Gen UnitInterval
genUnitInterval =
  truncateUnitInterval
    <$> Gen.realFrac_ (Range.linearFrac 0.01 1)

genGenesisDelegationList ::
  Crypto c =>
  Gen [(KeyHash 'Genesis c, GenDelegPair c)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair ::
  forall c.
  Crypto c =>
  Gen (KeyHash 'Genesis c, GenDelegPair c)
genGenesisDelegationPair =
  (,) <$> genKeyHash <*> (GenDelegPair <$> genKeyHash <*> genVRFKeyHash)
  where
    genVRFKeyHash :: Gen (Hash c (VerKeyVRF (VRF c)))
    genVRFKeyHash = hashVerKeyVRF . snd <$> (genVRFKeyPair @c)

genVRFKeyPair ::
  forall c.
  Crypto c =>
  Gen (SignKeyVRF (VRF c), VerKeyVRF (VRF c))
genVRFKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyVRF seed
      vk = deriveVerKeyVRF sk
  pure (sk, vk)
  where
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF c)))

genFundsList :: Crypto c => Gen [(Addr c, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair

genSeed :: Int -> Gen Seed
genSeed n = mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genKeyHash ::
  ( IsKeyRole disc c
  ) =>
  Gen (KeyHash disc c)
genKeyHash = hashKey . snd <$> genKeyPair

-- | Generate a deterministic key pair given a seed.
genKeyPair ::
  forall c krole.
  DSIGNAlgorithm (DSIGN c) =>
  Gen
    ( SignKeyDSIGN (DSIGN c),
      VKey krole c
    )
genKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyDSIGN seed
      vk = deriveVerKeyDSIGN sk
  pure (sk, VKey vk)
  where
    seedSize :: Int
    seedSize = fromIntegral (seedSizeDSIGN (Proxy :: Proxy (DSIGN c)))

genGenesisFundPair :: Crypto c => Gen (Addr c, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genAddress :: Crypto c => Gen (Addr c)
genAddress = do
  (secKey1, verKey1) <- genKeyPair
  (secKey2, verKey2) <- genKeyPair
  nw <- Gen.element [Mainnet, Testnet]
  let keyPair1 = KeyPair {sKey = secKey1, vKey = verKey1}
      keyPair2 = KeyPair {sKey = secKey2, vKey = verKey2}
  pure $ toAddr nw (keyPair1, keyPair2)

genNetworkMagic :: Gen Word32
genNetworkMagic = Gen.enumBounded

genCoin :: Gen Coin
genCoin =
  Coin <$> Gen.integral (Range.linear 1 1000000000)

genSecurityParam :: Gen Word64
genSecurityParam = Gen.word64 (Range.linear 1 20000)

genSlotLength :: Gen NominalDiffTime
genSlotLength = conv <$> Gen.integral (Range.linear 2000 60000)
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Integer -> NominalDiffTime
    conv =
      (realToFrac :: Pico -> NominalDiffTime)
        . (/ 1000)
        . (fromInteger :: Integer -> Pico)

genUTCTime :: Gen UTCTime
genUTCTime =
  posixSecondsToUTCTime . realToFrac <$> Gen.int (Range.linear 1000000000 5000000000)
