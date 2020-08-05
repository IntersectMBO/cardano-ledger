{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash hiding (Hash)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Cardano.Crypto.VRF.Class
import Cardano.Prelude (Natural, Word32, Word64, Word8)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))
import Data.Fixed
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
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
import Shelley.Spec.Ledger.Credential
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.TxData
import Test.Shelley.Spec.Ledger.Utils (mkHash)

genShelleyGenesis :: Crypto c => Gen (ShelleyGenesis c)
genShelleyGenesis =
  ShelleyGenesis
    <$> genUTCTime
    <*> genNetworkMagic
    <*> Gen.element [Mainnet, Testnet]
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
    <*> genStaking

genStaking :: Crypto c => Gen (ShelleyGenesisStaking c)
genStaking =
  ShelleyGenesisStaking
    <$> fmap Map.fromList genPools
    <*> fmap Map.fromList genStake

genPools ::
  Crypto c =>
  Gen
    [ ( KeyHash 'StakePool c,
        PoolParams c
      )
    ]
genPools =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genPoolParams

genStake ::
  Crypto c =>
  Gen
    [ ( KeyHash 'Staking c,
        KeyHash 'StakePool c
      )
    ]
genStake =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genKeyHash

genPoolParams :: forall c. Crypto c => Gen (PoolParams c)
genPoolParams =
  PoolParams
    <$> genKeyHash
    <*> genVRFKeyHash @c
    <*> genCoin
    <*> genCoin
    <*> genUnitInterval
    <*> genRewardAcnt
    <*> (Set.fromList <$> (Gen.list (Range.linear 1 10) $ genKeyHash))
    <*> (StrictSeq.fromList <$> (Gen.list (Range.linear 1 10) $ genStakePoolRelay))
    <*> genStrictMaybe genPoolMetaData

genStakePoolRelay :: Gen StakePoolRelay
genStakePoolRelay =
  Gen.choice
    [ SingleHostAddr <$> genStrictMaybe genPort <*> genStrictMaybe genIPv4 <*> genStrictMaybe genIPv6,
      SingleHostName <$> genStrictMaybe genPort <*> genDnsName,
      MultiHostName <$> genDnsName
    ]

genStrictMaybe :: Gen a -> Gen (StrictMaybe a)
genStrictMaybe gen = maybeToStrictMaybe <$> Gen.maybe gen

genDnsName :: Gen DnsName
genDnsName = do
  txt <- Gen.text (Range.linear 1 63) Gen.ascii
  case textToDns txt of
    Nothing -> error "wrong generator for DnsName"
    Just dns -> return dns

genPoolMetaData :: Gen PoolMetaData
genPoolMetaData =
  PoolMetaData
    <$> genUrl
    <*> Gen.bytes (Range.linear 1 30)

genPort :: Gen Port
genPort = Port <$> Gen.enumBounded

genUrl :: Gen Url
genUrl = do
  txt <- Gen.text (Range.linear 1 63) Gen.ascii
  case textToUrl txt of
    Nothing -> error "wrong generator for Url"
    Just url -> return url

genRewardAcnt :: Crypto c => Gen (RewardAcnt c)
genRewardAcnt = RewardAcnt Testnet <$> genCredential

genCredential :: forall c. Crypto c => Gen (Credential 'Staking c)
genCredential =
  Gen.choice
    [ ScriptHashObj . ScriptHash <$> genHash @c,
      KeyHashObj <$> genKeyHash
    ]

genHash :: forall c a. HashAlgorithm (HASH c) => Gen (Hash c a)
genHash = mkHash <$> Gen.int (Range.linear 0 1000)

genWords :: Natural -> Gen [Word8]
genWords n
  | n > 0 = (:) <$> Gen.word8 Range.constantBounded <*> genWords (n -1)
  | otherwise = pure []

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
    [ mkNonceFromNumber <$> Gen.word64 (Range.linear 1 123),
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
  (,) <$> genKeyHash <*> (GenDelegPair <$> genKeyHash <*> genVRFKeyHash @c)

genVRFKeyHash :: forall c. Crypto c => Gen (Hash c (VerKeyVRF (VRF c)))
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
  ( Crypto c
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

genIPv4 :: Gen IPv4
genIPv4 = fromHostAddress <$> (Gen.word32 Range.constantBounded)

genIPv6 :: Hedgehog.Gen IPv6
genIPv6 = do
  w1 <- Gen.word32 Range.constantBounded
  w2 <- Gen.word32 Range.constantBounded
  w3 <- Gen.word32 Range.constantBounded
  w4 <- Gen.word32 Range.constantBounded
  pure $ fromHostAddress6 (w1, w2, w3, w4)
