{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis where

import Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes hiding (Seed)
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (DSIGN, VRF)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
  )
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.TxBody
import Cardano.Prelude (Natural, Word32, Word64, Word8)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))
import Data.Fixed
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen ()
import Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range
import Test.Cardano.Ledger.Shelley.Utils (mkHash, unsafeBoundRational)

genShelleyGenesis :: Era era => Gen (ShelleyGenesis era)
genShelleyGenesis =
  ShelleyGenesis
    <$> genUTCTime
    <*> genNetworkMagic
    <*> Gen.element [Mainnet, Testnet]
    <*> genPositiveUnitInterval
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

genStaking :: CC.Crypto crypto => Gen (ShelleyGenesisStaking crypto)
genStaking =
  ShelleyGenesisStaking
    <$> fmap Map.fromList genPools
    <*> fmap Map.fromList genStake

genPools ::
  CC.Crypto crypto =>
  Gen
    [ ( KeyHash 'StakePool crypto,
        PoolParams crypto
      )
    ]
genPools =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genPoolParams

genStake ::
  CC.Crypto crypto =>
  Gen
    [ ( KeyHash 'Staking crypto,
        KeyHash 'StakePool crypto
      )
    ]
genStake =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genKeyHash

genPoolParams :: forall crypto. CC.Crypto crypto => Gen (PoolParams crypto)
genPoolParams =
  PoolParams
    <$> genKeyHash
    <*> genVRFKeyHash @crypto
    <*> genCoin
    <*> genCoin
    <*> genUnitInterval
    <*> genRewardAcnt
    <*> (Set.fromList <$> (Gen.list (Range.linear 1 10) $ genKeyHash))
    <*> (StrictSeq.fromList <$> (Gen.list (Range.linear 1 10) $ genStakePoolRelay))
    <*> genStrictMaybe genPoolMetadata

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

genPoolMetadata :: Gen PoolMetadata
genPoolMetadata =
  PoolMetadata
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

genRewardAcnt :: CC.Crypto crypto => Gen (RewardAcnt crypto)
genRewardAcnt = RewardAcnt Testnet <$> genCredential

genCredential :: CC.Crypto crypto => Gen (Credential 'Staking crypto)
genCredential =
  Gen.choice
    [ ScriptHashObj . ScriptHash <$> genHash,
      KeyHashObj <$> genKeyHash
    ]

genHash :: Hash.HashAlgorithm v => Gen (Hash.Hash v a)
genHash = mkHash <$> Gen.int (Range.linear 0 1000)

genWords :: Natural -> Gen [Word8]
genWords n
  | n > 0 = (:) <$> Gen.word8 Range.constantBounded <*> genWords (n - 1)
  | otherwise = pure []

genPParams :: Gen (PParams era)
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
    <*> genNonNegativeInterval
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genUnitInterval
    <*> genNonce
    <*> genProtVer
    <*> genMinUTxOValue
    <*> (pure mempty) -- TODO handle a min pool cost > 0

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
genUnitInterval = genDecimalBoundedRational (Gen.word64 . Range.linear 0)

genPositiveUnitInterval :: Gen PositiveUnitInterval
genPositiveUnitInterval = genDecimalBoundedRational (Gen.word64 . Range.linear 1)

genNonNegativeInterval :: Gen NonNegativeInterval
genNonNegativeInterval =
  genDecimalBoundedRational (Gen.word64 . Range.linear 0 . const (10 ^ (19 :: Int)))

-- | Only numbers in Scientific format can roundtrip JSON, so we generate numbers
-- that can be represented in both decimal form and some bounded type rational form.
genDecimalBoundedRational :: (Integral a, BoundedRational r) => (a -> Gen a) -> Gen r
genDecimalBoundedRational gen = do
  let maxExp = 19
  denom <- (10 ^) <$> Gen.int (Range.linear 0 maxExp)
  num <- gen denom
  pure $ unsafeBoundRational $ toInteger num % toInteger denom

genGenesisDelegationList ::
  CC.Crypto crypto =>
  Gen [(KeyHash 'Genesis crypto, GenDelegPair crypto)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair ::
  forall crypto.
  CC.Crypto crypto =>
  Gen (KeyHash 'Genesis crypto, GenDelegPair crypto)
genGenesisDelegationPair =
  (,) <$> genKeyHash <*> (GenDelegPair <$> genKeyHash <*> genVRFKeyHash @crypto)

genVRFKeyHash ::
  forall crypto.
  CC.Crypto crypto =>
  Gen (Hash crypto (VerKeyVRF (VRF crypto)))
genVRFKeyHash = hashVerKeyVRF . snd <$> (genVRFKeyPair @crypto)

genVRFKeyPair ::
  forall crypto.
  CC.Crypto crypto =>
  Gen (SignKeyVRF (VRF crypto), VerKeyVRF (VRF crypto))
genVRFKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyVRF seed
      vk = deriveVerKeyVRF sk
  pure (sk, vk)
  where
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF crypto)))

genFundsList :: CC.Crypto crypto => Gen [(Addr crypto, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair

genSeed :: Int -> Gen Seed
genSeed n = mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genKeyHash ::
  ( CC.Crypto crypto
  ) =>
  Gen (KeyHash disc crypto)
genKeyHash = hashKey . snd <$> genKeyPair

-- | Generate a deterministic key pair given a seed.
genKeyPair ::
  forall crypto krole.
  DSIGNAlgorithm (DSIGN crypto) =>
  Gen
    ( SignKeyDSIGN (DSIGN crypto),
      VKey krole crypto
    )
genKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyDSIGN seed
      vk = deriveVerKeyDSIGN sk
  pure (sk, VKey vk)
  where
    seedSize :: Int
    seedSize =
      fromIntegral
        ( seedSizeDSIGN
            ( Proxy ::
                Proxy (DSIGN crypto)
            )
        )

genGenesisFundPair :: CC.Crypto crypto => Gen (Addr crypto, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genAddress :: CC.Crypto crypto => Gen (Addr crypto)
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
