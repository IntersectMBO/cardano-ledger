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
import Data.Fixed
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6)
import qualified Data.ListMap as LM
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Word (Word32, Word64, Word8)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Gen ()
import Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range
import Numeric.Natural
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
    <*> fmap LM.ListMap genFundsList
    <*> genStaking

genStaking :: CC.Crypto c => Gen (ShelleyGenesisStaking c)
genStaking =
  ShelleyGenesisStaking
    <$> fmap LM.ListMap genPools
    <*> fmap LM.ListMap genStake

genPools ::
  CC.Crypto c =>
  Gen
    [ ( KeyHash 'StakePool c,
        PoolParams c
      )
    ]
genPools =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genPoolParams

genStake ::
  CC.Crypto c =>
  Gen
    [ ( KeyHash 'Staking c,
        KeyHash 'StakePool c
      )
    ]
genStake =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genKeyHash

genPoolParams :: forall c. CC.Crypto c => Gen (PoolParams c)
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

genRewardAcnt :: CC.Crypto c => Gen (RewardAcnt c)
genRewardAcnt = RewardAcnt Testnet <$> genCredential

genCredential :: CC.Crypto c => Gen (Credential 'Staking c)
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

genPParams :: Gen (ShelleyPParams era)
genPParams =
  ShelleyPParams
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
    <$> genVersion (Range.linear (getVersion64 minBound) (getVersion64 maxBound))
    <*> genNatural (Range.linear 0 1000)
  where
    genVersion r = do
      v64 <- Gen.word64 r
      case mkVersion64 v64 of
        Nothing -> error $ "Invalid version generated: " ++ show v64
        Just v -> pure v

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
  CC.Crypto c =>
  Gen [(KeyHash 'Genesis c, GenDelegPair c)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair ::
  forall c.
  CC.Crypto c =>
  Gen (KeyHash 'Genesis c, GenDelegPair c)
genGenesisDelegationPair =
  (,) <$> genKeyHash <*> (GenDelegPair <$> genKeyHash <*> genVRFKeyHash @c)

genVRFKeyHash ::
  forall c.
  CC.Crypto c =>
  Gen (Hash c (VerKeyVRF (VRF c)))
genVRFKeyHash = hashVerKeyVRF . snd <$> (genVRFKeyPair @c)

genVRFKeyPair ::
  forall c.
  CC.Crypto c =>
  Gen (SignKeyVRF (VRF c), VerKeyVRF (VRF c))
genVRFKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyVRF seed
      vk = deriveVerKeyVRF sk
  pure (sk, vk)
  where
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF c)))

genFundsList :: CC.Crypto c => Gen [(Addr c, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair

genSeed :: Int -> Gen Seed
genSeed n = mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genKeyHash ::
  ( CC.Crypto c
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
    seedSize =
      fromIntegral
        ( seedSizeDSIGN
            ( Proxy ::
                Proxy (DSIGN c)
            )
        )

genGenesisFundPair :: CC.Crypto c => Gen (Addr c, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genAddress :: CC.Crypto c => Gen (Addr c)
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
