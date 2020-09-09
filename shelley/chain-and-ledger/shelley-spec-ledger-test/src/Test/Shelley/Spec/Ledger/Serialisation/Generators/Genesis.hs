{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis where

import Cardano.Crypto.DSIGN.Class
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.Crypto (DSIGN, VRF)
import Cardano.Ledger.Era
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
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
  )
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.TxBody
import Test.Shelley.Spec.Ledger.Utils (mkHash)

genShelleyGenesis :: Era era => Gen (ShelleyGenesis era)
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

genStaking :: Era era => Gen (ShelleyGenesisStaking era)
genStaking =
  ShelleyGenesisStaking
    <$> fmap Map.fromList genPools
    <*> fmap Map.fromList genStake

genPools ::
  Era era =>
  Gen
    [ ( KeyHash 'StakePool era,
        PoolParams era
      )
    ]
genPools =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genPoolParams

genStake ::
  Era era =>
  Gen
    [ ( KeyHash 'Staking era,
        KeyHash 'StakePool era
      )
    ]
genStake =
  Gen.list (Range.linear 1 10) $
    (,) <$> genKeyHash <*> genKeyHash

genPoolParams :: forall era. Era era => Gen (PoolParams era)
genPoolParams =
  PoolParams
    <$> genKeyHash
    <*> genVRFKeyHash @era
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

genRewardAcnt :: Era era => Gen (RewardAcnt era)
genRewardAcnt = RewardAcnt Testnet <$> genCredential

genCredential :: Era era => Gen (Credential 'Staking era)
genCredential =
  Gen.choice
    [ ScriptHashObj . ScriptHash <$> genHash,
      KeyHashObj <$> genKeyHash
    ]

genHash :: Hash.HashAlgorithm v => Gen (Hash.Hash v a)
genHash = mkHash <$> Gen.int (Range.linear 0 1000)

genWords :: Natural -> Gen [Word8]
genWords n
  | n > 0 = (:) <$> Gen.word8 Range.constantBounded <*> genWords (n -1)
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
    <*> genRational
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
genUnitInterval =
  truncateUnitInterval
    <$> Gen.realFrac_ (Range.linearFrac 0.01 1)

genGenesisDelegationList ::
  Era era =>
  Gen [(KeyHash 'Genesis era, GenDelegPair era)]
genGenesisDelegationList = Gen.list (Range.linear 1 10) genGenesisDelegationPair

genGenesisDelegationPair ::
  forall era.
  Era era =>
  Gen (KeyHash 'Genesis era, GenDelegPair era)
genGenesisDelegationPair =
  (,) <$> genKeyHash <*> (GenDelegPair <$> genKeyHash <*> genVRFKeyHash @era)

genVRFKeyHash ::
  forall era.
  Era era =>
  Gen (Hash era (VerKeyVRF (VRF (Crypto era))))
genVRFKeyHash = hashVerKeyVRF . snd <$> (genVRFKeyPair @era)

genVRFKeyPair ::
  forall era.
  Era era =>
  Gen (SignKeyVRF (VRF (Crypto era)), VerKeyVRF (VRF (Crypto era)))
genVRFKeyPair = do
  seed <- genSeed seedSize
  let sk = genKeyVRF seed
      vk = deriveVerKeyVRF sk
  pure (sk, vk)
  where
    seedSize = fromIntegral (seedSizeVRF (Proxy :: Proxy (VRF (Crypto era))))

genFundsList :: Era era => Gen [(Addr era, Coin)]
genFundsList = Gen.list (Range.linear 1 100) genGenesisFundPair

genSeed :: Int -> Gen Seed
genSeed n = mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genKeyHash ::
  ( Era era
  ) =>
  Gen (KeyHash disc era)
genKeyHash = hashKey . snd <$> genKeyPair

-- | Generate a deterministic key pair given a seed.
genKeyPair ::
  forall era krole.
  DSIGNAlgorithm (DSIGN (Crypto era)) =>
  Gen
    ( SignKeyDSIGN (DSIGN (Crypto era)),
      VKey krole era
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
                Proxy (DSIGN (Crypto era))
            )
        )

genGenesisFundPair :: Era era => Gen (Addr era, Coin)
genGenesisFundPair =
  (,) <$> genAddress <*> genCoin

genAddress :: Era era => Gen (Addr era)
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
