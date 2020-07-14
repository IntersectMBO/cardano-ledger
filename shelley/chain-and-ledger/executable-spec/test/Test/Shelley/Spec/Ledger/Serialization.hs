{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialization where

import Cardano.Binary
  ( Annotator,
    DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeFullDecoder,
    serialize,
    serialize',
    serializeEncoding,
    toCBOR,
  )
import Cardano.Crypto.DSIGN (encodeSignedDSIGN, encodeVerKeyDSIGN)
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Monomorphic
import Cardano.Prelude (LByteString)
import Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Coerce (coerce)
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6, toIPv4)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromJust)
import Data.Proxy
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog
  ( Gen,
    Property,
    forAll,
    property,
    tripping,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constantBounded)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    Nonce (..),
    StrictMaybe (..),
    mkNonceFromNumber,
    textToDns,
    textToUrl,
    truncateUnitInterval,
  )
import Shelley.Spec.Ledger.BlockChain
  ( Block (..),
    TxSeq (..),
    bbHash,
    bhash,
    bheaderBlockNo,
    bheaderEta,
    bheaderL,
    bheaderOCert,
    bheaderPrev,
    bheaderSlotNo,
    bheaderVk,
    bheaderVrfVk,
    bprotver,
    bsize,
    mkSeed,
    seedEta,
    seedL,
    pattern BHBody,
    pattern BHeader,
    pattern BlockHash,
    pattern HashHeader,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (pattern KeyHashObj, pattern ScriptHashObj, pattern StakeRefNull)
import Shelley.Spec.Ledger.Delegation.Certificates
  ( pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern PoolDistr,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    SnapShot (..),
    SnapShots (..),
    Stake (..),
  )
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyRole (..),
    asWitness,
    encodeSignedKES,
    hashKey,
    hashWithSerialiser,
    sKey,
    signedDSIGN,
    signedKES,
    vKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    EpochState (..),
    NewEpochState (..),
    deltaF,
    deltaR,
    deltaT,
    emptyLedgerState,
    nonMyopic,
    rs,
    pattern ActiveSlot,
    pattern RewardUpdate,
  )
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.OCert (KESPeriod (..), pattern OCert)
import Shelley.Spec.Ledger.PParams
  ( PParams' (..),
    PParamsUpdate,
    ProtVer (..),
    emptyPParams,
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import Shelley.Spec.Ledger.Scripts (pattern RequireSignature, pattern ScriptHash)
import Shelley.Spec.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    ipv4FromBytes,
    ipv4ToBytes,
    ipv6FromBytes,
    ipv6ToBytes,
  )
import Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Shelley.Spec.Ledger.Tx (Tx (..), WitnessSetHKD (..), hashScript)
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    PoolMetaData (..),
    StakePoolRelay (..),
    Wdrl (..),
    WitVKey (..),
    _poolCost,
    _poolMD,
    _poolMDHash,
    _poolMDUrl,
    _poolMargin,
    _poolOwners,
    _poolPledge,
    _poolPubKey,
    _poolRAcnt,
    _poolRelays,
    _poolVrf,
    _unTxId,
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertMir,
    pattern DCertPool,
    pattern Delegation,
    pattern PoolParams,
    pattern RewardAcnt,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessVKey)
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    BHBody,
    ConcreteCrypto,
    Credential,
    GenesisKeyPair,
    HashHeader,
    KeyHash,
    KeyPair,
    MultiSig,
    PoolDistr,
    RewardUpdate,
    ScriptHash,
    SignKeyKES,
    SignKeyVRF,
    SignedDSIGN,
    TxBody,
    TxId,
    TxIn,
    VKey,
    VRFKeyHash,
    VerKeyKES,
    VerKeyVRF,
    hashKeyVRF,
    pattern KeyHash,
    pattern KeyPair,
    pattern VKey,
  )
import Test.Shelley.Spec.Ledger.Generator.Core (genesisId)
import Test.Shelley.Spec.Ledger.SerializationProperties
  ( prop_roundtrip_Addr,
    prop_roundtrip_Block,
    prop_roundtrip_BlockHeaderHash,
    prop_roundtrip_BootstrapWitness,
    prop_roundtrip_Header,
    prop_roundtrip_LEDGER_PredicateFails,
    prop_roundtrip_LedgerState,
    prop_roundtrip_NewEpochState,
    prop_roundtrip_PrtclState,
    prop_roundtrip_RewardAcnt,
    prop_roundtrip_Tx,
    prop_roundtrip_TxId,
  )
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import qualified Test.Tasty.QuickCheck as QC (testProperty)

roundTrip ::
  (Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  a ->
  Assertion
roundTrip encode decode x =
  case (decode . serializeEncoding . encode) x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

checkEncoding ::
  (Show a, Eq a) =>
  (a -> Encoding) ->
  (LByteString -> Either DecoderError a) ->
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncoding encode decode name x t =
  testCase testName $
    assertEqual
      testName
      (Base16.encode $ serialize t)
      (Base16.encode . serializeEncoding . encode $ x)
      >> roundTrip encode decode x
  where
    testName = "prop_serialize_" <> name

checkEncodingCBOR ::
  (FromCBOR a, ToCBOR a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBOR name x t =
  let d = decodeFullDecoder (fromString name) fromCBOR
   in checkEncoding toCBOR d name x t

checkEncodingCBORAnnotated ::
  (FromCBOR (Annotator a), ToCBOR a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORAnnotated name x t =
  let d = decodeAnnotator (fromString name) fromCBOR
   in checkEncoding toCBOR d name x annTokens
  where
    annTokens = T $ TkEncoded $ serialize' t

checkEncodingCBORCBORGroup ::
  (FromCBORGroup a, ToCBORGroup a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORCBORGroup name x t =
  let d = decodeFullDecoder (fromString name) fromCBORGroup
   in checkEncoding toCBORGroup d name x t

getRawKeyHash :: KeyHash h 'Payment -> ByteString
getRawKeyHash (KeyHash hsh) = Monomorphic.hashToBytes hsh

getRawGenKeyHash :: KeyHash h 'Genesis -> ByteString
getRawGenKeyHash (KeyHash hsh) = Monomorphic.hashToBytes hsh

getRawScriptHash :: ScriptHash h -> ByteString
getRawScriptHash (ScriptHash hsh) = Monomorphic.hashToBytes hsh

getRawTxId :: TxId h -> ByteString
getRawTxId = Monomorphic.hashToBytes . _unTxId

getRawNonce :: Nonce -> ByteString
getRawNonce (Nonce hsh) = Monomorphic.hashToBytes hsh
getRawNonce NeutralNonce = error "The neutral nonce has no bytes"

testGKey :: GenesisKeyPair h
testGKey = KeyPair vk sk
  where
    (sk, vk) = mkGenKey (0, 0, 0, 0, 0)

testGKeyHash :: HashAlgorithm h => Proxy h -> KeyHash h 'Genesis
testGKeyHash _ = (hashKey . vKey) testGKey

testVRF :: (SignKeyVRF h, VerKeyVRF h)
testVRF = mkVRFKeyPair (0, 0, 0, 0, 5)

testVRFKH :: HashAlgorithm h => proxy h -> VRFKeyHash h
testVRFKH _ = hashKeyVRF $ snd testVRF

testTxb :: HashAlgorithm h => TxBody h
testTxb = TxBody Set.empty StrictSeq.empty StrictSeq.empty (Wdrl Map.empty) (Coin 0) (SlotNo 0) SNothing SNothing

testTxbHash :: HashAlgorithm h => Hash (ConcreteCrypto h) (TxBody h)
testTxbHash = hashTxBody testTxb

testKey1 :: proxy h -> KeyPair h 'Payment
testKey1 _ = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

testKey2 :: proxy h -> KeyPair h kr
testKey2 _ = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 2)

testKey3 :: proxy h -> KeyPair h kr
testKey3 _ = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 3)

testBlockIssuerKey :: KeyPair h 'BlockIssuer
testBlockIssuerKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 4)

testStakePoolKey :: proxy h -> KeyPair h 'StakePool
testStakePoolKey _ = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 5)

testGenesisDelegateKey :: proxy h -> KeyPair h 'GenesisDelegate
testGenesisDelegateKey _ = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 6)

testKey1Token :: forall proxy h. proxy h -> Tokens -> Tokens
testKey1Token p = e
  where
    (VKey vk) = vKey (testKey1 p) :: VKey h 'Payment
    Encoding e = encodeVerKeyDSIGN vk

testBlockIssuerKeyTokens :: Tokens -> Tokens
testBlockIssuerKeyTokens = e
  where
    (VKey vk) = vKey testBlockIssuerKey
    Encoding e = encodeVerKeyDSIGN vk

testKey1SigToken :: forall proxy h. HashAlgorithm h => proxy h -> Tokens -> Tokens
testKey1SigToken p = e
  where
    s =
      signedDSIGN @(ConcreteCrypto h) (sKey (testKey1 p)) testTxbHash ::
        SignedDSIGN h (Hash (ConcreteCrypto h) (TxBody h))
    Encoding e = encodeSignedDSIGN s

testOpCertSigTokens :: forall proxy h. HashAlgorithm h => proxy h -> Tokens -> Tokens
testOpCertSigTokens p = e
  where
    s = signedDSIGN @(ConcreteCrypto h) (sKey (testKey1 p)) (snd testKESKeys, 0 :: Natural, KESPeriod 0)
    Encoding e = encodeSignedDSIGN s

testKeyHash1 :: HashAlgorithm h => proxy h -> KeyHash h 'Payment
testKeyHash1 p = (hashKey . vKey) (testKey1 p)

testKeyHash2 :: HashAlgorithm h => proxy h -> KeyHash h 'Staking
testKeyHash2 p = (hashKey . vKey) (testKey2 p)

testKeyHash3 :: HashAlgorithm h => proxy h -> KeyHash h 'Payment
testKeyHash3 p = (hashKey . vKey) (testKey3 p)

testKESKeys :: (SignKeyKES h, VerKeyKES h)
testKESKeys = mkKESKeyPair (0, 0, 0, 0, 3)

testAddrE :: HashAlgorithm h => Proxy h -> Addr h
testAddrE p = Addr Testnet (KeyHashObj (testKeyHash1 p)) StakeRefNull

testPayCred :: HashAlgorithm h => proxy h -> Credential h 'Payment
testPayCred p = KeyHashObj (testKeyHash1 p)

testStakeCred :: HashAlgorithm h => proxy h -> Credential h 'Staking
testStakeCred p = KeyHashObj (testKeyHash2 p)

testScript :: HashAlgorithm h => proxy h -> MultiSig h
testScript p = RequireSignature $ asWitness (testKeyHash1 p)

testScriptHash :: HashAlgorithm h => proxy h -> ScriptHash h
testScriptHash p = hashScript (testScript p)

testScript2 :: HashAlgorithm h => proxy h -> MultiSig h
testScript2 p = RequireSignature $ asWitness (testKeyHash2 p)

testScriptHash2 :: forall h. HashAlgorithm h => ScriptHash h
testScriptHash2 = hashScript (testScript2 p)
  where
    p :: Proxy h
    p = Proxy

testHeaderHash :: forall proxy h. proxy h -> HashAlgorithm h => HashHeader h
testHeaderHash _ = HashHeader $ coerce (hashWithSerialiser toCBOR 0 :: Hash (ConcreteCrypto h) Int)

testBHB :: forall proxy h. HashAlgorithm h => proxy h -> BHBody h
testBHB p =
  BHBody
    { bheaderBlockNo = BlockNo 44,
      bheaderSlotNo = SlotNo 33,
      bheaderPrev = BlockHash (testHeaderHash p),
      bheaderVk = vKey testBlockIssuerKey,
      bheaderVrfVk = snd testVRF,
      bheaderEta =
        coerce $
          mkCertifiedVRF
            ( WithResult
                (mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0))
                1
            )
            (fst testVRF),
      bheaderL =
        coerce $
          mkCertifiedVRF
            ( WithResult
                (mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0))
                1
            )
            (fst testVRF),
      bsize = 0,
      bhash = bbHash $ TxSeq StrictSeq.empty,
      bheaderOCert =
        OCert
          (snd testKESKeys)
          0
          (KESPeriod 0)
          (signedDSIGN @(ConcreteCrypto h) (sKey (testKey1 p)) (snd testKESKeys, 0, KESPeriod 0)),
      bprotver = ProtVer 0 0
    }

testBHBSigTokens :: HashAlgorithm h => proxy h -> Tokens -> Tokens
testBHBSigTokens p = e
  where
    s = signedKES () 0 (testBHB p) (fst testKESKeys)
    Encoding e = encodeSignedKES s

data ToTokens where
  T :: (Tokens -> Tokens) -> ToTokens
  S :: ToCBOR a => a -> ToTokens
  G :: ToCBORGroup a => a -> ToTokens
  Plus :: ToTokens -> ToTokens -> ToTokens

instance ToCBOR ToTokens where
  toCBOR (T xs) = Encoding xs
  toCBOR (S s) = toCBOR s
  toCBOR (G g) = toCBORGroup g
  toCBOR (Plus a b) = toCBOR a <> toCBOR b

instance Semigroup ToTokens where
  (<>) = Plus

instance Monoid ToTokens where
  mempty = T id

testNegativeCoin :: Assertion
testNegativeCoin =
  let enc@(Encoding tokens) = toCBOR (Coin (-1))
   in (tokens TkEnd @?= (TkInteger (-1)) TkEnd)
        >> ( case (decodeFullDecoder "negative_coin" (fromCBOR @Coin) . serializeEncoding) enc of
               Left _ -> pure ()
               Right _ -> assertFailure "should not deserialize negative coins"
           )

roundTripIpv4 :: Property
roundTripIpv4 =
  -- We are using a QC generator which means we need QC test
  Hedgehog.property $ do
    ha <- forAll genIPv4
    Hedgehog.tripping ha ipv4ToBytes ipv4FromBytes

genIPv4 :: Gen IPv4
genIPv4 = fromHostAddress <$> (Gen.word32 constantBounded)

roundTripIpv6 :: Property
roundTripIpv6 =
  -- We are using a QC generator which means we need QC test
  Hedgehog.property $ do
    ha <- forAll genIPv6
    Hedgehog.tripping ha ipv6ToBytes ipv6FromBytes

genIPv6 :: Hedgehog.Gen IPv6
genIPv6 = do
  w1 <- Gen.word32 constantBounded
  w2 <- Gen.word32 constantBounded
  w3 <- Gen.word32 constantBounded
  w4 <- Gen.word32 constantBounded
  pure $ fromHostAddress6 (w1, w2, w3, w4)

serializationTests :: TestTree
serializationTests =
  testGroup
    "Shelley Serialization Tests"
    [ serializationUnitTests,
      serializationPropertyTests
    ]

serializationPropertyTests :: TestTree
serializationPropertyTests =
  testGroup
    "Serialisation roundtrip Property Tests"
    [ QC.testProperty "roundtrip Block" prop_roundtrip_Block,
      QC.testProperty "roundtrip Addr" prop_roundtrip_Addr,
      QC.testProperty "roundtrip RewardAcnt" prop_roundtrip_RewardAcnt,
      QC.testProperty "roundtrip Header" prop_roundtrip_Header,
      QC.testProperty "roundtrip Block Header Hash" prop_roundtrip_BlockHeaderHash,
      QC.testProperty "roundtrip Tx" prop_roundtrip_Tx,
      QC.testProperty "roundtrip Bootstrap Witness" prop_roundtrip_BootstrapWitness,
      QC.testProperty "roundtrip TxId" prop_roundtrip_TxId,
      QC.testProperty "roundtrip LEDGER Predicate Failures" prop_roundtrip_LEDGER_PredicateFails,
      QC.testProperty "roundtrip Protocol State" prop_roundtrip_PrtclState,
      QC.testProperty "roundtrip Ledger State" prop_roundtrip_LedgerState,
      QC.testProperty "roundtrip NewEpoch State" prop_roundtrip_NewEpochState
    ]

serializationUnitTests :: TestTree
serializationUnitTests =
  testGroup
    "CBOR Serialization Tests"
    [ checkEncodingCBOR
        "list"
        [1 :: Integer]
        (T (TkListBegin . TkInteger 1 . TkBreak)),
      checkEncodingCBOR
        "set"
        (Set.singleton (1 :: Integer))
        (T (TkTag 258 . TkListLen 1 . TkInteger 1)),
      checkEncodingCBOR
        "map"
        (Map.singleton (1 :: Integer) (1 :: Integer))
        (T (TkMapLen 1 . TkInteger 1 . TkInteger 1)),
      checkEncodingCBOR
        "coin"
        (Coin 30)
        (T (TkWord64 30)),
      testCase "prop_serialize_negative-coin" testNegativeCoin,
      checkEncodingCBOR
        "rational"
        (truncateUnitInterval (1 % 2))
        (T (TkTag 30 . TkListLen 2 . TkWord64 1 . TkWord64 2)),
      checkEncodingCBOR
        "slot"
        (SlotNo 7)
        (T (TkWord64 7)),
      checkEncodingCBOR
        "neutral_nonce"
        NeutralNonce
        (T (TkListLen 1 . TkWord 0)),
      checkEncodingCBOR
        "nonce"
        (mkNonceFromNumber 99)
        (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonceFromNumber 99))),
      checkEncodingCBOR
        "key_hash"
        (testKeyHash1 p)
        (T (TkBytes (getRawKeyHash (testKeyHash1 p)))),
      checkEncodingCBOR
        "credential_key_hash"
        (testPayCred p)
        (T (TkListLen 2 . TkWord 0) <> S (testKeyHash1 p)),
      checkEncodingCBOR
        "txin"
        (TxIn genesisId 0 :: TxIn Monomorphic.ShortHash)
        (T (TkListLen 2) <> S (genesisId :: TxId Monomorphic.ShortHash) <> T (TkWord64 0)),
      let a = Addr Testnet (testPayCred p) StakeRefNull
       in checkEncodingCBOR
            "txout"
            (TxOut a (Coin 2))
            ( T (TkListLen 2)
                <> S a
                <> S (Coin 2)
            ),
      case makeWitnessVKey testTxbHash (testKey1 p) of
        w@(WitVKey vk _sig) ->
          checkEncodingCBORAnnotated
            "vkey_witnesses"
            w -- Transaction _witnessVKeySet element
            ( T (TkListLen 2)
                <> S vk -- vkey
                <> T (testKey1SigToken p) -- signature
            ),
      checkEncodingCBOR
        "script_hash_to_scripts"
        (Map.singleton (hashScript (testScript p) :: ScriptHash Monomorphic.ShortHash) (testScript p)) -- Transaction _witnessMSigMap
        ( T (TkMapLen 1)
            <> S (hashScript (testScript p) :: ScriptHash Monomorphic.ShortHash)
            <> S (testScript p)
        ),
      -- checkEncodingCBOR "withdrawal_key"
      let r = (RewardAcnt Testnet (testStakeCred p))
       in checkEncodingCBOR
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      -- checkEncodingCBOR "withdrawal_script"
      --
      let r = RewardAcnt Testnet (ScriptHashObj (testScriptHash p))
       in checkEncodingCBOR
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      checkEncodingCBOR
        "register_stake_reference"
        (DCertDeleg (RegKey (testStakeCred p)))
        ( T (TkListLen 2)
            <> T (TkWord 0) -- Reg cert
            <> S (testStakeCred p) -- keyhash
        ),
      checkEncodingCBOR
        "deregister_stake_reference"
        (DCertDeleg (DeRegKey (testStakeCred p)))
        ( T (TkListLen 2)
            <> T (TkWord 1) -- DeReg cert
            <> S (testStakeCred p) -- keyhash
        ),
      checkEncodingCBOR
        "stake_delegation"
        (DCertDeleg (Delegate (Delegation (testStakeCred p) (hashKey . vKey $ testStakePoolKey p))))
        ( T
            ( TkListLen 3
                . TkWord 2 -- delegation cert with key
            )
            <> S (testStakeCred p)
            <> S (hashKey . vKey $ testStakePoolKey p)
        ),
      -- checkEncodingCBOR "register-pool"
      let poolOwner = testKeyHash2 p
          poolMargin = unsafeMkUnitInterval 0.7
          poolRAcnt = RewardAcnt Testnet (testStakeCred p)
          poolPledge = Coin 11
          poolCost = Coin 55
          poolUrl = "pool.io"
          poolMDHash = BS.pack "{}"
          ipv4 = toIPv4 [127, 0, 0, 1]
          ipv4Bytes = ipv4ToBytes . toIPv4 $ [127, 0, 0, 1]
          poolRelays =
            StrictSeq.fromList
              [ SingleHostAddr SNothing (SJust ipv4) SNothing,
                SingleHostName (SJust 42) $ Maybe.fromJust $ textToDns "singlehost.relay.com",
                MultiHostName $ Maybe.fromJust $ textToDns "multihost.relay.com"
              ]
       in checkEncodingCBOR
            "register_pool"
            ( DCertPool
                ( RegPool
                    ( PoolParams
                        { _poolPubKey = hashKey . vKey $ testStakePoolKey p,
                          _poolVrf = testVRFKH p,
                          _poolPledge = poolPledge,
                          _poolCost = poolCost,
                          _poolMargin = poolMargin,
                          _poolRAcnt = poolRAcnt,
                          _poolOwners = Set.singleton poolOwner,
                          _poolRelays = poolRelays,
                          _poolMD =
                            SJust $
                              PoolMetaData
                                { _poolMDUrl = Maybe.fromJust $ textToUrl poolUrl,
                                  _poolMDHash = poolMDHash
                                }
                        }
                    )
                )
            )
            ( T (TkListLen 10)
                <> T (TkWord 3) -- Reg Pool
                <> S (hashKey . vKey $ testStakePoolKey p) -- operator
                <> S (testVRFKH p) -- vrf keyhash
                <> S poolPledge -- pledge
                <> S poolCost -- cost
                <> S poolMargin -- margin
                <> S poolRAcnt -- reward acct
                <> T (TkListLen 1)
                <> S poolOwner -- owners
                <> T (TkListLen 3) -- relays
                <> T (TkListLen 4 . TkWord 0 . TkNull . TkBytes ipv4Bytes . TkNull)
                <> T (TkListLen 3 . TkWord 1 . (TkWord 42) . TkString ("singlehost.relay.com"))
                <> T (TkListLen 2 . TkWord 2 . TkString ("multihost.relay.com"))
                <> T (TkListLen 2) -- metadata present
                <> S poolUrl -- metadata url
                <> S poolMDHash -- metadata hash
            ),
      checkEncodingCBOR
        "retire_pool"
        (DCertPool (RetirePool (hashKey . vKey $ testStakePoolKey p) (EpochNo 1729)))
        ( T
            ( TkListLen 3
                . TkWord 4 -- Pool Retire
            )
            <> S (hashKey . vKey $ testStakePoolKey p) -- key hash
            <> S (EpochNo 1729) -- epoch
        ),
      checkEncodingCBOR
        "genesis_delegation"
        ( DCertGenesis
            ( GenesisDelegCert
                (testGKeyHash p)
                (hashKey . vKey $ testGenesisDelegateKey p)
                (testVRFKH p)
            )
        )
        ( T
            ( TkListLen 4
                . TkWord 5 -- genesis delegation cert
            )
            <> S (testGKeyHash p) -- delegator credential
            <> S (hashKey . vKey $ testGenesisDelegateKey p) -- delegatee key hash
            <> S (testVRFKH p) -- delegatee vrf key hash
        ),
      -- checkEncodingCBOR "mir"
      let rws = Map.singleton (testStakeCred p) 77
       in checkEncodingCBOR
            "mir"
            (DCertMir (MIRCert ReservesMIR rws))
            ( T
                ( TkListLen 2
                    . TkWord 6 -- make instantaneous rewards cert
                    . TkListLen 2
                    . TkWord 0 -- take from the reserves
                )
                <> S rws
            ),
      checkEncodingCBOR
        "pparams_update_key_deposit_only"
        ( PParams
            { _minfeeA = SNothing,
              _minfeeB = SNothing,
              _maxBBSize = SNothing,
              _maxTxSize = SNothing,
              _maxBHSize = SNothing,
              _keyDeposit = SJust (Coin 5),
              _poolDeposit = SNothing,
              _eMax = SNothing,
              _nOpt = SNothing,
              _a0 = SNothing,
              _rho = SNothing,
              _tau = SNothing,
              _d = SNothing,
              _extraEntropy = SNothing,
              _protocolVersion = SNothing,
              _minUTxOValue = SNothing,
              _minPoolCost = SNothing
            } ::
            PParamsUpdate
        )
        ((T $ TkMapLen 1 . TkWord 5) <> S (Coin 5)),
      -- checkEncodingCBOR "pparams_update_all"
      let minfeea = 0
          minfeeb = 1
          maxbbsize = 2
          maxtxsize = 3
          maxbhsize = 4
          keydeposit = Coin 5
          pooldeposit = Coin 6
          emax = EpochNo 7
          nopt = 8
          a0 = 1 % 6
          rho = truncateUnitInterval $ 1 % 6
          tau = truncateUnitInterval $ 1 % 7
          d = truncateUnitInterval $ 1 % 9
          extraEntropy = NeutralNonce
          protocolVersion = ProtVer 0 1
          minUTxOValue = 121
          minPoolCost = 987
       in checkEncodingCBOR
            "pparams_update_all"
            ( PParams
                { _minfeeA = SJust minfeea,
                  _minfeeB = SJust minfeeb,
                  _maxBBSize = SJust maxbbsize,
                  _maxTxSize = SJust maxtxsize,
                  _maxBHSize = SJust maxbhsize,
                  _keyDeposit = SJust keydeposit,
                  _poolDeposit = SJust pooldeposit,
                  _eMax = SJust emax,
                  _nOpt = SJust nopt,
                  _a0 = SJust a0,
                  _rho = SJust rho,
                  _tau = SJust tau,
                  _d = SJust d,
                  _extraEntropy = SJust extraEntropy,
                  _protocolVersion = SJust protocolVersion,
                  _minUTxOValue = SJust minUTxOValue,
                  _minPoolCost = SJust minPoolCost
                } ::
                PParamsUpdate
            )
            ( (T $ TkMapLen 17)
                <> (T $ TkWord 0)
                <> S minfeea
                <> (T $ TkWord 1)
                <> S minfeeb
                <> (T $ TkWord 2)
                <> S maxbbsize
                <> (T $ TkWord 3)
                <> S maxtxsize
                <> (T $ TkWord 4)
                <> S maxbhsize
                <> (T $ TkWord 5)
                <> S keydeposit
                <> (T $ TkWord 6)
                <> S pooldeposit
                <> (T $ TkWord 7)
                <> S emax
                <> (T $ TkWord 8)
                <> S nopt
                <> (T $ TkWord 9 . TkTag 30)
                <> S a0
                <> (T $ TkWord 10)
                <> S rho
                <> (T $ TkWord 11)
                <> S tau
                <> (T $ TkWord 12)
                <> S d
                <> (T $ TkWord 13)
                <> S extraEntropy
                <> (T $ TkWord 14)
                <> S protocolVersion
                <> (T $ TkWord 15)
                <> S minUTxOValue
                <> (T $ TkWord 16)
                <> S minPoolCost
            ),
      -- checkEncodingCBOR "full_update"
      let ppup =
            ProposedPPUpdates
              ( Map.singleton
                  (testGKeyHash p)
                  ( PParams
                      { _minfeeA = SNothing,
                        _minfeeB = SNothing,
                        _maxBBSize = SNothing,
                        _maxTxSize = SNothing,
                        _maxBHSize = SNothing,
                        _keyDeposit = SNothing,
                        _poolDeposit = SNothing,
                        _eMax = SNothing,
                        _nOpt = SJust 100,
                        _a0 = SNothing,
                        _rho = SNothing,
                        _tau = SNothing,
                        _d = SNothing,
                        _extraEntropy = SNothing,
                        _protocolVersion = SNothing,
                        _minUTxOValue = SNothing,
                        _minPoolCost = SNothing
                      }
                  )
              )
          e = EpochNo 0
       in checkEncodingCBOR
            "full_update"
            (Update ppup e)
            ( (T $ TkListLen 2)
                <> S ppup
                <> S e
            ),
      -- checkEncodingCBOR "minimal_txn_body"
      let tin = TxIn genesisId 1
          tout = TxOut (testAddrE p) (Coin 2)
       in checkEncodingCBORAnnotated
            "txbody"
            ( TxBody -- minimal transaction body
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 9)
                (SlotNo 500)
                SNothing
                SNothing
            )
            ( T (TkMapLen 4)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> T (TkWord64 9)
                <> T (TkWord 3) -- Tx TTL
                <> T (TkWord64 500)
            ),
      -- checkEncodingCBOR "transaction_mixed"
      let tin = TxIn genesisId 1
          tout = TxOut (testAddrE p) (Coin 2)
          ra = RewardAcnt Testnet (KeyHashObj (testKeyHash2 p))
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      (testGKeyHash p)
                      ( PParams
                          { _minfeeA = SNothing,
                            _minfeeB = SNothing,
                            _maxBBSize = SNothing,
                            _maxTxSize = SNothing,
                            _maxBHSize = SNothing,
                            _keyDeposit = SNothing,
                            _poolDeposit = SNothing,
                            _eMax = SNothing,
                            _nOpt = SJust 100,
                            _a0 = SNothing,
                            _rho = SNothing,
                            _tau = SNothing,
                            _d = SNothing,
                            _extraEntropy = SNothing,
                            _protocolVersion = SNothing,
                            _minUTxOValue = SNothing,
                            _minPoolCost = SNothing
                          }
                      )
                  )
              )
              (EpochNo 0)
       in checkEncodingCBORAnnotated
            "txbody_partial"
            ( TxBody -- transaction body with some optional components
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                StrictSeq.Empty
                (Wdrl ras)
                (Coin 9)
                (SlotNo 500)
                (SJust up)
                SNothing
            )
            ( T (TkMapLen 6)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> S (Coin 9)
                <> T (TkWord 3) -- Tx TTL
                <> S (SlotNo 500)
                <> T (TkWord 5) -- Tx Reward Withdrawals
                <> S ras
                <> T (TkWord 6) -- Tx Update
                <> S up
            ),
      -- checkEncodingCBOR "full_txn_body"
      let tin = TxIn genesisId 1
          tout = TxOut (testAddrE p) (Coin 2)
          reg = DCertDeleg (RegKey (testStakeCred p))
          ra = RewardAcnt Testnet (KeyHashObj (testKeyHash2 p))
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      (testGKeyHash p)
                      ( PParams
                          { _minfeeA = SNothing,
                            _minfeeB = SNothing,
                            _maxBBSize = SNothing,
                            _maxTxSize = SNothing,
                            _maxBHSize = SNothing,
                            _keyDeposit = SNothing,
                            _poolDeposit = SNothing,
                            _eMax = SNothing,
                            _nOpt = SJust 100,
                            _a0 = SNothing,
                            _rho = SNothing,
                            _tau = SNothing,
                            _d = SNothing,
                            _extraEntropy = SNothing,
                            _protocolVersion = SNothing,
                            _minUTxOValue = SNothing,
                            _minPoolCost = SNothing
                          }
                      )
                  )
              )
              (EpochNo 0)
          mdh = MD.hashMetaData $ MD.MetaData $ Map.singleton 13 (MD.I 17)
       in checkEncodingCBORAnnotated
            "txbody_full"
            ( TxBody -- transaction body with all components
                (Set.fromList [tin])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Wdrl ras)
                (Coin 9)
                (SlotNo 500)
                (SJust up)
                (SJust mdh)
            )
            ( T (TkMapLen 8)
                <> T (TkWord 0) -- Tx Ins
                <> T (TkListLen 1)
                <> S tin
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> S (Coin 9)
                <> T (TkWord 3) -- Tx TTL
                <> S (SlotNo 500)
                <> T (TkWord 4) -- Tx Certs
                <> T (TkListLen 1) -- Seq list begin
                <> S reg
                <> T (TkWord 5) -- Tx Reward Withdrawals
                <> S ras
                <> T (TkWord 6) -- Tx Update
                <> S up
                <> T (TkWord 7) -- Tx Metadata
                <> S mdh
            ),
      -- checkEncodingCBOR "minimal_txn"
      let txb =
            TxBody
              (Set.fromList [TxIn genesisId 1])
              (StrictSeq.singleton $ TxOut (testAddrE p) (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashTxBody txb
          w = makeWitnessVKey txbh (testKey1 p)
       in checkEncodingCBORAnnotated
            "tx_min"
            (Tx txb mempty {addrWits = Set.singleton w} SNothing)
            ( T (TkListLen 3)
                <> S txb
                <> T (TkMapLen 1)
                <> T (TkWord 0)
                <> T (TkListLen 1)
                <> S w
                <> T TkNull
            ),
      -- checkEncodingCBOR "full_txn"
      let txb =
            TxBody
              (Set.fromList [TxIn genesisId 1])
              (StrictSeq.singleton $ TxOut (testAddrE p) (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashTxBody txb
          w = makeWitnessVKey txbh (testKey1 p)
          s = Map.singleton (hashScript (testScript p)) (testScript p)
          wits = mempty {addrWits = Set.singleton w, msigWits = s}
          md = MD.MetaData $ Map.singleton 17 (MD.I 42)
       in checkEncodingCBORAnnotated
            "tx_full"
            (Tx txb wits (SJust md))
            ( T (TkListLen 3)
                <> S txb
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 1)
                <> S w
                <> T (TkWord 1)
                <> T (TkListLen 1)
                <> S (testScript p)
                <> S md
            ),
      -- checkEncodingCBOR "block_header_body"
      let prevhash = BlockHash (testHeaderHash p)
          vrfVkey = snd testVRF
          slot = SlotNo 33
          nonce = mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0)
          nonceProof = coerce $ mkCertifiedVRF (WithResult nonce 1) (fst testVRF)
          leaderValue = mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0)
          leaderProof = coerce $ mkCertifiedVRF (WithResult leaderValue 1) (fst testVRF)
          size = 0
          blockNo = BlockNo 44
          bbhash = bbHash $ TxSeq StrictSeq.empty
          ocert =
            OCert
              (snd testKESKeys)
              0
              (KESPeriod 0)
              (signedDSIGN @(ConcreteCrypto Monomorphic.ShortHash) (sKey testBlockIssuerKey) (snd testKESKeys, 0, KESPeriod 0))
          protover = ProtVer 0 0
       in checkEncodingCBOR
            "block_header_body"
            ( BHBody
                { bheaderBlockNo = blockNo,
                  bheaderSlotNo = slot,
                  bheaderPrev = prevhash,
                  bheaderVk = vKey testBlockIssuerKey,
                  bheaderVrfVk = vrfVkey,
                  bheaderEta = nonceProof,
                  bheaderL = leaderProof,
                  bsize = size,
                  bhash = bbhash,
                  bheaderOCert = ocert,
                  bprotver = protover
                }
            )
            ( T (TkListLen $ 9 + 4 + 2)
                <> S blockNo
                <> S slot
                <> S prevhash
                <> T testBlockIssuerKeyTokens
                <> S vrfVkey
                <> S nonceProof
                <> S leaderProof
                <> S size
                <> S bbhash
                <> G ocert -- 5
                <> G protover -- 3
            ),
      -- checkEncodingCBOR "operational_cert"
      let vkHot = snd testKESKeys
          counter = 0
          kesperiod = KESPeriod 0
          signature = signedDSIGN @(ConcreteCrypto Monomorphic.ShortHash) (sKey (testKey1 p)) (snd testKESKeys, 0, KESPeriod 0)
       in checkEncodingCBORCBORGroup
            "operational_cert"
            (OCert @(ConcreteCrypto Monomorphic.ShortHash) vkHot counter kesperiod signature)
            ( S vkHot
                <> S counter
                <> S kesperiod
                <> T (testOpCertSigTokens p)
            ),
      -- checkEncodingCBOR "block_header"
      let sig = signedKES () 0 (testBHB p) (fst testKESKeys)
       in checkEncodingCBORAnnotated
            "block_header"
            (BHeader (testBHB p) sig)
            ( (T $ TkListLen 2)
                <> S (testBHB p)
                <> T (testBHBSigTokens p)
            ),
      -- checkEncodingCBOR "empty_block"
      let sig = signedKES () 0 (testBHB p) (fst testKESKeys)
          bh = BHeader (testBHB p) sig
          txns = TxSeq StrictSeq.Empty
       in checkEncodingCBORAnnotated
            "empty_block"
            (Block bh txns)
            ( (T $ TkListLen 4)
                <> S bh
                <> T (TkListLen 0 . TkListLen 0 . TkMapLen 0)
            ),
      -- checkEncodingCBOR "rich_block"
      let sig = signedKES () 0 (testBHB p) (fst testKESKeys)
          bh = BHeader (testBHB p) sig
          tin = Set.fromList [TxIn genesisId 1]
          tout = StrictSeq.singleton $ TxOut (testAddrE p) (Coin 2)
          txb s = TxBody tin tout StrictSeq.empty (Wdrl Map.empty) (Coin 9) (SlotNo s) SNothing SNothing
          txb1 = txb 500
          txb2 = txb 501
          txb3 = txb 502
          txb4 = txb 503
          txb5 = txb 504
          w1 = makeWitnessVKey (hashTxBody txb1) (testKey1 p)
          w2 = makeWitnessVKey (hashTxBody txb1) (testKey2 p :: KeyPair Monomorphic.ShortHash 'Payment)
          ws = Set.fromList [w1, w2]
          tx1 = Tx txb1 mempty {addrWits = Set.singleton w1} SNothing
          tx2 = Tx txb2 mempty {addrWits = ws} SNothing
          tx3 =
            Tx
              txb3
              mempty {msigWits = Map.singleton (hashScript (testScript p)) (testScript p)}
              SNothing
          ss =
            Map.fromList
              [ (hashScript (testScript p), testScript p),
                (hashScript (testScript2 p), testScript2 p)
              ]
          tx4 = Tx txb4 mempty {msigWits = ss} SNothing
          tx5MD = MD.MetaData $ Map.singleton 17 (MD.I 42)
          tx5 = Tx txb5 mempty {addrWits = ws, msigWits = ss} (SJust tx5MD)
          txns = TxSeq $ StrictSeq.fromList [tx1, tx2, tx3, tx4, tx5]
       in checkEncodingCBORAnnotated
            "rich_block"
            (Block bh txns)
            ( (T $ TkListLen 4)
                -- header
                <> S bh
                -- bodies
                <> T (TkListLen 5)
                <> S txb1
                <> S txb2
                <> S txb3
                <> S txb4
                <> S txb5
                -- witnesses
                <> T (TkListLen 5)
                -- tx 1, one key
                <> T (TkMapLen 1 . TkWord 0)
                <> T (TkListLen 1)
                <> S w1
                -- tx 2, two keys
                <> T (TkMapLen 1 . TkWord 0)
                <> T (TkListLen 2)
                -- The test is unfortunately sensitive to this ordering. TODO make it
                -- better
                <> S w2
                <> S w1
                -- tx 3, one script
                <> T (TkMapLen 1 . TkWord 1)
                <> T (TkListLen 1)
                <> S (testScript p)
                -- tx 4, two scripts
                <> T (TkMapLen 1 . TkWord 1)
                <> T (TkListLen 2)
                <> S (testScript2 p)
                <> S (testScript p)
                -- tx 5, two keys and two scripts
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 2)
                <> S w2
                <> S w1
                <> T (TkWord 1)
                <> T (TkListLen 2)
                <> S (testScript2 p)
                <> S (testScript p)
                -- metadata
                <> T (TkMapLen 1)
                <> T (TkInt 4)
                <> S tx5MD
            ),
      checkEncodingCBOR
        "epoch"
        (EpochNo 13)
        (T (TkWord64 13)),
      let n = (17 :: Natural)
          bs = Map.singleton (hashKey . vKey $ testStakePoolKey p) n
       in checkEncodingCBOR
            "blocks_made"
            (BlocksMade bs)
            ( T (TkMapLen 1)
                <> S (hashKey . vKey $ testStakePoolKey p)
                <> S n
            ),
      checkEncodingCBOR
        "account_state"
        (AccountState (Coin 1) (Coin 2))
        ( T (TkListLen 2)
            <> S (Coin 1)
            <> S (Coin 2)
        ),
      let stk = Map.singleton (testStakeCred p) (Coin 13)
       in checkEncodingCBOR
            "stake"
            (Stake stk)
            ( T (TkMapLen 1)
                <> S (testStakeCred p)
                <> S (Coin 13)
            ),
      let mark =
            SnapShot
              (Stake $ Map.singleton (testStakeCred p) (Coin 11))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          set =
            SnapShot
              (Stake $ Map.singleton (KeyHashObj (testKeyHash2 p)) (Coin 22))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          go =
            SnapShot
              (Stake $ Map.singleton (testStakeCred p) (Coin 33))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          params =
            PoolParams
              { _poolPubKey = (hashKey $ vKey (testStakePoolKey p)),
                _poolVrf = testVRFKH p,
                _poolPledge = Coin 5,
                _poolCost = Coin 4,
                _poolMargin = unsafeMkUnitInterval 0.7,
                _poolRAcnt = RewardAcnt Testnet (testStakeCred p),
                _poolOwners = Set.singleton (testKeyHash2 p),
                _poolRelays = StrictSeq.empty,
                _poolMD =
                  SJust $
                    PoolMetaData
                      { _poolMDUrl = Maybe.fromJust $ textToUrl "web.site",
                        _poolMDHash = BS.pack "{}"
                      }
              }
          ps = Map.singleton (hashKey $ vKey (testStakePoolKey p)) params
          fs = Coin 123
       in checkEncodingCBOR
            "snapshots"
            (SnapShots mark set go fs)
            ( T (TkListLen 4)
                <> S mark
                <> S set
                <> S go
                <> S fs
            ),
      let e = EpochNo 0
          ac = AccountState (Coin 100) (Coin 100)
          mark =
            SnapShot
              (Stake $ Map.singleton (testStakeCred p) (Coin 11))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          set =
            SnapShot
              (Stake $ Map.singleton (KeyHashObj (testKeyHash2 p)) (Coin 22))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          go =
            SnapShot
              (Stake $ Map.singleton (testStakeCred p) (Coin 33))
              (Map.singleton (testStakeCred p) (hashKey $ vKey (testStakePoolKey p)))
              ps
          params =
            PoolParams
              { _poolPubKey = (hashKey $ vKey (testStakePoolKey p)),
                _poolVrf = testVRFKH p,
                _poolPledge = Coin 5,
                _poolCost = Coin 4,
                _poolMargin = unsafeMkUnitInterval 0.7,
                _poolRAcnt = RewardAcnt Testnet (testStakeCred p),
                _poolOwners = Set.singleton (testKeyHash2 p),
                _poolRelays = StrictSeq.empty,
                _poolMD =
                  SJust $
                    PoolMetaData
                      { _poolMDUrl = Maybe.fromJust $ textToUrl "web.site",
                        _poolMDHash = BS.pack "{}"
                      }
              }
          ps = Map.singleton (hashKey $ vKey (testStakePoolKey p)) params
          fs = Coin 123
          ss = SnapShots mark set go fs
          ls = emptyLedgerState
          pps = emptyPParams
          bs = Map.singleton (hashKey $ vKey (testStakePoolKey p)) 1
          nm = emptyNonMyopic
          es = EpochState ac ss ls pps pps nm
          ru =
            ( SJust
                RewardUpdate
                  { deltaT = Coin 100,
                    deltaR = Coin (-200),
                    rs = Map.empty,
                    deltaF = Coin (-10),
                    nonMyopic = nm
                  }
            ) ::
              StrictMaybe (RewardUpdate Monomorphic.ShortHash)
          pd = (PoolDistr Map.empty) :: PoolDistr Monomorphic.ShortHash
          os = Map.singleton (SlotNo 1) (ActiveSlot (testGKeyHash p))
          compactOs = Map.singleton (ActiveSlot (testGKeyHash p)) (SlotNo 1 :| [])
          nes =
            NewEpochState
              e
              (BlocksMade bs)
              (BlocksMade bs)
              es
              ru
              pd
              os
       in checkEncodingCBOR
            "new_epoch_state"
            nes
            ( T (TkListLen 7)
                <> S e
                <> S (BlocksMade bs)
                <> S (BlocksMade bs)
                <> S es
                <> S ru
                <> S pd
                <> S compactOs
            ),
      testProperty "Roundtrip IPv4 serialisation Hedghog" roundTripIpv4,
      testProperty "Roundtrip IPv6 serialisation Hedghog" roundTripIpv6
    ]
  where
    p :: Proxy Monomorphic.ShortHash
    p = Proxy
