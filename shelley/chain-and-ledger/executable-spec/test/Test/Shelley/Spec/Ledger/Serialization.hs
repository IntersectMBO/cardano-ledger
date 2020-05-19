{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialization where

import Cardano.Binary
  ( Annotator,
    DecoderError,
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeFullDecoder,
    serialize',
    serializeEncoding,
    toCBOR,
  )
import Cardano.Crypto.DSIGN (encodeSignedDSIGN, encodeVerKeyDSIGN)
import Cardano.Crypto.Hash (getHash)
import Cardano.Prelude (LByteString)
import Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Coerce (coerce)
import Data.IP (IPv4, IPv6, fromHostAddress, fromHostAddress6, toIPv4)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromJust)
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
    UnitInterval (..),
    mkNonce,
    textToDns,
    textToUrl,
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
    coerceKeyRole,
    encodeSignedKES,
    hash,
    hashKey,
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
    genesisId,
    nonMyopic,
    rs,
    pattern ActiveSlot,
    pattern RewardUpdate,
  )
import qualified Shelley.Spec.Ledger.MetaData as MD
import Shelley.Spec.Ledger.OCert (KESPeriod (..), pattern OCert)
import Shelley.Spec.Ledger.PParams
  ( PParams' (PParams),
    PParamsUpdate,
    ProtVer (..),
    _a0,
    _d,
    _eMax,
    _extraEntropy,
    _keyDecayRate,
    _keyDeposit,
    _keyMinRefund,
    _maxBBSize,
    _maxBHSize,
    _maxTxSize,
    _minUTxOValue,
    _minfeeA,
    _minfeeB,
    _nOpt,
    _poolDecayRate,
    _poolDeposit,
    _poolMinRefund,
    _protocolVersion,
    _rho,
    _tau,
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
import Shelley.Spec.Ledger.Tx (Tx (..), hashScript)
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    PoolMetaData (..),
    StakePoolRelay (..),
    Wdrl (..),
    WitVKey (..),
    _TxId,
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
    CoreKeyPair,
    Credential,
    HashHeader,
    KeyHash,
    KeyPair,
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
import Test.Shelley.Spec.Ledger.SerializationProperties
  ( prop_roundtrip_Block,
    prop_roundtrip_BlockHeaderHash,
    prop_roundtrip_Header,
    prop_roundtrip_LEDGER_PredicateFails,
    prop_roundtrip_LedgerState,
    prop_roundtrip_NewEpochState,
    prop_roundtrip_PrtclState,
    prop_roundtrip_Tx,
    prop_roundtrip_TxId,
  )
import Test.Shelley.Spec.Ledger.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertEqual, assertFailure, testCase)
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
    assertEqual testName (fromEncoding $ toCBOR t) (fromEncoding $ encode x)
      >> roundTrip encode decode x
  where
    testName = "prop_serialize_" <> name
    fromEncoding :: Encoding -> Tokens
    fromEncoding (Encoding e) = e TkEnd

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

getRawKeyHash :: KeyHash 'Payment -> ByteString
getRawKeyHash (KeyHash hsh) = getHash hsh

getRawGenKeyHash :: KeyHash 'Genesis -> ByteString
getRawGenKeyHash (KeyHash hsh) = getHash hsh

getRawScriptHash :: ScriptHash -> ByteString
getRawScriptHash (ScriptHash hsh) = getHash hsh

getRawTxId :: TxId -> ByteString
getRawTxId = getHash . _TxId

getRawNonce :: Nonce -> ByteString
getRawNonce (Nonce hsh) = getHash hsh
getRawNonce NeutralNonce = error "The neutral nonce has no bytes"

testGKey :: CoreKeyPair
testGKey = KeyPair vk sk
  where
    (sk, vk) = mkGenKey (0, 0, 0, 0, 0)

testGKeyHash :: KeyHash 'Genesis
testGKeyHash = (hashKey . vKey) testGKey

testVRF :: (SignKeyVRF, VerKeyVRF)
testVRF = mkVRFKeyPair (0, 0, 0, 0, 5)

testVRFKH :: VRFKeyHash
testVRFKH = hashKeyVRF $ snd testVRF

testTxb :: TxBody
testTxb = TxBody Set.empty StrictSeq.empty StrictSeq.empty (Wdrl Map.empty) (Coin 0) (SlotNo 0) SNothing SNothing

testTxbHash :: Hash ConcreteCrypto TxBody
testTxbHash = hashTxBody testTxb

testKey1 :: KeyPair 'Payment
testKey1 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

testKey2 :: KeyPair kr
testKey2 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 2)

testKey3 :: KeyPair kr
testKey3 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 3)

testBlockIssuerKey :: KeyPair 'BlockIssuer
testBlockIssuerKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (0, 0, 0, 0, 4)

testKey1Token :: Tokens -> Tokens
testKey1Token = e
  where
    (VKey vk) = vKey testKey1 :: VKey 'Payment
    Encoding e = encodeVerKeyDSIGN vk

testBlockIssuerKeyTokens :: Tokens -> Tokens
testBlockIssuerKeyTokens = e
  where
    (VKey vk) = vKey testBlockIssuerKey
    Encoding e = encodeVerKeyDSIGN vk

testKey1SigToken :: Tokens -> Tokens
testKey1SigToken = e
  where
    s =
      signedDSIGN @ConcreteCrypto (sKey testKey1) testTxbHash ::
        SignedDSIGN (Hash ConcreteCrypto TxBody)
    Encoding e = encodeSignedDSIGN s

testOpCertSigTokens :: Tokens -> Tokens
testOpCertSigTokens = e
  where
    s = signedDSIGN @ConcreteCrypto (sKey testKey1) (snd testKESKeys, 0 :: Natural, KESPeriod 0)
    Encoding e = encodeSignedDSIGN s

testKeyHash1 :: KeyHash 'Payment
testKeyHash1 = (hashKey . vKey) testKey1

testKeyHash2 :: KeyHash 'Staking
testKeyHash2 = (hashKey . vKey) testKey2

testKeyHash3 :: KeyHash 'Payment
testKeyHash3 = (hashKey . vKey) testKey3

testKESKeys :: (SignKeyKES, VerKeyKES)
testKESKeys = mkKESKeyPair (0, 0, 0, 0, 3)

testAddrE :: Addr
testAddrE = Addr Testnet (KeyHashObj testKeyHash1) StakeRefNull

testPayCred :: Credential 'Payment
testPayCred = KeyHashObj testKeyHash1

testStakeCred :: Credential 'Staking
testStakeCred = KeyHashObj testKeyHash2

testScript :: MultiSig
testScript = RequireSignature $ asWitness testKeyHash1

testScriptHash :: ScriptHash
testScriptHash = hashScript testScript

testScript2 :: MultiSig
testScript2 = RequireSignature $ asWitness testKeyHash2

testScriptHash2 :: ScriptHash
testScriptHash2 = hashScript testScript2

testHeaderHash :: HashHeader
testHeaderHash = HashHeader $ coerce (hash 0 :: Hash ConcreteCrypto Int)

testBHB :: BHBody
testBHB =
  BHBody
    { bheaderBlockNo = BlockNo 44,
      bheaderSlotNo = SlotNo 33,
      bheaderPrev = BlockHash testHeaderHash,
      bheaderVk = coerceKeyRole $ vKey testKey1,
      bheaderVrfVk = snd testVRF,
      bheaderEta =
        coerce $
          mkCertifiedVRF
            ( WithResult
                (mkSeed seedEta (SlotNo 33) (mkNonce 0))
                1
            )
            (fst testVRF),
      bheaderL =
        coerce $
          mkCertifiedVRF
            ( WithResult
                (mkSeed seedL (SlotNo 33) (mkNonce 0))
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
          (signedDSIGN @ConcreteCrypto (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0)),
      bprotver = ProtVer 0 0
    }

testBHBSigTokens :: Tokens -> Tokens
testBHBSigTokens = e
  where
    s = signedKES () 0 testBHB (fst testKESKeys)
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
  where
    genIPv4 :: Gen IPv4
    genIPv4 = fromHostAddress <$> (Gen.word32 constantBounded)

roundTripIpv6 :: Property
roundTripIpv6 =
  -- We are using a QC generator which means we need QC test
  Hedgehog.property $ do
    ha <- forAll genIPv6
    Hedgehog.tripping ha ipv6ToBytes ipv6FromBytes
  where
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
      QC.testProperty "roundtrip Header" prop_roundtrip_Header,
      QC.testProperty "roundtrip Block Header Hash" prop_roundtrip_BlockHeaderHash,
      QC.testProperty "roundtrip Tx" prop_roundtrip_Tx,
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
        (UnsafeUnitInterval (1 % 2))
        (T (TkTag 30 . TkListLen 2 . TkInteger 1 . TkInteger 2)),
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
        (mkNonce 99)
        (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonce 99))),
      checkEncodingCBOR
        "key_hash"
        testKeyHash1
        (T (TkBytes (getRawKeyHash testKeyHash1))),
      checkEncodingCBOR
        "credential_key_hash"
        testPayCred
        (T (TkListLen 2 . TkWord 0) <> S testKeyHash1),
      checkEncodingCBOR
        "txin"
        (TxIn genesisId 0 :: TxIn)
        (T (TkListLen 2) <> S (genesisId :: TxId) <> T (TkWord64 0)),
      let a = Addr Testnet testPayCred StakeRefNull
       in checkEncodingCBOR
            "txout"
            (TxOut a (Coin 2))
            ( T (TkListLen 2)
                <> S a
                <> S (Coin 2)
            ),
      case makeWitnessVKey testTxbHash testKey1 of
        w@(WitVKey vk _sig) ->
          checkEncodingCBORAnnotated
            "vkey_witnesses"
            w -- Transaction _witnessVKeySet element
            ( T (TkListLen 2)
                <> S vk -- vkey
                <> T testKey1SigToken -- signature
            ),
      checkEncodingCBOR
        "script_hash_to_scripts"
        (Map.singleton (hashScript testScript :: ScriptHash) testScript) -- Transaction _witnessMSigMap
        ( T (TkMapLen 1)
            <> S (hashScript testScript :: ScriptHash)
            <> S testScript
        ),
      -- checkEncodingCBOR "withdrawal_key"
      let r = (RewardAcnt Testnet testStakeCred)
       in checkEncodingCBOR
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      -- checkEncodingCBOR "withdrawal_script"
      --
      let r = RewardAcnt Testnet (ScriptHashObj testScriptHash)
       in checkEncodingCBOR
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      checkEncodingCBOR
        "register_stake_reference"
        (DCertDeleg (RegKey testStakeCred))
        ( T (TkListLen 2)
            <> T (TkWord 0) -- Reg cert
            <> S testStakeCred -- keyhash
        ),
      checkEncodingCBOR
        "deregister_stake_reference"
        (DCertDeleg (DeRegKey testStakeCred))
        ( T (TkListLen 2)
            <> T (TkWord 1) -- DeReg cert
            <> S testStakeCred -- keyhash
        ),
      checkEncodingCBOR
        "stake_delegation"
        (DCertDeleg (Delegate (Delegation testStakeCred (coerceKeyRole testKeyHash2))))
        ( T
            ( TkListLen 3
                . TkWord 2 -- delegation cert with key
            )
            <> S testStakeCred
            <> S testKeyHash2
        ),
      -- checkEncodingCBOR "register-pool"
      let poolOwner = testKeyHash2
          poolMargin = unsafeMkUnitInterval 0.7
          poolRAcnt = RewardAcnt Testnet testStakeCred
          poolPledge = Coin 11
          poolCost = Coin 55
          poolUrl = "pool.io"
          poolMDHash = BS.pack "{}"
          ipv4 = toIPv4 [127, 0, 0, 1]
          ipv4Bytes = ipv4ToBytes . toIPv4 $ [127, 0, 0, 1]
          poolRelays =
            StrictSeq.fromList
              [ SingleHostAddr SNothing (SJust ipv4) SNothing,
                SingleHostName SNothing $ Maybe.fromJust $ textToDns "singlehost.relay.com",
                MultiHostName (SJust 42) $ Maybe.fromJust $ textToDns "multihost.relay.com"
              ]
       in checkEncodingCBOR
            "register_pool"
            ( DCertPool
                ( RegPool
                    ( PoolParams
                        { _poolPubKey = coerceKeyRole testKeyHash1,
                          _poolVrf = testVRFKH,
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
                <> S testKeyHash1 -- operator
                <> S testVRFKH -- vrf keyhash
                <> S poolPledge -- pledge
                <> S poolCost -- cost
                <> S poolMargin -- margin
                <> S poolRAcnt -- reward acct
                <> T (TkListLen 1)
                <> S poolOwner -- owners
                <> T (TkListLen 3) -- relays
                <> T (TkListLen 4 . TkWord 0 . TkNull . TkBytes ipv4Bytes . TkNull)
                <> T (TkListLen 3 . TkWord 1 . TkNull . TkString ("singlehost.relay.com"))
                <> T (TkListLen 3 . TkWord 2 . (TkWord 42) . TkString ("multihost.relay.com"))
                <> T (TkListLen 2) -- metadata present
                <> S poolUrl -- metadata url
                <> S poolMDHash -- metadata hash
            ),
      checkEncodingCBOR
        "retire_pool"
        (DCertPool (RetirePool (coerceKeyRole testKeyHash1) (EpochNo 1729)))
        ( T
            ( TkListLen 3
                . TkWord 4 -- Pool Retire
            )
            <> S testKeyHash1 -- key hash
            <> S (EpochNo 1729) -- epoch
        ),
      checkEncodingCBOR
        "genesis_delegation"
        (DCertGenesis (GenesisDelegCert testGKeyHash (coerceKeyRole testKeyHash1) testVRFKH))
        ( T
            ( TkListLen 4
                . TkWord 5 -- genesis delegation cert
            )
            <> S testGKeyHash -- delegator credential
            <> S testKeyHash1 -- delegatee key hash
            <> S testVRFKH -- delegatee vrf key hash
        ),
      -- checkEncodingCBOR "mir"
      let rws = Map.singleton testStakeCred 77
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
              _keyMinRefund = SNothing,
              _keyDecayRate = SNothing,
              _poolDeposit = SNothing,
              _poolMinRefund = SNothing,
              _poolDecayRate = SNothing,
              _eMax = SNothing,
              _nOpt = SNothing,
              _a0 = SNothing,
              _rho = SNothing,
              _tau = SNothing,
              _d = SNothing,
              _extraEntropy = SNothing,
              _protocolVersion = SNothing,
              _minUTxOValue = SNothing
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
          keyminrefund = UnsafeUnitInterval $ 1 % 2
          keydecayrate = 1 % 3
          pooldeposit = Coin 6
          poolminrefund = UnsafeUnitInterval $ 1 % 4
          pooldecayrate = 1 % 5
          emax = EpochNo 7
          nopt = 8
          a0 = 1 % 6
          rho = UnsafeUnitInterval $ 1 % 6
          tau = UnsafeUnitInterval $ 1 % 7
          d = UnsafeUnitInterval $ 1 % 9
          extraEntropy = NeutralNonce
          protocolVersion = ProtVer 0 1
          minUTxOValue = 121
       in checkEncodingCBOR
            "pparams_update_all"
            ( PParams
                { _minfeeA = SJust minfeea,
                  _minfeeB = SJust minfeeb,
                  _maxBBSize = SJust maxbbsize,
                  _maxTxSize = SJust maxtxsize,
                  _maxBHSize = SJust maxbhsize,
                  _keyDeposit = SJust keydeposit,
                  _keyMinRefund = SJust keyminrefund,
                  _keyDecayRate = SJust keydecayrate,
                  _poolDeposit = SJust pooldeposit,
                  _poolMinRefund = SJust poolminrefund,
                  _poolDecayRate = SJust pooldecayrate,
                  _eMax = SJust emax,
                  _nOpt = SJust nopt,
                  _a0 = SJust a0,
                  _rho = SJust rho,
                  _tau = SJust tau,
                  _d = SJust d,
                  _extraEntropy = SJust extraEntropy,
                  _protocolVersion = SJust protocolVersion,
                  _minUTxOValue = SJust minUTxOValue
                } ::
                PParamsUpdate
            )
            ( (T $ TkMapLen 20)
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
                <> S keyminrefund
                <> (T $ TkWord 7 . TkTag 30)
                <> S keydecayrate
                <> (T $ TkWord 8)
                <> S pooldeposit
                <> (T $ TkWord 9)
                <> S poolminrefund
                <> (T $ TkWord 10 . TkTag 30)
                <> S pooldecayrate
                <> (T $ TkWord 11)
                <> S emax
                <> (T $ TkWord 12)
                <> S nopt
                <> (T $ TkWord 13 . TkTag 30)
                <> S a0
                <> (T $ TkWord 14)
                <> S rho
                <> (T $ TkWord 15)
                <> S tau
                <> (T $ TkWord 16)
                <> S d
                <> (T $ TkWord 17)
                <> S extraEntropy
                <> (T $ TkWord 18)
                <> S protocolVersion
                <> (T $ TkWord 19)
                <> S minUTxOValue
            ),
      -- checkEncodingCBOR "full_update"
      let ppup =
            ProposedPPUpdates
              ( Map.singleton
                  testGKeyHash
                  ( PParams
                      { _minfeeA = SNothing,
                        _minfeeB = SNothing,
                        _maxBBSize = SNothing,
                        _maxTxSize = SNothing,
                        _maxBHSize = SNothing,
                        _keyDeposit = SNothing,
                        _keyMinRefund = SNothing,
                        _keyDecayRate = SNothing,
                        _poolDeposit = SNothing,
                        _poolMinRefund = SNothing,
                        _poolDecayRate = SNothing,
                        _eMax = SNothing,
                        _nOpt = SJust 100,
                        _a0 = SNothing,
                        _rho = SNothing,
                        _tau = SNothing,
                        _d = SNothing,
                        _extraEntropy = SNothing,
                        _protocolVersion = SNothing,
                        _minUTxOValue = SNothing
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
          tout = TxOut testAddrE (Coin 2)
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
          tout = TxOut testAddrE (Coin 2)
          ra = RewardAcnt Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      testGKeyHash
                      ( PParams
                          { _minfeeA = SNothing,
                            _minfeeB = SNothing,
                            _maxBBSize = SNothing,
                            _maxTxSize = SNothing,
                            _maxBHSize = SNothing,
                            _keyDeposit = SNothing,
                            _keyMinRefund = SNothing,
                            _keyDecayRate = SNothing,
                            _poolDeposit = SNothing,
                            _poolMinRefund = SNothing,
                            _poolDecayRate = SNothing,
                            _eMax = SNothing,
                            _nOpt = SJust 100,
                            _a0 = SNothing,
                            _rho = SNothing,
                            _tau = SNothing,
                            _d = SNothing,
                            _extraEntropy = SNothing,
                            _protocolVersion = SNothing,
                            _minUTxOValue = SNothing
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
          tout = TxOut testAddrE (Coin 2)
          reg = DCertDeleg (RegKey testStakeCred)
          ra = RewardAcnt Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      testGKeyHash
                      ( PParams
                          { _minfeeA = SNothing,
                            _minfeeB = SNothing,
                            _maxBBSize = SNothing,
                            _maxTxSize = SNothing,
                            _maxBHSize = SNothing,
                            _keyDeposit = SNothing,
                            _keyMinRefund = SNothing,
                            _keyDecayRate = SNothing,
                            _poolDeposit = SNothing,
                            _poolMinRefund = SNothing,
                            _poolDecayRate = SNothing,
                            _eMax = SNothing,
                            _nOpt = SJust 100,
                            _a0 = SNothing,
                            _rho = SNothing,
                            _tau = SNothing,
                            _d = SNothing,
                            _extraEntropy = SNothing,
                            _protocolVersion = SNothing,
                            _minUTxOValue = SNothing
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
              (StrictSeq.singleton $ TxOut testAddrE (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashTxBody txb
          w = makeWitnessVKey txbh testKey1
       in checkEncodingCBORAnnotated
            "tx_min"
            (Tx txb (Set.singleton w) Map.empty SNothing)
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
              (StrictSeq.singleton $ TxOut testAddrE (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashTxBody txb
          w = makeWitnessVKey txbh testKey1
          s = Map.singleton (hashScript testScript) testScript
          md = MD.MetaData $ Map.singleton 17 (MD.I 42)
       in checkEncodingCBORAnnotated
            "tx_full"
            (Tx txb (Set.singleton w) s (SJust md))
            ( T (TkListLen 3)
                <> S txb
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 1)
                <> S w
                <> T (TkWord 1)
                <> T (TkListLen 1)
                <> S testScript
                <> S md
            ),
      -- checkEncodingCBOR "block_header_body"
      let prevhash = BlockHash testHeaderHash
          vrfVkey = snd testVRF
          slot = SlotNo 33
          nonce = mkSeed seedEta (SlotNo 33) (mkNonce 0)
          nonceProof = coerce $ mkCertifiedVRF (WithResult nonce 1) (fst testVRF)
          leaderValue = mkSeed seedL (SlotNo 33) (mkNonce 0)
          leaderProof = coerce $ mkCertifiedVRF (WithResult leaderValue 1) (fst testVRF)
          size = 0
          blockNo = BlockNo 44
          bbhash = bbHash $ TxSeq StrictSeq.empty
          ocert =
            OCert
              (snd testKESKeys)
              0
              (KESPeriod 0)
              (signedDSIGN @ConcreteCrypto (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0))
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
          signature = signedDSIGN @ConcreteCrypto (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0)
       in checkEncodingCBORCBORGroup
            "operational_cert"
            (OCert @ConcreteCrypto vkHot counter kesperiod signature)
            ( S vkHot
                <> S counter
                <> S kesperiod
                <> T testOpCertSigTokens
            ),
      -- checkEncodingCBOR "block_header"
      let sig = signedKES () 0 testBHB (fst testKESKeys)
       in checkEncodingCBORAnnotated
            "block_header"
            (BHeader testBHB sig)
            ( (T $ TkListLen 2)
                <> S testBHB
                <> T testBHBSigTokens
            ),
      -- checkEncodingCBOR "empty_block"
      let sig = signedKES () 0 testBHB (fst testKESKeys)
          bh = BHeader testBHB sig
          txns = TxSeq StrictSeq.Empty
       in checkEncodingCBORAnnotated
            "empty_block"
            (Block bh txns)
            ( (T $ TkListLen 4)
                <> S bh
                <> T (TkListLen 0 . TkListLen 0 . TkMapLen 0)
            ),
      -- checkEncodingCBOR "rich_block"
      let sig = signedKES () 0 testBHB (fst testKESKeys)
          bh = BHeader testBHB sig
          tin = Set.fromList [TxIn genesisId 1]
          tout = StrictSeq.singleton $ TxOut testAddrE (Coin 2)
          txb s = TxBody tin tout StrictSeq.empty (Wdrl Map.empty) (Coin 9) (SlotNo s) SNothing SNothing
          txb1 = txb 500
          txb2 = txb 501
          txb3 = txb 502
          txb4 = txb 503
          txb5 = txb 504
          w1 = makeWitnessVKey (hashTxBody txb1) testKey1
          w2 = makeWitnessVKey (hashTxBody txb1) testKey2
          ws = Set.fromList [w1, w2]
          tx1 = Tx txb1 (Set.singleton w1) mempty SNothing
          tx2 = Tx txb2 ws mempty SNothing
          tx3 = Tx txb3 mempty (Map.singleton (hashScript testScript) testScript) SNothing
          ss =
            Map.fromList
              [ (hashScript testScript, testScript),
                (hashScript testScript2, testScript2)
              ]
          tx4 = Tx txb4 mempty ss SNothing
          tx5MD = MD.MetaData $ Map.singleton 17 (MD.I 42)
          tx5 = Tx txb5 ws ss (SJust tx5MD)
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
                <> S testScript
                -- tx 4, two scripts
                <> T (TkMapLen 1 . TkWord 1)
                <> T (TkListLen 2)
                <> S testScript
                <> S testScript2
                -- tx 5, two keys and two scripts
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 2)
                <> S w2
                <> S w1
                <> T (TkWord 1)
                <> T (TkListLen 2)
                <> S testScript
                <> S testScript2
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
          bs = Map.singleton (coerceKeyRole testKeyHash1) n
       in checkEncodingCBOR
            "blocks_made"
            (BlocksMade bs)
            ( T (TkMapLen 1)
                <> S testKeyHash1
                <> S n
            ),
      checkEncodingCBOR
        "account_state"
        (AccountState (Coin 1) (Coin 2))
        ( T (TkListLen 2)
            <> S (Coin 1)
            <> S (Coin 2)
        ),
      let stk = Map.singleton testStakeCred (Coin 13)
       in checkEncodingCBOR
            "stake"
            (Stake stk)
            ( T (TkMapLen 1)
                <> S testStakeCred
                <> S (Coin 13)
            ),
      let mark =
            SnapShot
              (Stake $ Map.singleton testStakeCred (Coin 11))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          set =
            SnapShot
              (Stake $ Map.singleton (KeyHashObj testKeyHash2) (Coin 22))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          go =
            SnapShot
              (Stake $ Map.singleton testStakeCred (Coin 33))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          p =
            PoolParams
              { _poolPubKey = coerceKeyRole testKeyHash1,
                _poolVrf = testVRFKH,
                _poolPledge = Coin 5,
                _poolCost = Coin 4,
                _poolMargin = unsafeMkUnitInterval 0.7,
                _poolRAcnt = RewardAcnt Testnet testStakeCred,
                _poolOwners = Set.singleton testKeyHash2,
                _poolRelays = StrictSeq.empty,
                _poolMD =
                  SJust $
                    PoolMetaData
                      { _poolMDUrl = Maybe.fromJust $ textToUrl "web.site",
                        _poolMDHash = BS.pack "{}"
                      }
              }
          ps = Map.singleton (coerceKeyRole testKeyHash1) p
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
              (Stake $ Map.singleton testStakeCred (Coin 11))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          set =
            SnapShot
              (Stake $ Map.singleton (KeyHashObj testKeyHash2) (Coin 22))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          go =
            SnapShot
              (Stake $ Map.singleton testStakeCred (Coin 33))
              (Map.singleton testStakeCred (coerceKeyRole testKeyHash3))
              ps
          p =
            PoolParams
              { _poolPubKey = coerceKeyRole testKeyHash1,
                _poolVrf = testVRFKH,
                _poolPledge = Coin 5,
                _poolCost = Coin 4,
                _poolMargin = unsafeMkUnitInterval 0.7,
                _poolRAcnt = RewardAcnt Testnet testStakeCred,
                _poolOwners = Set.singleton testKeyHash2,
                _poolRelays = StrictSeq.empty,
                _poolMD =
                  SJust $
                    PoolMetaData
                      { _poolMDUrl = Maybe.fromJust $ textToUrl "web.site",
                        _poolMDHash = BS.pack "{}"
                      }
              }
          ps = Map.singleton (coerceKeyRole testKeyHash1) p
          fs = Coin 123
          ss = SnapShots mark set go fs
          ls = emptyLedgerState
          pps = emptyPParams
          bs = Map.singleton (coerceKeyRole testKeyHash1) 1
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
              StrictMaybe RewardUpdate
          pd = (PoolDistr Map.empty) :: PoolDistr
          os = Map.singleton (SlotNo 1) (ActiveSlot testGKeyHash)
          compactOs = Map.singleton (ActiveSlot testGKeyHash) (SlotNo 1 :| [])
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
