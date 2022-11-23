{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Golden tests that check CBOR token encoding.
module Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding (tests) where

import qualified Cardano.Crypto.Hash as Monomorphic
import Cardano.Crypto.KES (SignedKES)
import Cardano.Crypto.VRF (CertifiedVRF)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    BoundedRational (..),
    Network (..),
    Nonce (..),
    ProtVer (..),
    StrictMaybe (..),
    UnitInterval,
    mkNonceFromNumber,
    mkTxIxPartial,
    textToDns,
    textToUrl,
  )
import Cardano.Ledger.Binary
  ( Annotator,
    Decoder,
    DecoderError,
    FromCBOR (..),
    FromCBORGroup (..),
    ToCBOR (..),
    ToCBORGroup (..),
    Tokens (..),
    byronProtVer,
    decodeFullAnnotator,
    decodeFullDecoder,
    decodeMapTraverse,
    fromNotSharedCBOR,
    hashWithEncoder,
    ipv4ToBytes,
    serialize',
    shelleyProtVer,
    toCBOR,
    toPlainEncoding,
  )
import Cardano.Ledger.Binary.Crypto
  ( encodeSignedDSIGN,
    encodeVerKeyDSIGN,
  )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Core (EraTx, Tx, hashScript, hashTxAuxData)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.EpochBoundary
  ( SnapShot (..),
    SnapShots (..),
    Stake (..),
  )
import Cardano.Ledger.Era (EraCrypto (..))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    SignKeyKES,
    SignKeyVRF,
    SignedDSIGN,
    VKey (..),
    VerKeyKES,
    VerKeyVRF,
    asWitness,
    encodeSignedKES,
    hashKey,
    hashVerKeyVRF,
    sKey,
    signedDSIGN,
    signedKES,
    vKey,
  )
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash, hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
  ( MultiSig,
    ScriptHash,
  )
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..), bbHash)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( pattern DeRegKey,
    pattern Delegate,
    pattern GenesisDelegCert,
    pattern MIRCert,
    pattern RegKey,
    pattern RegPool,
    pattern RetirePool,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    EpochState (..),
    NewEpochState (..),
    PulsingRewUpdate (Complete),
    RewardUpdate (..),
  )
import qualified Cardano.Ledger.Shelley.Metadata as MD
import Cardano.Ledger.Shelley.PParams
  ( ShelleyPParamsHKD (..),
    ShelleyPParamsUpdate,
    emptyPParams,
    pattern ProposedPPUpdates,
    pattern Update,
  )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxBody
  ( MIRPot (..),
    MIRTarget (..),
    PoolMetadata (..),
    ShelleyTxBody (..),
    ShelleyTxOut (..),
    StakePoolRelay (..),
    Wdrl (..),
    WitVKey (..),
    pmHash,
    pmUrl,
    ppCost,
    ppId,
    ppMargin,
    ppMetadata,
    ppOwners,
    ppPledge,
    ppRelays,
    ppRewardAcnt,
    ppVrf,
    pattern DCertDeleg,
    pattern DCertGenesis,
    pattern DCertMir,
    pattern DCertPool,
    pattern Delegation,
    pattern PoolParams,
    pattern RewardAcnt,
  )
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits, addrWits, scriptWits)
import Cardano.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Cardano.Ledger.UTxO (UTxO (UTxO), makeWitnessVKey)
import Cardano.Protocol.TPraos.BHeader
  ( BHBody (..),
    BHeader (..),
    HashHeader (..),
    PrevHash (..),
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
  )
import Cardano.Protocol.TPraos.OCert
  ( KESPeriod (..),
    OCert,
    OCertSignable (..),
    pattern OCert,
  )
import qualified Codec.CBOR.Encoding as CBOR (Encoding (..))
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Data.Coerce (coerce)
import Data.Default.Class (def)
import Data.IP (toIPv4)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromJust)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.String (fromString)
import Numeric.Natural (Natural)
import Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (CBORBytes), diffExpr)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto, ExMock, Mock)
import Test.Cardano.Ledger.Shelley.Examples.Consensus as Ex (ledgerExamplesShelley, sleNewEpochState)
import Test.Cardano.Ledger.Shelley.Generator.Core (PreAlonzo)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils
  ( ToTokens (..),
    checkEncoding,
    checkEncodingCBOR,
    checkEncodingCBORAnnotated,
  )
import Test.Cardano.Ledger.Shelley.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- ============================================

type MultiSigMap = Map.Map (ScriptHash C_Crypto) (MultiSig (ShelleyEra C_Crypto))

decodeMultiSigMap :: Decoder s (Annotator MultiSigMap)
decodeMultiSigMap = decodeMapTraverse (pure <$> fromCBOR) fromCBOR

deserializeMultiSigMap :: BSL.ByteString -> Either DecoderError MultiSigMap
deserializeMultiSigMap =
  decodeFullAnnotator shelleyProtVer "Map ScriptHash MultiSig" decodeMultiSigMap

checkEncodingCBORCBORGroup ::
  (FromCBORGroup a, ToCBORGroup a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORCBORGroup name x t =
  let d = decodeFullDecoder shelleyProtVer (fromString name) fromCBORGroup
   in checkEncoding shelleyProtVer toCBORGroup d name x t

getRawKeyHash :: KeyHash 'Payment h -> ByteString
getRawKeyHash (KeyHash hsh) = Monomorphic.hashToBytes hsh

getRawNonce :: Nonce -> ByteString
getRawNonce (Nonce hsh) = Monomorphic.hashToBytes hsh
getRawNonce NeutralNonce = error "The neutral nonce has no bytes"

testGKey :: CC.Crypto c => GenesisKeyPair c
testGKey = KeyPair vk sk
  where
    (sk, vk) = mkGenKey (RawSeed 0 0 0 0 0)

testGKeyHash :: CC.Crypto c => KeyHash 'Genesis c
testGKeyHash = (hashKey . vKey) testGKey

testVRF :: CC.Crypto c => (SignKeyVRF c, VerKeyVRF c)
testVRF = mkVRFKeyPair (RawSeed 0 0 0 0 5)

testVRFKH :: forall c. CC.Crypto c => Hash c (VerKeyVRF c)
testVRFKH = hashVerKeyVRF $ snd (testVRF @c)

testTxb :: ShelleyTest era => ShelleyTxBody era
testTxb =
  ShelleyTxBody
    Set.empty
    StrictSeq.empty
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 0)
    SNothing
    SNothing

testTxbHash ::
  forall era.
  ShelleyTest era =>
  SafeHash (EraCrypto era) EraIndependentTxBody
testTxbHash = hashAnnotated $ testTxb @era

testKey1 :: CC.Crypto c => KeyPair 'Payment c
testKey1 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

testKey2 :: CC.Crypto c => KeyPair kr c
testKey2 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 2)

testBlockIssuerKey :: CC.Crypto c => KeyPair 'BlockIssuer c
testBlockIssuerKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 4)

testStakePoolKey :: CC.Crypto c => KeyPair 'StakePool c
testStakePoolKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 5)

testGenesisDelegateKey ::
  CC.Crypto c =>
  KeyPair 'GenesisDelegate c
testGenesisDelegateKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 6)

testBlockIssuerKeyTokens :: Tokens -> Tokens
testBlockIssuerKeyTokens = e
  where
    VKey vk = vKey (testBlockIssuerKey @C_Crypto)
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeVerKeyDSIGN vk)

testKey1SigToken ::
  forall era.
  (ShelleyTest era, Mock (EraCrypto era)) =>
  Tokens ->
  Tokens
testKey1SigToken = e
  where
    s =
      signedDSIGN @(EraCrypto era)
        (sKey $ testKey1 @(EraCrypto era))
        (extractHash (testTxbHash @era)) ::
        SignedDSIGN (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedDSIGN s)

testOpCertSigTokens ::
  forall c.
  (Mock c) =>
  Tokens ->
  Tokens
testOpCertSigTokens = e
  where
    s =
      signedDSIGN @c
        (sKey $ testKey1 @c)
        (OCertSignable @c (snd $ testKESKeys @c) 0 (KESPeriod 0))
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedDSIGN s)

testKeyHash1 :: CC.Crypto c => KeyHash 'Payment c
testKeyHash1 = (hashKey . vKey) testKey1

testKeyHash2 :: CC.Crypto c => KeyHash 'Staking c
testKeyHash2 = (hashKey . vKey) testKey2

testKESKeys :: CC.Crypto c => (SignKeyKES c, VerKeyKES c)
testKESKeys = mkKESKeyPair (RawSeed 0 0 0 0 3)

testAddrE :: CC.Crypto c => Addr c
testAddrE =
  Addr
    Testnet
    (KeyHashObj testKeyHash1)
    StakeRefNull

testPayCred :: forall c. CC.Crypto c => Credential 'Payment c
testPayCred = KeyHashObj (testKeyHash1 @c)

testStakeCred :: forall c. CC.Crypto c => Credential 'Staking c
testStakeCred = KeyHashObj $ testKeyHash2 @c

testScript :: forall c. CC.Crypto c => MultiSig (ShelleyEra c)
testScript = RequireSignature $ asWitness (testKeyHash1 @c)

testScriptHash :: forall c. CC.Crypto c => ScriptHash c
testScriptHash = hashScript @(ShelleyEra c) testScript

testScript2 :: forall c. CC.Crypto c => MultiSig (ShelleyEra c)
testScript2 = RequireSignature $ asWitness (testKeyHash2 @c)

testHeaderHash ::
  forall c.
  CC.Crypto c =>
  HashHeader c
testHeaderHash =
  HashHeader $
    coerce
      (hashWithEncoder shelleyProtVer toCBOR 0 :: Hash c Int)

testBHB ::
  forall era c.
  ( EraTx era,
    PreAlonzo era,
    ExMock c,
    c ~ EraCrypto era,
    Tx era ~ ShelleyTx era
  ) =>
  BHBody c
testBHB =
  BHBody
    { bheaderBlockNo = BlockNo 44,
      bheaderSlotNo = SlotNo 33,
      bheaderPrev = BlockHash testHeaderHash,
      bheaderVk = vKey testBlockIssuerKey,
      bheaderVrfVk = snd $ testVRF @c,
      bheaderEta =
        mkCertifiedVRF
          ( WithResult
              (mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0))
              1
          )
          (fst $ testVRF @c),
      bheaderL =
        mkCertifiedVRF
          ( WithResult
              (mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0))
              1
          )
          (fst $ testVRF @c),
      bsize = 0,
      bhash = bbHash @era $ ShelleyTxSeq @era StrictSeq.empty,
      bheaderOCert =
        OCert
          (snd $ testKESKeys @c)
          0
          (KESPeriod 0)
          ( signedDSIGN @c
              (sKey $ testKey1 @c)
              (OCertSignable (snd $ testKESKeys @c) 0 (KESPeriod 0))
          ),
      bprotver = ProtVer minBound 0
    }

testBHBSigTokens ::
  forall era.
  ( EraTx era,
    PreAlonzo era,
    ExMock (EraCrypto era),
    Tx era ~ ShelleyTx era
  ) =>
  Tokens ->
  Tokens
testBHBSigTokens = e
  where
    s =
      signedKES @(CC.KES (EraCrypto era))
        ()
        0
        (testBHB @era)
        (fst $ testKESKeys @(EraCrypto era))
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedKES s)

tests :: TestTree
tests =
  testGroup
    "CBOR Serialization Tests (Encoding)"
    [ checkEncodingCBOR
        byronProtVer
        "list (Byron)"
        ([1] :: [Integer])
        (T (TkListBegin . TkInteger 1 . TkBreak)),
      checkEncodingCBOR
        shelleyProtVer
        "list (Shelley)"
        ([1] :: [Integer])
        (T (TkListLen 1 . TkInteger 1)),
      checkEncodingCBOR
        byronProtVer
        "set (Byron)"
        (Set.singleton (1 :: Integer))
        (T (TkTag 258 . TkListLen 1 . TkInteger 1)),
      checkEncodingCBOR
        shelleyProtVer
        "set (Shelley)"
        (Set.singleton (1 :: Integer))
        (T (TkListLen 1 . TkInteger 1)),
      checkEncodingCBOR
        shelleyProtVer
        "map"
        (Map.singleton (1 :: Integer) (1 :: Integer))
        (T (TkMapLen 1 . TkInteger 1 . TkInteger 1)),
      checkEncodingCBOR
        shelleyProtVer
        "coin"
        (Coin 30)
        (T (TkWord64 30)),
      checkEncodingCBOR
        shelleyProtVer
        "rational"
        (unsafeBoundRational (1 % 2) :: UnitInterval)
        (T (TkTag 30 . TkListLen 2 . TkWord64 1 . TkWord64 2)),
      checkEncodingCBOR
        shelleyProtVer
        "slot"
        (SlotNo 7)
        (T (TkWord64 7)),
      checkEncodingCBOR
        shelleyProtVer
        "neutral_nonce"
        NeutralNonce
        (T (TkListLen 1 . TkWord 0)),
      checkEncodingCBOR
        shelleyProtVer
        "nonce"
        (mkNonceFromNumber 99)
        (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonceFromNumber 99))),
      checkEncodingCBOR
        shelleyProtVer
        "key_hash"
        (testKeyHash1 @C_Crypto)
        (T (TkBytes (getRawKeyHash (testKeyHash1 @C_Crypto)))),
      checkEncodingCBOR
        shelleyProtVer
        "credential_key_hash"
        (testPayCred @C_Crypto)
        (T (TkListLen 2 . TkWord 0) <> S (testKeyHash1 @C_Crypto)),
      checkEncodingCBOR
        shelleyProtVer
        "txin"
        (TxIn @C_Crypto genesisId minBound)
        (T (TkListLen 2) <> S (genesisId :: TxId C_Crypto) <> T (TkWord64 0)),
      let a = Addr Testnet testPayCred StakeRefNull
       in checkEncodingCBOR
            shelleyProtVer
            "txout"
            (ShelleyTxOut @C a (Coin 2))
            ( T (TkListLen 2)
                <> S a
                <> S (Coin 2)
            ),
      case makeWitnessVKey @C_Crypto (testTxbHash @C) testKey1 of
        w@(WitVKey vk _sig) ->
          checkEncodingCBORAnnotated
            shelleyProtVer
            "vkey_witnesses"
            w -- Transaction _witnessVKeySet element
            ( T (TkListLen 2)
                <> S vk -- vkey
                <> T (testKey1SigToken @C) -- signature
            ),
      checkEncoding
        shelleyProtVer
        toCBOR
        deserializeMultiSigMap
        "script_hash_to_scripts"
        (Map.singleton (hashScript @C testScript) testScript) -- Transaction _witnessMSigMap
        ( T (TkMapLen 1)
            <> S (hashScript @C testScript)
            <> S (testScript @C_Crypto)
        ),
      -- checkEncodingCBOR "withdrawal_key"
      let r = RewardAcnt Testnet (testStakeCred @C_Crypto)
       in checkEncodingCBOR
            shelleyProtVer
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      -- checkEncodingCBOR "withdrawal_script"
      --
      let r = RewardAcnt Testnet (ScriptHashObj (testScriptHash @C_Crypto))
       in checkEncodingCBOR
            shelleyProtVer
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            ),
      checkEncodingCBOR
        shelleyProtVer
        "register_stake_reference"
        (DCertDeleg (RegKey (testStakeCred @C_Crypto)))
        ( T (TkListLen 2)
            <> T (TkWord 0) -- Reg cert
            <> S (testStakeCred @C_Crypto) -- keyhash
        ),
      checkEncodingCBOR
        shelleyProtVer
        "deregister_stake_reference"
        (DCertDeleg (DeRegKey (testStakeCred @C_Crypto)))
        ( T (TkListLen 2)
            <> T (TkWord 1) -- DeReg cert
            <> S (testStakeCred @C_Crypto) -- keyhash
        ),
      checkEncodingCBOR
        shelleyProtVer
        "stake_delegation"
        (DCertDeleg (Delegate (Delegation (testStakeCred @C_Crypto) (hashKey . vKey $ testStakePoolKey))))
        ( T
            ( TkListLen 3
                . TkWord 2 -- delegation cert with key
            )
            <> S (testStakeCred @C_Crypto)
            <> S (hashKey . vKey $ testStakePoolKey @C_Crypto)
        ),
      -- checkEncodingCBOR "register-pool"
      let poolOwner = testKeyHash2 @C_Crypto
          poolMargin = unsafeBoundRational 0.7
          poolRAcnt = RewardAcnt Testnet (testStakeCred @C_Crypto)
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
            shelleyProtVer
            "register_pool"
            ( DCertPool
                ( RegPool
                    ( PoolParams
                        { ppId = hashKey . vKey $ testStakePoolKey,
                          ppVrf = testVRFKH @C_Crypto,
                          ppPledge = poolPledge,
                          ppCost = poolCost,
                          ppMargin = poolMargin,
                          ppRewardAcnt = poolRAcnt,
                          ppOwners = Set.singleton poolOwner,
                          ppRelays = poolRelays,
                          ppMetadata =
                            SJust $
                              PoolMetadata
                                { pmUrl = Maybe.fromJust $ textToUrl poolUrl,
                                  pmHash = poolMDHash
                                }
                        }
                    )
                )
            )
            ( T (TkListLen 10)
                <> T (TkWord 3) -- Reg Pool
                <> S (hashKey . vKey $ testStakePoolKey @C_Crypto) -- operator
                <> S (testVRFKH @C_Crypto) -- vrf keyhash
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
        shelleyProtVer
        "retire_pool"
        ( DCertPool
            ( RetirePool @C_Crypto
                (hashKey . vKey $ testStakePoolKey @C_Crypto)
                (EpochNo 1729)
            )
        )
        ( T
            ( TkListLen 3
                . TkWord 4 -- Pool Retire
            )
            <> S (hashKey . vKey $ testStakePoolKey @C_Crypto) -- key hash
            <> S (EpochNo 1729) -- epoch
        ),
      checkEncodingCBOR
        shelleyProtVer
        "genesis_delegation"
        ( DCertGenesis
            ( GenesisDelegCert @C_Crypto
                testGKeyHash
                (hashKey . vKey $ testGenesisDelegateKey @C_Crypto)
                (testVRFKH @C_Crypto)
            )
        )
        ( T
            ( TkListLen 4
                . TkWord 5 -- genesis delegation cert
            )
            <> S (testGKeyHash @C_Crypto) -- delegator credential
            <> S (hashKey . vKey $ testGenesisDelegateKey @C_Crypto) -- delegatee key hash
            <> S (testVRFKH @C_Crypto) -- delegatee vrf key hash
        ),
      -- checkEncodingCBOR "mir"
      let rws = StakeAddressesMIR $ Map.singleton (testStakeCred @C_Crypto) (DeltaCoin 77)
       in checkEncodingCBOR
            shelleyProtVer
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
        shelleyProtVer
        "pparams_update_key_deposit_only"
        ( ShelleyPParams
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
            ShelleyPParamsUpdate C
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
          a0 = unsafeBoundRational $ 1 % 6
          rho = unsafeBoundRational $ 1 % 6
          tau = unsafeBoundRational $ 1 % 7
          d = unsafeBoundRational $ 1 % 9
          extraEntropy = NeutralNonce
          protocolVersion = ProtVer minBound 1
          minUTxOValue = Coin 121
          minPoolCost = Coin 987
       in checkEncodingCBOR
            shelleyProtVer
            "pparams_update_all"
            ( ShelleyPParams
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
                ShelleyPParamsUpdate C
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
                <> S (unboundRational a0)
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
            ProposedPPUpdates @C
              ( Map.singleton
                  (testGKeyHash @C_Crypto)
                  ( ShelleyPParams
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
            shelleyProtVer
            "full_update"
            (Update ppup e)
            ( T (TkListLen 2)
                <> S ppup
                <> S e
            ),
      -- checkEncodingCBOR "minimal_txn_body"
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "txbody"
            ( ShelleyTxBody @C -- minimal transaction body
                (Set.fromList [genesisTxIn1])
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
                <> S genesisTxIn1
                <> T (TkWord 1) -- Tx Outs
                <> T (TkListLen 1)
                <> S tout
                <> T (TkWord 2) -- Tx Fee
                <> T (TkWord64 9)
                <> T (TkWord 3) -- Tx TTL
                <> T (TkWord64 500)
            ),
      -- checkEncodingCBOR "transaction_mixed"
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
          ra = RewardAcnt Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      testGKeyHash
                      ( ShelleyPParams
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
            shelleyProtVer
            "txbody_partial"
            ( ShelleyTxBody @C -- transaction body with some optional components
                (Set.fromList [genesisTxIn1])
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
                <> S genesisTxIn1
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
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
          reg = DCertDeleg (RegKey (testStakeCred @C_Crypto))
          ra = RewardAcnt Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates
                  ( Map.singleton
                      testGKeyHash
                      ( ShelleyPParams
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
          mdh = hashTxAuxData @C $ MD.ShelleyTxAuxData $ Map.singleton 13 (MD.I 17)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "txbody_full"
            ( ShelleyTxBody @C -- transaction body with all components
                (Set.fromList [genesisTxIn1])
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
                <> S genesisTxIn1
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
            ShelleyTxBody @C
              (Set.fromList [TxIn genesisId (mkTxIxPartial 1)])
              (StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = (hashAnnotated txb)
          w = makeWitnessVKey @C_Crypto txbh testKey1
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "tx_min"
            ( ShelleyTx @(ShelleyEra C_Crypto)
                txb
                (mempty {addrWits = Set.singleton w} :: ShelleyTxWits C)
                SNothing
            )
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
            ShelleyTxBody @C
              (Set.fromList [genesisTxIn1])
              (StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2))
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashAnnotated txb
          w = makeWitnessVKey @C_Crypto txbh testKey1
          s = Map.singleton (hashScript @C testScript) (testScript @C_Crypto)
          txwits :: ShelleyTxWits C
          txwits = mempty {addrWits = Set.singleton w, scriptWits = s}
          md = (MD.ShelleyTxAuxData @C) $ Map.singleton 17 (MD.I 42)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "tx_full"
            (ShelleyTx @(ShelleyEra C_Crypto) txb txwits (SJust md))
            ( T (TkListLen 3)
                <> S txb
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 1)
                <> S w
                <> T (TkWord 1)
                <> T (TkListLen 1)
                <> S (testScript @C_Crypto)
                <> S md
            ),
      -- checkEncodingCBOR "block_header_body"
      let prevhash = BlockHash testHeaderHash
          vrfVkey = snd $ testVRF @C_Crypto
          slot = SlotNo 33
          nonce = mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0)
          nonceProof :: CertifiedVRF (CC.VRF C_Crypto) Nonce
          nonceProof =
            mkCertifiedVRF
              (WithResult nonce 1)
              (fst $ testVRF @C_Crypto)
          leaderValue = mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0)
          leaderProof :: CertifiedVRF (CC.VRF C_Crypto) Natural
          leaderProof =
            mkCertifiedVRF
              (WithResult leaderValue 1)
              (fst $ testVRF @C_Crypto)
          size = 0
          blockNo = BlockNo 44
          bbhash = bbHash @C $ ShelleyTxSeq StrictSeq.empty
          ocert :: OCert C_Crypto
          ocert =
            OCert
              (snd $ testKESKeys @C_Crypto)
              0
              (KESPeriod 0)
              ( signedDSIGN @C_Crypto
                  (sKey (testBlockIssuerKey @C_Crypto))
                  (OCertSignable (snd $ testKESKeys @C_Crypto) 0 (KESPeriod 0))
              )
          protover = ProtVer minBound 0
       in checkEncodingCBOR
            shelleyProtVer
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
      let vkHot = snd $ testKESKeys @C_Crypto
          counter = 0
          kesperiod = KESPeriod 0
          signature =
            signedDSIGN @C_Crypto
              (sKey $ testKey1 @C_Crypto)
              (OCertSignable (snd $ testKESKeys @C_Crypto) 0 (KESPeriod 0))
       in checkEncodingCBORCBORGroup
            "operational_cert"
            ( OCert @C_Crypto
                vkHot
                counter
                kesperiod
                signature
            )
            ( S vkHot
                <> S counter
                <> S kesperiod
                <> T (testOpCertSigTokens @C_Crypto)
            ),
      -- checkEncodingCBOR "block_header"
      let sig :: (SignedKES (CC.KES C_Crypto) (BHBody C_Crypto))
          sig = signedKES () 0 (testBHB @C) (fst $ testKESKeys @C_Crypto)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "block_header"
            (BHeader (testBHB @C) sig)
            ( (T $ TkListLen 2)
                <> S (testBHB @C)
                <> T (testBHBSigTokens @C)
            ),
      -- checkEncodingCBOR "empty_block"
      let sig :: (SignedKES (CC.KES C_Crypto) (BHBody C_Crypto))
          sig = signedKES () 0 (testBHB @C) (fst $ testKESKeys @C_Crypto)
          bh = BHeader (testBHB @C) sig
          txns = ShelleyTxSeq StrictSeq.Empty
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "empty_block"
            (Block @C bh txns)
            ( (T $ TkListLen 4)
                <> S bh
                <> T (TkListLen 0 . TkListLen 0 . TkMapLen 0)
            ),
      -- checkEncodingCBOR "rich_block"
      let sig :: (SignedKES (CC.KES C_Crypto) (BHBody C_Crypto))
          sig = signedKES () 0 (testBHB @C) (fst $ testKESKeys @C_Crypto)
          bh = BHeader (testBHB @C) sig
          tout = StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2)
          txb s =
            ShelleyTxBody @C
              (Set.fromList [genesisTxIn1])
              tout
              StrictSeq.empty
              (Wdrl Map.empty)
              (Coin 9)
              (SlotNo s)
              SNothing
              SNothing
          txb1 = txb 500
          txb2 = txb 501
          txb3 = txb 502
          txb4 = txb 503
          txb5 = txb 504
          w1 = makeWitnessVKey (hashAnnotated txb1) testKey1
          w2 = makeWitnessVKey (hashAnnotated txb1) testKey2
          ws = Set.fromList [w1, w2]
          tx1 = ShelleyTx @C txb1 mempty {addrWits = Set.singleton w1} SNothing
          tx2 = ShelleyTx @C txb2 mempty {addrWits = ws} SNothing
          tx3 =
            ShelleyTx @C
              txb3
              mempty
                { scriptWits =
                    Map.singleton (hashScript @C testScript) testScript
                }
              SNothing
          ss =
            Map.fromList
              [ (hashScript @C testScript, testScript),
                (hashScript @C testScript2, testScript2)
              ]
          tx4 = ShelleyTx txb4 mempty {scriptWits = ss} SNothing
          tx5MD = MD.ShelleyTxAuxData @C $ Map.singleton 17 (MD.I 42)
          tx5 = ShelleyTx txb5 mempty {addrWits = ws, scriptWits = ss} (SJust tx5MD)
          txns = ShelleyTxSeq $ StrictSeq.fromList [tx1, tx2, tx3, tx4, tx5]
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "rich_block"
            (Block @C bh txns)
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
                <> S (testScript @C_Crypto)
                -- tx 4, two scripts
                <> T (TkMapLen 1 . TkWord 1)
                <> T (TkListLen 2)
                <> S (testScript2 @C_Crypto)
                <> S (testScript @C_Crypto)
                -- tx 5, two keys and two scripts
                <> T (TkMapLen 2)
                <> T (TkWord 0)
                <> T (TkListLen 2)
                <> S w2
                <> S w1
                <> T (TkWord 1)
                <> T (TkListLen 2)
                <> S (testScript2 @C_Crypto)
                <> S (testScript @C_Crypto)
                -- metadata
                <> T (TkMapLen 1)
                <> T (TkInt 4)
                <> S tx5MD
            ),
      checkEncodingCBOR
        shelleyProtVer
        "epoch"
        (EpochNo 13)
        (T (TkWord64 13)),
      let n = (17 :: Natural)
          bs = Map.singleton (hashKey . vKey $ testStakePoolKey @C_Crypto) n
       in checkEncodingCBOR
            shelleyProtVer
            "blocks_made"
            (BlocksMade bs)
            ( T (TkMapLen 1)
                <> S (hashKey . vKey $ testStakePoolKey @C_Crypto)
                <> S n
            ),
      checkEncodingCBOR
        shelleyProtVer
        "account_state"
        (AccountState (Coin 1) (Coin 2))
        ( T (TkListLen 2)
            <> S (Coin 1)
            <> S (Coin 2)
        ),
      let stk = [(testStakeCred @C_Crypto, CompactCoin 13)]
       in checkEncodingCBOR
            shelleyProtVer
            "stake"
            (Stake stk)
            ( T (TkMapLen 1)
                <> S (testStakeCred @C_Crypto)
                <> S (Coin 13)
            ),
      let mark =
            SnapShot
              (Stake [(testStakeCred @C_Crypto, CompactCoin 11)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          set =
            SnapShot
              (Stake [(KeyHashObj testKeyHash2, CompactCoin 22)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          go =
            SnapShot
              (Stake [(testStakeCred @C_Crypto, CompactCoin 33)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          params =
            PoolParams
              { ppId = hashKey $ vKey testStakePoolKey,
                ppVrf = testVRFKH @C_Crypto,
                ppPledge = Coin 5,
                ppCost = Coin 4,
                ppMargin = unsafeBoundRational 0.7,
                ppRewardAcnt = RewardAcnt Testnet (testStakeCred @C_Crypto),
                ppOwners = Set.singleton testKeyHash2,
                ppRelays = StrictSeq.empty,
                ppMetadata =
                  SJust $
                    PoolMetadata
                      { pmUrl = Maybe.fromJust $ textToUrl "web.site",
                        pmHash = BS.pack "{}"
                      }
              }
          ps = [(hashKey $ vKey testStakePoolKey, params)]
          fs = Coin 123
       in checkEncodingCBOR
            shelleyProtVer
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
              (Stake [(testStakeCred @C_Crypto, CompactCoin 11)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          set =
            SnapShot
              (Stake [(KeyHashObj testKeyHash2, CompactCoin 22)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          go =
            SnapShot
              (Stake [(testStakeCred @C_Crypto, CompactCoin 33)])
              [(testStakeCred @C_Crypto, hashKey $ vKey testStakePoolKey)]
              ps
          params =
            PoolParams
              { ppId = hashKey $ vKey testStakePoolKey,
                ppVrf = testVRFKH @C_Crypto,
                ppPledge = Coin 5,
                ppCost = Coin 4,
                ppMargin = unsafeBoundRational 0.7,
                ppRewardAcnt = RewardAcnt Testnet (testStakeCred @C_Crypto),
                ppOwners = Set.singleton testKeyHash2,
                ppRelays = StrictSeq.empty,
                ppMetadata =
                  SJust $
                    PoolMetadata
                      { pmUrl = Maybe.fromJust $ textToUrl "web.site",
                        pmHash = BS.pack "{}"
                      }
              }
          ps = [(hashKey $ vKey testStakePoolKey, params)]
          fs = Coin 123
          ss = SnapShots mark set go fs
          ls = def
          pps = emptyPParams
          bs = Map.singleton (hashKey $ vKey testStakePoolKey) 1
          nm = def
          es = EpochState @C ac ss ls pps pps nm
          ru =
            ( Complete $
                RewardUpdate
                  { deltaT = DeltaCoin 100,
                    deltaR = DeltaCoin (-200),
                    rs = Map.empty,
                    deltaF = DeltaCoin (-10),
                    nonMyopic = nm
                  }
            )
          pd = PoolDistr @C_Crypto Map.empty
          nes =
            NewEpochState
              e
              (BlocksMade bs)
              (BlocksMade bs)
              es
              (SJust ru)
              pd
              (UTxO mempty)
       in checkEncodingCBOR
            shelleyProtVer
            "new_epoch_state"
            nes
            ( T (TkListLen 7)
                <> S e
                <> S (BlocksMade @C_Crypto bs)
                <> S (BlocksMade @C_Crypto bs)
                <> S es
                <> S (SJust ru)
                <> S pd
                <> S (UTxO @(ShelleyEra C_Crypto) mempty)
            ),
      let actual =
            serialize' shelleyProtVer $
              Ex.sleNewEpochState Ex.ledgerExamplesShelley
          expected = either error id $ B16.decode expectedHex
          actualHex = B16.encode actual
          expectedHex =
            "8700a1581ce0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b082541"
              <> "0aa1581ca646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a03"
              <> "86821927101903e8828283a0a0a08482a0a0a0a084a0a0000085a1825820ee155a"
              <> "ce9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e2500825839"
              <> "00cb9358529df4729c3246a2a033cb9821abbfd16de4888005904abc410d6a577e"
              <> "9441ad8ed9663931906e4d43ece8f82c712b1d0235affb060a1903e80182a0a082"
              <> "a0a08483a0a0a083a0a0a083a0a0a00092000000190800000000001864d81e8200"
              <> "01d81e820001d81e820001d81e8200018100020000009200000019080000000000"
              <> "1864d81e820001d81e820001d81e820001d81e82000181000200010082a0008183"
              <> "00880082020082a000000000a0a0840185a0803903ba820200a0a082a0a0a1581c"
              <> "e0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b0825418282010158"
              <> "20c5e21ab1c9f6022d81c3b25e3436cb7f1df77f9652ae3e1310c28e621dd87b4ca0"
       in testCase "ledger state golden test" $
            unless (actual == expected) $
              assertFailure $
                unlines
                  [ "Expected: " ++ show expectedHex,
                    "Actual: " ++ show actualHex,
                    diffExpr (CBORBytes expected) (CBORBytes actual)
                  ]
    ]
  where
    genesisTxIn1 = TxIn @C_Crypto genesisId (mkTxIxPartial 1)

-- ===============
-- From CBOR instances for things that only have FromCBORSharing instances

instance FromCBOR (Stake C_Crypto) where
  fromCBOR = fromNotSharedCBOR

instance FromCBOR (SnapShots C_Crypto) where
  fromCBOR = fromNotSharedCBOR
