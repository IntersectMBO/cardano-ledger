{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Golden tests that check CBOR token encoding.
module Test.Cardano.Ledger.Shelley.Serialisation.Golden.Encoding (tests) where

import Cardano.Crypto.DSIGN (SignedDSIGN)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.KES (SignedKES, unsoundPureSignedKES)
import Cardano.Crypto.VRF (CertifiedVRF)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  EpochInterval (..),
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
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecCBORGroup (..),
  Decoder,
  DecoderError,
  EncCBOR (..),
  EncCBORGroup (..),
  Tokens (..),
  byronProtVer,
  decodeFullAnnotator,
  decodeFullDecoder,
  decodeMapTraverse,
  encCBOR,
  fromPlainEncoding,
  hashWithEncoder,
  ipv4ToBytes,
  shelleyProtVer,
  toCBOR,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Crypto (
  encodeSignedDSIGN,
  encodeSignedKES,
  encodeVerKeyDSIGN,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (
  DSIGN,
  VKey (..),
  WitVKey (..),
  asWitness,
  signedDSIGN,
 )
import Cardano.Ledger.PoolParams (
  PoolMetadata (..),
  PoolParams (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (MultiSig)
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..), bbHash)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (..),
  pattern ProposedPPUpdates,
  pattern Update,
 )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Scripts (pattern RequireSignature)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import qualified Cardano.Ledger.Shelley.TxAuxData as TxAuxData
import Cardano.Ledger.Shelley.TxBody (TxBody (ShelleyTxBody))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits, addrWits)
import Cardano.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
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
import Cardano.Protocol.TPraos.OCert (
  KESPeriod (..),
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
import Data.IP (toIPv4)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (fromJust)
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Word (Word64)
import Lens.Micro ((&), (.~))
import Numeric.Natural (Natural)
import qualified Prettyprinter as Pretty
import Test.Cardano.Ledger.Binary.TreeDiff (CBORBytes (CBORBytes), ansiDocToString, diffExpr)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey, sKey, vKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, MockCrypto)
import Test.Cardano.Ledger.Shelley.Examples.Consensus as Ex (
  ledgerExamplesShelley,
  sleNewEpochState,
 )
import Test.Cardano.Ledger.Shelley.Generator.Core (KESKeyPair (..), PreAlonzo, VRFKeyPair (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Serialisation.GoldenUtils (
  ToTokens (..),
  checkEncoding,
  checkEncodingCBOR,
  checkEncodingCBORAnnotated,
 )
import Test.Cardano.Ledger.Shelley.Utils
import Test.Cardano.Protocol.Binary.Annotator ()
import Test.Cardano.Protocol.Crypto.VRF.Fake (WithResult (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- ============================================

type MultiSigMap = Map.Map ScriptHash (MultiSig ShelleyEra)

decodeMultiSigMap :: Decoder s (Annotator MultiSigMap)
decodeMultiSigMap = decodeMapTraverse (pure <$> decCBOR) decCBOR

deserializeMultiSigMap :: BSL.ByteString -> Either DecoderError MultiSigMap
deserializeMultiSigMap =
  decodeFullAnnotator shelleyProtVer "Map ScriptHash MultiSig" decodeMultiSigMap

checkEncodingCBORCBORGroup ::
  (DecCBORGroup a, EncCBORGroup a, Show a, Eq a) =>
  String ->
  a ->
  ToTokens ->
  TestTree
checkEncodingCBORCBORGroup name x t =
  let d = decodeFullDecoder shelleyProtVer (fromString name) decCBORGroup
   in checkEncoding shelleyProtVer encCBORGroup d name x t

getRawKeyHash :: KeyHash 'Payment -> ByteString
getRawKeyHash (KeyHash hsh) = Hash.hashToBytes hsh

getRawNonce :: Nonce -> ByteString
getRawNonce (Nonce hsh) = Hash.hashToBytes hsh
getRawNonce NeutralNonce = error "The neutral nonce has no bytes"

testGKey :: GenesisKeyPair c
testGKey = KeyPair vk sk
  where
    (sk, vk) = mkGenKey (RawSeed 0 0 0 0 0)

testGKeyHash :: KeyHash 'Genesis
testGKeyHash = hashKey $ vKey testGKey

testVRF :: VRFKeyPair MockCrypto
testVRF = mkVRFKeyPair (RawSeed 0 0 0 0 5)

testVRFKH :: VRFVerKeyHash r
testVRFKH = hashVerKeyVRF @MockCrypto $ vrfVerKey testVRF

testTxb :: TxBody ShelleyEra
testTxb =
  ShelleyTxBody
    Set.empty
    StrictSeq.empty
    StrictSeq.empty
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 0)
    SNothing
    SNothing

testTxbHash ::
  SafeHash EraIndependentTxBody
testTxbHash = hashAnnotated testTxb

testKey1 :: KeyPair 'Payment
testKey1 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 1)

testKey2 :: KeyPair kr
testKey2 = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 2)

testBlockIssuerKey :: KeyPair 'BlockIssuer
testBlockIssuerKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 4)

testStakePoolKey :: KeyPair 'StakePool
testStakePoolKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 5)

testGenesisDelegateKey :: KeyPair 'GenesisDelegate
testGenesisDelegateKey = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 0 0 0 0 6)

testBlockIssuerKeyTokens :: Tokens -> Tokens
testBlockIssuerKeyTokens = e
  where
    VKey vk = vKey testBlockIssuerKey
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeVerKeyDSIGN vk)

testKey1SigToken :: Tokens -> Tokens
testKey1SigToken = e
  where
    s =
      signedDSIGN
        (sKey testKey1)
        (extractHash testTxbHash) ::
        SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedDSIGN s)

testOpCertSigTokens ::
  Tokens ->
  Tokens
testOpCertSigTokens = e
  where
    s =
      signedDSIGN
        (sKey testKey1)
        (OCertSignable @MockCrypto (kesVerKey testKESKeys) 0 (KESPeriod 0))
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedDSIGN s)

testKeyHash1 :: KeyHash 'Payment
testKeyHash1 = hashKey $ vKey testKey1

testKeyHash2 :: KeyHash 'Staking
testKeyHash2 = hashKey $ vKey testKey2

testKESKeys :: KESKeyPair MockCrypto
testKESKeys = mkKESKeyPair (RawSeed 0 0 0 0 3)

testAddrE :: Addr
testAddrE =
  Addr
    Testnet
    (KeyHashObj testKeyHash1)
    StakeRefNull

testPayCred :: Credential 'Payment
testPayCred = KeyHashObj testKeyHash1

testStakeCred :: Credential 'Staking
testStakeCred = KeyHashObj testKeyHash2

testScript :: MultiSig ShelleyEra
testScript = RequireSignature $ asWitness testKeyHash1

testScriptHash :: ScriptHash
testScriptHash = hashScript @ShelleyEra testScript

testScript2 :: MultiSig ShelleyEra
testScript2 = RequireSignature $ asWitness testKeyHash2

testHeaderHash :: HashHeader
testHeaderHash =
  HashHeader $ coerce (hashWithEncoder shelleyProtVer encCBOR 0 :: Hash HASH Int)

testBHB ::
  forall era.
  ( EraTx era
  , PreAlonzo era
  , Tx era ~ ShelleyTx era
  ) =>
  BHBody MockCrypto
testBHB =
  BHBody
    { bheaderBlockNo = BlockNo 44
    , bheaderSlotNo = SlotNo 33
    , bheaderPrev = BlockHash testHeaderHash
    , bheaderVk = vKey testBlockIssuerKey
    , bheaderVrfVk = vrfVerKey testVRF
    , bheaderEta =
        mkCertifiedVRF
          ( WithResult
              (mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0))
              1
          )
          (vrfSignKey testVRF)
    , bheaderL =
        mkCertifiedVRF
          ( WithResult
              (mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0))
              1
          )
          (vrfSignKey testVRF)
    , bsize = 0
    , bhash = bbHash @era $ ShelleyTxSeq @era StrictSeq.empty
    , bheaderOCert =
        OCert
          (kesVerKey testKESKeys)
          0
          (KESPeriod 0)
          ( signedDSIGN
              (sKey testKey1)
              (OCertSignable (kesVerKey testKESKeys) 0 (KESPeriod 0))
          )
    , bprotver = ProtVer minBound 0
    }

testBHBSigTokens ::
  forall era.
  ( EraTx era
  , PreAlonzo era
  , Tx era ~ ShelleyTx era
  ) =>
  Tokens ->
  Tokens
testBHBSigTokens = e
  where
    s =
      unsoundPureSignedKES @(KES MockCrypto)
        ()
        0
        (testBHB @era)
        (kesSignKey testKESKeys)
    CBOR.Encoding e = toPlainEncoding shelleyProtVer (encodeSignedKES s)

tests :: TestTree
tests =
  testGroup
    "CBOR Serialization Tests (Encoding)"
    [ checkEncodingCBOR
        byronProtVer
        "list (Byron)"
        ([1] :: [Integer])
        (T (TkListBegin . TkInteger 1 . TkBreak))
    , checkEncodingCBOR
        shelleyProtVer
        "list (Shelley)"
        ([1] :: [Integer])
        (T (TkListLen 1 . TkInteger 1))
    , checkEncodingCBOR
        byronProtVer
        "set (Byron)"
        (Set.singleton (1 :: Integer))
        (T (TkTag 258 . TkListLen 1 . TkInteger 1))
    , checkEncodingCBOR
        shelleyProtVer
        "set (Shelley)"
        (Set.singleton (1 :: Integer))
        (T (TkListLen 1 . TkInteger 1))
    , checkEncodingCBOR
        shelleyProtVer
        "map"
        (Map.singleton (1 :: Integer) (1 :: Integer))
        (T (TkMapLen 1 . TkInteger 1 . TkInteger 1))
    , checkEncodingCBOR
        shelleyProtVer
        "coin"
        (Coin 30)
        (T (TkWord64 30))
    , checkEncodingCBOR
        shelleyProtVer
        "rational"
        (unsafeBoundRational (1 % 2) :: UnitInterval)
        (T (TkTag 30 . TkListLen 2 . TkWord64 1 . TkWord64 2))
    , checkEncodingCBOR
        shelleyProtVer
        "slot"
        (SlotNo 7)
        (T (TkWord64 7))
    , checkEncodingCBOR
        shelleyProtVer
        "neutral_nonce"
        NeutralNonce
        (T (TkListLen 1 . TkWord 0))
    , checkEncodingCBOR
        shelleyProtVer
        "nonce"
        (mkNonceFromNumber 99)
        (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonceFromNumber 99)))
    , checkEncodingCBOR
        shelleyProtVer
        "key_hash"
        testKeyHash1
        (T (TkBytes (getRawKeyHash testKeyHash1)))
    , checkEncodingCBOR
        shelleyProtVer
        "credential_key_hash"
        testPayCred
        (T (TkListLen 2 . TkWord 0) <> S testKeyHash1)
    , checkEncodingCBOR
        shelleyProtVer
        "txin"
        (TxIn genesisId minBound)
        (T (TkListLen 2) <> S (genesisId :: TxId) <> T (TkWord64 0))
    , let a = Addr Testnet testPayCred StakeRefNull
       in checkEncodingCBOR
            shelleyProtVer
            "txout"
            (ShelleyTxOut @C a (Coin 2))
            ( T (TkListLen 2)
                <> S a
                <> S (Coin 2)
            )
    , case mkWitnessVKey testTxbHash testKey1 of
        w@(WitVKey vk _sig) ->
          checkEncodingCBORAnnotated
            shelleyProtVer
            "vkey_witnesses"
            w -- Transaction _witnessVKeySet element
            ( T (TkListLen 2)
                <> S vk -- vkey
                <> T testKey1SigToken -- signature
            )
    , checkEncoding
        shelleyProtVer
        (fromPlainEncoding . toCBOR)
        deserializeMultiSigMap
        "script_hash_to_scripts"
        (Map.singleton (hashScript @C testScript) testScript) -- Transaction _witnessMSigMap
        ( T (TkMapLen 1)
            <> S (hashScript @C testScript)
            <> S testScript
        )
    , -- checkEncodingCBOR "withdrawal_key"
      let r = RewardAccount Testnet testStakeCred
       in checkEncodingCBOR
            shelleyProtVer
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            )
    , -- checkEncodingCBOR "withdrawal_script"
      --
      let r = RewardAccount Testnet (ScriptHashObj testScriptHash)
       in checkEncodingCBOR
            shelleyProtVer
            "withdrawal"
            (Map.singleton r (Coin 123))
            ( (T $ TkMapLen 1)
                <> S r
                <> S (Coin 123)
            )
    , checkEncodingCBOR
        shelleyProtVer
        "register_stake_reference"
        (RegTxCert @C testStakeCred)
        ( T (TkListLen 2)
            <> T (TkWord 0) -- Reg cert
            <> S testStakeCred -- keyhash
        )
    , checkEncodingCBOR
        shelleyProtVer
        "deregister_stake_reference"
        (UnRegTxCert @C testStakeCred)
        ( T (TkListLen 2)
            <> T (TkWord 1) -- DeReg cert
            <> S testStakeCred -- keyhash
        )
    , checkEncodingCBOR
        shelleyProtVer
        "stake_delegation"
        (DelegStakeTxCert @C testStakeCred (hashKey $ vKey testStakePoolKey))
        ( T
            ( TkListLen 3
                . TkWord 2 -- delegation cert with key
            )
            <> S testStakeCred
            <> S (hashKey $ vKey testStakePoolKey)
        )
    , -- checkEncodingCBOR "register-pool"
      let poolOwner = testKeyHash2
          poolMargin = unsafeBoundRational 0.7
          poolRAcnt = RewardAccount Testnet testStakeCred
          poolPledge = Coin 11
          poolCost = Coin 55
          poolUrl = "pool.io"
          poolMDHash = BS.pack "{}"
          ipv4 = toIPv4 [127, 0, 0, 1]
          ipv4Bytes = ipv4ToBytes . toIPv4 $ [127, 0, 0, 1]
          poolRelays =
            StrictSeq.fromList
              [ SingleHostAddr SNothing (SJust ipv4) SNothing
              , SingleHostName (SJust 42) $ Maybe.fromJust $ textToDns 64 "singlehost.relay.com"
              , MultiHostName $ Maybe.fromJust $ textToDns 64 "multihost.relay.com"
              ]
          vrfKeyHash :: VRFVerKeyHash 'StakePoolVRF
          vrfKeyHash = testVRFKH
       in checkEncodingCBOR
            shelleyProtVer
            "register_pool"
            ( RegPoolTxCert @C
                ( PoolParams
                    { ppId = hashKey $ vKey testStakePoolKey
                    , ppVrf = vrfKeyHash
                    , ppPledge = poolPledge
                    , ppCost = poolCost
                    , ppMargin = poolMargin
                    , ppRewardAccount = poolRAcnt
                    , ppOwners = Set.singleton poolOwner
                    , ppRelays = poolRelays
                    , ppMetadata =
                        SJust $
                          PoolMetadata
                            { pmUrl = Maybe.fromJust $ textToUrl 64 poolUrl
                            , pmHash = poolMDHash
                            }
                    }
                )
            )
            ( T (TkListLen 10)
                <> T (TkWord 3) -- Reg Pool
                <> S (hashKey $ vKey testStakePoolKey) -- operator
                <> S vrfKeyHash
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
            )
    , checkEncodingCBOR
        shelleyProtVer
        "retire_pool"
        ( RetirePoolTxCert @C
            (hashKey $ vKey testStakePoolKey)
            (EpochNo 1729)
        )
        ( T
            ( TkListLen 3
                . TkWord 4 -- Pool Retire
            )
            <> S (hashKey $ vKey testStakePoolKey) -- key hash
            <> S (EpochNo 1729) -- epoch
        )
    , let vrfKeyHash :: VRFVerKeyHash 'GenDelegVRF
          vrfKeyHash = testVRFKH
          genesisDelegate :: KeyHash 'GenesisDelegate
          genesisDelegate = hashKey $ vKey testGenesisDelegateKey
       in checkEncodingCBOR
            shelleyProtVer
            "genesis_delegation"
            (GenesisDelegTxCert @C testGKeyHash genesisDelegate vrfKeyHash)
            ( T
                (TkListLen 4 . TkWord 5) -- genesis delegation cert
                <> S testGKeyHash -- delegator credential
                <> S genesisDelegate -- delegatee key hash
                <> S vrfKeyHash
            )
    , -- checkEncodingCBOR "mir"
      let rws = StakeAddressesMIR $ Map.singleton testStakeCred (DeltaCoin 77)
       in checkEncodingCBOR
            shelleyProtVer
            "mir"
            (MirTxCert @C (MIRCert ReservesMIR rws))
            ( T
                ( TkListLen 2
                    . TkWord 6 -- make instantaneous rewards cert
                    . TkListLen 2
                    . TkWord 0 -- take from the reserves
                )
                <> S rws
            )
    , checkEncodingCBOR
        shelleyProtVer
        "pparams_update_key_deposit_only"
        (emptyPParamsUpdate @ShelleyEra & ppuKeyDepositL .~ SJust (Coin 5))
        ((T $ TkMapLen 1 . TkWord 5) <> S (Coin 5))
    , -- checkEncodingCBOR "pparams_update_all"
      let minfeea = Coin 0
          minfeeb = Coin 1
          maxbbsize = 2
          maxtxsize = 3
          maxbhsize = 4
          keydeposit = Coin 5
          pooldeposit = Coin 6
          emax = EpochInterval 7
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
            ( emptyPParamsUpdate @ShelleyEra
                & ppuMinFeeAL .~ SJust minfeea
                & ppuMinFeeBL .~ SJust minfeeb
                & ppuMaxBBSizeL .~ SJust maxbbsize
                & ppuMaxTxSizeL .~ SJust maxtxsize
                & ppuMaxBHSizeL .~ SJust maxbhsize
                & ppuKeyDepositL .~ SJust keydeposit
                & ppuPoolDepositL .~ SJust pooldeposit
                & ppuEMaxL .~ SJust emax
                & ppuNOptL .~ SJust nopt
                & ppuA0L .~ SJust a0
                & ppuRhoL .~ SJust rho
                & ppuTauL .~ SJust tau
                & ppuDL .~ SJust d
                & ppuExtraEntropyL .~ SJust extraEntropy
                & ppuProtocolVersionL .~ SJust protocolVersion
                & ppuMinUTxOValueL .~ SJust minUTxOValue
                & ppuMinPoolCostL .~ SJust minPoolCost
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
            )
    , -- checkEncodingCBOR "full_update"
      let ppup =
            ProposedPPUpdates @C
              ( Map.singleton
                  testGKeyHash
                  (emptyPParamsUpdate & ppuNOptL .~ SJust 100)
              )
          e = EpochNo 0
       in checkEncodingCBOR
            shelleyProtVer
            "full_update"
            (Update ppup e)
            ( T (TkListLen 2)
                <> S ppup
                <> S e
            )
    , -- checkEncodingCBOR "minimal_txn_body"
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "txbody"
            ( ShelleyTxBody -- minimal transaction body
                (Set.fromList [genesisTxIn1])
                (StrictSeq.singleton tout)
                StrictSeq.empty
                (Withdrawals Map.empty)
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
            )
    , -- checkEncodingCBOR "transaction_mixed"
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
          ra = RewardAccount Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates $
                  Map.singleton testGKeyHash $
                    emptyPParamsUpdate & ppuNOptL .~ SJust 100
              )
              (EpochNo 0)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "txbody_partial"
            ( ShelleyTxBody -- transaction body with some optional components
                (Set.fromList [genesisTxIn1])
                (StrictSeq.singleton tout)
                StrictSeq.Empty
                (Withdrawals ras)
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
            )
    , -- checkEncodingCBOR "full_txn_body"
      let tout = ShelleyTxOut @C testAddrE (Coin 2)
          reg = RegTxCert testStakeCred
          ra = RewardAccount Testnet (KeyHashObj testKeyHash2)
          ras = Map.singleton ra (Coin 123)
          up =
            Update
              ( ProposedPPUpdates $
                  Map.singleton testGKeyHash $
                    emptyPParamsUpdate & ppuNOptL .~ SJust 100
              )
              (EpochNo 0)
          mdh = hashTxAuxData @C $ TxAuxData.ShelleyTxAuxData $ Map.singleton 13 (TxAuxData.I 17)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "txbody_full"
            ( ShelleyTxBody -- transaction body with all components
                (Set.fromList [genesisTxIn1])
                (StrictSeq.singleton tout)
                (StrictSeq.fromList [reg])
                (Withdrawals ras)
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
            )
    , -- checkEncodingCBOR "minimal_txn"
      let txb =
            ShelleyTxBody
              (Set.fromList [TxIn genesisId (mkTxIxPartial 1)])
              (StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2))
              StrictSeq.empty
              (Withdrawals Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = (hashAnnotated txb)
          w = mkWitnessVKey txbh testKey1
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "tx_min"
            ( ShelleyTx @(ShelleyEra)
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
            )
    , -- checkEncodingCBOR "full_txn"
      let txb =
            ShelleyTxBody
              (Set.fromList [genesisTxIn1])
              (StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2))
              StrictSeq.empty
              (Withdrawals Map.empty)
              (Coin 9)
              (SlotNo 500)
              SNothing
              SNothing
          txbh = hashAnnotated txb
          w = mkWitnessVKey txbh testKey1
          s = Map.singleton (hashScript @C testScript) testScript
          txwits :: ShelleyTxWits C
          txwits = mkBasicTxWits @C & addrTxWitsL .~ Set.singleton w & scriptTxWitsL .~ s
          md = (TxAuxData.ShelleyTxAuxData @C) $ Map.singleton 17 (TxAuxData.I 42)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "tx_full"
            (ShelleyTx @ShelleyEra txb txwits (SJust md))
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
            )
    , -- checkEncodingCBOR "block_header_body"
      let prevhash = BlockHash testHeaderHash
          vrfVkey = vrfVerKey testVRF
          slot = SlotNo 33
          nonce = mkSeed seedEta (SlotNo 33) (mkNonceFromNumber 0)
          nonceProof :: CertifiedVRF (VRF MockCrypto) Nonce
          nonceProof =
            mkCertifiedVRF
              (WithResult nonce 1)
              (vrfSignKey testVRF)
          leaderValue = mkSeed seedL (SlotNo 33) (mkNonceFromNumber 0)
          leaderProof :: CertifiedVRF (VRF MockCrypto) Natural
          leaderProof =
            mkCertifiedVRF
              (WithResult leaderValue 1)
              (vrfSignKey testVRF)
          size = 0
          blockNo = BlockNo 44
          bbhash = bbHash @C $ ShelleyTxSeq StrictSeq.empty
          ocert :: OCert MockCrypto
          ocert =
            OCert
              (kesVerKey testKESKeys)
              0
              (KESPeriod 0)
              ( signedDSIGN
                  (sKey testBlockIssuerKey)
                  (OCertSignable (kesVerKey testKESKeys) 0 (KESPeriod 0))
              )
          protover = ProtVer minBound 0
       in checkEncodingCBOR
            shelleyProtVer
            "block_header_body"
            ( BHBody
                { bheaderBlockNo = blockNo
                , bheaderSlotNo = slot
                , bheaderPrev = prevhash
                , bheaderVk = vKey testBlockIssuerKey
                , bheaderVrfVk = vrfVkey
                , bheaderEta = nonceProof
                , bheaderL = leaderProof
                , bsize = size
                , bhash = bbhash
                , bheaderOCert = ocert
                , bprotver = protover
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
            )
    , -- checkEncodingCBOR "operational_cert"
      let vkHot = kesVerKey testKESKeys
          counter = 0
          kesperiod = KESPeriod 0
          signature =
            signedDSIGN
              (sKey testKey1)
              (OCertSignable (kesVerKey testKESKeys) 0 (KESPeriod 0))
       in checkEncodingCBORCBORGroup
            "operational_cert"
            ( OCert @MockCrypto
                vkHot
                counter
                kesperiod
                signature
            )
            ( S vkHot
                <> S counter
                <> S kesperiod
                <> T testOpCertSigTokens
            )
    , -- checkEncodingCBOR "block_header"
      let sig :: (SignedKES (KES MockCrypto) (BHBody MockCrypto))
          sig = unsoundPureSignedKES () 0 (testBHB @C) (kesSignKey testKESKeys)
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "block_header"
            (BHeader (testBHB @C) sig)
            ( (T $ TkListLen 2)
                <> S (testBHB @C)
                <> T (testBHBSigTokens @C)
            )
    , -- checkEncodingCBOR "empty_block"
      let sig :: (SignedKES (KES MockCrypto) (BHBody MockCrypto))
          sig = unsoundPureSignedKES () 0 (testBHB @C) (kesSignKey testKESKeys)
          bh = BHeader (testBHB @C) sig
          txns = ShelleyTxSeq StrictSeq.Empty
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "empty_block"
            (Block @(BHeader MockCrypto) @C bh txns)
            ( (T $ TkListLen 4)
                <> S bh
                <> T (TkListLen 0 . TkListLen 0 . TkMapLen 0)
            )
    , -- checkEncodingCBOR "rich_block"
      let sig :: SignedKES (KES MockCrypto) (BHBody MockCrypto)
          sig = unsoundPureSignedKES () 0 (testBHB @C) (kesSignKey testKESKeys)
          bh = BHeader (testBHB @C) sig
          tout = StrictSeq.singleton $ ShelleyTxOut @C testAddrE (Coin 2)
          txb :: Word64 -> TxBody ShelleyEra
          txb s =
            ShelleyTxBody
              (Set.fromList [genesisTxIn1])
              tout
              StrictSeq.empty
              (Withdrawals Map.empty)
              (Coin 9)
              (SlotNo s)
              SNothing
              SNothing
          txb1, txb2, txb3, txb4, txb5 :: TxBody ShelleyEra
          txb1 = txb 500
          txb2 = txb 501
          txb3 = txb 502
          txb4 = txb 503
          txb5 = txb 504
          w1 = mkWitnessVKey (hashAnnotated txb1) testKey1
          w2 = mkWitnessVKey (hashAnnotated txb1) testKey2
          ws = Set.fromList [w1, w2]
          tx1, tx2, tx3, tx4, tx5 :: ShelleyTx C
          tx1 =
            mkBasicTx txb1
              & witsTxL @C .~ (mkBasicTxWits @C & addrTxWitsL .~ Set.singleton w1)
          tx2 =
            mkBasicTx txb2
              & witsTxL @C .~ (mkBasicTxWits @C & addrTxWitsL .~ ws)
          tx3 =
            mkBasicTx txb3
              & witsTxL @C
                .~ ( mkBasicTxWits @C
                       & scriptTxWitsL
                         .~ Map.singleton (hashScript @C testScript) testScript
                   )
          ss =
            Map.fromList
              [ (hashScript @C testScript, testScript)
              , (hashScript @C testScript2, testScript2)
              ]
          tx4 =
            mkBasicTx txb4
              & witsTxL @C .~ (mkBasicTxWits @C & scriptTxWitsL .~ ss)
          tx5MD = TxAuxData.ShelleyTxAuxData @C $ Map.singleton 17 (TxAuxData.I 42)
          tx5 =
            mkBasicTx txb5
              & witsTxL @C .~ (mkBasicTxWits @C & addrTxWitsL .~ ws & scriptTxWitsL .~ ss)
              & auxDataTxL @C .~ SJust tx5MD
          txns = ShelleyTxSeq $ StrictSeq.fromList [tx1, tx2, tx3, tx4, tx5]
       in checkEncodingCBORAnnotated
            shelleyProtVer
            "rich_block"
            (Block @(BHeader MockCrypto) @C bh txns)
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
            )
    , let actual =
            Plain.serialize' $ Ex.sleNewEpochState Ex.ledgerExamplesShelley
          expected = either error id $ B16.decode expectedHex
          actualHex = B16.encode actual
          expectedHex =
            mconcat
              [ "8700a1581ce0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b0825410aa158"
              , "1ca646474b8f5431261506b6c273d307c7569a4eb6c96b42dd4a29520a03848219271019"
              , "03e8828284a0a0a0a08482a0a0a0a084a0a0000086a15822ee155ace9c40292074cb6aff"
              , "8c9ccdd273c81648ff1149ef36bcea6ebb8a3e250000583d003900cb9358529df4729c32"
              , "46a2a033cb9821abbfd16de4888005904abc410d6a577e9441ad8ed9663931906e4d43ec"
              , "e8f82c712b1d0235affb06000a1903e80185a0a091000000190800000000001864d81e82"
              , "0001d81e820001d81e820001d81e8200018100820200010091000000190800000000001864"
              , "d81e820001d81e820001d81e820001d81e82000181008202000000810082a0a0008483a0a0"
              , "a083a0a0a083a0a0a00082a000818300880082020082a000000000a0a0840185a0800082"
              , "0200a0a082a0a082a1581ce0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b"
              , "08254183820101015820c5e21ab1c9f6022d81c3b25e3436cb7f1df77f9652ae3e1310c2"
              , "8e621dd87b4c01a0"
              ]
       in testCase "ledger state golden test" $
            unless (actual == expected) $
              assertFailure . ansiDocToString $
                Pretty.vsep
                  [ "Expected: " <> Pretty.viaShow expectedHex
                  , "Actual:   " <> Pretty.viaShow actualHex
                  , "Difference:"
                  , Pretty.indent 2 $ diffExpr (CBORBytes expected) (CBORBytes actual)
                  ]
    ]
  where
    genesisTxIn1 = TxIn genesisId (mkTxIxPartial 1)
