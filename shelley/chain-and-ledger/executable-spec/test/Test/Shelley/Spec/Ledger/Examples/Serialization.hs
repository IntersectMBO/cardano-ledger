{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.Serialization where

import qualified Data.Maybe as Maybe (fromJust)
import           Data.String (fromString)
import qualified Shelley.Spec.Ledger.MetaData as MD

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFullDecoder,
                     serializeEncoding, serialize', toCBOR, DecoderError, Annotator,
                     decodeAnnotator)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (encodeVerKeyDSIGN), encodeSignedDSIGN)
import           Cardano.Crypto.Hash (ShortHash, getHash)
import           Cardano.Prelude (LByteString)
import           Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase, (@?=))

import           Data.Coerce (coerce)
import           Data.Ratio ((%))
import           Numeric.Natural (Natural)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (..), StrictMaybe (..), UnitInterval (..),
                     mkNonce, text64)
import           Shelley.Spec.Ledger.BlockChain (pattern BHBody, pattern BHeader, Block (..),
                     pattern BlockHash, pattern HashHeader, TxSeq (..), bbHash, bhash,
                     bheaderBlockNo, bheaderEta, bheaderL, bheaderOCert, bheaderPrev,
                     bheaderSlotNo, bheaderVk, bheaderVrfVk, bprotver, bsize, mkSeed, seedEta,
                     seedL)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern MIRCert, pattern PoolDistr, pattern RegKey,
                     pattern RegPool, pattern RetirePool)
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), SnapShot (..), SnapShots (..),
                     Stake (..))
import           Shelley.Spec.Ledger.Keys (DiscVKey (..), pattern GenKeyHash, Hash, pattern KeyHash,
                     pattern KeyPair, pattern UnsafeSig, hash, hashKey, sKey, sign, signKES,
                     undiscriminateKeyHash, vKey)
import           Shelley.Spec.Ledger.LedgerState (AccountState (..), pattern ActiveSlot,
                     EpochState (..), NewEpochState (..), pattern RewardUpdate, deltaF, deltaR,
                     deltaT, emptyLedgerState, genesisId, nonMyopic, rs)
import           Shelley.Spec.Ledger.PParams (PParams' (PParams), PParamsUpdate,
                     pattern ProposedPPUpdates, ProtVer (..), pattern Update, emptyPParams,
                     mkActiveSlotCoeff, _a0, _activeSlotCoeff, _d, _eMax, _extraEntropy,
                     _keyDecayRate, _keyDeposit, _keyMinRefund, _maxBBSize, _maxBHSize, _maxTxSize,
                     _minfeeA, _minfeeB, _nOpt, _poolDecayRate, _poolDeposit, _poolMinRefund,
                     _protocolVersion, _rho, _tau)
import           Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import           Shelley.Spec.Ledger.Serialization (FromCBORGroup (..), ToCBORGroup (..))
import           Shelley.Spec.Ledger.Slot (BlockNo (..), EpochNo (..), SlotNo (..))
import           Shelley.Spec.Ledger.Tx (Tx (..), hashScript)
import           Shelley.Spec.Ledger.TxData (pattern AddrBase, pattern AddrEnterprise,
                     pattern AddrPtr, Credential (..), pattern DCertDeleg, pattern DCertGenesis,
                     pattern DCertMir, pattern DCertPool, pattern Delegation, PoolMetaData (..),
                     pattern PoolParams, Ptr (..), pattern RewardAcnt, pattern TxBody,
                     pattern TxIn, pattern TxOut, Url (..), Wdrl (..), WitVKey (..), _TxId,
                     _poolCost, _poolMD, _poolMDHash, _poolMDUrl, _poolMargin, _poolOwners,
                     _poolPledge, _poolPubKey, _poolRAcnt, _poolRelays, _poolVrf)

import           Shelley.Spec.Ledger.OCert (KESPeriod (..), pattern OCert)
import           Shelley.Spec.Ledger.Scripts (pattern RequireSignature, pattern ScriptHash)
import           Shelley.Spec.Ledger.UTxO (makeWitnessVKey)

import           Test.Cardano.Crypto.VRF.Fake (WithResult (..))
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, BHBody, CoreKeyPair,
                     GenKeyHash, HashHeader, KeyHash, KeyPair, MultiSig, PoolDistr, RewardUpdate,
                     SKeyES, ScriptHash, Sig, SignKeyVRF, TxBody, TxId, TxIn, VKey, VKeyES,
                     VRFKeyHash, VerKeyVRF, hashKeyVRF)
import           Test.Shelley.Spec.Ledger.Utils

import           Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

roundTrip :: (Show a, Eq a)
   => (a -> Encoding)
   -> (LByteString -> Either DecoderError a)
   -> a
   -> Assertion
roundTrip encode decode x =
  case (decode . serializeEncoding . encode) x of
    Left e -> assertFailure $ "could not decode serialization of " ++ show x ++ ", " ++ show e
    Right y -> y @?= x

checkEncoding
  :: (Show a, Eq a)
    => (a -> Encoding)
    -> (LByteString -> Either DecoderError a)
    -> String
    -> a
    -> ToTokens
    -> TestTree
checkEncoding encode decode name x t = testCase testName $
  assertEqual testName (fromEncoding $ toCBOR t) (fromEncoding $ encode x)
    >> roundTrip encode decode x
  where
   testName = "prop_serialize_" <> name
   fromEncoding :: Encoding -> Tokens
   fromEncoding (Encoding e) = e TkEnd

checkEncodingCBOR
  :: (FromCBOR a, ToCBOR a, Show a, Eq a)
  => String
  -> a
  -> ToTokens
  -> TestTree
checkEncodingCBOR name x t =
  let d = decodeFullDecoder (fromString name) fromCBOR
  in checkEncoding toCBOR d name x t

checkEncodingCBORAnnotated
  :: (FromCBOR (Annotator a), ToCBOR a, Show a, Eq a)
  => String
  -> a
  -> ToTokens
  -> TestTree
checkEncodingCBORAnnotated name x t =
  let d = decodeAnnotator (fromString name) fromCBOR
  in checkEncoding toCBOR d name x annTokens
  where
    annTokens = T $ TkEncoded $ serialize' t

checkEncodingCBORCBORGroup
  :: (FromCBORGroup a, ToCBORGroup a, Show a, Eq a)
  => String
  -> a
  -> ToTokens
  -> TestTree
checkEncodingCBORCBORGroup name x t =
  let d = decodeFullDecoder (fromString name) fromCBORGroup
  in checkEncoding toCBORGroup d name x t


getRawKeyHash :: KeyHash -> ByteString
getRawKeyHash (KeyHash hsh) = getHash hsh

getRawGenKeyHash :: GenKeyHash -> ByteString
getRawGenKeyHash (GenKeyHash hsh) = getHash hsh

getRawScriptHash :: ScriptHash -> ByteString
getRawScriptHash (ScriptHash hsh) = getHash hsh

getRawTxId :: TxId -> ByteString
getRawTxId = getHash . _TxId

getRawNonce :: Nonce -> ByteString
getRawNonce (Nonce hsh) = getHash hsh
getRawNonce NeutralNonce = error "The neutral nonce has no bytes"

testGKey :: CoreKeyPair
testGKey = KeyPair vk sk
  where (sk, vk) = mkGenKey (0, 0, 0, 0, 0)

testGKeyHash :: GenKeyHash
testGKeyHash = (hashKey . vKey) testGKey

testVRF :: (SignKeyVRF, VerKeyVRF)
testVRF = mkVRFKeyPair (0, 0, 0, 0, 5)

testVRFKH :: VRFKeyHash
testVRFKH = hashKeyVRF $ snd testVRF

testTxb :: TxBody
testTxb = TxBody Set.empty StrictSeq.empty StrictSeq.empty (Wdrl Map.empty) (Coin 0) (SlotNo 0) SNothing SNothing

testKey1 :: KeyPair
testKey1 = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

testKey2 :: KeyPair
testKey2 = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 2)

testKey3 :: KeyPair
testKey3 = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 3)

testKey1Token :: Tokens -> Tokens
testKey1Token = e
  where
    (DiscVKey vk) = vKey testKey1 :: VKey
    Encoding e = encodeVerKeyDSIGN vk

testKey1SigToken :: Tokens -> Tokens
testKey1SigToken = e
  where
    (UnsafeSig s) = sign (sKey testKey1) testTxb :: Sig TxBody
    Encoding e = encodeSignedDSIGN s

testKeyHash1 :: KeyHash
testKeyHash1 = (hashKey . vKey) testKey1

testKeyHash2 :: KeyHash
testKeyHash2 = (hashKey . vKey) testKey2

testKeyHash3 :: KeyHash
testKeyHash3 = (hashKey . vKey) testKey3

testKESKeys :: (SKeyES, VKeyES)
testKESKeys = mkKESKeyPair (0, 0, 0, 0, 3)

testAddrE :: Addr
testAddrE = AddrEnterprise (KeyHashObj testKeyHash1)

testScript :: MultiSig
testScript = RequireSignature $ undiscriminateKeyHash testKeyHash1

testScriptHash :: ScriptHash
testScriptHash = hashScript testScript

testScript2 :: MultiSig
testScript2 = RequireSignature $ undiscriminateKeyHash testKeyHash2

testScriptHash2 :: ScriptHash
testScriptHash2 = hashScript testScript2

testHeaderHash :: HashHeader
testHeaderHash = HashHeader $ unsafeCoerce (hash 0 :: Hash ShortHash Int)

testBHB :: BHBody
testBHB = BHBody
          { bheaderPrev    = BlockHash testHeaderHash
          , bheaderVk      = vKey testKey1
          , bheaderVrfVk   = snd testVRF
          , bheaderSlotNo  = SlotNo 33
          , bheaderEta     = coerce $ mkCertifiedVRF (WithResult
            (mkSeed seedEta (SlotNo 33) (mkNonce 0)) 1) (fst testVRF)
          , bheaderL       = coerce $ mkCertifiedVRF (WithResult
            (mkSeed seedL (SlotNo 33) (mkNonce 0)) 1) (fst testVRF)
          , bsize          = 0
          , bheaderBlockNo = BlockNo 44
          , bhash          = bbHash $ TxSeq StrictSeq.empty
          , bheaderOCert   = OCert (snd testKESKeys)
            0 (KESPeriod 0) (sign (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0))
          , bprotver       = ProtVer 0 0
          }

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
  in
    (tokens TkEnd @?= (TkInteger (-1)) TkEnd)
    >>
    (case (decodeFullDecoder "negative_coin" (fromCBOR @Coin) . serializeEncoding) enc of
      Left _ -> pure ()
      Right _ -> assertFailure "should not deserialize negative coins"
    )

serializationTests :: TestTree
serializationTests = testGroup "Serialization Tests"

  [ checkEncodingCBOR "list"
    [1::Integer]
    (T (TkListBegin . TkInteger 1 . TkBreak))
  , checkEncodingCBOR "set"
    (Set.singleton (1 :: Integer))
    (T (TkTag 258 . TkListLen 1 . TkInteger 1))
  , checkEncodingCBOR "map"
    (Map.singleton (1 :: Integer) (1 :: Integer))
    (T (TkMapLen 1 . TkInteger 1 . TkInteger 1))
  , checkEncodingCBOR "coin"
    (Coin 30)
    (T (TkWord64 30))
  , testCase "prop_serialize_negative-coin" testNegativeCoin
  , checkEncodingCBOR "rational"
    (UnsafeUnitInterval (1 % 2))
    (T (TkTag 30 . TkListLen 2 . TkInteger 1 . TkInteger 2))
  , checkEncodingCBOR "slot"
    (SlotNo 7)
    (T (TkWord64 7))
  , checkEncodingCBOR "neutral_nonce"
    NeutralNonce
    (T (TkListLen 1 . TkWord 0))
  , checkEncodingCBOR "nonce"
    (mkNonce 99)
    (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonce 99)))
  , checkEncodingCBOR "key_hash"
    testKeyHash1
    (T (TkBytes (getRawKeyHash testKeyHash1)))
  , checkEncodingCBOR "credential_key_hash"
    (KeyHashObj testKeyHash1)
    (T (TkListLen 2 . TkWord 0) <> S testKeyHash1)
  , checkEncodingCBOR "base_address_key_key"
    (AddrBase (KeyHashObj testKeyHash1) (KeyHashObj testKeyHash2))
    ( (T $ TkListLen 3)
        <> (T $ TkWord 0)
        <> S testKeyHash1
        <> S testKeyHash2
    )
  , checkEncodingCBOR "base_address_key_script"
    (AddrBase (KeyHashObj testKeyHash1) (ScriptHashObj testScriptHash))
    ( (T $ TkListLen 3)
        <> (T $ TkWord 1)
        <> S testKeyHash1
        <> S testScriptHash
    )
  , checkEncodingCBOR "base_address_script_key"
    (AddrBase (ScriptHashObj testScriptHash) (KeyHashObj testKeyHash2))
    ( (T $ TkListLen 3)
        <> (T $ TkWord 2)
        <> S testScriptHash
        <> S testKeyHash2
    )
  , checkEncodingCBOR "base_address_script_script"
    (AddrBase (ScriptHashObj testScriptHash) (ScriptHashObj testScriptHash2))
    ( (T $ TkListLen 3)
        <> (T $ TkWord 3)
        <> S testScriptHash
        <> S testScriptHash2
    )
  , let ptr = Ptr (SlotNo 12) 0 3 in
    checkEncodingCBOR "pointer_address_key"
    (AddrPtr (KeyHashObj testKeyHash1) ptr)
    ( (T $ TkListLen (2 + fromIntegral (listLen ptr)))
       <> T (TkWord 4)
       <> S testKeyHash1
       <> G ptr
    )
  , let ptr = Ptr (SlotNo 12) 0 3 in
    checkEncodingCBOR "pointer_address_script"
    (AddrPtr (ScriptHashObj testScriptHash) ptr)
    ( (T $ TkListLen (2 + fromIntegral (listLen ptr)))
       <> T (TkWord 5)
       <> S testScriptHash
       <> G ptr
    )
  , checkEncodingCBOR "enterprise_address_key"
    (AddrEnterprise (KeyHashObj testKeyHash1))
    (T (TkListLen 2) <> T (TkWord 6) <> S testKeyHash1)
  , checkEncodingCBOR "enterprise_address_script"
    (AddrEnterprise (ScriptHashObj testScriptHash))
    (T (TkListLen 2) <> T (TkWord 7) <> S testScriptHash)
  , checkEncodingCBOR "txin"
    (TxIn genesisId 0 :: TxIn)
    (T (TkListLen 2) <> S (genesisId :: TxId) <> T (TkWord64 0))
  , let a = AddrEnterprise (KeyHashObj testKeyHash1) in
    checkEncodingCBOR "txout"
    (TxOut a (Coin 2))
    (T (TkListLen 3)
      <> G a
      <> S (Coin 2)
    )
  , case makeWitnessVKey testTxb testKey1 of
    w@(WitVKey vk sig) ->
      checkEncodingCBOR "vkey_witnesses"
      w  -- Transaction _witnessVKeySet element
      ( T (TkListLen 2)
        <> S vk -- vkey
        <> S sig -- signature
      )
  , checkEncodingCBOR "script_hash_to_scripts"
    (Map.singleton (hashScript testScript :: ScriptHash) testScript)  -- Transaction _witnessMSigMap
    ( T (TkMapLen 1)
      <> S (hashScript testScript :: ScriptHash)
      <> S testScript
    )

  -- checkEncodingCBOR "withdrawal_key"
  , checkEncodingCBOR "withdrawal"
    (Map.singleton (RewardAcnt (KeyHashObj testKeyHash1)) (Coin 123))
    ( (T $ TkMapLen 1 . TkListLen 2)
        <> (T $ TkWord 0)
        <> S testKeyHash1
        <> S (Coin 123)
    )

  -- checkEncodingCBOR "withdrawal_script"
  , checkEncodingCBOR "withdrawal"
    (Map.singleton (RewardAcnt (ScriptHashObj testScriptHash)) (Coin 123))
    ( (T $ TkMapLen 1 . TkListLen 2)
        <> (T $ TkWord 1)
        <> S testScriptHash
        <> S (Coin 123)
    )

  , checkEncodingCBOR "register_key"
    (DCertDeleg (RegKey (KeyHashObj testKeyHash1)))
    ( T (TkListLen 2)
      <> T (TkWord 0) -- Reg cert
      <> S testKeyHash1 -- keyhash
    )

  , checkEncodingCBOR "register_script"
    (DCertDeleg (RegKey (ScriptHashObj testScriptHash)))
    ( T (TkListLen 2)
      <> T (TkWord 1) -- Reg cert
      <> S testScriptHash -- scripthash
    )

  , checkEncodingCBOR "deregister_key"
    (DCertDeleg (DeRegKey (KeyHashObj testKeyHash1)))
    ( T (TkListLen 2)
      <> T (TkWord 2) -- DeReg cert
      <> S testKeyHash1 -- keyhash
    )
  , checkEncodingCBOR "deregister_key"
    (DCertDeleg (DeRegKey (KeyHashObj testKeyHash1)))
    ( T (TkListLen 2)
      <> T (TkWord 2) -- DeReg cert
      <> S testKeyHash1 -- keyhash
    )

  , checkEncodingCBOR "deregister_script"
    (DCertDeleg (DeRegKey (ScriptHashObj testScriptHash)))
    ( T (TkListLen 2)
      <> T (TkWord 3) -- DeReg cert
      <> S testScriptHash -- script hash
    )

    -- checkEncodingCBOR "register-pool"
  , let poolOwner = testKeyHash2
        poolMargin = unsafeMkUnitInterval 0.7
        poolRAcnt = RewardAcnt (KeyHashObj testKeyHash1)
        poolPledge = Coin 11
        poolCost = Coin 55
        poolUrl = "pool.io"
        poolMDHash = BS.pack "{}"
    in
    checkEncodingCBOR "register_pool"
    (DCertPool (RegPool (PoolParams
               { _poolPubKey = testKeyHash1
               , _poolVrf = testVRFKH
               , _poolPledge = poolPledge
               , _poolCost = poolCost
               , _poolMargin = poolMargin
               , _poolRAcnt = poolRAcnt
               , _poolOwners = Set.singleton poolOwner
               , _poolRelays = StrictSeq.empty
               , _poolMD = SJust $ PoolMetaData
                             { _poolMDUrl = Url $ text64 poolUrl
                             , _poolMDHash = poolMDHash
                             }
               })))
    ( T (TkListLen 10)
      <> T (TkWord 6) -- Reg Pool
      <> S testKeyHash1 -- operator
      <> S testVRFKH    -- vrf keyhash
      <> S poolPledge   -- pledge
      <> S poolCost     -- cost
      <> S poolMargin   -- margin
      <> S poolRAcnt    -- reward acct
      <> T (TkTag 258 . TkListLen 1) <> S poolOwner   -- owners
      <> T (TkListLen 0) -- relays
      <> T (TkListLen 1 . TkListLen 2) -- metadata present
      <> S poolUrl       -- metadata url
      <> S poolMDHash    -- metadata hash
    )

  , checkEncodingCBOR "retire_pool"
    (DCertPool (RetirePool testKeyHash1 (EpochNo 1729)))
    ( T (TkListLen 3
      . TkWord 7) -- Pool Retire
      <> S testKeyHash1 -- key hash
      <> S (EpochNo 1729) -- epoch
    )

  , checkEncodingCBOR "Key_delegation"
    (DCertDeleg (Delegate (Delegation (KeyHashObj testKeyHash1) testKeyHash2)))
    ( T (TkListLen 3
      . TkWord 4) -- delegation cert with key
      <> S testKeyHash1
      <> S testKeyHash2
    )

  , checkEncodingCBOR "script_delegation"
    (DCertDeleg (Delegate (Delegation (ScriptHashObj testScriptHash) testKeyHash2)))
    ( T (TkListLen 3
      . TkWord 5) -- delegation cert with script
      <> S testScriptHash
      <> S testKeyHash2
    )

  , checkEncodingCBOR "genesis_delegation"
    (DCertGenesis (GenesisDelegate testGKeyHash testKeyHash1))
    ( T (TkListLen 3
      . TkWord 8) -- genesis delegation cert
      <> S testGKeyHash -- delegator credential
      <> S testKeyHash1 -- delegatee key hash
    )

    -- checkEncodingCBOR "mir"
    , let rws = Map.singleton (KeyHashObj testKeyHash1) 77
    in
    checkEncodingCBOR "mir"
    (DCertMir (MIRCert rws))
    ( T (TkListLen 2
       . TkWord 9) -- make instantaneous rewards cert
      <> S rws
    )

  , checkEncodingCBOR "pparams_update_key_deposit_only"
    (PParams
       { _minfeeA = SNothing
       , _minfeeB = SNothing
       , _maxBBSize = SNothing
       , _maxTxSize = SNothing
       , _maxBHSize = SNothing
       , _keyDeposit = SJust (Coin 5)
       , _keyMinRefund = SNothing
       , _keyDecayRate = SNothing
       , _poolDeposit = SNothing
       , _poolMinRefund = SNothing
       , _poolDecayRate = SNothing
       , _eMax = SNothing
       , _nOpt = SNothing
       , _a0 = SNothing
       , _rho = SNothing
       , _tau = SNothing
       , _activeSlotCoeff = SNothing
       , _d = SNothing
       , _extraEntropy = SNothing
       , _protocolVersion = SNothing
       } :: PParamsUpdate)
    ((T $ TkMapLen 1 . TkWord 5) <> S (Coin 5))

  -- checkEncodingCBOR "pparams_update_all"
  , let minfeea               = 0
        minfeeb               = 1
        maxbbsize             = 2
        maxtxsize             = 3
        maxbhsize             = 4
        keydeposit            = Coin 5
        keyminrefund          = UnsafeUnitInterval $ 1 % 2
        keydecayrate          = 1 % 3
        pooldeposit           = Coin 6
        poolminrefund         = UnsafeUnitInterval $ 1 % 4
        pooldecayrate         = 1 % 5
        emax                  = EpochNo 7
        nopt                  = 8
        a0                    = 1 % 6
        rho                   = UnsafeUnitInterval $ 1 % 6
        tau                   = UnsafeUnitInterval $ 1 % 7
        activeSlotCoefficient = mkActiveSlotCoeff $ UnsafeUnitInterval $ 1 % 8
        d                     = UnsafeUnitInterval $ 1 % 9
        extraEntropy          = NeutralNonce
        protocolVersion       = ProtVer 0 1
    in
    checkEncodingCBOR "pparams_update_all"
    (PParams
       { _minfeeA         = SJust minfeea
       , _minfeeB         = SJust minfeeb
       , _maxBBSize       = SJust maxbbsize
       , _maxTxSize       = SJust maxtxsize
       , _maxBHSize       = SJust maxbhsize
       , _keyDeposit      = SJust keydeposit
       , _keyMinRefund    = SJust keyminrefund
       , _keyDecayRate    = SJust keydecayrate
       , _poolDeposit     = SJust pooldeposit
       , _poolMinRefund   = SJust poolminrefund
       , _poolDecayRate   = SJust pooldecayrate
       , _eMax            = SJust emax
       , _nOpt            = SJust nopt
       , _a0              = SJust a0
       , _rho             = SJust rho
       , _tau             = SJust tau
       , _activeSlotCoeff = SJust activeSlotCoefficient
       , _d               = SJust d
       , _extraEntropy    = SJust extraEntropy
       , _protocolVersion = SJust protocolVersion
       } :: PParamsUpdate)
    ((T $ TkMapLen 20)
      <> (T $ TkWord 0) <> S minfeea
      <> (T $ TkWord 1) <> S minfeeb
      <> (T $ TkWord 2) <> S maxbbsize
      <> (T $ TkWord 3) <> S maxtxsize
      <> (T $ TkWord 4) <> S maxbhsize
      <> (T $ TkWord 5) <> S keydeposit
      <> (T $ TkWord 6) <> S keyminrefund
      <> (T $ TkWord 7 . TkTag 30) <> S keydecayrate
      <> (T $ TkWord 8) <> S pooldeposit
      <> (T $ TkWord 9) <> S poolminrefund
      <> (T $ TkWord 10 . TkTag 30) <> S pooldecayrate
      <> (T $ TkWord 11) <> S emax
      <> (T $ TkWord 12) <> S nopt
      <> (T $ TkWord 13 . TkTag 30) <> S a0
      <> (T $ TkWord 14) <> S rho
      <> (T $ TkWord 15) <> S tau
      <> (T $ TkWord 16) <> S activeSlotCoefficient
      <> (T $ TkWord 17) <> S d
      <> (T $ TkWord 18) <> S extraEntropy
      <> (T $ TkWord 19) <> S protocolVersion)

    -- checkEncodingCBOR "full_update"
  , let
      ppup = ProposedPPUpdates (Map.singleton
               testGKeyHash
               (PParams
                  { _minfeeA = SNothing
                  , _minfeeB = SNothing
                  , _maxBBSize = SNothing
                  , _maxTxSize = SNothing
                  , _maxBHSize = SNothing
                  , _keyDeposit = SNothing
                  , _keyMinRefund = SNothing
                  , _keyDecayRate = SNothing
                  , _poolDeposit = SNothing
                  , _poolMinRefund = SNothing
                  , _poolDecayRate = SNothing
                  , _eMax = SNothing
                  , _nOpt = SJust 100
                  , _a0 = SNothing
                  , _rho = SNothing
                  , _tau = SNothing
                  , _activeSlotCoeff = SNothing
                  , _d = SNothing
                  , _extraEntropy = SNothing
                  , _protocolVersion = SNothing
                  }))
      e = EpochNo 0
    in checkEncodingCBOR "full_update"
    (Update ppup e)
    ( (T $ TkListLen 2)
      <> S ppup
      <> S e
    )

  -- checkEncodingCBOR "minimal_txn_body"
  , let
      tin = Set.fromList [TxIn genesisId 1]
      tout = TxOut testAddrE (Coin 2)
    in checkEncodingCBOR "txbody"
    ( TxBody -- minimal transaction body
      tin
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
      <> S tin
      <> T (TkWord 1) -- Tx Outs
      <> T (TkListLen 1)
      <> S tout
      <> T (TkWord 2) -- Tx Fee
      <> T (TkWord64 9)
      <> T (TkWord 3) -- Tx TTL
      <> T (TkWord64 500)
    )

  -- checkEncodingCBOR "transaction_mixed"
  , let
      tin = Set.fromList [TxIn genesisId 1]
      tout = TxOut testAddrE (Coin 2)
      ra = RewardAcnt (KeyHashObj testKeyHash2)
      ras = Map.singleton ra (Coin 123)
      up = Update (ProposedPPUpdates (Map.singleton
             testGKeyHash
             (PParams
                { _minfeeA = SNothing
                , _minfeeB = SNothing
                , _maxBBSize = SNothing
                , _maxTxSize = SNothing
                , _maxBHSize = SNothing
                , _keyDeposit = SNothing
                , _keyMinRefund = SNothing
                , _keyDecayRate = SNothing
                , _poolDeposit = SNothing
                , _poolMinRefund = SNothing
                , _poolDecayRate = SNothing
                , _eMax = SNothing
                , _nOpt = SJust 100
                , _a0 = SNothing
                , _rho = SNothing
                , _tau = SNothing
                , _activeSlotCoeff = SNothing
                , _d = SNothing
                , _extraEntropy = SNothing
                , _protocolVersion = SNothing
                })))
             (EpochNo 0)
    in checkEncodingCBOR "txbody_partial"
    ( TxBody -- transaction body with some optional components
        tin
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
      )

  -- checkEncodingCBOR "full_txn_body"
  , let
      tin = Set.fromList [TxIn genesisId 1]
      tout = TxOut testAddrE (Coin 2)
      reg = DCertDeleg (RegKey (KeyHashObj testKeyHash1))
      ra = RewardAcnt (KeyHashObj testKeyHash2)
      ras = Map.singleton ra (Coin 123)
      up = Update
             (ProposedPPUpdates (Map.singleton
                         testGKeyHash
                         (PParams
                            { _minfeeA = SNothing
                            , _minfeeB = SNothing
                            , _maxBBSize = SNothing
                            , _maxTxSize = SNothing
                            , _maxBHSize = SNothing
                            , _keyDeposit = SNothing
                            , _keyMinRefund = SNothing
                            , _keyDecayRate = SNothing
                            , _poolDeposit = SNothing
                            , _poolMinRefund = SNothing
                            , _poolDecayRate = SNothing
                            , _eMax = SNothing
                            , _nOpt = SJust 100
                            , _a0 = SNothing
                            , _rho = SNothing
                            , _tau = SNothing
                            , _activeSlotCoeff = SNothing
                            , _d = SNothing
                            , _extraEntropy = SNothing
                            , _protocolVersion = SNothing
                            })))
             (EpochNo 0)
      mdh = MD.hashMetaData $ MD.MetaData $ Map.singleton 13 (MD.I 17)
    in checkEncodingCBOR "txbody_full"
    ( TxBody -- transaction body with all components
        tin
        (StrictSeq.singleton tout)
        (StrictSeq.fromList [ reg ])
        (Wdrl ras)
        (Coin 9)
        (SlotNo 500)
        (SJust up)
        (SJust mdh)
     )
     ( T (TkMapLen 8)
       <> T (TkWord 0) -- Tx Ins
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
      )

  -- checkEncodingCBOR "minimal_txn"
  , let txb = TxBody
                (Set.fromList [TxIn genesisId 1])
                (StrictSeq.singleton $ TxOut testAddrE (Coin 2))
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 9)
                (SlotNo 500)
                SNothing
                SNothing
        w = makeWitnessVKey txb testKey1
        md = SNothing :: StrictMaybe MD.MetaData
    in
    checkEncodingCBOR "tx_min"
    ( Tx txb (Set.singleton w) Map.empty md )
    ( T (TkListLen 3)
      <> S txb
      <> T (TkMapLen 1)
       <> T (TkWord 0)
       <> T (TkListLen 1)
         <> S w
      <> S md
    )

  -- checkEncodingCBOR "full_txn"
  , let txb = TxBody
                (Set.fromList [TxIn genesisId 1])
                (StrictSeq.singleton $ TxOut testAddrE (Coin 2))
                StrictSeq.empty
                (Wdrl Map.empty)
                (Coin 9)
                (SlotNo 500)
                SNothing
                SNothing
        w = makeWitnessVKey txb testKey1
        s = Map.singleton (hashScript testScript) testScript
        md = SJust $ MD.MetaData $ Map.singleton 17 (MD.I 42)
    in
    checkEncodingCBOR "tx_full"
    ( Tx txb (Set.singleton w) s md )
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


  -- checkEncodingCBOR "block_header_body"
  , let
      prevhash = BlockHash testHeaderHash
      issuerVkey = vKey testKey1
      vrfVkey = snd testVRF
      slot = SlotNo 33
      nonce = mkSeed seedEta (SlotNo 33) (mkNonce 0)
      nonceProof = coerce $ mkCertifiedVRF (WithResult nonce 1) (fst testVRF)
      leaderValue = mkSeed seedL (SlotNo 33) (mkNonce 0)
      leaderProof = coerce $ mkCertifiedVRF (WithResult leaderValue 1) (fst testVRF)
      size = 0
      blockNo = BlockNo 44
      bbhash = bbHash $ TxSeq StrictSeq.empty
      ocert = OCert
                (snd testKESKeys)
                0
                (KESPeriod 0)
                (sign (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0))
      protover = ProtVer 0 0
    in
    checkEncodingCBOR "block_header_body"
    ( BHBody
      { bheaderPrev    = prevhash
      , bheaderVk      = issuerVkey
      , bheaderVrfVk   = vrfVkey
      , bheaderSlotNo  = slot
      , bheaderEta     = nonceProof
      , bheaderL       = leaderProof
      , bsize          = size
      , bheaderBlockNo = blockNo
      , bhash          = bbhash
      , bheaderOCert   = ocert
      , bprotver       = protover
      }
    )
    ( T (TkListLen $ 9 + 4 + 2)
      <> S prevhash
      <> S issuerVkey
      <> S vrfVkey
      <> S slot
      <> S nonceProof
      <> S leaderProof
      <> S size
      <> S blockNo
      <> S bbhash
      <> G ocert    -- 5
      <> G protover -- 3
    )

  -- checkEncodingCBOR "operational_cert"
  , let vkHot     = snd testKESKeys
        counter   = 0
        kesperiod = KESPeriod 0
        signature = sign (sKey testKey1) (snd testKESKeys, 0, KESPeriod 0)
    in
    checkEncodingCBORCBORGroup "operational_cert"
    (OCert vkHot counter kesperiod signature)
    (    S vkHot
      <> S counter
      <> S kesperiod
      <> S signature
    )

    -- checkEncodingCBOR "block_header"
  , let sig = Maybe.fromJust $ signKES (fst testKESKeys) testBHB 0
    in
    checkEncodingCBORAnnotated "block_header"
    (BHeader testBHB sig)
    ( (T $ TkListLen 16)
        <> G testBHB
        <> S sig
    )

    -- checkEncodingCBOR "empty_block"
  , let sig = Maybe.fromJust $ signKES (fst testKESKeys) testBHB 0
        bh = BHeader testBHB sig
        txns = TxSeq StrictSeq.Empty
    in
    checkEncodingCBORAnnotated "empty_block"
    (Block bh txns)
    ( (T $ TkListLen 4)
        <> S bh
        <> T (TkListLen 0 . TkListLen 0 . TkMapLen 0)
    )

    -- checkEncodingCBOR "rich_block"
  , let sig = Maybe.fromJust $ signKES (fst testKESKeys) testBHB 0
        bh = BHeader testBHB sig
        tin = Set.fromList [TxIn genesisId 1]
        tout = StrictSeq.singleton $ TxOut testAddrE (Coin 2)
        txb s = TxBody tin tout StrictSeq.empty (Wdrl Map.empty) (Coin 9) (SlotNo s) SNothing SNothing
        txb1 = txb 500
        txb2 = txb 501
        txb3 = txb 502
        txb4 = txb 503
        txb5 = txb 504
        w1 = makeWitnessVKey txb1 testKey1
        w2 = makeWitnessVKey txb1 testKey2
        ws = Set.fromList [w1, w2]
        tx1 = Tx txb1 (Set.singleton w1) mempty SNothing
        tx2 = Tx txb2 ws mempty SNothing
        tx3 = Tx txb3 mempty (Map.singleton (hashScript testScript) testScript) SNothing
        ss = Map.fromList [ (hashScript testScript, testScript)
                          , (hashScript testScript2, testScript2)]
        tx4 = Tx txb4 mempty ss SNothing
        tx5MD = MD.MetaData $ Map.singleton 17 (MD.I 42)
        tx5 = Tx txb5 ws ss (SJust tx5MD)
        txns = TxSeq $ StrictSeq.fromList [tx1, tx2, tx3, tx4, tx5]
    in
    checkEncodingCBORAnnotated "rich_block"
    (Block bh txns)
    ( (T $ TkListLen 4)
        -- header
        <> S bh

        -- bodies
        <> T (TkListLen 5)
        <> S txb1 <> S txb2 <> S txb3 <> S txb4 <> S txb5

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
  , checkEncodingCBOR "epoch"
    (EpochNo 13)
    (T (TkWord64 13))
  , let n = (17 :: Natural)
        bs = Map.singleton testKeyHash1 n
    in
    checkEncodingCBOR "blocks_made"
    (BlocksMade bs)
    ( T (TkMapLen 1)
      <> S testKeyHash1
      <> S n)
  , checkEncodingCBOR "account_state"
    (AccountState (Coin 1) (Coin 2))
    ( T (TkListLen 2)
      <> S (Coin 1)
      <> S (Coin 2))
  , let stk = Map.singleton (KeyHashObj testKeyHash1) (Coin 13)
    in
    checkEncodingCBOR "stake"
    (Stake stk)
    ( T (TkMapLen 1)
      <> S (KeyHashObj testKeyHash1)
      <> S (Coin 13))
  , let mark = SnapShot
                 (Stake $ Map.singleton (KeyHashObj testKeyHash1) (Coin 11))
                 (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
                 ps
        set  = SnapShot
                 (Stake $ Map.singleton (KeyHashObj testKeyHash2) (Coin 22))
                 (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
                 ps
        go   = SnapShot
                 (Stake $ Map.singleton (KeyHashObj testKeyHash1) (Coin 33))
                 (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
                 ps
        p = PoolParams
              { _poolPubKey = testKeyHash1
              , _poolVrf = testVRFKH
              , _poolPledge = Coin 5
              , _poolCost = Coin 4
              , _poolMargin = unsafeMkUnitInterval 0.7
              , _poolRAcnt = RewardAcnt (KeyHashObj testKeyHash1)
              , _poolOwners = Set.singleton testKeyHash2
              , _poolRelays = StrictSeq.empty
              , _poolMD = SJust $ PoolMetaData
                            { _poolMDUrl  = Url $ text64 "web.site"
                            , _poolMDHash = BS.pack "{}"
                            }
              }
        ps = Map.singleton testKeyHash1 p
        fs = Coin 123
    in
    checkEncodingCBOR "snapshots"
    (SnapShots mark set go fs)
    ( T (TkListLen 4)
      <> S mark
      <> S set
      <> S go
      <> S fs )
  , let
      e  = EpochNo 0
      ac = AccountState (Coin 100) (Coin 100)
      mark = SnapShot
               (Stake $ Map.singleton (KeyHashObj testKeyHash1) (Coin 11))
               (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
               ps
      set  = SnapShot
               (Stake $ Map.singleton (KeyHashObj testKeyHash2) (Coin 22))
               (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
               ps
      go   = SnapShot
               (Stake $ Map.singleton (KeyHashObj testKeyHash1) (Coin 33))
               (Map.singleton (KeyHashObj testKeyHash1) testKeyHash3)
               ps
      p = PoolParams
            { _poolPubKey = testKeyHash1
            , _poolVrf = testVRFKH
            , _poolPledge = Coin 5
            , _poolCost = Coin 4
            , _poolMargin = unsafeMkUnitInterval 0.7
            , _poolRAcnt = RewardAcnt (KeyHashObj testKeyHash1)
            , _poolOwners = Set.singleton testKeyHash2
            , _poolRelays = StrictSeq.empty
            , _poolMD = SJust $ PoolMetaData
                          { _poolMDUrl  = Url $ text64 "web.site"
                          , _poolMDHash = BS.pack "{}"
                          }
            }
      ps = Map.singleton testKeyHash1 p
      fs = Coin 123
      ss = SnapShots mark set go fs
      ls = emptyLedgerState
      pps = emptyPParams
      bs = Map.singleton testKeyHash1 1
      nm = emptyNonMyopic
      es = EpochState ac ss ls pps pps nm
      ru = (SJust RewardUpdate
             { deltaT        = Coin 100
             , deltaR        = Coin (-200)
             , rs            = Map.empty
             , deltaF        = Coin (-10)
             , nonMyopic     = nm
             }) :: StrictMaybe RewardUpdate
      pd = (PoolDistr Map.empty) :: PoolDistr
      os = Map.singleton (SlotNo 1) (ActiveSlot testGKeyHash)
      nes = NewEpochState
              e
              (BlocksMade bs)
              (BlocksMade bs)
              es
              ru
              pd
              os
    in
    checkEncodingCBOR "new_epoch_state"
    nes
    ( T (TkListLen 7)
      <> S e
      <> S (BlocksMade bs)
      <> S (BlocksMade bs)
      <> S es
      <> S ru
      <> S pd
      <> S os
    )
 ]
