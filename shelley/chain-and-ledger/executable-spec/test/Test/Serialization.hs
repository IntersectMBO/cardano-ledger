{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Serialization where

import           Cardano.Binary (ToCBOR, toCBOR)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (encodeVerKeyDSIGN), encodeSignedDSIGN)
import           Cardano.Crypto.Hash (getHash)
import           Codec.CBOR.Encoding (Encoding (..), Tokens (..))
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)


import           BaseTypes (Nonce (..), UnitInterval (..), mkNonce)
import           Coin (Coin (..))
import           Data.Ratio ((%))
import           Delegation.Certificates (pattern DeRegKey, pattern Delegate,
                     pattern GenesisDelegate, pattern InstantaneousRewards, pattern RegKey,
                     pattern RegPool, pattern RetirePool)
import           Keys (DiscVKey (..), pattern GenKeyHash, pattern KeyHash, pattern KeyPair,
                     pattern UnsafeSig, hash, hashKey, sKey, sign, undiscriminateKeyHash, vKey)
import           LedgerState (genesisId)
import           Serialization (ToCBORGroup (..))
import           Slot (Epoch (..), Slot (..))
import           Test.Utils
import           Tx (hashScript)
import           TxData (pattern AddrBase, pattern AddrEnterprise, pattern AddrPtr, Credential (..),
                     pattern Delegation, pattern PoolParams, Ptr (..), pattern RequireSignature,
                     pattern RewardAcnt, pattern ScriptHash, pattern TxBody, pattern TxIn,
                     pattern TxOut, WitVKey (..), _TxId, _poolCost, _poolMargin, _poolOwners,
                     _poolPledge, _poolPubKey, _poolRAcnt, _poolVrf)
import           Updates (pattern AVUpdate, ApName (..), ApVer (..), pattern Applications,
                     pattern InstallerHash, pattern Mdt, pattern PPUpdate, Ppm (..),
                     SystemTag (..), pattern Update, emptyUpdate)

import           MockTypes (Addr, GenKeyHash, InstallerHash, KeyHash, KeyPair, MultiSig, ScriptHash,
                     Sig, TxBody, TxId, TxIn, VKey, VRFKeyHash, hashKeyVRF)
import           UTxO (makeWitnessVKey)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set


checkEncoding :: ToCBOR a => String -> a -> ToTokens -> TestTree
checkEncoding name x t = testCase testName $
  assertEqual testName (fromEncoding $ tokens t) (fromEncoding $ toCBOR x)
  where
   testName = "prop_serialize_" <> name
   tokens :: ToTokens -> Encoding
   tokens (T xs) = Encoding xs
   tokens (S s) = toCBOR s
   tokens (G g) = toCBORGroup g
   tokens (Plus a b) = tokens a <> tokens b

   fromEncoding :: Encoding -> Tokens
   fromEncoding (Encoding e) = e TkEnd


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

testGKeyHash :: GenKeyHash
testGKeyHash = (hashKey . snd . mkGenKey) (0, 0, 0, 0, 0)

testVRFKH :: VRFKeyHash
testVRFKH = hashKeyVRF . snd $ mkVRFKeyPair (0, 0, 0, 0, 5)

testTxb :: TxBody
testTxb = TxBody Set.empty [] Seq.empty Map.empty (Coin 0) (Slot 0) emptyUpdate

testKey1 :: KeyPair
testKey1 = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

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
testKeyHash1 = (hashKey . snd . mkKeyPair) (0, 0, 0, 0, 1)

testKeyHash2 :: KeyHash
testKeyHash2 = (hashKey . snd . mkKeyPair) (0, 0, 0, 0, 2)

testAddrE :: Addr
testAddrE = AddrEnterprise (KeyHashObj testKeyHash1)

testInstallerHash :: InstallerHash
testInstallerHash = (InstallerHash . hash . pack) "ABC"

getRawInstallerHash :: InstallerHash -> ByteString
getRawInstallerHash (InstallerHash hsh) = getHash hsh

testScript :: MultiSig
testScript = RequireSignature $ undiscriminateKeyHash testKeyHash1

testScriptHash :: ScriptHash
testScriptHash = hashScript testScript

data ToTokens where
  T :: (Tokens -> Tokens) -> ToTokens
  S :: ToCBOR a => a -> ToTokens
  G :: ToCBORGroup a => a -> ToTokens
  Plus :: ToTokens -> ToTokens -> ToTokens

instance Semigroup ToTokens where
  (<>) = Plus

instance Monoid ToTokens where
  mempty = T id

serializationTests :: TestTree
serializationTests = testGroup "Serialization Tests"
  [ checkEncoding "list"
    [1::Integer]
    (T (TkListBegin . TkInteger 1 . TkBreak))
  , checkEncoding "set"
    (Set.singleton (1 :: Integer))
    (T (TkTag 258 . TkListLen 1 . TkInteger 1))
  , checkEncoding "map"
    (Map.singleton (1 :: Integer) (1 :: Integer))
    (T (TkMapLen 1 . TkInteger 1 . TkInteger 1))
  , checkEncoding "coin"
    (Coin 30)
    (T (TkWord64 30))
  , checkEncoding "rational"
    (UnsafeUnitInterval (1 % 2))
    (T (TkWord64 1 . TkWord64 2))
  , checkEncoding "slot"
    (Slot 7)
    (T (TkWord64 7))
  , checkEncoding "neutral_nonce"
    NeutralNonce
    (T (TkListLen 1 . TkWord 0))
  , checkEncoding "nonce"
    (mkNonce 99)
    (T (TkListLen 2 . TkWord 1 . TkBytes (getRawNonce $ mkNonce 99)))
  , checkEncoding "key_hash"
    testKeyHash1
    (T (TkBytes (getRawKeyHash testKeyHash1)))
  , checkEncoding "credential_key_hash"
    (KeyHashObj testKeyHash1)
    (T (TkListLen 2 . TkWord 0) <> S testKeyHash1)
  , checkEncoding "base_address"
    (AddrBase (KeyHashObj testKeyHash1) (KeyHashObj testKeyHash2))
    ( (T $ TkListLen 3)
        <> (T $ TkWord 0)
        <> S testKeyHash1
        <> S testKeyHash2
    )
  , let ptr = Ptr (Slot 12) 0 3 in
    checkEncoding "pointer_address"
    (AddrPtr (KeyHashObj testKeyHash1) ptr)
    ( (T $ TkListLen (2 + fromIntegral (listLen ptr)))
       <> T (TkWord 4)
       <> S testKeyHash1
       <> G ptr
    )
  , checkEncoding "enterprise_address"
    (AddrEnterprise (KeyHashObj testKeyHash1))
    (T (TkListLen 2) <> T (TkWord 6) <> S testKeyHash1)
  , checkEncoding "txin"
    (TxIn genesisId 0 :: TxIn)
    (T (TkListLen 2) <> S (genesisId :: TxId) <> T (TkWord64 0))
  , let a = AddrEnterprise (KeyHashObj testKeyHash1) in
    checkEncoding "txout"
    (TxOut a (Coin 2))
    (T (TkListLen 2)
      <> S a
      <> S (Coin 2)
    )
  , let
      tin = Set.fromList [TxIn genesisId 1]
      tout = [TxOut testAddrE (Coin 2)]
    in checkEncoding "txbody"
    ( TxBody -- minimal transaction body
      tin
      tout
      Seq.empty
      Map.empty
      (Coin 9)
      (Slot 500)
      emptyUpdate
    )
    ( T (TkMapLen 4)
      <> T (TkWord 0) -- Tx Ins
      <> S tin
      <> T (TkWord 1) -- Tx Outs
      <> S tout
      <> T (TkWord 2) -- Tx Fee
      <> T (TkWord64 9)
      <> T (TkWord 3) -- Tx TTL
      <> T (TkWord64 500)
    )
  , case makeWitnessVKey testTxb testKey1 of
    (WitGVKey _ _) -> error "unreachable"
    w@(WitVKey vk sig) ->
      checkEncoding "vkey_witnesses"
      w  -- Transaction _witnessVKeySet element
      ( T (TkListLen 2)
        <> S vk -- vkey
        <> S sig -- signature
      )
  , checkEncoding "script_hash_to_scripts"
    (Map.singleton (hashScript testScript :: ScriptHash) testScript)  -- Transaction _witnessMSigMap
    ( T (TkMapLen 1)
      <> S (hashScript testScript :: ScriptHash)
      <> S testScript
    )
  , let
      tin = Set.fromList [TxIn genesisId 1]
      tout = [TxOut testAddrE (Coin 2)]
      reg = RegKey (KeyHashObj testKeyHash1)
      ra = RewardAcnt (KeyHashObj testKeyHash2)
      ras = Map.singleton ra (Coin 123)
      up = Update
             (PPUpdate (Map.singleton
                         testGKeyHash
                         (Set.singleton (Nopt 100))))
             (AVUpdate (Map.singleton
                         testGKeyHash
                         (Applications (Map.singleton
                                (ApName $ pack "Daedalus")
                                (ApVer 17
                                , Mdt $ Map.singleton
                                    (SystemTag $ pack "DOS")
                                    testInstallerHash
                                )))))
    in checkEncoding "txbody_full"
    ( TxBody -- transaction body with all components
        tin
        tout
        (Seq.fromList [ reg ])
        ras
        (Coin 9)
        (Slot 500)
        up
     )
     ( T (TkMapLen 7)
       <> T (TkWord 0) -- Tx Ins
       <> S tin
       <> T (TkWord 1) -- Tx Outs
       <> S tout
       <> T (TkWord 2) -- Tx Fee
       <> S (Coin 9)
       <> T (TkWord 3) -- Tx TTL
       <> S (Slot 500)
       <> T (TkWord 4) -- Tx Certs
       <> T (TkListLen 1) -- Seq list begin
       <> S reg
       <> T (TkWord 5) -- Tx Reward Withdrawals
       <> S ras
       <> T (TkWord 6) -- Tx Update
       <> S up
      )
  , checkEncoding "register script"
    (DeRegKey (ScriptHashObj testScriptHash))
    ( T (TkListLen 2)
      <> T (TkWord 3) -- DeReg cert
      <> S testScriptHash -- keyhash
    )
  , checkEncoding "deregister_key"
    (DeRegKey (KeyHashObj testKeyHash1))
    ( T (TkListLen 2)
      <> T (TkWord 2) -- DeReg cert
      <> S testKeyHash1 -- keyhash
    )
  , checkEncoding "deregister_script"
    (DeRegKey (ScriptHashObj testScriptHash))
    ( T (TkListLen 2)
      <> T (TkWord 3) -- DeReg cert
      <> S testScriptHash -- script hash
    )
  , let poolOwner = testKeyHash2
        poolMargin = unsafeMkUnitInterval 0.7
        poolRAcnt = RewardAcnt (KeyHashObj testKeyHash1)
        poolPledge = Coin 11
        poolCost = Coin 55
    in
    checkEncoding "register-pool"
    (RegPool (PoolParams
               { _poolPubKey = testKeyHash1
               , _poolVrf = testVRFKH
               , _poolPledge = poolPledge
               , _poolCost = poolCost
               , _poolMargin = poolMargin
               , _poolRAcnt = poolRAcnt
               , _poolOwners = Set.singleton poolOwner
               }))
    ( T (TkListLen 8)
      <> T (TkWord 6) -- Reg Pool
      <> T (TkTag 258 . TkListLen 1) <> S poolOwner   -- owners
      <> S poolCost     -- cost
      <> S poolMargin   -- margin
      <> S poolPledge   -- pledge
      <> S testKeyHash1 -- operator
      <> S testVRFKH    -- vrf keyhash
      <> S poolRAcnt    -- reward acct
    )
  , checkEncoding "retire_pool"
    (RetirePool testKeyHash1 (Epoch 1729))
    ( T (TkListLen 3
      . TkWord 7) -- Pool Retire
      <> S testKeyHash1 -- key hash
      <> S (Epoch 1729) -- epoch
    )
  , checkEncoding "Key delegation"
    (Delegate (Delegation (KeyHashObj testKeyHash1) testKeyHash2))
    ( T (TkListLen 3
      . TkWord 4) -- delegation cert with key
      <> S testKeyHash1
      <> S testKeyHash2
    )
  , checkEncoding "script delegation"
    (Delegate (Delegation (ScriptHashObj testScriptHash) testKeyHash2))
    ( T (TkListLen 3
      . TkWord 5) -- delegation cert with script
      <> S testScriptHash
      <> S testKeyHash2
    )
  , checkEncoding "genesis-delegation"
    (GenesisDelegate (testGKeyHash, testKeyHash1))
    ( T (TkListLen 3
      . TkWord 8) -- genesis delegation cert
      <> S testGKeyHash -- delegator credential
      <> S testKeyHash1 -- delegatee key hash
    )
  , let rs = Map.singleton (KeyHashObj testKeyHash1) 77
    in
    checkEncoding "mir"
    (InstantaneousRewards rs)
    ( T (TkListLen 2
       . TkWord 9) -- make instantaneous rewards cert
      <> S rs
    )
 ]
