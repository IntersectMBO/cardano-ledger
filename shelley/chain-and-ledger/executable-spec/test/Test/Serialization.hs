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


import           BaseTypes (UnitInterval (..))
import           Coin (Coin (..))
import           Data.Ratio ((%))
import           Delegation.Certificates (pattern RegKey)
import           Keys (DiscVKey (..), pattern GenKeyHash, pattern KeyHash, pattern KeyPair,
                     pattern UnsafeSig, hash, hashKey, sKey, sign, undiscriminateKeyHash, vKey)
import           LedgerState (genesisId)
import           Slot (Slot (..))
import           Test.Utils
import           Tx (hashScript)
import           TxData (pattern AddrBase, pattern AddrEnterprise, pattern AddrPtr, Credential (..),
                     Ptr (..), pattern RequireSignature, pattern RewardAcnt, pattern ScriptHash,
                     pattern Tx, pattern TxBody, pattern TxIn, pattern TxOut, _TxId)
import           Updates (pattern AVUpdate, ApName (..), ApVer (..), pattern Applications,
                     pattern InstallerHash, pattern Mdt, pattern PPUpdate, Ppm (..),
                     SystemTag (..), pattern Update, emptyUpdate)

import           MockTypes (Addr, GenKeyHash, InstallerHash, KeyHash, KeyPair, MultiSig, ScriptHash,
                     Sig, TxBody, TxId, TxIn, TxOut, VKey, WitVKey)
import           UTxO (makeWitnessVKey)

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Typeable (Typeable, typeOf)



data EncodingPair where
  EncodePair :: (Typeable a, ToCBOR a) => a -> Encoding -> EncodingPair

(#>) :: (ToCBOR a) => a -> Encoding -> EncodingPair
(#>) = EncodePair

checkEncoding :: EncodingPair -> TestTree
checkEncoding (EncodePair x t) = testCase testName $
  assertEqual typeName (fromEncoding t) (fromEncoding $ toCBOR x)
  where
   typeName = show (typeOf x)
   testName = "prop_serialize_" <> map (\c -> if c == ' ' then '-' else c) typeName

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

testGKeyHash :: GenKeyHash
testGKeyHash = (hashKey . snd . mkGenKeys) (0, 0, 0, 0, 0)

testTxb :: TxBody
testTxb = TxBody Set.empty [] Seq.empty Map.empty (Coin 0) (Slot 0) emptyUpdate

testKey1 :: KeyPair
testKey1 = KeyPair vk sk
  where (sk, vk) = mkKeyPair (0, 0, 0, 0, 1)

testKeyWit1 :: WitVKey
testKeyWit1 = makeWitnessVKey testTxb testKey1

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

serializationTests :: TestTree
serializationTests = testGroup "Serialization Tests" $ checkEncoding <$>
  [ Coin 30 #> Encoding (TkWord64 30)
  , UnsafeUnitInterval (1 % 2) #> Encoding (TkWord64 1 . TkWord64 2)
  , Slot 7 #> Encoding (TkWord64 7)
  , testKeyHash1 #> Encoding (TkBytes (getRawKeyHash testKeyHash1))
  , KeyHashObj testKeyHash1 #>
      Encoding (TkListLen 2 . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1))
  , AddrBase (KeyHashObj testKeyHash1) (KeyHashObj testKeyHash2) #>
      Encoding (TkWord 0
        . TkBytes (getRawKeyHash testKeyHash1)
        . TkBytes (getRawKeyHash testKeyHash2))
  , AddrPtr (KeyHashObj testKeyHash1) (Ptr (Slot 12) 0 3) #>
      Encoding (TkWord 4
        . TkBytes (getRawKeyHash testKeyHash1)
        . TkWord64 12 . TkWord 0 . TkWord 3)
  , AddrEnterprise (KeyHashObj testKeyHash1) #>
      Encoding (TkWord 6
        . TkBytes (getRawKeyHash testKeyHash1))
  , (TxIn genesisId 0 :: TxIn) #>
      Encoding (TkListLen 2 . TkBytes (getRawTxId genesisId) . TkInteger 0)
  , (TxOut (AddrEnterprise (KeyHashObj testKeyHash1)) (Coin 2) :: TxOut) #>
      Encoding (TkListLen 2 . TkWord 6
        . TkBytes (getRawKeyHash testKeyHash1) . TkWord64  2)
  , (Map.fromList [(10, 1), (20, 2), (30, 3)] :: (Map.Map Int Int)) #>
      Encoding (TkMapLen 3 . TkInt 10 . TkInt 1 . TkInt 20 . TkInt 2 . TkInt 30 . TkInt 3)
  , TxBody -- minimal transaction body
      (Set.fromList [TxIn genesisId 1])
      [TxOut testAddrE (Coin 2)]
      Seq.empty
      Map.empty
      (Coin 9)
      (Slot 500)
      emptyUpdate #>

      Encoding (TkMapLen 6
        . TkWord 0 -- Tx Ins
          . TkTag 258 . TkListLen 1 -- one input
            . TkListLen 2 . TkBytes (getRawTxId genesisId) . TkInteger 1
        . TkWord 1 -- Tx Outs
          . TkListBegin
            . TkListLen 2 . TkWord 6 . TkBytes (getRawKeyHash testKeyHash1) . TkWord64 2
          . TkBreak
        . TkWord 2 -- Tx Fee
          . TkWord64 9
        . TkWord 3 -- Tx TTL
          . TkWord64 500
      )
  , Tx -- minimal transaction
      (TxBody
        (Set.fromList [TxIn genesisId 1])
        [TxOut testAddrE (Coin 2)]
        Seq.empty
        Map.empty
        (Coin 9)
        (Slot 500)
        emptyUpdate)
      Set.empty
      Map.empty #>

      Encoding (TkListLen 3
        . TkMapLen 6 -- Transaction
          . TkWord 0 -- Tx Ins
            . TkTag 258 . TkListLen 1 -- one input
              . TkListLen 2 . TkBytes (getRawTxId genesisId) . TkInteger 1
          . TkWord 1 -- Tx Outs
            . TkListBegin
              . TkListLen 2 . TkWord 6 . TkBytes (getRawKeyHash testKeyHash1) . TkWord64 2
            . TkBreak
          . TkWord 2 -- Tx Fee
            . TkWord64 9
          . TkWord 3 -- Tx TTL
            . TkWord64 500
        . TkTag 258 . TkListLen 0 -- no key witnesses
        . TkMapLen 0 -- no scripts
      )
  , Set.singleton testKeyWit1 #> -- Transaction _witnessVKeySet
      Encoding (TkTag 258  . TkListLen 1 -- one vkey witness
        . TkListLen 2
        . testKey1Token -- vkey
        . testKey1SigToken -- signature
      )
  , Map.singleton (hashScript testScript :: ScriptHash) testScript #> -- Transaction _witnessMSigMap
      Encoding (TkMapLen 1
        . TkBytes (getRawScriptHash $ hashScript testScript)
        . TkListLen 2 . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1)
      )
  , TxBody -- transaction body with all components
      (Set.fromList [TxIn genesisId 1])
      [TxOut testAddrE (Coin 2)]
      (Seq.fromList [ RegKey (KeyHashObj testKeyHash1) ])
      (Map.singleton (RewardAcnt (KeyHashObj testKeyHash2)) (Coin 123))
      (Coin 9)
      (Slot 500)
      (Update
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
      ) #>

      Encoding (TkMapLen 6
        . TkWord 0 -- Tx Ins
          . TkTag 258 . TkListLen 1 -- one input
            . TkListLen 2 . TkBytes (getRawTxId genesisId) . TkInteger 1
        . TkWord 1 -- Tx Outs
          . TkListBegin
            . TkListLen 2 . TkWord 6 . TkBytes (getRawKeyHash testKeyHash1) . TkWord64 2
          . TkBreak
        . TkWord 2 -- Tx Fee
          . TkWord64 9
        . TkWord 3 -- Tx TTL
          . TkWord64 500
        . TkWord 4 -- Tx Certs
          . TkListBegin
            -- A stake key registration certificate
            . TkListLen 2
              . TkWord 0 -- Reg Cert
              . TkListLen 2 . TkWord 0 . TkBytes (getRawKeyHash testKeyHash1)
          . TkBreak
        . TkWord 5 -- Tx Reward Withdrawals
          . TkMapLen 1 -- one withdrawal
            . TkListLen 2 . TkWord 0 . TkBytes (getRawKeyHash testKeyHash2)
            . TkWord64 123
        . TkWord 6 -- Tx Reward Withdrawals
          . TkListLen 2
            . TkMapLen 1 -- one protocol parameter update
              . TkBytes (getRawGenKeyHash testGKeyHash)
              . TkTag 258 . TkListLen 1 -- one paratemer change
                . TkListLen 2 . TkWord 11 . TkInteger 100 -- set nopt (word 11) to 100
            . TkMapLen 1 -- one software version update
              . TkBytes (getRawGenKeyHash testGKeyHash) -- genesis key hash
              . TkMapLen 1 -- one application
                . TkBytes (pack "Daedalus")
                . TkListLen 2
                  . TkInteger 17 -- version 17
                  . TkMapLen 1 -- metadata
                    . TkBytes (pack "DOS") -- system tag
                    . TkBytes (getRawInstallerHash testInstallerHash)
      )
  ]

