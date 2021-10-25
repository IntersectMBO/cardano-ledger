{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE  RankNTypes  #-}
{-# LANGUAGE  ScopedTypeVariables  #-}
{-# LANGUAGE  TypeApplications  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Alonzo.CompactTxOut where

import Data.ByteString(ByteString,pack,unpack,index)
import Cardano.Binary(serialize')
import qualified Data.ByteString as BS
import Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut (..))
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr(..), compactAddr, decompactAddr)
import Cardano.Ledger.BaseTypes(StrictMaybe(..),Network(..))
import Cardano.Ledger.Era(Era,Crypto)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Alonzo.Data(DataHash(..))
import Cardano.Ledger.SafeHash(SafeHash(..),extractHash,unsafeMakeSafeHash)
import Cardano.Crypto.Hash.Class(Hash(),hash,hashToBytes,hashFromBytesShort,hashWith,HashAlgorithm,castHash)

import Cardano.Ledger.Address(Addr(..))
import Cardano.Ledger.Credential
  ( Credential(..),
    PaymentCredential(..),
    StakeReference(..),
    StakeCredential(..),
    Ptr(..),
    Ix(..))
import Cardano.Ledger.Hashes(ScriptHash(..))
import Cardano.Ledger.Keys(KeyHash(..))
import Cardano.Slotting.Slot(SlotNo(..))
import Cardano.Ledger.Mary.Value(Value(..))
import qualified Data.Map as Map
import Data.Word(Word8,Word64)
import Data.Bits
  ( Bits,(.&.),
    (.|.),
    complement,
    popCount,
    unsafeShiftL,
    shiftR,
    shiftL,
    setBit,
    testBit,
    clearBit,
    zeroBits,
  )
import Cardano.Ledger.Address(BootstrapAddress(..))
import Cardano.Ledger.Compactible (Compactible (..),CompactForm)
import qualified Cardano.Ledger.Crypto as CC

-- ==================================================
-- Getting to the ByteString that Underlies each Hash

safeHashToBytes :: SafeHash crypto a -> ByteString
safeHashToBytes = hashToBytes . extractHash

bytesToSafeHash ::  HashAlgorithm (CC.HASH c) => ByteString -> SafeHash c a
bytesToSafeHash x = unsafeMakeSafeHash (makeHash x)

makeHash :: HashAlgorithm c => ByteString -> Hash c a
makeHash bs = castHash (hashWith id bs)

-- ===============================================

-- | binary encoding of 'n', least significant bit on the front of the list
binary :: Integral n => n -> [n]
binary 0 = []
binary 1 = [(1)]
binary n = (mod n 2) : binary (div n 2)

-- | Show 'n' as a binary number with most significant bits on the left.
bin :: Integral n => n -> [n]
bin x = reverse (binary x)

-- ========================================================
-- More for documentation, than nything else.
-- What 'Pattern' of construction goes with which Tag

labelDataHash :: StrictMaybe (DataHash crypto) -> Int
labelDataHash SNothing = 0
labelDataHash (SJust mhash) = 1

labelValue :: Value crypto -> Int
labelValue (Value 0 m) | Map.null m = 0
labelValue (Value n m) | Map.null m = 1
labelValue (Value n m) = 2

labelAddrShare :: Addr crypto -> Int
labelAddrShare (Addr Testnet (ScriptHashObj hash1) stake) = 0
labelAddrShare (Addr Testnet (KeyHashObj hash1) stake) = 1
labelAddrShare (Addr Mainnet (ScriptHashObj hash1) stake) = 2
labelAddrShare (Addr Mainnet (KeyHashObj hash1) stake) = 3
labelAddrShare (AddrBootstrap byron) = 4


-- ======================================================
makeTag :: Word8 -> Word8 -> Word8 -> Word8
makeTag addr val dhash
  | (addr >= 0 && addr <= 4) && (val >= 0 && val <= 2) && (dhash >= 0 && dhash <= 1) =
    (addr * 8 + val * 2 + dhash) -- (shiftL addr 3 .|. shiftL val 1 .|. dhash)
  | otherwise = error ("tags are not in the correct ranges "++show (addr,val,dhash) ++"in (0-4,0-2,0-1).")

getTags :: Word8 -> (Word8,Word8,Word8)
getTags tag = (addr,val,dhash)
  where dhash = mod tag 2
        val = mod (div tag 2) 4
        addr = mod (div tag 8) 8

testTag = [ getTags(makeTag addr val dhash) == (addr,val,dhash) | addr <- [0..4], val <- [0..2], dhash <- [0..1]]

-- ===============================================

getAddrBytes :: Addr crypto -> (Word8,StakeReference crypto,ByteString)
getAddrBytes (Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake) = (0,stake,hashToBytes hash1)
getAddrBytes (Addr Testnet (KeyHashObj (KeyHash hash1)) stake)       = (1,stake,hashToBytes hash1)
getAddrBytes (Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake) = (2,stake,hashToBytes hash1)
getAddrBytes (Addr Mainnet (KeyHashObj (KeyHash hash1)) stake)       = (3,stake,hashToBytes hash1)
getAddrBytes (AddrBootstrap byron)                                   = (4,undefined,undefined)

getValueBytes :: CC.Crypto crypto => Value crypto -> (Word8,ByteString)
getValueBytes (Value 0 m) | Map.null m = (0,mempty)
getValueBytes (Value n m) | Map.null m = (1,word64ToByteString (fromIntegral n))
getValueBytes (v@(Value _ _))          = (2,serialize' (unJust(toCompact v)))
    where unJust (Just x) = x
          unJust Nothing = error ("Value does not have compact form.")

readVal :: forall crypto. Word8 -> Int -> ByteString -> (Int, Value crypto)
readVal 0 i bs = (i,Value 0 Map.empty)
readVal 1 i bs = (j, Value (fromIntegral n) Map.empty)
  where (j,n) = readWord64 i bs
readVal 2 i bs = undefined


getDataHashBytes :: StrictMaybe (DataHash crypto) -> (Word8,ByteString)
getDataHashBytes SNothing      = (0,mempty)
getDataHashBytes (SJust mhash) = (1,safeHashToBytes mhash)

-- ===============================================

data CompactTxOut era
   = PostByron !(StakeReference (Crypto era)) !ByteString
   | Byron !(BootstrapAddress (Crypto era)) !ByteString


transTxOut ::
  ( Era era,
    Show(Core.Value era),
    Core.Value era ~ Value (Crypto era)
  ) => TxOut era -> CompactTxOut era
transTxOut (TxOut addr val dhash) =
   case addrTag of
     4 -> Byron undefined (valueBytes <> dhashBytes)
     _ -> PostByron stake (tagBytes <> addrBytes <> valueBytes <>dhashBytes)
  where (dhashTag,dhashBytes) = getDataHashBytes dhash
        (valueTag,valueBytes) = getValueBytes val
        (addrTag,stake,addrBytes) = getAddrBytes addr
        tagBytes = pack[makeTag addrTag valueTag dhashTag]

decompactTxOut :: forall era.
  ( Era era,
    Core.Value era ~ Value (Crypto era)
  ) => CompactTxOut era -> TxOut era
decompactTxOut (PostByron stake bytes) = TxOut addr val dhash
  where (i1,(addrtag,valtag,dhashtag)) = readTags 0 bytes
        (i2,addr) = readAddr @(Crypto era) addrtag i1 stake bytes
        (i3,val) = readVal @(Crypto era) valtag i2 bytes
        (i4,dhash) = readDataHash @(Crypto era) dhashtag i3 bytes

readAddr :: forall crypto. Word8 -> Int -> StakeReference crypto -> ByteString -> (Int, Addr crypto)
readAddr tag i stake bs = undefined


readDataHash :: forall crypto. Word8 -> Int -> ByteString -> (Int, StrictMaybe (DataHash crypto))
readDataHash tag i bs = undefined

-- =============================================
showBS :: ByteString -> String
showBS bs = show(unpack bs)

word64ToByteString :: Word64 -> ByteString
word64ToByteString w64 = pack(loop 8 w64 [])
  where loop :: Word64 -> Word64 -> [Word8] -> [Word8]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)):ans)

readWord8:: Int -> ByteString -> (Int,Word8)
readWord8 i bs | i > (BS.length bs -1) = error ("Not enough bytes to read a Word8")
readWord8 i bs = (i+1,index bs i)

readTags:: Int -> ByteString -> (Int,(Word8,Word8,Word8))
readTags i bs | i > (BS.length bs -1) = error ("Not enough bytes to read the Tags")
readTags i bs = (i+1,getTags(index bs i))
  
readWord64:: Int -> ByteString -> (Int,Word64)
readWord64 i bs | i+7 > BS.length bs = error ("Not enough bytes to read a Word64")
readWord64 i bs = (i+8,loop 0 0)
  where loop :: Int -> Word64 -> Word64
        loop i ans | i >= 8 = ans
        loop i ans = loop (i+1) (ans * 256 + fromIntegral(index bs i))

-- | Read a (sub) ByteString of length 'len', starting at index 'i' from 'bs'
readByteString:: Int -> Int -> ByteString -> (Int,ByteString)
readByteString len i bs | i+len > BS.length bs =
    error ("Not enough bytes to read a ByteString of length "++show len)
readByteString len i bs = (i+len,BS.take len (BS.drop i bs))