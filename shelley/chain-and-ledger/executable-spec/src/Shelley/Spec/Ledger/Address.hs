{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Address
  ( mkVKeyRwdAcnt
  , mkRwdAcnt
  , scriptsToAddr
  , scriptToCred
  , toAddr
  , toCred
  , serialiseAddr
  , deserialiseAddr
  , Addr (..)

  -- internals exported for testing
  , getAddr
  , getKeyHash
  , getPtr
  , getScriptHash
  , getVariableLengthNat
  , putAddr
  , putCredential
  , putPtr
  , putVariableLengthNat
  , natToWord7s
  , word7sToNat
  , Word7 (..)
  )
where

import           Cardano.Prelude (NoUnexpectedThunks, NFData, cborError)

import           Cardano.Binary (ToCBOR(..), FromCBOR(..), DecoderError (..))
import qualified Cardano.Crypto.Hash.Class as Hash

import           Control.Monad (unless)
import           Data.Binary (Get, Put, Word8)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import           Data.Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (foldl')
import           Data.String (fromString)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.Credential (Credential (..), PaymentCredential, Ptr (..),
                     RewardAcnt (..), StakeReference (..))
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Keys (KeyRole (..), KeyPair(..), hashKey)

import           Shelley.Spec.Ledger.Slot (SlotNo (..))
import           Shelley.Spec.Ledger.Scripts

mkVKeyRwdAcnt
  :: Crypto crypto
  => KeyPair 'Staking crypto
  -> RewardAcnt crypto
mkVKeyRwdAcnt keys = RewardAcnt $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt
  :: Credential 'Staking crypto
  -> RewardAcnt crypto
mkRwdAcnt script@(ScriptHashObj _) = RewardAcnt script
mkRwdAcnt key@(KeyHashObj _) = RewardAcnt key

toAddr
  :: Crypto crypto
  => (KeyPair 'Payment crypto, KeyPair 'Staking crypto)
  -> Addr crypto
toAddr (payKey, stakeKey) = Addr (toCred payKey) (StakeRefBase $ toCred stakeKey)

toCred
  :: Crypto crypto
  => KeyPair kr crypto
  -> Credential kr crypto
toCred k = KeyHashObj . hashKey $ vKey k

-- | Convert a given multi-sig script to a credential by hashing it and wrapping
-- into the `Credential` data type.
--
-- TODO nc what is the role of this credential?
scriptToCred :: Crypto crypto => MultiSig crypto -> Credential kr crypto
scriptToCred = ScriptHashObj . hashMultiSigScript

-- | Create a base address from a pair of multi-sig scripts (pay and stake)
scriptsToAddr :: Crypto crypto => (MultiSig crypto, MultiSig crypto) -> Addr crypto
scriptsToAddr (payScript, stakeScript) =
  Addr (scriptToCred payScript) (StakeRefBase $ scriptToCred stakeScript)

-- | Serialise an address to the external format.
--
-- Note: this is not the final format, this function will be updated later to
-- use the final format.
--
-- See <https://github.com/input-output-hk/cardano-ledger-specs/issues/1367>
--
serialiseAddr :: Crypto crypto => Addr crypto -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
--
-- Note: this is not the final format, this function will be updated later to
-- use the final format.
--
-- See <https://github.com/input-output-hk/cardano-ledger-specs/issues/1367>
--
deserialiseAddr :: Crypto crypto => ByteString -> Maybe (Addr crypto)
deserialiseAddr bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- |An address for UTxO.
data Addr crypto
  = Addr !(PaymentCredential crypto) !(StakeReference crypto)
  | AddrBootstrap !(KeyHash crypto) -- TODO: replace with bigger byron address
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (Addr crypto)

byron :: Int
byron = 7
notBaseAddr :: Int
notBaseAddr = 6
isEnterpriseAddr :: Int
isEnterpriseAddr = 5
stakeCredIsScript :: Int
stakeCredIsScript = 5
payCredIsScript :: Int
payCredIsScript = 4

putAddr :: forall crypto. Crypto crypto => Addr crypto -> Put
putAddr (AddrBootstrap _kh) = undefined -- TODO: defer to byron
putAddr (Addr pc sr) =
  let setPayCredBit = case pc of
          ScriptHashObj _ -> flip setBit payCredIsScript
          KeyHashObj _ -> id
      netId = networkToWord8 $ networkMagicId ([] @crypto)
   in case sr of
        StakeRefBase sc -> do
          let setStakeCredBit = case sc of
                      ScriptHashObj _ -> flip setBit stakeCredIsScript
                      KeyHashObj _ -> id
              header = setStakeCredBit . setPayCredBit $ netId
          B.putWord8 header
          putCredential pc
          putCredential sc
        StakeRefPtr ptr -> do
          let header = setPayCredBit $ netId `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
          putPtr ptr
        StakeRefNull -> do
          let header = setPayCredBit $ netId `setBit` isEnterpriseAddr `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc

getAddr :: forall crypto. Crypto crypto => Get (Addr crypto)
getAddr = do
  header <- B.lookAhead B.getWord8
  if testBit header byron
    then getByron
    else do
      _ <- B.getWord8 -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
          netId = networkToWord8 $ networkMagicId ([] @crypto)
      unless (addrNetId == netId) $ fail $ concat
        [ "Got address with incorrect network Id. \n"
        , "Expected: ", show netId, "\n"
        , "Got: ", show addrNetId
        ]
      Addr <$> getPayCred header <*> getStakeReference header

getHash :: forall h a. HashAlgorithm h => Get (Hash h a)
getHash = Hash.UnsafeHash <$> B.getByteString (fromIntegral $ Hash.byteCount ([] @h))

putHash :: Hash h a -> Put
putHash (Hash.UnsafeHash b) = B.putByteString b

getPayCred :: Crypto crypto => Word8 -> Get (PaymentCredential crypto)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: Crypto crypto => Get (Credential crypto)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: Crypto crypto => Get (Credential crypto)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: Crypto crypto => Word8 -> Get (StakeReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

putCredential :: Credential crypto -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h

getByron :: Get (Addr crypto)
getByron = undefined

putPtr :: Ptr -> Put
putPtr (Ptr slot txIx certIx)= do
  putSlot slot
  putVariableLengthNat txIx
  putVariableLengthNat certIx
  where
  putSlot (SlotNo n) = putVariableLengthNat . fromIntegral $ n

getPtr :: Get Ptr
getPtr = Ptr <$> (SlotNo . fromIntegral <$> getVariableLengthNat)
             <*> getVariableLengthNat
             <*> getVariableLengthNat

newtype Word7 = Word7 Word8
  deriving (Eq, Show)

toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F) -- 0x7F = 0b01111111

putWord7s :: [Word7] -> Put
putWord7s [] = pure ()
putWord7s [Word7 x] = B.putWord8 x
putWord7s (Word7 x: xs) = B.putWord8 (x .|. 0x80) >> putWord7s xs

getWord7s :: Get [Word7]
getWord7s = do
  next <- B.getWord8
  case next .&. 0x80 of
    0x80 -> (:) (toWord7 next) <$> getWord7s
    _ -> pure [Word7 next]

natToWord7s :: Natural -> [Word7]
natToWord7s = reverse . go
  where
   go n | n <= 0x7F = [Word7 . fromIntegral $ n]
        | otherwise = (toWord7 . fromIntegral) n : go (shiftR n 7)

putVariableLengthNat :: Natural -> Put
putVariableLengthNat = putWord7s . natToWord7s

word7sToNat :: [Word7] -> Natural
word7sToNat = foldl' f 0
  where
  f n (Word7 r) = shiftL n 7 .|. (fromIntegral r)

getVariableLengthNat :: Get Natural
getVariableLengthNat = word7sToNat <$> getWord7s

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (Addr crypto)
 where
  toCBOR = toCBOR . B.runPut . putAddr

instance Crypto crypto => FromCBOR (Addr crypto) where
  fromCBOR = do
    bytes <- fromCBOR
    case B.runGetOrFail getAddr bytes of
      Right (_remaining,_offset,value) -> pure value
      Left (_remaining,_offset,message) ->
        cborError (DecoderErrorCustom "Addr" $ fromString message)
