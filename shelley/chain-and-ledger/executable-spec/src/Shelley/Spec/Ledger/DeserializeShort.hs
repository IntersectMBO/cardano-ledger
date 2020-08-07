{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash       #-}

module Shelley.Spec.Ledger.DeserializeShort
  ( deserialiseAddrStakeRef,
    deserializeShortAddr,
  )
where


import qualified Cardano.Crypto.Hash.Class as Hash
import Control.Monad (ap)
import Control.Monad (join)
import Data.Bits (testBit, (.&.))
import Data.ByteString.Short as SBS
import Data.Word (Word8)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address
  ( Word7 (..),
    Addr(..),
    byron,
    isEnterpriseAddr,
    notBaseAddr,
    payCredIsScript,
    stakeCredIsScript,
    toWord7,
    word7sToNat,
  )
import Shelley.Spec.Ledger.BaseTypes (word8ToNetwork)
import Shelley.Spec.Ledger.Credential
  ( Credential(KeyHashObj,ScriptHashObj),
    Ptr (..),
    StakeReference (..),
    PaymentCredential,
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Keys (KeyHash (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))

newtype GetShort a = GetShort {runGetShort :: Int -> ShortByteString -> Maybe (Int, a)}
  deriving (Functor)

instance Applicative GetShort where
  pure a = GetShort $ \i _sbs -> Just (i, a)
  (<*>) = ap

instance Monad GetShort where
  (GetShort g) >>= f = GetShort $ \i sbs ->
    case g i sbs of
      Nothing -> Nothing
      Just (i', x) -> runGetShort (f x) i' sbs

deserialiseAddrStakeRef :: Crypto crypto => ShortByteString -> Maybe (StakeReference crypto)
deserialiseAddrStakeRef sbs = join $ snd <$> runGetShort getAddrStakeReference 0 sbs

getAddrStakeReference :: forall crypto. Crypto crypto => GetShort (Maybe (StakeReference crypto))
getAddrStakeReference = do
  header <- getWord
  if testBit header byron
    then pure Nothing
    else skipHash ([] @(HASH crypto)) >> Just <$> getStakeReference header

deserializeShortAddr:: Crypto crypto => ShortByteString -> Maybe (Addr crypto)
deserializeShortAddr short =
   case runGetShort getShortAddr 0 short of
     Just(_,maybe_addr) -> maybe_addr
     Nothing -> Nothing

getShortAddr :: forall crypto. Crypto crypto => GetShort (Maybe (Addr crypto))
getShortAddr = do
  header <- peekWord8
  if testBit header byron
    then pure Nothing
    else do
      _ <- getWord -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> do { c <- getPayCred header; h <- getStakeReference header ; pure(Just(Addr n c h))}
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

getWord :: GetShort Word8
getWord = GetShort $ \i sbs ->
  if i < SBS.length sbs
    then Just (i + 1, SBS.index sbs i)
    else Nothing

peekWord8 :: GetShort Word8
peekWord8 = GetShort peek
  where peek i sbs =  if i < SBS.length sbs then Just(i,SBS.index sbs i) else Nothing

skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
skipHash p = skip . fromIntegral $ Hash.sizeHash p

getHash :: forall a h. Hash.HashAlgorithm h => GetShort (Hash.Hash h a)
getHash = GetShort $ \i sbs ->
  let hashLen = Hash.sizeHash ([] @h)
      offsetStop = i + fromIntegral hashLen
   in if offsetStop <= SBS.length sbs
        then Just (offsetStop, Hash.UnsafeHash (substring sbs i offsetStop))
        else Nothing

substring :: ShortByteString -> Int -> Int -> ShortByteString
substring sbs start stop = SBS.pack [SBS.index sbs n | n <- [start .. stop -1]]

skip :: Int -> GetShort ()
skip n = GetShort $ \i sbs ->
  let offsetStop = i + n
   in if offsetStop <= SBS.length sbs
        then Just (offsetStop, ())
        else Nothing

getWord7s :: GetShort [Word7]
getWord7s = do
  next <- getWord
  case next .&. 0x80 of -- 0x80 ~ 0b10000000
  -- is the high bit set?
  -- if so, grab more words
    0x80 -> (:) (toWord7 next) <$> getWord7s
    -- otherwise, this is the last one
    _ -> pure [Word7 next]

getVariableLengthNat :: GetShort Natural
getVariableLengthNat = word7sToNat <$> getWord7s

getPtr :: GetShort Ptr
getPtr =
  Ptr <$> (SlotNo . fromIntegral <$> getVariableLengthNat)
    <*> getVariableLengthNat
    <*> getVariableLengthNat

getKeyHash :: Crypto crypto => GetShort (Credential kr crypto)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getScriptHash :: Crypto crypto => GetShort (Credential kr crypto)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getStakeReference :: Crypto crypto => Word8 -> GetShort (StakeReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

getPayCred :: Crypto crypto => Word8 -> GetShort (PaymentCredential crypto)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash
