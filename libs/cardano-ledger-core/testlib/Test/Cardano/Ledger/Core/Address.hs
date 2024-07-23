{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains previous implementations for Addr and CoompactAddr
-- deserialization. This is used as an alternative implementation for testing and as a
-- perforance reference for benchmarking.
module Test.Cardano.Ledger.Core.Address (
  deserialiseAddrOld,
  deserialiseRewardAccountOld,
  decompactAddrOld,
  decompactAddrOldLazy,

  -- * Deprecations
  deserialiseRewardAcntOld,
)
where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress (BootstrapAddress),
  CompactAddr,
  RewardAccount (..),
  Word7 (..),
  toWord7,
  unCompactAddr,
 )
import Cardano.Ledger.BaseTypes (CertIx (..), SlotNo (..), TxIx (..), word8ToNetwork)
import Cardano.Ledger.Binary (byronProtVer, decodeFull, decodeFull')
import Cardano.Ledger.Credential (
  Credential (..),
  PaymentCredential,
  Ptr (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (ADDRHASH, Crypto)
import Cardano.Ledger.Hashes (ScriptHash (ScriptHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Control.Monad (ap)
import qualified Control.Monad.Fail
import Data.Binary (Get)
import qualified Data.Binary.Get as B
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short as SBS (ShortByteString, fromShort, index, length)
import Data.ByteString.Short.Internal as SBS (ShortByteString (SBS))
import Data.Foldable as F (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Word (Word64, Word8)

------------------------------------------------------------------------------------------
-- Old Address Deserializer --------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAddrOld :: forall c m. (Crypto c, MonadFail m) => BS.ByteString -> m (Addr c)
deserialiseAddrOld bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, message) ->
    fail $ "Old Addr decoder failed: " <> fromString message
  Right (remaining, _offset, result) ->
    if BSL.null remaining
      then pure result
      else fail $ "Old Addr decoder did not consume all input"

-- | Deserialise an reward account from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseRewardAccountOld ::
  forall c m. (Crypto c, MonadFail m) => BS.ByteString -> m (RewardAccount c)
deserialiseRewardAccountOld bs = case B.runGetOrFail getRewardAccount (BSL.fromStrict bs) of
  Left (_remaining, _offset, message) ->
    fail $ "Old RewardAcnt decoder failed: " <> fromString message
  Right (remaining, _offset, result) ->
    if BSL.null remaining
      then pure result
      else fail $ "Old RewardAcnt decoder did not consume all input"

deserialiseRewardAcntOld ::
  forall c m. (Crypto c, MonadFail m) => BS.ByteString -> m (RewardAccount c)
deserialiseRewardAcntOld = deserialiseRewardAccountOld
{-# DEPRECATED deserialiseRewardAcntOld "Use `deserialiseRewardAccountOld` instead" #-}

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

rewardCredIsScript :: Int
rewardCredIsScript = 4

getAddr :: Crypto c => Get (Addr c)
getAddr = do
  header <- B.lookAhead B.getWord8
  if testBit header byron
    then getByron
    else do
      _ <- B.getWord8 -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> Addr n <$> getPayCred header <*> getStakeReference header
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

getRewardAccount :: Crypto c => Get (RewardAccount c)
getRewardAccount = do
  header <- B.getWord8
  let rewardAccountPrefix = 0xE0 -- 0b11100000 are always set for reward accounts
      isRewardAccount = (header .&. rewardAccountPrefix) == rewardAccountPrefix
      netId = header .&. 0x0F -- 0b00001111 is the mask for the network id
  case (word8ToNetwork netId, isRewardAccount) of
    (Nothing, _) ->
      fail $ concat ["Reward account with unknown network Id. (", show netId, ")"]
    (_, False) ->
      fail $ concat ["Expected reward account. Got account with header: ", show header]
    (Just network, True) -> do
      cred <- case testBit header rewardCredIsScript of
        True -> getScriptHash
        False -> getKeyHash
      pure $ RewardAccount network cred

getHash :: forall h a. Hash.HashAlgorithm h => Get (Hash.Hash h a)
getHash = do
  bytes <- B.getByteString . fromIntegral $ Hash.sizeHash ([] @h)
  case Hash.hashFromBytes bytes of
    Nothing -> fail "getHash: implausible hash length mismatch"
    Just !h -> pure h

getPayCred :: Crypto c => Word8 -> Get (PaymentCredential c)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: Crypto c => Get (Credential kr c)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: Crypto c => Get (Credential kr c)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: Crypto c => Word8 -> Get (StakeReference c)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

getByron :: Get (Addr c)
getByron =
  decodeFull byronProtVer <$> B.getRemainingLazyByteString >>= \case
    Left e -> fail (show e)
    Right r -> pure $ AddrBootstrap $ BootstrapAddress r

getPtr :: Get Ptr
getPtr =
  Ptr
    <$> (SlotNo <$> getVariableLengthWord64)
    <*> (TxIx . fromIntegral <$> getVariableLengthWord64)
    <*> (CertIx . fromIntegral <$> getVariableLengthWord64)

getWord7s :: Get [Word7]
getWord7s = do
  next <- B.getWord8
  -- is the high bit set?
  if testBit next 7
    then -- if so, grab more words
      (:) (toWord7 next) <$> getWord7s
    else -- otherwise, this is the last one
      pure [Word7 next]

-- invariant: length [Word7] < 8
word7sToWord64 :: [Word7] -> Word64
word7sToWord64 = F.foldl' f 0
  where
    f n (Word7 r) = shiftL n 7 .|. fromIntegral r

getVariableLengthWord64 :: Get Word64
getVariableLengthWord64 = word7sToWord64 <$> getWord7s

------------------------------------------------------------------------------------------
-- Old Compact Address Deserializer ------------------------------------------------------
------------------------------------------------------------------------------------------

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

instance Control.Monad.Fail.MonadFail GetShort where
  fail _ = GetShort $ \_ _ -> Nothing

getShortBootstrapAddress :: GetShort (BootstrapAddress c)
getShortBootstrapAddress = do
  bs <- getShortRemainingAsByteString
  case decodeFull' byronProtVer bs of
    Left e -> fail $ show e
    Right r -> pure $ BootstrapAddress r

getShortWord :: GetShort Word8
getShortWord = GetShort $ \i sbs ->
  if i < SBS.length sbs
    then Just (i + 1, SBS.index sbs i)
    else Nothing

peekWord8 :: GetShort Word8
peekWord8 = GetShort peek
  where
    peek i sbs = if i < SBS.length sbs then Just (i, SBS.index sbs i) else Nothing

getShortRemainingAsByteString :: GetShort BS.ByteString
getShortRemainingAsByteString = GetShort $ \i sbs ->
  let l = SBS.length sbs
   in if i < l
        then Just (l, SBS.fromShort $ substring sbs i l)
        else Nothing

getShortHash :: forall a h. Hash.HashAlgorithm h => GetShort (Hash.Hash h a)
getShortHash = GetShort $ \i sbs ->
  let hashLen = Hash.sizeHash ([] @h)
      offsetStop = i + fromIntegral hashLen
   in if offsetStop <= SBS.length sbs
        then do
          hash <- Hash.hashFromBytesShort $ substring sbs i offsetStop
          Just (offsetStop, hash)
        else Nothing

-- start is the first index copied
-- stop is the index after the last index copied
substring :: ShortByteString -> Int -> Int -> ShortByteString
substring (SBS ba) start stop =
  case BA.cloneByteArray (BA.ByteArray ba) start (stop - start) of
    BA.ByteArray ba' -> SBS ba'

getShortWord7s :: GetShort [Word7]
getShortWord7s = do
  next <- getShortWord
  -- is the high bit set?
  if testBit next 7
    then -- if so, grab more words
      (:) (toWord7 next) <$> getShortWord7s
    else -- otherwise, this is the last one
      pure [Word7 next]

getShortVariableLengthWord64 :: GetShort Word64
getShortVariableLengthWord64 = word7sToWord64 <$> getShortWord7s

getShortPtr :: GetShort Ptr
getShortPtr =
  Ptr
    <$> (SlotNo <$> getShortVariableLengthWord64)
    <*> (TxIx . fromIntegral <$> getShortVariableLengthWord64)
    <*> (CertIx . fromIntegral <$> getShortVariableLengthWord64)

getShortKeyHash :: Crypto c => GetShort (Credential kr c)
getShortKeyHash = KeyHashObj . KeyHash <$> getShortHash

getShortScriptHash :: Crypto c => GetShort (Credential kr c)
getShortScriptHash = ScriptHashObj . ScriptHash <$> getShortHash

getShortStakeReference :: Crypto c => Word8 -> GetShort (StakeReference c)
getShortStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getShortPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getShortScriptHash
    False -> StakeRefBase <$> getShortKeyHash

getShortPayCred :: Crypto c => Word8 -> GetShort (PaymentCredential c)
getShortPayCred header = case testBit header payCredIsScript of
  True -> getShortScriptHash
  False -> getShortKeyHash

getShortShortAddr :: Crypto c => GetShort (Addr c)
getShortShortAddr = do
  header <- peekWord8
  if testBit header byron
    then AddrBootstrap <$> getShortBootstrapAddress
    else do
      _ <- getShortWord -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> do
          c <- getShortPayCred header
          h <- getShortStakeReference header
          pure (Addr n c h)
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

-- | This is an old decompacter that didn't guard against random junk at the end.
decompactAddrOld :: Crypto c => CompactAddr c -> Addr c
decompactAddrOld cAddr =
  snd . unwrap "CompactAddr" $ runGetShort getShortShortAddr 0 (unCompactAddr cAddr)
  where
    -- The reason failure is impossible here is that the only way to call this code
    -- is using a CompactAddr, which can only be constructed using compactAddr.
    -- compactAddr serializes an Addr, so this is guaranteed to work.
    unwrap :: forall a. Text -> Maybe a -> a
    unwrap name = fromMaybe (error $ unpack $ "Impossible failure when decoding " <> name)

-- | This lazy deserializer is kept around purely for benchmarking, so we can
-- verify that new deserializer `decodeAddrStateT` is doing the work lazily.
decompactAddrOldLazy :: forall c. Crypto c => CompactAddr c -> Addr c
decompactAddrOldLazy cAddr =
  if testBit header byron
    then AddrBootstrap $ run "byron address" 0 bytes getShortBootstrapAddress
    else Addr addrNetId paycred stakecred
  where
    bytes = unCompactAddr cAddr
    run :: forall a. Text -> Int -> ShortByteString -> GetShort a -> a
    run name i sbs g = snd . unwrap name $ runGetShort g i sbs
    -- The reason failure is impossible here is that the only way to call this code
    -- is using a CompactAddr, which can only be constructed using compactAddr.
    -- compactAddr serializes an Addr, so this is guaranteed to work.
    unwrap :: forall a. Text -> Maybe a -> a
    unwrap name = fromMaybe (error $ unpack $ "Impossible failure when decoding " <> name)
    header = run "address header" 0 bytes getShortWord
    addrNetId =
      unwrap "address network id" $
        word8ToNetwork $
          header .&. 0x0F -- 0b00001111 is the mask for the network id
          -- The address format is
          -- header | pay cred | stake cred
          -- where the header is 1 byte
          -- the pay cred is (sizeHash (ADDRHASH crypto)) bytes
          -- and the stake cred can vary
    paycred = run "payment credential" 1 bytes (getShortPayCred header)
    stakecred = run "staking credential" 1 bytes $ do
      skipHash ([] @(ADDRHASH c))
      getShortStakeReference header
    skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
    skipHash p = skip . fromIntegral $ Hash.sizeHash p
    skip :: Int -> GetShort ()
    skip n = GetShort $ \i sbs ->
      let offsetStop = i + n
       in if offsetStop <= SBS.length sbs
            then Just (offsetStop, ())
            else Nothing
{-# INLINE decompactAddrOldLazy #-}
