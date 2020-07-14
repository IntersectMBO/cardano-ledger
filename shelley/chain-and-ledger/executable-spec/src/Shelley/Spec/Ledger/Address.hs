{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Address
  ( mkVKeyRwdAcnt,
    mkRwdAcnt,
    scriptsToAddr,
    scriptToCred,
    toAddr,
    toCred,
    serialiseAddr,
    deserialiseAddr,
    Addr (..),
    BootstrapAddress (..),
    getNetwork,
    RewardAcnt (..),
    serialiseRewardAcnt,
    deserialiseRewardAcnt,
    -- internals exported for testing
    getAddr,
    getKeyHash,
    bootstrapKeyHash,
    getPtr,
    getRewardAcnt,
    getScriptHash,
    getVariableLengthNat,
    putAddr,
    putCredential,
    putPtr,
    putRewardAcnt,
    putVariableLengthNat,
    natToWord7s,
    word7sToNat,
    Word7 (..),
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeFull,
    serialize,
  )
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Prelude (NFData, NoUnexpectedThunks, Text, cborError, panic, parseBase16)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Binary (Get, Put, Word8)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import Data.Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.BaseTypes (Network (..), networkToWord8, word8ToNetwork)
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Slot (SlotNo (..))

mkVKeyRwdAcnt ::
  Crypto crypto =>
  Network ->
  KeyPair 'Staking crypto ->
  RewardAcnt crypto
mkVKeyRwdAcnt network keys = RewardAcnt network $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt ::
  Network ->
  Credential 'Staking crypto ->
  RewardAcnt crypto
mkRwdAcnt network script@(ScriptHashObj _) = RewardAcnt network script
mkRwdAcnt network key@(KeyHashObj _) = RewardAcnt network key

toAddr ::
  Crypto crypto =>
  Network ->
  (KeyPair 'Payment crypto, KeyPair 'Staking crypto) ->
  Addr crypto
toAddr n (payKey, stakeKey) = Addr n (toCred payKey) (StakeRefBase $ toCred stakeKey)

toCred ::
  (Crypto crypto) =>
  KeyPair kr crypto ->
  Credential kr crypto
toCred k = KeyHashObj . hashKey $ vKey k

-- | Convert a given multi-sig script to a credential by hashing it and wrapping
-- into the `Credential` data type.
--
-- TODO nc what is the role of this credential?
scriptToCred :: Crypto crypto => MultiSig crypto -> Credential kr crypto
scriptToCred = ScriptHashObj . hashMultiSigScript

-- | Create a base address from a pair of multi-sig scripts (pay and stake)
scriptsToAddr :: Crypto crypto => Network -> (MultiSig crypto, MultiSig crypto) -> Addr crypto
scriptsToAddr n (payScript, stakeScript) =
  Addr n (scriptToCred payScript) (StakeRefBase $ scriptToCred stakeScript)

-- | Serialise an address to the external format.
serialiseAddr :: Addr crypto -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAddr :: Crypto crypto => ByteString -> Maybe (Addr crypto)
deserialiseAddr bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | Serialise a reward account to the external format.
serialiseRewardAcnt :: RewardAcnt crypto -> ByteString
serialiseRewardAcnt = BSL.toStrict . B.runPut . putRewardAcnt

-- | Deserialise an reward account from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseRewardAcnt :: Crypto crypto => ByteString -> Maybe (RewardAcnt crypto)
deserialiseRewardAcnt bs = case B.runGetOrFail getRewardAcnt (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | An address for UTxO.
data Addr crypto
  = Addr !Network !(PaymentCredential crypto) !(StakeReference crypto)
  | AddrBootstrap !(BootstrapAddress crypto)
  deriving (Show, Eq, Generic, NFData, Ord)

getNetwork :: Addr crypto -> Network
getNetwork (Addr n _ _) = n
getNetwork (AddrBootstrap (BootstrapAddress byronAddr)) =
  case Byron.aaNetworkMagic . Byron.attrData . Byron.addrAttributes $ byronAddr of
    Byron.NetworkMainOrStage -> Mainnet
    Byron.NetworkTestnet _ -> Testnet

instance NoUnexpectedThunks (Addr crypto)

-- | An account based address for rewards
data RewardAcnt crypto = RewardAcnt
  { getRwdNetwork :: Network,
    getRwdCred :: Credential 'Staking crypto
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

instance Crypto crypto => ToJSON (RewardAcnt crypto) where
  toJSON ra =
    Aeson.object
      [ "network" .= getRwdNetwork ra,
        "credential" .= getRwdCred ra
      ]

instance Crypto crypto => FromJSON (RewardAcnt crypto) where
  parseJSON =
    Aeson.withObject "RewardAcnt" $ \obj ->
      RewardAcnt
        <$> obj .: "network"
        <*> obj .: "credential"

instance NoUnexpectedThunks (RewardAcnt crypto)

instance Crypto crypto => ToJSONKey (Addr crypto) where
  toJSONKey = Aeson.ToJSONKeyText addrToText (Aeson.text . addrToText)

instance Crypto crypto => FromJSONKey (Addr crypto) where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseAddr

instance Crypto crypto => ToJSON (Addr crypto) where
  toJSON = toJSON . addrToText

instance Crypto crypto => FromJSON (Addr crypto) where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Addr crypto -> Text
addrToText =
  Text.decodeLatin1 . Base16.encode . serialiseAddr

parseAddr :: Crypto crypto => Text -> Aeson.Parser (Addr crypto)
parseAddr t = do
  bytes <- either badHex return (parseBase16 t)
  maybe badFormat return (deserialiseAddr bytes)
  where
    badHex h = fail $ "Addresses are expected in hex encoding for now: " ++ show h
    badFormat = fail "Address is not in the right format"

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

putAddr :: Addr crypto -> Put
putAddr (AddrBootstrap (BootstrapAddress byronAddr)) = B.putLazyByteString (serialize byronAddr)
putAddr (Addr network pc sr) =
  let setPayCredBit = case pc of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
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
      case word8ToNetwork addrNetId of
        Just n -> Addr n <$> getPayCred header <*> getStakeReference header
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

putRewardAcnt :: RewardAcnt crypto -> Put
putRewardAcnt (RewardAcnt network cred) = do
  let setPayCredBit = case cred of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
      rewardAcntPrefix = 0xE0 -- 0b1110000 are always set for reward accounts
      header = setPayCredBit (netId .|. rewardAcntPrefix)
  B.putWord8 header
  putCredential cred

getRewardAcnt :: forall crypto. Crypto crypto => Get (RewardAcnt crypto)
getRewardAcnt = do
  header <- B.getWord8
  let rewardAcntPrefix = 0xE0 -- 0b1110000 are always set for reward accounts
      isRewardAcnt = (header .&. rewardAcntPrefix) == rewardAcntPrefix
      netId = header .&. 0x0F -- 0b00001111 is the mask for the network id
  case (word8ToNetwork netId, isRewardAcnt) of
    (Nothing, _) ->
      fail $ concat ["Reward account with unknown network Id. (", show netId, ")"]
    (_, False) ->
      fail $ concat ["Expected reward account. Got account with header: ", show header]
    (Just network, True) -> do
      cred <- case testBit header rewardCredIsScript of
        True -> getScriptHash
        False -> getKeyHash
      pure $ RewardAcnt network cred

getHash :: forall h a. Hash.HashAlgorithm h => Get (Hash.Hash h a)
getHash = do
  bytes <- B.getByteString . fromIntegral $ Hash.sizeHash ([] @h)
  case Hash.hashFromBytes bytes of
    Nothing -> fail "getHash: implausible hash length mismatch"
    Just h -> pure h

putHash :: Hash.Hash h a -> Put
putHash = B.putByteString . Hash.hashToBytes

getPayCred :: Crypto crypto => Word8 -> Get (PaymentCredential crypto)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: Crypto crypto => Get (Credential kr crypto)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: (Crypto crypto) => Get (Credential kr crypto)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: Crypto crypto => Word8 -> Get (StakeReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

putCredential :: Credential kr crypto -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h

getByron :: Get (Addr crypto)
getByron =
  decodeFull <$> B.getRemainingLazyByteString >>= \case
    Left e -> fail (show e)
    Right r -> pure $ AddrBootstrap $ BootstrapAddress r

putPtr :: Ptr -> Put
putPtr (Ptr slot txIx certIx) = do
  putSlot slot
  putVariableLengthNat txIx
  putVariableLengthNat certIx
  where
    putSlot (SlotNo n) = putVariableLengthNat . fromIntegral $ n

getPtr :: Get Ptr
getPtr =
  Ptr <$> (SlotNo . fromIntegral <$> getVariableLengthNat)
    <*> getVariableLengthNat
    <*> getVariableLengthNat

newtype Word7 = Word7 Word8
  deriving (Eq, Show)

toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F) -- 0x7F = 0b01111111

putWord7s :: [Word7] -> Put
putWord7s [] = pure ()
putWord7s [Word7 x] = B.putWord8 x
putWord7s (Word7 x : xs) = B.putWord8 (x .|. 0x80) >> putWord7s xs

getWord7s :: Get [Word7]
getWord7s = do
  next <- B.getWord8
  case next .&. 0x80 of
    0x80 -> (:) (toWord7 next) <$> getWord7s
    _ -> pure [Word7 next]

natToWord7s :: Natural -> [Word7]
natToWord7s = reverse . go
  where
    go n
      | n <= 0x7F = [Word7 . fromIntegral $ n]
      | otherwise = (toWord7 . fromIntegral) n : go (shiftR n 7)

putVariableLengthNat :: Natural -> Put
putVariableLengthNat = putWord7s . natToWord7s

word7sToNat :: [Word7] -> Natural
word7sToNat = foldl' f 0
  where
    f n (Word7 r) = shiftL n 7 .|. (fromIntegral r)

getVariableLengthNat :: Get Natural
getVariableLengthNat = word7sToNat <$> getWord7s

decoderFromGet :: Text -> Get a -> Decoder s a
decoderFromGet name get = do
  bytes <- fromCBOR
  case B.runGetOrFail get bytes of
    Right (_remaining, _offset, value) -> pure value
    Left (_remaining, _offset, message) ->
      cborError (DecoderErrorCustom name $ fromString message)

instance Crypto crypto => ToCBOR (Addr crypto) where
  toCBOR = toCBOR . B.runPut . putAddr

instance Crypto crypto => FromCBOR (Addr crypto) where
  fromCBOR = decoderFromGet "Addr" getAddr

instance Crypto crypto => ToCBOR (RewardAcnt crypto) where
  toCBOR = toCBOR . B.runPut . putRewardAcnt

instance Crypto crypto => FromCBOR (RewardAcnt crypto) where
  fromCBOR = decoderFromGet "RewardAcnt" getRewardAcnt

newtype BootstrapAddress crypto = BootstrapAddress
  { unBootstrapAddress :: Byron.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet (BootstrapAddress crypto)

instance NoUnexpectedThunks (BootstrapAddress crypto)

bootstrapKeyHash ::
  forall crypto.
  -- TODO: enforce this constraint
  --(HASH crypto ~ Hash.Blake2b_224) =>
  Crypto crypto =>
  BootstrapAddress crypto ->
  KeyHash 'Payment crypto
bootstrapKeyHash (BootstrapAddress byronAddress) =
  let root = Byron.addrRoot byronAddress
      bytes = Byron.abstractHashToBytes root
      hash =
        fromMaybe (panic "bootstrapKeyHash: incorrect hash length") $
          Hash.hashFromBytes bytes
   in KeyHash hash
