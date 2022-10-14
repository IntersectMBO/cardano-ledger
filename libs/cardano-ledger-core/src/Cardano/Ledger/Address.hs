{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Address
  ( mkVKeyRwdAcnt,
    mkRwdAcnt,
    toAddr,
    toCred,
    serialiseAddr,
    deserialiseAddr,
    deserialiseAddrStakeRef,
    Addr (..),
    BootstrapAddress (..),
    bootstrapAddressAttrsSize,
    isBootstrapRedeemer,
    getNetwork,
    RewardAcnt (..),
    serialiseRewardAcnt,
    deserialiseRewardAcnt,
    --  Bits
    byron,
    notBaseAddr,
    isEnterpriseAddr,
    stakeCredIsScript,
    -- internals exported for testing
    getAddr,
    getKeyHash,
    bootstrapKeyHash,
    getPtr,
    getRewardAcnt,
    getScriptHash,
    getVariableLengthWord64,
    payCredIsScript,
    putAddr,
    putCredential,
    putPtr,
    putRewardAcnt,
    putVariableLengthWord64,
    -- TODO: these should live somewhere else
    word64ToWord7s,
    word7sToWord64,
    Word7 (..),
    toWord7,
  )
where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.BaseTypes
  ( CertIx (..),
    Network (..),
    TxIx (..),
    networkToWord8,
    word8ToNetwork,
  )
import Cardano.Ledger.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    byronProtVer,
    cborError,
    decodeFull,
    serialize,
  )
import Cardano.Ledger.Credential
  ( Credential (..),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Key as Aeson (fromText)
import qualified Data.Aeson.Types as Aeson
import Data.Binary (Get, Put, Word8)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import Data.Bits (setBit, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

mkVKeyRwdAcnt ::
  CC.Crypto c =>
  Network ->
  KeyPair 'Staking c ->
  RewardAcnt c
mkVKeyRwdAcnt network keys = RewardAcnt network $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt ::
  Network ->
  Credential 'Staking c ->
  RewardAcnt c
mkRwdAcnt network script@(ScriptHashObj _) = RewardAcnt network script
mkRwdAcnt network key@(KeyHashObj _) = RewardAcnt network key

toAddr ::
  CC.Crypto c =>
  Network ->
  (KeyPair 'Payment c, KeyPair 'Staking c) ->
  Addr c
toAddr n (payKey, stakeKey) = Addr n (toCred payKey) (StakeRefBase $ toCred stakeKey)

toCred ::
  CC.Crypto c =>
  KeyPair kr c ->
  Credential kr c
toCred k = KeyHashObj . hashKey $ vKey k

-- | Serialise an address to the external format.
serialiseAddr :: Addr c -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAddr :: CC.Crypto c => ByteString -> Maybe (Addr c)
deserialiseAddr bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (remaining, _offset, result) ->
    if BSL.null remaining
      then Just result
      else Nothing

-- | Deserialise a stake refence from a address. This will fail if this
-- is a Bootstrap address (or malformed).
deserialiseAddrStakeRef :: CC.Crypto c => ByteString -> Maybe (StakeReference c)
deserialiseAddrStakeRef bs =
  case B.runGetOrFail getAddrStakeReference (BSL.fromStrict bs) of
    Right (remaining, _offset, result)
      | not (BSL.null remaining) -> result
    _ -> Nothing

-- | Serialise a reward account to the external format.
serialiseRewardAcnt :: RewardAcnt c -> ByteString
serialiseRewardAcnt = BSL.toStrict . B.runPut . putRewardAcnt

-- | Deserialise an reward account from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseRewardAcnt :: CC.Crypto c => ByteString -> Maybe (RewardAcnt c)
deserialiseRewardAcnt bs = case B.runGetOrFail getRewardAcnt (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (remaining, _offset, result) ->
    if BSL.null remaining
      then Just result
      else Nothing

-- | An address for UTxO.
--
-- Contents of Addr data type are intentionally left as lazy, otherwise
-- operating on compact form of an address will result in redundant work.
data Addr c
  = Addr Network (PaymentCredential c) (StakeReference c)
  | AddrBootstrap (BootstrapAddress c)
  deriving (Show, Eq, Generic, NFData, Ord)

getNetwork :: Addr c -> Network
getNetwork (Addr n _ _) = n
getNetwork (AddrBootstrap (BootstrapAddress byronAddr)) =
  case Byron.aaNetworkMagic . Byron.attrData . Byron.addrAttributes $ byronAddr of
    Byron.NetworkMainOrStage -> Mainnet
    Byron.NetworkTestnet _ -> Testnet

instance NoThunks (Addr c)

-- | An account based address for rewards
data RewardAcnt c = RewardAcnt
  { getRwdNetwork :: !Network,
    getRwdCred :: !(Credential 'Staking c)
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

instance CC.Crypto c => ToJSON (RewardAcnt c) where
  toJSON ra =
    Aeson.object
      [ "network" .= getRwdNetwork ra,
        "credential" .= getRwdCred ra
      ]

instance CC.Crypto c => FromJSON (RewardAcnt c) where
  parseJSON =
    Aeson.withObject "RewardAcnt" $ \obj ->
      RewardAcnt
        <$> obj .: "network"
        <*> obj .: "credential"

instance NoThunks (RewardAcnt c)

instance ToJSONKey (Addr c) where
  toJSONKey = Aeson.ToJSONKeyText (Aeson.fromText . addrToText) (Aeson.text . addrToText)

instance CC.Crypto c => FromJSONKey (Addr c) where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseAddr

instance ToJSON (Addr c) where
  toJSON = toJSON . addrToText

instance CC.Crypto c => FromJSON (Addr c) where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Addr c -> Text
addrToText = Text.decodeLatin1 . B16.encode . serialiseAddr

parseAddr :: CC.Crypto c => Text -> Aeson.Parser (Addr c)
parseAddr t = do
  bytes <- either badHex return (B16.decode (Text.encodeUtf8 t))
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

putAddr :: Addr c -> Put
putAddr (AddrBootstrap (BootstrapAddress byronAddr)) =
  B.putLazyByteString (serialize byronProtVer byronAddr)
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

getAddr :: CC.Crypto c => Get (Addr c)
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

-- | We are "expecting" an address, but we are only interested in the StakeReference.
--   If the Addr is A Byron style address, there are no Stake References, return Nothing.
getAddrStakeReference :: forall c. CC.Crypto c => Get (Maybe (StakeReference c))
getAddrStakeReference = do
  header <- B.getWord8
  if testBit header byron
    then pure Nothing
    else skipHash ([] @(ADDRHASH c)) >> Just <$> getStakeReference header

putRewardAcnt :: RewardAcnt c -> Put
putRewardAcnt (RewardAcnt network cred) = do
  let setPayCredBit = case cred of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
      rewardAcntPrefix = 0xE0 -- 0b11100000 are always set for reward accounts
      header = setPayCredBit (netId .|. rewardAcntPrefix)
  B.putWord8 header
  putCredential cred

getRewardAcnt :: CC.Crypto c => Get (RewardAcnt c)
getRewardAcnt = do
  header <- B.getWord8
  let rewardAcntPrefix = 0xE0 -- 0b11100000 are always set for reward accounts
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

skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> Get ()
skipHash p = B.skip . fromIntegral $ Hash.sizeHash p

getHash :: forall h a. Hash.HashAlgorithm h => Get (Hash.Hash h a)
getHash = do
  bytes <- B.getByteString . fromIntegral $ Hash.sizeHash ([] @h)
  case Hash.hashFromBytes bytes of
    Nothing -> fail "getHash: implausible hash length mismatch"
    Just !h -> pure h

putHash :: Hash.Hash h a -> Put
putHash = B.putByteString . Hash.hashToBytes

getPayCred :: CC.Crypto c => Word8 -> Get (PaymentCredential c)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: CC.Crypto c => Get (Credential kr c)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: CC.Crypto c => Get (Credential kr c)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: CC.Crypto c => Word8 -> Get (StakeReference c)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

putCredential :: Credential kr c -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h

getByron :: Get (Addr c)
getByron =
  decodeFull byronProtVer <$> B.getRemainingLazyByteString >>= \case
    Left e -> fail (show e)
    Right r -> pure $ AddrBootstrap $ BootstrapAddress r

-- | The size of the extra attributes in a bootstrp (ie Byron) address. Used
-- to help enforce that people do not post huge ones on the chain.
bootstrapAddressAttrsSize :: BootstrapAddress c -> Int
bootstrapAddressAttrsSize (BootstrapAddress addr) =
  -- I'm sorry this code is formatted so weridly below.
  -- It is to apease the capricious god Ormolu. A sacrifice is demanded!
  maybe
    0
    (BS.length . Byron.getHDAddressPayload)
    (Byron.aaVKDerivationPath (Byron.attrData attrs))
    + Byron.unknownAttributesLength attrs
  where
    attrs = Byron.addrAttributes addr

-- | Return True if a given address is a redeemer address from the Byron Era
isBootstrapRedeemer :: BootstrapAddress c -> Bool
isBootstrapRedeemer (BootstrapAddress (Byron.Address _ _ Byron.ATRedeem)) = True
isBootstrapRedeemer _ = False

putPtr :: Ptr -> Put
putPtr (Ptr slot (TxIx txIx) (CertIx certIx)) = do
  putSlot slot
  putVariableLengthWord64 ((fromIntegral @_ @Word64) txIx)
  putVariableLengthWord64 ((fromIntegral @_ @Word64) certIx)
  where
    putSlot (SlotNo n) = putVariableLengthWord64 n

getPtr :: Get Ptr
getPtr =
  Ptr
    <$> (SlotNo <$> getVariableLengthWord64)
    <*> (TxIx . fromIntegral <$> getVariableLengthWord64)
    <*> (CertIx . fromIntegral <$> getVariableLengthWord64)

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
  -- is the high bit set?
  if testBit next 7
    then -- if so, grab more words
      (:) (toWord7 next) <$> getWord7s
    else -- otherwise, this is the last one
      pure [Word7 next]

word64ToWord7s :: Word64 -> [Word7]
word64ToWord7s = reverse . go
  where
    go :: Word64 -> [Word7]
    go n
      | n > 0x7F = (toWord7 . fromIntegral) n : go (shiftR n 7)
      | otherwise = [Word7 . fromIntegral $ n]

putVariableLengthWord64 :: Word64 -> Put
putVariableLengthWord64 = putWord7s . word64ToWord7s

-- invariant: length [Word7] < 8
word7sToWord64 :: [Word7] -> Word64
word7sToWord64 = foldl' f 0
  where
    f n (Word7 r) = shiftL n 7 .|. fromIntegral r

getVariableLengthWord64 :: Get Word64
getVariableLengthWord64 = word7sToWord64 <$> getWord7s

decoderFromGet :: Text -> Get a -> Decoder s a
decoderFromGet name get = do
  bytes <- fromCBOR
  case B.runGetOrFail get bytes of
    Right (_remaining, _offset, value) -> pure value
    Left (_remaining, _offset, message) ->
      cborError (DecoderErrorCustom name $ fromString message)

instance CC.Crypto c => ToCBOR (Addr c) where
  toCBOR = toCBOR . B.runPut . putAddr

instance CC.Crypto c => FromCBOR (Addr c) where
  fromCBOR = decoderFromGet "Addr" getAddr

instance CC.Crypto c => ToCBOR (RewardAcnt c) where
  toCBOR = toCBOR . B.runPut . putRewardAcnt

instance CC.Crypto c => FromCBOR (RewardAcnt c) where
  fromCBOR = decoderFromGet "RewardAcnt" getRewardAcnt

newtype BootstrapAddress c = BootstrapAddress
  { unBootstrapAddress :: Byron.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet (BootstrapAddress c)

instance NoThunks (BootstrapAddress c)

bootstrapKeyHash ::
  forall c.
  CC.Crypto c =>
  -- TODO: enforce this constraint
  -- (HASH era ~ Hash.Blake2b_224) =>
  BootstrapAddress c ->
  KeyHash 'Payment c
bootstrapKeyHash (BootstrapAddress byronAddress) =
  let root = Byron.addrRoot byronAddress
      bytes = Byron.abstractHashToBytes root
      !hash =
        fromMaybe (error "bootstrapKeyHash: incorrect hash length") $
          Hash.hashFromBytes bytes
   in KeyHash hash
