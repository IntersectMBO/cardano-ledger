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
    deserialiseAddrStakeRef,
    Addr (..),
    BootstrapAddress (..),
    bootstrapAddressAttrsSize,
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
    getVariableLengthNat,
    payCredIsScript,
    putAddr,
    putCredential,
    putPtr,
    putRewardAcnt,
    putVariableLengthNat,
    -- TODO: these should live somewhere else
    natToWord7s,
    word7sToNat,
    Word7 (..),
    toWord7,
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
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Era
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
import qualified Data.ByteString as BS
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
import Shelley.Spec.Ledger.Keys
  ( KeyHash (..),
    KeyPair (..),
    KeyRole (..),
    hashKey,
  )
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Slot (SlotNo (..))

mkVKeyRwdAcnt ::
  Era era =>
  Network ->
  KeyPair 'Staking era ->
  RewardAcnt era
mkVKeyRwdAcnt network keys = RewardAcnt network $ KeyHashObj (hashKey $ vKey keys)

mkRwdAcnt ::
  Network ->
  Credential 'Staking era ->
  RewardAcnt era
mkRwdAcnt network script@(ScriptHashObj _) = RewardAcnt network script
mkRwdAcnt network key@(KeyHashObj _) = RewardAcnt network key

toAddr ::
  Era era =>
  Network ->
  (KeyPair 'Payment era, KeyPair 'Staking era) ->
  Addr era
toAddr n (payKey, stakeKey) = Addr n (toCred payKey) (StakeRefBase $ toCred stakeKey)

toCred ::
  (Era era) =>
  KeyPair kr era ->
  Credential kr era
toCred k = KeyHashObj . hashKey $ vKey k

-- | Convert a given multi-sig script to a credential by hashing it and wrapping
-- into the `Credential` data type.
--
-- TODO nc what is the role of this credential?
scriptToCred :: Era era => MultiSig era -> Credential kr era
scriptToCred = ScriptHashObj . hashMultiSigScript

-- | Create a base address from a pair of multi-sig scripts (pay and stake)
scriptsToAddr :: Era era => Network -> (MultiSig era, MultiSig era) -> Addr era
scriptsToAddr n (payScript, stakeScript) =
  Addr n (scriptToCred payScript) (StakeRefBase $ scriptToCred stakeScript)

-- | Serialise an address to the external format.
serialiseAddr :: Addr era -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr

-- | Deserialise an address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAddr :: Era era => ByteString -> Maybe (Addr era)
deserialiseAddr bs = case B.runGetOrFail getAddr (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | Deserialise a stake refence from a address. This will fail if this
-- is a Bootstrap address (or malformed).
deserialiseAddrStakeRef :: Era era => ByteString -> Maybe (StakeReference era)
deserialiseAddrStakeRef bs = case B.runGetOrFail getAddrStakeReference (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> result

-- | Serialise a reward account to the external format.
serialiseRewardAcnt :: RewardAcnt era -> ByteString
serialiseRewardAcnt = BSL.toStrict . B.runPut . putRewardAcnt

-- | Deserialise an reward account from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseRewardAcnt :: Era era => ByteString -> Maybe (RewardAcnt era)
deserialiseRewardAcnt bs = case B.runGetOrFail getRewardAcnt (BSL.fromStrict bs) of
  Left (_remaining, _offset, _message) -> Nothing
  Right (_remaining, _offset, result) -> Just result

-- | An address for UTxO.
data Addr era
  = Addr !Network !(PaymentCredential era) !(StakeReference era)
  | AddrBootstrap !(BootstrapAddress era)
  deriving (Show, Eq, Generic, NFData, Ord)

getNetwork :: Addr era -> Network
getNetwork (Addr n _ _) = n
getNetwork (AddrBootstrap (BootstrapAddress byronAddr)) =
  case Byron.aaNetworkMagic . Byron.attrData . Byron.addrAttributes $ byronAddr of
    Byron.NetworkMainOrStage -> Mainnet
    Byron.NetworkTestnet _ -> Testnet

instance NoUnexpectedThunks (Addr era)

-- | An account based address for rewards
data RewardAcnt era = RewardAcnt
  { getRwdNetwork :: !Network,
    getRwdCred :: !(Credential 'Staking era)
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

instance Era era => ToJSON (RewardAcnt era) where
  toJSON ra =
    Aeson.object
      [ "network" .= getRwdNetwork ra,
        "credential" .= getRwdCred ra
      ]

instance Era era => FromJSON (RewardAcnt era) where
  parseJSON =
    Aeson.withObject "RewardAcnt" $ \obj ->
      RewardAcnt
        <$> obj .: "network"
        <*> obj .: "credential"

instance NoUnexpectedThunks (RewardAcnt era)

instance Era era => ToJSONKey (Addr era) where
  toJSONKey = Aeson.ToJSONKeyText addrToText (Aeson.text . addrToText)

instance Era era => FromJSONKey (Addr era) where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseAddr

instance Era era => ToJSON (Addr era) where
  toJSON = toJSON . addrToText

instance Era era => FromJSON (Addr era) where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Addr era -> Text
addrToText =
  Text.decodeLatin1 . Base16.encode . serialiseAddr

parseAddr :: Era era => Text -> Aeson.Parser (Addr era)
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

putAddr :: Addr era -> Put
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

getAddr :: forall era. Era era => Get (Addr era)
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
getAddrStakeReference :: forall era. Era era => Get (Maybe (StakeReference era))
getAddrStakeReference = do
  header <- B.getWord8
  if testBit header byron
    then pure Nothing
    else skipHash ([] @(ADDRHASH (Crypto era))) >> Just <$> getStakeReference header

putRewardAcnt :: RewardAcnt era -> Put
putRewardAcnt (RewardAcnt network cred) = do
  let setPayCredBit = case cred of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
      rewardAcntPrefix = 0xE0 -- 0b1110000 are always set for reward accounts
      header = setPayCredBit (netId .|. rewardAcntPrefix)
  B.putWord8 header
  putCredential cred

getRewardAcnt :: forall era. Era era => Get (RewardAcnt era)
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

skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> Get ()
skipHash p = B.skip . fromIntegral $ Hash.sizeHash p

getHash :: forall h a. Hash.HashAlgorithm h => Get (Hash.Hash h a)
getHash = do
  bytes <- B.getByteString . fromIntegral $ Hash.sizeHash ([] @h)
  case Hash.hashFromBytes bytes of
    Nothing -> fail "getHash: implausible hash length mismatch"
    Just h -> pure h

putHash :: Hash.Hash h a -> Put
putHash = B.putByteString . Hash.hashToBytes

getPayCred :: Era era => Word8 -> Get (PaymentCredential era)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getScriptHash :: Era era => Get (Credential kr era)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getKeyHash :: (Era era) => Get (Credential kr era)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getStakeReference :: Era era => Word8 -> Get (StakeReference era)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

putCredential :: Credential kr era -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h

getByron :: Get (Addr era)
getByron =
  decodeFull <$> B.getRemainingLazyByteString >>= \case
    Left e -> fail (show e)
    Right r -> pure $ AddrBootstrap $ BootstrapAddress r

-- | The size of the extra attributes in a bootstrp (ie Byron) address. Used
-- to help enforce that people do not post huge ones on the chain.
bootstrapAddressAttrsSize :: BootstrapAddress era -> Int
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

instance Era era => ToCBOR (Addr era) where
  toCBOR = toCBOR . B.runPut . putAddr

instance Era era => FromCBOR (Addr era) where
  fromCBOR = decoderFromGet "Addr" getAddr

instance Era era => ToCBOR (RewardAcnt era) where
  toCBOR = toCBOR . B.runPut . putRewardAcnt

instance Era era => FromCBOR (RewardAcnt era) where
  fromCBOR = decoderFromGet "RewardAcnt" getRewardAcnt

newtype BootstrapAddress era = BootstrapAddress
  { unBootstrapAddress :: Byron.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet (BootstrapAddress era)

instance NoUnexpectedThunks (BootstrapAddress era)

bootstrapKeyHash ::
  forall era.
  -- TODO: enforce this constraint
  --(HASH era ~ Hash.Blake2b_224) =>
  Era era =>
  BootstrapAddress era ->
  KeyHash 'Payment era
bootstrapKeyHash (BootstrapAddress byronAddress) =
  let root = Byron.addrRoot byronAddress
      bytes = Byron.abstractHashToBytes root
      hash =
        fromMaybe (panic "bootstrapKeyHash: incorrect hash length") $
          Hash.hashFromBytes bytes
   in KeyHash hash
