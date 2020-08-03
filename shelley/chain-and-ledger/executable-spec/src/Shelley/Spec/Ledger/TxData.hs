{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    Ix,
    MIRCert (..),
    MIRPot (..),
    PoolCert (..),
    PoolMetaData (..),
    PoolParams (..),
    Ptr (..),
    RewardAcnt (..),
    StakeCreds (..),
    StakePoolRelay (..),
    TxBody
      ( TxBody,
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash,
        extraSize
      ),
    TxId (..),
    TxIn (TxIn),
    pattern TxInCompact,
    TxOut (TxOut, TxOutCompact),
    Url,
    Wdrl (..),
    WitVKey (WitVKey, wvkBytes),
    --
    witKeyHash,
    addStakeCreds,
    --
    SizeOfPoolOwners (..),
    SizeOfPoolRelays (..),
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
    Decoder,
    FromCBOR (fromCBOR),
    Size,
    ToCBOR (..),
    annotatorSlice,
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodePreEncoded,
    encodeWord,
    serializeEncoding,
    serializeEncoding',
    szCases,
    withSlice,
  )
import Cardano.Prelude
  ( AllowThunksIn (..),
    LByteString,
    NFData (rnf),
    NoUnexpectedThunks (..),
    UseIsNormalFormNamed (..),
    Word64,
    asum,
    catMaybes,
    panic,
  )
import Control.Iterate.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Foldable (fold)
import Data.IP (IPv4, IPv6)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
    deserialiseAddr,
    serialiseAddr,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( DnsName,
    Port,
    StrictMaybe (..),
    UnitInterval,
    Url,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), word64ToCoin)

import Shelley.Spec.Ledger.Value

import Shelley.Spec.Ledger.Core (Relation (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ix,
    Ptr (..),
    StakeCredential,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Hashing
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey,
    VerKeyVRF,
    asWitness,
    decodeSignedDSIGN,
    encodeSignedDSIGN,
    hashKey,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    CborSeq (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeNullMaybe,
    decodeRecordNamed,
    decodeRecordSum,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    encodeNullMaybe,
    ipv4FromCBOR,
    ipv4ToCBOR,
    ipv6FromCBOR,
    ipv6ToCBOR,
    listLenInt,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

instance HasExp (StakeCreds crypto) (Map (Credential 'Staking crypto) SlotNo) where
  toExp (StakeCreds x) = Base MapR x

instance Embed (StakeCreds crypto) (Map (Credential 'Staking crypto) SlotNo) where
  toBase (StakeCreds x) = x
  fromBase x = (StakeCreds x)

-- | The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: !(StakeCredential crypto),
    _delegatee :: !(KeyHash 'StakePool crypto)
  }
  deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (Delegation crypto)

data PoolMetaData = PoolMetaData
  { _poolMDUrl :: !Url,
    _poolMDHash :: !ByteString
  }
  deriving (Eq, Ord, Generic, Show, NFData)

instance ToJSON PoolMetaData where
  toJSON pmd =
    Aeson.object
      [ "url" .= _poolMDUrl pmd,
        "hash" .= (Text.decodeLatin1 . Base16.encode) (_poolMDHash pmd)
      ]

instance FromJSON PoolMetaData where
  parseJSON =
    Aeson.withObject "PoolMetaData" $ \obj ->
      PoolMetaData
        <$> obj .: "url"
        <*> explicitParseField (fmap (fst . Base16.decode . Char8.pack) . parseJSON) obj "hash"

instance NoUnexpectedThunks PoolMetaData

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    SingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
  | -- | An @A@ or @AAAA@ DNS record
    SingleHostName !(StrictMaybe Port) !DnsName
  | -- | A @SRV@ DNS record
    MultiHostName !DnsName
  deriving (Eq, Ord, Generic, Show)

instance FromJSON StakePoolRelay where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum
        [ explicitParseField parser1 obj "single host address",
          explicitParseField parser2 obj "single host name",
          explicitParseField parser3 obj "multi host name"
        ]
    where
      parser1 = Aeson.withObject "SingleHostAddr" $ \obj ->
        SingleHostAddr
          <$> obj .:? "port" .!= SNothing
          <*> obj .:? "IPv4" .!= SNothing
          <*> obj .:? "IPv6" .!= SNothing
      parser2 = Aeson.withObject "SingleHostName" $ \obj ->
        SingleHostName
          <$> obj .:? "port" .!= SNothing
          <*> obj .: "dnsName"
      parser3 = Aeson.withObject "MultiHostName" $ \obj ->
        MultiHostName
          <$> obj .: "dnsName"

instance ToJSON StakePoolRelay where
  toJSON (SingleHostAddr port ipv4 ipv6) =
    Aeson.object
      [ "single host address"
          .= Aeson.object
            [ "port" .= port,
              "IPv4" .= ipv4,
              "IPv6" .= ipv6
            ]
      ]
  toJSON (SingleHostName port dnsName) =
    Aeson.object
      [ "single host name"
          .= Aeson.object
            [ "port" .= port,
              "dnsName" .= dnsName
            ]
      ]
  toJSON (MultiHostName dnsName) =
    Aeson.object
      [ "multi host name"
          .= Aeson.object
            [ "dnsName" .= dnsName
            ]
      ]

instance NoUnexpectedThunks StakePoolRelay

instance NFData StakePoolRelay

instance ToCBOR StakePoolRelay where
  toCBOR (SingleHostAddr p ipv4 ipv6) =
    encodeListLen 4
      <> toCBOR (0 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> encodeNullMaybe ipv4ToCBOR (strictMaybeToMaybe ipv4)
      <> encodeNullMaybe ipv6ToCBOR (strictMaybeToMaybe ipv6)
  toCBOR (SingleHostName p n) =
    encodeListLen 3
      <> toCBOR (1 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> toCBOR n
  toCBOR (MultiHostName n) =
    encodeListLen 2
      <> toCBOR (2 :: Word8)
      <> toCBOR n

instance FromCBOR StakePoolRelay where
  fromCBOR = decodeRecordSum "StakePoolRelay" $
    \case
      0 ->
        (\x y z -> (4, SingleHostAddr x y z))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv4FromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv6FromCBOR)
      1 ->
        (\x y -> (3, SingleHostName x y))
          <$> (maybeToStrictMaybe <$> decodeNullMaybe fromCBOR)
          <*> fromCBOR
      2 -> do
        x <- fromCBOR
        pure (2, MultiHostName x)
      k -> invalidKey k

-- | A stake pool.
data PoolParams crypto = PoolParams
  { _poolPubKey :: !(KeyHash 'StakePool crypto),
    _poolVrf :: !(Hash crypto (VerKeyVRF crypto)),
    _poolPledge :: !Coin,
    _poolCost :: !Coin,
    _poolMargin :: !UnitInterval,
    _poolRAcnt :: !(RewardAcnt crypto),
    _poolOwners :: !(Set (KeyHash 'Staking crypto)),
    _poolRelays :: !(StrictSeq StakePoolRelay),
    _poolMD :: !(StrictMaybe PoolMetaData)
  }
  deriving (Show, Generic, Eq, Ord, NFData)
  deriving (ToCBOR) via CBORGroup (PoolParams crypto)
  deriving (FromCBOR) via CBORGroup (PoolParams crypto)

instance NoUnexpectedThunks (PoolParams crypto)

newtype Wdrl crypto = Wdrl {unWdrl :: Map (RewardAcnt crypto) Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = mapToCBOR . unWdrl

instance Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl <$> mapFromCBOR

instance Crypto crypto => ToJSON (PoolParams crypto) where
  toJSON pp =
    Aeson.object
      [ "publicKey" .= _poolPubKey pp,
        "vrf" .= _poolVrf pp,
        "pledge" .= _poolPledge pp,
        "cost" .= _poolCost pp,
        "margin" .= _poolMargin pp,
        "rewardAccount" .= _poolRAcnt pp,
        "owners" .= _poolOwners pp,
        "relays" .= _poolRelays pp,
        "metadata" .= _poolMD pp
      ]

instance Crypto crypto => FromJSON (PoolParams crypto) where
  parseJSON =
    Aeson.withObject "PoolParams" $ \obj ->
      PoolParams
        <$> obj .: "publicKey"
        <*> obj .: "vrf"
        <*> obj .: "pledge"
        <*> obj .: "cost"
        <*> obj .: "margin"
        <*> obj .: "rewardAccount"
        <*> obj .: "owners"
        <*> obj .: "relays"
        <*> obj .: "metadata"

-- | A unique ID of a transaction, which is computable from the transaction.
newtype CV crypto v => TxId crypto v = TxId {_unTxId :: Hash crypto (TxBody crypto v)}
  deriving (Show, Eq, Ord)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving newtype instance CV crypto v => ToCBOR (TxId crypto v)

deriving newtype instance CV crypto v => FromCBOR (TxId crypto v)

-- | The input of a UTxO.
data CV crypto v => TxIn crypto v = TxInCompact {-# UNPACK #-} !(TxId crypto v) {-# UNPACK #-} !Word64
  deriving (Show, Eq, Ord)

-- TODO: We will also want to have the TxId be compact, but the representation
-- depends on the crypto.


deriving instance NFData v => NFData (TxIn crypto v)

pattern TxIn ::
  CV crypto v =>
  TxId crypto v ->
  Natural -> -- TODO We might want to change this to Word64 generally
  TxIn crypto v
pattern TxIn addr index <-
  TxInCompact addr (fromIntegral -> index)
  where
    TxIn addr index =
      TxInCompact addr (fromIntegral index)

{-# COMPLETE TxIn #-}

instance NoUnexpectedThunks (TxIn crypto v)

-- | The output of a UTxO.
-- data TxOut crypto
--   = TxOutCompact
--       {-# UNPACK #-} !BSS.ShortByteString
--       {-# UNPACK #-} !Word64
--   deriving (Show, Eq, Ord)

-- | Parametrized tx output
data TxOut crypto v where
   TxOutCompact :: CV crypto v =>
     {-# UNPACK #-} !BSS.ShortByteString ->
     v ->
     TxOut crypto v

deriving instance Show v => Show (TxOut crypto v)

deriving instance Eq v => Eq (TxOut crypto v)

deriving instance NoUnexpectedThunks v => NoUnexpectedThunks (TxOut crypto v)

instance NFData v => NFData (TxOut crypto v) where
  rnf = (`seq` ())

-- deriving via UseIsNormalFormNamed "TxOut" (TxOut crypto v) instance NoUnexpectedThunks (TxOut crypto v)

pattern TxOut ::
  CV crypto v =>
  Addr crypto ->
  v ->
  TxOut crypto v
pattern TxOut addr v <-
  (viewCompactTxOut -> (addr, v))
  where
    TxOut addr v =
      TxOutCompact (BSS.toShort $ serialiseAddr addr) v

{-# COMPLETE TxOut #-}

viewCompactTxOut :: forall crypto v. CV crypto v => TxOut crypto v -> (Addr crypto, Coin)
viewCompactTxOut (TxOutCompact bs c) = (addr, coin)
  where
    addr = case deserialiseAddr (BSS.fromShort bs) of
      Nothing -> panic "viewCompactTxOut: impossible"
      Just (a :: Addr crypto) -> a
    coin = word64ToCoin c

data DelegCert crypto
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential crypto)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential crypto)
  | -- | A stake delegation certificate.
    Delegate !(Delegation crypto)
  deriving (Show, Generic, Eq)

data PoolCert crypto
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams crypto)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool crypto) !EpochNo
  deriving (Show, Generic, Eq)

-- | Genesis key delegation certificate
data GenesisDelegCert crypto
  = GenesisDelegCert
      !(KeyHash 'Genesis crypto)
      !(KeyHash 'GenesisDelegate crypto)
      !(Hash crypto (VerKeyVRF crypto))
  deriving (Show, Generic, Eq)

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NoUnexpectedThunks)

instance ToCBOR MIRPot where
  toCBOR ReservesMIR = toCBOR (0 :: Word8)
  toCBOR TreasuryMIR = toCBOR (1 :: Word8)

instance FromCBOR MIRPot where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure ReservesMIR
      1 -> pure TreasuryMIR
      k -> invalidKey k

-- | Move instantaneous rewards certificate
data MIRCert crypto = MIRCert
  { mirPot :: MIRPot,
    mirRewards :: (Map (Credential 'Staking crypto) Coin)
  }
  deriving (Show, Generic, Eq)

instance Crypto crypto => FromCBOR (MIRCert crypto) where
  fromCBOR = decodeRecordNamed "SingleHostAddr" (const 2) $ do
    pot <- fromCBOR
    values <- mapFromCBOR
    pure $ MIRCert pot values

instance Crypto crypto => ToCBOR (MIRCert crypto) where
  toCBOR (MIRCert pot values) =
    encodeListLen 2
      <> toCBOR pot
      <> mapToCBOR values

-- | A heavyweight certificate.
data DCert crypto
  = DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  | DCertMir !(MIRCert crypto)
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (DelegCert crypto)

instance NoUnexpectedThunks (PoolCert crypto)

instance NoUnexpectedThunks (GenesisDelegCert crypto)

instance NoUnexpectedThunks (MIRCert crypto)

instance NoUnexpectedThunks (DCert crypto)

-- | A raw transaction
data CV crypto v => TxBody crypto v = TxBody'
  { _inputs' :: !(Set (TxIn crypto v)),
    _outputs' :: !(StrictSeq (TxOut crypto v)),
    _certs' :: !(StrictSeq (DCert crypto)),
    _wdrls' :: !(Wdrl crypto),
    _txfee' :: !Coin,
    _ttl' :: !SlotNo,
    _txUpdate' :: !(StrictMaybe (Update crypto)),
    _mdHash' :: !(StrictMaybe (MetaDataHash crypto)),
    bodyBytes :: LByteString,
    extraSize :: !Int64 -- This is the contribution of inputs, outputs, and fees to the size of the transaction
  }
  deriving (Show, Eq)
  -- deriving
  --   (NoUnexpectedThunks)
  --   via AllowThunksIn '["bodyBytes"] (TxBody crypto v)

instance CV c v => HashAnnotated (TxBody c v) c

deriving instance NoUnexpectedThunks v => NoUnexpectedThunks (TxBody crypto v)

pattern TxBody ::
  CV crypto v =>
  Set (TxIn crypto v) ->
  StrictSeq (TxOut crypto v) ->
  StrictSeq (DCert crypto) ->
  Wdrl crypto ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update crypto) ->
  StrictMaybe (MetaDataHash crypto) ->
  TxBody crypto v
pattern TxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBody'
    { _inputs' = _inputs,
      _outputs' = _outputs,
      _certs' = _certs,
      _wdrls' = _wdrls,
      _txfee' = _txfee,
      _ttl' = _ttl,
      _txUpdate' = _txUpdate,
      _mdHash' = _mdHash
    }
  where
    TxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      let encodeMapElement ix enc x = Just (encodeWord ix <> enc x)
          encodeMapElementUnless condition ix enc x =
            if condition x
              then Nothing
              else encodeMapElement ix enc x
          l =
            catMaybes
              [ encodeMapElement 0 encodePreEncoded inputBytes,
                encodeMapElement 1 encodePreEncoded outputBytes,
                encodeMapElement 2 encodePreEncoded feeBytes,
                encodeMapElement 3 toCBOR _ttl,
                encodeMapElementUnless null 4 encodeFoldable _certs,
                encodeMapElementUnless (null . unWdrl) 5 toCBOR _wdrls,
                encodeMapElement 6 toCBOR =<< strictMaybeToMaybe _txUpdate,
                encodeMapElement 7 toCBOR =<< strictMaybeToMaybe _mdHash
              ]
          inputBytes = serializeEncoding' $ encodeFoldable _inputs
          outputBytes = serializeEncoding' $ encodeFoldable _outputs
          feeBytes = serializeEncoding' $ toCBOR _txfee
          es =
            fromIntegral $
              BS.length inputBytes
                + BS.length outputBytes
                + BS.length feeBytes
          n = fromIntegral $ length l
          bytes = serializeEncoding $ encodeMapLen n <> fold l
       in TxBody'
            _inputs
            _outputs
            _certs
            _wdrls
            _txfee
            _ttl
            _txUpdate
            _mdHash
            bytes
            es

{-# COMPLETE TxBody #-}

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto v kr = WitVKey'
  { wvkKey' :: !(VKey kr crypto),
    wvkSig' :: !(SignedDSIGN crypto (Hash crypto (TxBody crypto v))),
    -- | Hash of the witness vkey. We store this here to avoid repeated hashing
    --   when used in ordering.
    wvkKeyHash :: KeyHash 'Witness crypto,
    wvkBytes :: LByteString
  }
  deriving (Show, Eq, Generic)
  deriving (NoUnexpectedThunks) via AllowThunksIn '["wvkBytes"] (WitVKey crypto v kr)

instance (CV c v, Typeable k) => HashAnnotated (WitVKey c v k) c

pattern WitVKey ::
  (Typeable kr, CV crypto v) =>
  VKey kr crypto ->
  SignedDSIGN crypto (Hash crypto (TxBody crypto v)) ->
  WitVKey crypto v kr
pattern WitVKey k s <-
  WitVKey' k s _ _
  where
    WitVKey k s =
      let bytes =
            serializeEncoding $
              encodeListLen 2
                <> toCBOR k
                <> encodeSignedDSIGN s
          hash = asWitness $ hashKey k
       in WitVKey' k s hash bytes

{-# COMPLETE WitVKey #-}

witKeyHash ::
  WitVKey crypto v kr ->
  KeyHash 'Witness crypto
witKeyHash (WitVKey' _ _ kh _) = kh

instance
  forall crypto kr v.
  (Typeable kr, CV crypto v) =>
  Ord (WitVKey crypto v kr)
  where
  compare = comparing wvkKeyHash

newtype StakeCreds crypto = StakeCreds
  { unStakeCreds :: Map (Credential 'Staking crypto) SlotNo
  }
  deriving (Eq, Generic)
  deriving (Show) via (Quiet (StakeCreds crypto))
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR, ToJSON, FromJSON)

addStakeCreds ::
  (Credential 'Staking crypto) ->
  SlotNo ->
  (StakeCreds crypto) ->
  StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

-- CBOR

instance
  (Crypto crypto) =>
  ToCBOR (DCert crypto)
  where
  toCBOR = \case
    -- DCertDeleg
    DCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR cred
    DCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR cred
    DCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> toCBOR cred
        <> toCBOR poolkh
    -- DCertPool
    DCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> toCBOR (3 :: Word8)
        <> toCBORGroup poolParams
    DCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> toCBOR (4 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch
    -- DCertGenesis
    DCertGenesis (GenesisDelegCert gk kh vrf) ->
      encodeListLen 4
        <> toCBOR (5 :: Word8)
        <> toCBOR gk
        <> toCBOR kh
        <> toCBOR vrf
    -- DCertMIR
    DCertMir mir ->
      encodeListLen 2
        <> toCBOR (6 :: Word8)
        <> toCBOR mir

instance
  (Crypto crypto) =>
  FromCBOR (DCert crypto)
  where
  fromCBOR = decodeRecordSum "DCert crypto" $
    \case
      0 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . RegKey $ x)
      1 -> do
        x <- fromCBOR
        pure (2, DCertDeleg . DeRegKey $ x)
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- fromCBORGroup
        pure (fromIntegral (1 + listLenInt group), DCertPool (RegPool group))
      4 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, DCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, DCertGenesis $ GenesisDelegCert a b c)
      6 -> do
        x <- fromCBOR
        pure (2, DCertMir x)
      k -> invalidKey k

instance
  (Typeable crypto, CV crypto v) =>
  ToCBOR (TxIn crypto v)
  where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR (fromIntegral index :: Word64)

instance
  (CV crypto v) =>
  FromCBOR (TxIn crypto v)
  where
  fromCBOR = do
    decodeRecordNamed "TxIn" (const 2) $ do
      a <- fromCBOR
      (b :: Word64) <- fromCBOR
      pure $ TxIn a (fromInteger $ toInteger b)

instance
  (Typeable crypto, CV crypto v) =>
  ToCBOR (TxOut crypto v)
  where
  toCBOR (TxOut addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  (CV crypto v) =>
  FromCBOR (TxOut crypto v)
  where
  fromCBOR = decodeRecordNamed "TxOut" (const 2) $ do
    addr <- fromCBOR
    (b :: Word64) <- fromCBOR
    pure $ TxOut addr (Coin $ toInteger b)

instance
  (Typeable kr, CV crypto v) =>
  ToCBOR (WitVKey crypto v kr)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . wvkBytes

instance
  (Typeable kr, CV crypto v) =>
  FromCBOR (Annotator (WitVKey crypto v kr))
  where
  fromCBOR =
    annotatorSlice $
      decodeRecordNamed "WitVKey" (const 2) $
        fmap pure $
          mkWitVKey <$> fromCBOR <*> decodeSignedDSIGN
    where
      mkWitVKey k sig = WitVKey' k sig (asWitness $ hashKey k)

instance
  (Crypto crypto) =>
  ToCBOR (TxBody crypto v)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . bodyBytes

instance
  (Crypto crypto) =>
  FromCBOR (Annotator (TxBody crypto v))
  where
  fromCBOR = annotatorSlice $ do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> f 0 (decodeSet fromCBOR) $ \bytes x t ->
            t
              { _inputs' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          1 -> f 1 (decodeStrictSeq fromCBOR) $ \bytes x t ->
            t
              { _outputs' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          2 -> f 2 fromCBOR $ \bytes x t ->
            t
              { _txfee' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          3 -> f 3 fromCBOR $ \_ x t -> t {_ttl' = x}
          4 -> f 4 (decodeStrictSeq fromCBOR) $ \_ x t -> t {_certs' = x}
          5 -> f 5 fromCBOR $ \_ x t -> t {_wdrls' = x}
          6 -> f 6 fromCBOR $ \_ x t -> t {_txUpdate' = SJust x}
          7 -> f 7 fromCBOR $ \_ x t -> t {_mdHash' = SJust x}
          k -> invalidKey k
    let requiredFields :: Map Int String
        requiredFields =
          Map.fromList $
            [ (0, "inputs"),
              (1, "outputs"),
              (2, "fee"),
              (3, "ttl")
            ]
        fields = fst <$> mapParts
        missingFields = Map.filterWithKey (\k _ -> notElem k fields) requiredFields
    unless
      (null missingFields)
      (fail $ "missing required transaction component(s): " <> show missingFields)
    pure $
      Annotator $
        \fullbytes bytes ->
          (foldr ($) basebody (flip runAnnotator fullbytes . snd <$> mapParts)) {bodyBytes = bytes}
    where
      f ::
        Int ->
        Decoder s a ->
        (LByteString -> a -> TxBody crypto v -> TxBody crypto v) ->
        Decoder s (Int, Annotator (TxBody crypto v -> TxBody crypto v))
      f key decoder updater = do
        (x, annBytes) <- withSlice decoder
        let result = Annotator $ \fullbytes txbody ->
              updater (runAnnotator annBytes fullbytes) x txbody
        pure (key, result)
      basebody =
        TxBody'
          { _inputs' = Set.empty,
            _outputs' = StrictSeq.empty,
            _txfee' = Coin 0,
            _ttl' = SlotNo 0,
            _certs' = StrictSeq.empty,
            _wdrls' = Wdrl Map.empty,
            _txUpdate' = SNothing,
            _mdHash' = SNothing,
            bodyBytes = mempty,
            extraSize = 0
          }

instance ToCBOR PoolMetaData where
  toCBOR (PoolMetaData u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetaData where
  fromCBOR = do
    decodeRecordNamed "PoolMetaData" (const 2) $ do
      u <- fromCBOR
      h <- fromCBOR
      pure $ PoolMetaData u h

-- | The size of the '_poolOwners' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolOwners = SizeOfPoolOwners

instance ToCBOR SizeOfPoolOwners where
  toCBOR = panic "The `SizeOfPoolOwners` type cannot be encoded!"

-- | The size of the '_poolRelays' 'Set'.  Only used to compute size of encoded
-- 'PoolParams'.
data SizeOfPoolRelays = SizeOfPoolRelays

instance ToCBOR SizeOfPoolRelays where
  toCBOR = panic "The `SizeOfPoolRelays` type cannot be encoded!"

instance
  (Crypto crypto) =>
  ToCBORGroup (PoolParams crypto)
  where
  toCBORGroup poolParams =
    toCBOR (_poolPubKey poolParams)
      <> toCBOR (_poolVrf poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> encodeFoldable (_poolOwners poolParams)
      <> toCBOR (CborSeq (StrictSeq.getSeq (_poolRelays poolParams)))
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe (_poolMD poolParams))

  encodedGroupSizeExpr size' proxy =
    encodedSizeExpr size' (_poolPubKey <$> proxy)
      + encodedSizeExpr size' (_poolVrf <$> proxy)
      + encodedSizeExpr size' (_poolPledge <$> proxy)
      + encodedSizeExpr size' (_poolCost <$> proxy)
      + encodedSizeExpr size' (_poolMargin <$> proxy)
      + encodedSizeExpr size' (_poolRAcnt <$> proxy)
      + 2
      + poolSize * encodedSizeExpr size' (elementProxy (_poolOwners <$> proxy))
      + 2
      + relaySize * encodedSizeExpr size' (elementProxy (_poolRelays <$> proxy))
      + szCases
        [ Case "Nothing" 1,
          Case "Just" $ encodedSizeExpr size' (elementProxy (_poolMD <$> proxy))
        ]
    where
      poolSize, relaySize :: Size
      poolSize = size' (Proxy @SizeOfPoolOwners)
      relaySize = size' (Proxy @SizeOfPoolRelays)
      elementProxy :: Proxy (f a) -> Proxy a
      elementProxy _ = Proxy

  listLen _ = 9
  listLenBound _ = 9

instance
  (Crypto crypto) =>
  FromCBORGroup (PoolParams crypto)
  where
  fromCBORGroup = do
    vk <- fromCBOR
    vrf <- fromCBOR
    pledge <- fromCBOR
    cost <- fromCBOR
    margin <- fromCBOR
    ra <- fromCBOR
    owners <- decodeSet fromCBOR
    relays <- decodeStrictSeq fromCBOR
    md <- decodeNullMaybe fromCBOR
    pure $
      PoolParams
        { _poolPubKey = vk,
          _poolVrf = vrf,
          _poolPledge = pledge,
          _poolCost = cost,
          _poolMargin = margin,
          _poolRAcnt = ra,
          _poolOwners = owners,
          _poolRelays = relays,
          _poolMD = maybeToStrictMaybe md
        }

instance Relation (StakeCreds crypto) where
  type Domain (StakeCreds crypto) = Credential 'Staking crypto
  type Range (StakeCreds crypto) = SlotNo

  singleton k v = StakeCreds $ Map.singleton k v

  dom (StakeCreds stkCreds) = dom stkCreds

  range (StakeCreds stkCreds) = range stkCreds

  s ◁ (StakeCreds stkCreds) = StakeCreds $ s ◁ stkCreds

  s ⋪ (StakeCreds stkCreds) = StakeCreds $ s ⋪ stkCreds

  (StakeCreds stkCreds) ▷ s = StakeCreds $ stkCreds ▷ s

  (StakeCreds stkCreds) ⋫ s = StakeCreds $ stkCreds ⋫ s

  (StakeCreds a) ∪ (StakeCreds b) = StakeCreds $ a ∪ b

  (StakeCreds a) ⨃ (StakeCreds b) = StakeCreds $ a ⨃ b

  size (StakeCreds stkCreds) = size stkCreds

  {-# INLINE addpair #-}
  addpair k v (StakeCreds x) = StakeCreds (Map.insertWith (\y _z -> y) k v x)

  {-# INLINE haskey #-}
  haskey k (StakeCreds x) = case Map.lookup k x of
    Just _ -> True
    Nothing -> False -- haskey k x

  {-# INLINE removekey #-}
  removekey k (StakeCreds m) = StakeCreds (Map.delete k m)
