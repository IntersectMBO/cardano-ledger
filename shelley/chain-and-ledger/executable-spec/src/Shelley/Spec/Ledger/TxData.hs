{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
    StakePools (..),
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
    TxIn (..),
    TxOut (..),
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

import Byron.Spec.Ledger.Core (Relation (..))
import Cardano.Binary
  ( Annotator (..),
    Case (..),
    Decoder,
    FromCBOR (fromCBOR),
    Size,
    ToCBOR (..),
    annotatorSlice,
    decodeListLen,
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodePreEncoded,
    encodeWord,
    enforceSize,
    matchSize,
    serializeEncoding,
    serializeEncoding',
    szCases,
    withSlice,
  )
import Cardano.Prelude
  ( AllowThunksIn (..),
    LByteString,
    NFData,
    NoUnexpectedThunks (..),
    Word64,
    catMaybes,
    panic,
  )
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Address (Addr (..), RewardAcnt (..))
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
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ix,
    Ptr (..),
    StakeCredential,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey,
    VerKeyVRF,
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
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    encodeNullMaybe,
    ipv4FromCBOR,
    ipv4ToCBOR,
    ipv6FromCBOR,
    ipv6ToCBOR,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

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
  deriving (Eq, Ord, Generic, Show)

instance NoUnexpectedThunks PoolMetaData

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    SingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
  | -- | An @A@ or @AAAA@ DNS record
    SingleHostName !(StrictMaybe Port) !DnsName
  | -- | A @SRV@ DNS record
    MultiHostName !(StrictMaybe Port) !DnsName
  deriving (Eq, Ord, Generic, Show)

instance NoUnexpectedThunks StakePoolRelay

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
  toCBOR (MultiHostName p n) =
    encodeListLen 3
      <> toCBOR (2 :: Word8)
      <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
      <> toCBOR n

instance FromCBOR StakePoolRelay where
  fromCBOR = do
    n <- decodeListLen
    w <- decodeWord
    p <- maybeToStrictMaybe <$> decodeNullMaybe fromCBOR
    case w of
      0 ->
        matchSize "SingleHostAddr" 4 n
          >> SingleHostAddr p
          <$> (maybeToStrictMaybe <$> decodeNullMaybe ipv4FromCBOR)
          <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv6FromCBOR)
      1 -> matchSize "SingleHostName" 3 n >> SingleHostName p <$> fromCBOR
      2 -> matchSize "MultiHostName" 3 n >> MultiHostName p <$> fromCBOR
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
  deriving (Show, Generic, Eq, Ord)
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

-- | A unique ID of a transaction, which is computable from the transaction.
newtype TxId crypto = TxId {_TxId :: Hash crypto (TxBody crypto)}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving newtype instance Crypto crypto => ToCBOR (TxId crypto)

deriving newtype instance Crypto crypto => FromCBOR (TxId crypto)

-- | The input of a UTxO.
data TxIn crypto
  = TxIn !(TxId crypto) !Natural -- TODO use our own Natural type
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxIn crypto)

-- | The output of a UTxO.
data TxOut crypto
  = TxOut !(Addr crypto) !Coin
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxOut crypto)

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
  fromCBOR = decodeWord >>= \case
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
  fromCBOR = do
    n <- decodeListLen
    matchSize "SingleHostAddr" 2 n
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
data TxBody crypto = TxBody'
  { _inputs' :: !(Set (TxIn crypto)),
    _outputs' :: !(StrictSeq (TxOut crypto)),
    _certs' :: !(StrictSeq (DCert crypto)),
    _wdrls' :: !(Wdrl crypto),
    _txfee' :: !Coin,
    _ttl' :: !SlotNo,
    _txUpdate' :: !(StrictMaybe (Update crypto)),
    _mdHash' :: !(StrictMaybe (MetaDataHash crypto)),
    bodyBytes :: LByteString,
    extraSize :: Int64 -- This is the contribution of inputs, outputs, and fees to the size of the transaction
  }
  deriving (Show, Eq, Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bodyBytes"] (TxBody crypto)

pattern TxBody ::
  Crypto crypto =>
  Set (TxIn crypto) ->
  StrictSeq (TxOut crypto) ->
  StrictSeq (DCert crypto) ->
  Wdrl crypto ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update crypto) ->
  StrictMaybe (MetaDataHash crypto) ->
  TxBody crypto
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
          es = fromIntegral $ BS.length inputBytes + BS.length outputBytes + BS.length feeBytes
          n = fromIntegral $ length l
          bytes = serializeEncoding $ encodeMapLen n <> fold l
       in TxBody' _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash bytes es

{-# COMPLETE TxBody #-}

-- | Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto = WitVKey'
  { wvkKey' :: !(VKey 'Witness crypto),
    wvkSig' :: !(SignedDSIGN crypto (Hash crypto (TxBody crypto))),
    wvkBytes :: LByteString
  }
  deriving (Show, Eq, Generic)
  deriving (NoUnexpectedThunks) via AllowThunksIn '["wvkBytes"] (WitVKey crypto)

pattern WitVKey ::
  Crypto crypto =>
  VKey 'Witness crypto ->
  SignedDSIGN crypto (Hash crypto (TxBody crypto)) ->
  WitVKey crypto
pattern WitVKey k s <-
  WitVKey' k s _
  where
    WitVKey k s =
      let bytes = serializeEncoding $ encodeListLen 2 <> toCBOR k <> encodeSignedDSIGN s
       in WitVKey' k s bytes

{-# COMPLETE WitVKey #-}

witKeyHash ::
  forall crypto.
  (Crypto crypto) =>
  WitVKey crypto ->
  KeyHash 'Witness crypto
witKeyHash (WitVKey key _) = hashKey key

instance
  forall crypto.
  (Crypto crypto) =>
  Ord (WitVKey crypto)
  where
  compare = comparing witKeyHash

newtype StakeCreds crypto
  = StakeCreds (Map (Credential 'Staking crypto) SlotNo)
  deriving (Show, Eq, Generic)
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

addStakeCreds ::
  (Credential 'Staking crypto) ->
  SlotNo ->
  (StakeCreds crypto) ->
  StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

newtype StakePools crypto = StakePools {unStakePools :: (Map (KeyHash 'StakePool crypto) SlotNo)}
  deriving (Show, Eq, Generic)
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

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
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "RegKey" 2 n >> (DCertDeleg . RegKey) <$> fromCBOR
      1 -> matchSize "DeRegKey" 2 n >> (DCertDeleg . DeRegKey) <$> fromCBOR
      2 -> do
        matchSize "Delegate" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertDeleg $ Delegate (Delegation a b)
      3 -> do
        group <- fromCBORGroup
        matchSize "RegPool" (fromIntegral $ 1 + listLen group) n
        pure $ DCertPool $ RegPool group
      4 -> do
        matchSize "RetirePool" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertPool $ RetirePool a (EpochNo b)
      5 -> do
        matchSize "GenesisDelegate" 4 n
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure $ DCertGenesis $ GenesisDelegCert a b c
      6 -> matchSize "MIRCert" 2 n >> DCertMir <$> fromCBOR
      k -> invalidKey k

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (TxIn crypto)
  where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR (fromIntegral index :: Word64)

instance
  (Crypto crypto) =>
  FromCBOR (TxIn crypto)
  where
  fromCBOR = do
    enforceSize "TxIn" 2
    a <- fromCBOR
    (b :: Word64) <- fromCBOR
    pure $ TxIn a (fromInteger $ toInteger b)

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (TxOut crypto)
  where
  toCBOR (TxOut addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  (Crypto crypto) =>
  FromCBOR (TxOut crypto)
  where
  fromCBOR = decodeRecordNamed "TxOut" (const 2) $ do
    addr <- fromCBOR
    (b :: Word64) <- fromCBOR
    pure $ TxOut addr (Coin $ toInteger b)

instance
  Crypto crypto =>
  ToCBOR (WitVKey crypto)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . wvkBytes

instance
  Crypto crypto =>
  FromCBOR (Annotator (WitVKey crypto))
  where
  fromCBOR =
    annotatorSlice $ decodeRecordNamed "WitVKey" (const 2)
      $ fmap pure
      $ WitVKey' <$> fromCBOR <*> decodeSignedDSIGN

instance
  (Crypto crypto) =>
  ToCBOR (TxBody crypto)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . bodyBytes

instance
  (Crypto crypto) =>
  FromCBOR (Annotator (TxBody crypto))
  where
  fromCBOR = annotatorSlice $ do
    mapParts <- decodeMapContents $
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
    pure $ Annotator $ \fullbytes bytes ->
      (foldr ($) basebody (flip runAnnotator fullbytes . snd <$> mapParts)) {bodyBytes = bytes}
    where
      f ::
        Int ->
        Decoder s a ->
        (LByteString -> a -> TxBody crypto -> TxBody crypto) ->
        Decoder s (Int, Annotator (TxBody crypto -> TxBody crypto))
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
    enforceSize "PoolMetaData" 2
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

  (StakeCreds a) ⨃ b = StakeCreds $ a ⨃ b

  vmax <=◁ (StakeCreds stkCreds) = StakeCreds $ vmax <=◁ stkCreds

  (StakeCreds stkCreds) ▷<= vmax = StakeCreds $ stkCreds ▷<= vmax

  (StakeCreds stkCreds) ▷>= vmin = StakeCreds $ stkCreds ▷>= vmin

  size (StakeCreds stkCreds) = size stkCreds
