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
  ( Addr (..)
  , Credential (..)
  , DCert (..)
  , DelegCert (..)
  , Delegation (..)
  , GenesisDelegCert (..)
  , Ix
  , MIRCert (..)
  , PoolCert (..)
  , PoolMetaData (..)
  , PoolParams (..)
  , Ptr (..)
  , RewardAcnt (..)
  , StakeCreds (..)
  , StakePools (..)
  , StakePoolRelay (..)
  , StakeReference (..)
  , TxBody
    ( TxBody
    , _inputs
    , _outputs
    , _certs
    , _wdrls
    , _txfee
    , _ttl
    , _txUpdate
    , _mdHash
    )
  , TxId (..)
  , TxIn (..)
  , TxOut (..)
  , Url
  , Wdrl (..)
  , WitVKey (WitVKey, wvkBytes)
  --
  , witKeyHash
  , addStakeCreds
)
  where

import           Cardano.Binary (Annotator (..), FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     annotatorSlice, decodeListLen, decodeWord, encodeListLen, encodeMapLen,
                     encodePreEncoded, encodeWord, enforceSize, matchSize, serializeEncoding)
import           Cardano.Prelude (AllowThunksIn (..), LByteString, NFData, NoUnexpectedThunks (..),
                     Word64, catMaybes)
import           Control.Monad (unless)
import           Shelley.Spec.Ledger.Crypto

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (fold)
import           Data.IP (IPv4, IPv6)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Byron.Spec.Ledger.Core (Relation (..))
import           Shelley.Spec.Ledger.BaseTypes (DnsName, Port, StrictMaybe (..), UnitInterval, Url,
                     invalidKey, maybeToStrictMaybe, strictMaybeToMaybe)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (HasKeyRole (..), Hash, KeyHash (..), KeyRole (..),
                     SignedDSIGN, VKey, VerKeyVRF, decodeSignedDSIGN, encodeSignedDSIGN, hashKey)
import           Shelley.Spec.Ledger.MetaData (MetaDataHash)
import           Shelley.Spec.Ledger.Orphans ()
import           Shelley.Spec.Ledger.PParams (Update)
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

import           Shelley.Spec.Ledger.Serialization (CBORGroup (..), CborSeq (..),
                     FromCBORGroup (..), ToCBORGroup (..), decodeMapContents, decodeNullMaybe,
                     decodeRecordNamed, decodeSet, encodeFoldable, encodeNullMaybe, ipv4FromCBOR,
                     ipv4ToCBOR, ipv6FromCBOR, ipv6ToCBOR, mapFromCBOR, mapToCBOR,
                     unwrapCborStrictSeq)

import           Shelley.Spec.Ledger.Scripts

-- |The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: !(StakeCredential crypto)
  , _delegatee :: !(KeyHash 'StakePool crypto)
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (Delegation crypto)

data PoolMetaData = PoolMetaData
  { _poolMDUrl  :: !Url
  , _poolMDHash :: !ByteString
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks PoolMetaData

data StakePoolRelay =
     SingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
     -- ^ One or both of IPv4 & IPv6
   | SingleHostName !(StrictMaybe Port) !DnsName
     -- ^ An @A@ or @AAAA@ DNS record
   | MultiHostName  !(StrictMaybe Port) !DnsName
     -- ^ A @SRV@ DNS record
  deriving (Eq, Generic, Show)

instance NoUnexpectedThunks StakePoolRelay

instance ToCBOR StakePoolRelay where
  toCBOR (SingleHostAddr p ipv4 ipv6)
    = encodeListLen 4
        <> toCBOR (0 :: Word8)
        <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
        <> encodeNullMaybe ipv4ToCBOR (strictMaybeToMaybe ipv4)
        <> encodeNullMaybe ipv6ToCBOR (strictMaybeToMaybe ipv6)
  toCBOR (SingleHostName p n)
    = encodeListLen 3
        <> toCBOR (1 :: Word8)
        <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
        <> toCBOR n
  toCBOR (MultiHostName p n)
    = encodeListLen 3
        <> toCBOR (2 :: Word8)
        <> encodeNullMaybe toCBOR (strictMaybeToMaybe p)
        <> toCBOR n

instance FromCBOR StakePoolRelay where
  fromCBOR = do
    n <- decodeListLen
    w <- decodeWord
    p <- maybeToStrictMaybe <$> decodeNullMaybe fromCBOR
    case w of
      0 -> matchSize "SingleHostAddr" 4 n >>
             SingleHostAddr p
               <$> (maybeToStrictMaybe <$> decodeNullMaybe ipv4FromCBOR)
               <*> (maybeToStrictMaybe <$> decodeNullMaybe ipv6FromCBOR)
      1 -> matchSize "SingleHostName" 3 n >> SingleHostName p <$> fromCBOR
      2 -> matchSize "MultiHostName"  3 n >> MultiHostName p <$> fromCBOR
      k -> invalidKey k

-- |A stake pool.
data PoolParams crypto =
  PoolParams
    { _poolPubKey  :: !(KeyHash 'StakePool crypto)
    , _poolVrf     :: !(Hash crypto (VerKeyVRF crypto))
    , _poolPledge  :: !Coin
    , _poolCost    :: !Coin
    , _poolMargin  :: !UnitInterval
    , _poolRAcnt   :: !(RewardAcnt crypto)
    , _poolOwners  :: !(Set (KeyHash 'Staking crypto))
    , _poolRelays  :: !(StrictSeq StakePoolRelay)
    , _poolMD      :: !(StrictMaybe PoolMetaData)
    } deriving (Show, Generic, Eq)
      deriving ToCBOR via CBORGroup (PoolParams crypto)
      deriving FromCBOR via CBORGroup (PoolParams crypto)

instance NoUnexpectedThunks (PoolParams crypto)

-- |An account based address for rewards
--
-- A reward account uses the staking credential
newtype RewardAcnt crypto = RewardAcnt
  { getRwdCred :: Credential 'Staking crypto
  } deriving (Show, Eq, Generic, Ord)
    deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

-- | Script hash or key hash for a payment or a staking object.
data Credential (kr :: KeyRole) crypto =
    ScriptHashObj !(ScriptHash crypto)
  | KeyHashObj    !(KeyHash kr crypto)
    deriving (Show, Eq, Generic, NFData, Ord)
    deriving ToCBOR via (CBORGroup (Credential kr crypto))

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

newtype GenesisCredential crypto = GenesisCredential (KeyHash 'Genesis crypto)
  deriving (Show, Generic)

instance Ord (GenesisCredential crypto)
  where compare (GenesisCredential gh) (GenesisCredential gh')  = compare gh gh'

instance Eq (GenesisCredential crypto)
  where (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance NoUnexpectedThunks (Credential kr crypto)

type PaymentCredential crypto = Credential 'Payment crypto
type StakeCredential crypto = Credential 'Staking crypto

data StakeReference crypto
  = StakeRefBase !(StakeCredential crypto)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (StakeReference crypto)

-- |An address for UTxO.
data Addr crypto
  = Addr !(PaymentCredential crypto) !(StakeReference crypto)
  | AddrBootstrap !(KeyHash 'Payment crypto)
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (ToCBOR, FromCBOR) via (CBORGroup (Addr crypto))

instance NoUnexpectedThunks (Addr crypto)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr !SlotNo !Ix !Ix
  deriving (Show, Eq, Generic, Ord)
  deriving (ToCBOR, FromCBOR) via CBORGroup Ptr

instance NFData Ptr
instance NoUnexpectedThunks Ptr

newtype Wdrl crypto = Wdrl { unWdrl :: Map (RewardAcnt crypto) Coin }
  deriving (Show, Eq, Generic)
  deriving newtype NoUnexpectedThunks

instance Crypto crypto => ToCBOR (Wdrl crypto) where
  toCBOR = mapToCBOR . unWdrl

instance Crypto crypto => FromCBOR (Wdrl crypto) where
  fromCBOR = Wdrl <$> mapFromCBOR


-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId crypto
  = TxId { _TxId :: Hash crypto (TxBody crypto) }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving newtype instance Crypto crypto => ToCBOR (TxId crypto)
deriving newtype instance Crypto crypto => FromCBOR (TxId crypto)

-- |The input of a UTxO.
data TxIn crypto
  = TxIn !(TxId crypto) !Natural -- TODO use our own Natural type
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxIn crypto)

-- |The output of a UTxO.
data TxOut crypto
  = TxOut !(Addr crypto) !Coin
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxOut crypto)

data DelegCert crypto =
    -- | A stake key registration certificate.
    RegKey !(StakeCredential crypto)
    -- | A stake key deregistration certificate.
  | DeRegKey !(StakeCredential crypto)
    -- | A stake delegation certificate.
  | Delegate !(Delegation crypto)
  deriving (Show, Generic, Eq)

data PoolCert crypto =
    -- | A stake pool registration certificate.
    RegPool !(PoolParams crypto)
    -- | A stake pool retirement certificate.
  | RetirePool !(KeyHash 'StakePool crypto) !EpochNo
  deriving (Show, Generic, Eq)

-- | Genesis key delegation certificate
data GenesisDelegCert crypto =
    GenesisDelegCert !(KeyHash 'Genesis crypto) !(KeyHash 'GenesisDelegate crypto)
  deriving (Show, Generic, Eq)

-- | Move instantaneous rewards certificate
newtype MIRCert crypto = MIRCert (Map (Credential 'Staking crypto) Coin)
  deriving (Show, Generic, Eq)

instance Crypto crypto => FromCBOR (MIRCert crypto) where
  fromCBOR = MIRCert <$> mapFromCBOR

instance Crypto crypto => ToCBOR (MIRCert crypto) where
  toCBOR (MIRCert c) = mapToCBOR c

-- | A heavyweight certificate.
data DCert crypto =
    DCertDeleg !(DelegCert crypto)
  | DCertPool !(PoolCert crypto)
  | DCertGenesis !(GenesisDelegCert crypto)
  | DCertMir !(MIRCert crypto)
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (DelegCert crypto)
instance NoUnexpectedThunks (PoolCert crypto)
instance NoUnexpectedThunks (GenesisDelegCert crypto)
instance NoUnexpectedThunks (MIRCert crypto)
instance NoUnexpectedThunks (DCert crypto)

-- |A raw transaction
data TxBody crypto
  = TxBody'
      { _inputs'   :: !(Set (TxIn crypto))
      , _outputs'  :: !(StrictSeq (TxOut crypto))
      , _certs'    :: !(StrictSeq (DCert crypto))
      , _wdrls'    :: !(Wdrl crypto)
      , _txfee'    :: !Coin
      , _ttl'      :: !SlotNo
      , _txUpdate' :: !(StrictMaybe (Update crypto))
      , _mdHash'   :: !(StrictMaybe (MetaDataHash crypto))
      , bodyBytes  :: LByteString
      } deriving (Show, Eq, Generic)
        deriving NoUnexpectedThunks via
          AllowThunksIn '["bodyBytes"] (TxBody crypto)

pattern TxBody
  :: Crypto crypto
  => Set (TxIn crypto)
  -> StrictSeq (TxOut crypto)
  -> StrictSeq (DCert crypto)
  -> Wdrl crypto
  -> Coin
  -> SlotNo
  -> StrictMaybe (Update crypto)
  -> StrictMaybe (MetaDataHash crypto)
  -> TxBody crypto
pattern TxBody
 { _inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash } <-
 TxBody'
   { _inputs'   = _inputs
   , _outputs'  = _outputs
   , _certs'    =  _certs
   , _wdrls'    = _wdrls
   , _txfee'    = _txfee
   , _ttl'      = _ttl
   , _txUpdate' = _txUpdate
   , _mdHash'   = _mdHash
   }
  where
  TxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
    let encodeMapElement ix x = Just (encodeWord ix <> toCBOR x)
        encodeMapElementUnless condition ix x = if condition x
          then Nothing
          else encodeMapElement ix x
        l = catMaybes
          [ encodeMapElement 0 $ Set.toList _inputs
          , encodeMapElement 1 $ CborSeq $ StrictSeq.getSeq _outputs
          , encodeMapElement 2 _txfee
          , encodeMapElement 3 _ttl
          , encodeMapElementUnless null 4 $ CborSeq $ StrictSeq.getSeq _certs
          , encodeMapElementUnless (null . unWdrl) 5 _wdrls
          , encodeMapElement 6 =<< strictMaybeToMaybe _txUpdate
          , encodeMapElement 7 =<< strictMaybeToMaybe _mdHash
          ]
        n = fromIntegral $ length l
        bytes = serializeEncoding $ encodeMapLen n <> fold l
     in TxBody' _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash bytes

{-# COMPLETE TxBody #-}

-- |Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto
  = WitVKey'
    { wvkKey' :: !(VKey 'Witness crypto)
    , wvkSig' :: !(SignedDSIGN crypto (Hash crypto (TxBody crypto)))
    , wvkBytes :: LByteString
    }
  deriving (Show, Eq, Generic)
  deriving NoUnexpectedThunks via AllowThunksIn '["wvkBytes"] (WitVKey crypto)

pattern WitVKey
   :: Crypto crypto
   => VKey 'Witness crypto
   -> SignedDSIGN crypto (Hash crypto (TxBody crypto))
   -> WitVKey crypto
pattern WitVKey k s <- WitVKey' k s _
  where
  WitVKey k s =
    let bytes = serializeEncoding $ encodeListLen 2 <> toCBOR k <> encodeSignedDSIGN s
     in WitVKey' k s bytes

{-# COMPLETE WitVKey #-}

witKeyHash
  :: forall crypto. ( Crypto crypto)
  => WitVKey crypto
  -> KeyHash 'Witness crypto
witKeyHash (WitVKey key _) = hashKey key

instance forall crypto
  . ( Crypto crypto)
  => Ord (WitVKey crypto) where
    compare = comparing witKeyHash

newtype StakeCreds crypto =
  StakeCreds (Map (Credential 'Staking crypto) SlotNo)
  deriving (Show, Eq, Generic)
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

addStakeCreds
  :: (Credential 'Staking crypto)
  -> SlotNo
  -> (StakeCreds crypto)
  -> StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

newtype StakePools crypto =
  StakePools { unStakePools :: (Map (KeyHash 'StakePool crypto) SlotNo) }
  deriving (Show, Eq, Generic)
  deriving newtype (FromCBOR, NFData, NoUnexpectedThunks, ToCBOR)

-- CBOR

instance
  (Crypto crypto)
  => ToCBOR (DCert crypto)
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
     DCertGenesis (GenesisDelegCert gk kh) ->
           encodeListLen 3
           <> toCBOR (5 :: Word8)
           <> toCBOR gk
           <> toCBOR kh

           -- DCertMIR
     DCertMir mir ->
           encodeListLen 2
           <> toCBOR (6 :: Word8)
           <> toCBOR mir

instance
  (Crypto crypto)
  => FromCBOR (DCert crypto)
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
        matchSize "GenesisDelegate" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ DCertGenesis $ GenesisDelegCert a b
      6 -> matchSize "MIRCert" 2 n >> DCertMir <$> fromCBOR
      k -> invalidKey k

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxIn crypto)
 where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR (fromIntegral index :: Word64)

instance (Crypto crypto) =>
  FromCBOR (TxIn crypto) where
  fromCBOR = do
    enforceSize "TxIn" 2
    a <- fromCBOR
    (b :: Word64) <- fromCBOR
    pure $ TxIn a (fromInteger $ toInteger b)

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxOut crypto)
 where
  toCBOR (TxOut addr coin) =
    encodeListLen (listLen addr + 1)
      <> toCBORGroup addr
      <> toCBOR coin

instance (Crypto crypto) =>
  FromCBOR (TxOut crypto) where
  fromCBOR = do
    n <- decodeListLen
    addr <- fromCBORGroup
    (b :: Word64) <- fromCBOR
    matchSize "TxOut" ((fromIntegral . toInteger . listLen) addr + 1) n
    pure $ TxOut addr (Coin $ toInteger b)

instance
  Crypto crypto
  => ToCBOR (WitVKey crypto)
 where
  toCBOR = encodePreEncoded . BSL.toStrict . wvkBytes

instance
  Crypto crypto
  => FromCBOR (Annotator (WitVKey crypto))
 where
  fromCBOR = annotatorSlice $ decodeRecordNamed "WitVKey" (const 2) $
    fmap pure $ WitVKey' <$> fromCBOR <*> decodeSignedDSIGN

instance
  (Crypto crypto)
  => ToCBOR (TxBody crypto)
 where
  toCBOR = encodePreEncoded . BSL.toStrict . bodyBytes

instance
  (Crypto crypto)
  => FromCBOR (Annotator (TxBody crypto))
  where
   fromCBOR = annotatorSlice $ do
     mapParts <- decodeMapContents $
       decodeWord >>= \case
         0 -> decodeSet fromCBOR                 >>= \x -> pure (0, \t -> t { _inputs   = x })
         1 -> (unwrapCborStrictSeq <$> fromCBOR) >>= \x -> pure (1, \t -> t { _outputs'  = x })
         2 -> fromCBOR                           >>= \x -> pure (2, \t -> t { _txfee'    = x })
         3 -> fromCBOR                           >>= \x -> pure (3, \t -> t { _ttl'      = x })
         4 -> (unwrapCborStrictSeq <$> fromCBOR) >>= \x -> pure (4, \t -> t { _certs'    = x })
         5 -> fromCBOR                           >>= \x -> pure (5, \t -> t { _wdrls'    = x })
         6 -> fromCBOR                           >>= \x -> pure (6, \t -> t { _txUpdate' = SJust x })
         7 -> fromCBOR                           >>= \x -> pure (7, \t -> t { _mdHash'   = SJust x })
         k -> invalidKey k
     let requiredFields :: Map Int String
         requiredFields = Map.fromList $
           [ (0, "inputs")
           , (1, "outputs")
           , (2, "fee")
           , (3, "ttl")
           ]
         fields = fst <$> mapParts
         missingFields = Map.filterWithKey (\k _ -> notElem k fields) requiredFields
     unless (null missingFields)
       (fail $ "missing required transaction component(s): " <> show missingFields)
     pure $ Annotator $ \_fullbytes bytes ->
       (foldr ($) basebody (snd <$> mapParts)) { bodyBytes = bytes }
     where
       basebody = TxBody'
          { _inputs'  = Set.empty
          , _outputs' = StrictSeq.empty
          , _txfee'   = Coin 0
          , _ttl'     = SlotNo 0
          , _certs'   = StrictSeq.empty
          , _wdrls'   = Wdrl Map.empty
          , _txUpdate'= SNothing
          , _mdHash'  = SNothing
          , bodyBytes = mempty
          }


instance (Typeable kr, Typeable crypto, Crypto crypto)
  => ToCBORGroup (Credential kr crypto) where
  listLen _ = 2
  toCBORGroup = \case
    KeyHashObj     kh -> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj  hs -> toCBOR (1 :: Word8) <> toCBOR hs

instance (Typeable crypto, Crypto crypto)
  => ToCBOR (GenesisCredential crypto)
  where toCBOR (GenesisCredential kh) =
          toCBOR kh

instance (Crypto crypto, Typeable kr) =>
  FromCBOR (Credential kr crypto) where
  fromCBOR = do
    enforceSize "Credential" 2
    decodeWord >>= \case
      0 -> KeyHashObj <$> fromCBOR
      1 -> ScriptHashObj <$> fromCBOR
      k -> invalidKey k

instance
  (Typeable crypto, Crypto crypto)
  => ToCBORGroup (Addr crypto)
 where
  listLen (Addr _ (StakeRefBase _)) = 3
  listLen (Addr _ (StakeRefPtr p)) = 2 + listLen p
  listLen (Addr _ (StakeRefNull)) = 2
  listLen (AddrBootstrap  _) = 2

  toCBORGroup (Addr (KeyHashObj a) (StakeRefBase (KeyHashObj b))) =
    toCBOR (0 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (Addr (KeyHashObj a) (StakeRefBase (ScriptHashObj b))) =
    toCBOR (1 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (Addr (ScriptHashObj a) (StakeRefBase (KeyHashObj b))) =
    toCBOR (2 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (Addr (ScriptHashObj a) (StakeRefBase (ScriptHashObj b))) =
    toCBOR (3 :: Word8) <> toCBOR a <> toCBOR b
  toCBORGroup (Addr (KeyHashObj a) (StakeRefPtr pointer)) =
    toCBOR (4 :: Word8) <> toCBOR a <> toCBORGroup pointer
  toCBORGroup (Addr (ScriptHashObj a) (StakeRefPtr pointer)) =
    toCBOR (5 :: Word8) <> toCBOR a <> toCBORGroup pointer
  toCBORGroup (Addr (KeyHashObj a) StakeRefNull) =
    toCBOR (6 :: Word8) <> toCBOR a
  toCBORGroup (Addr (ScriptHashObj a) StakeRefNull) =
    toCBOR (7 :: Word8) <> toCBOR a
  toCBORGroup (AddrBootstrap a) =
    toCBOR (8 :: Word8) <> toCBOR a

instance (Crypto crypto) =>
  FromCBORGroup (Addr crypto) where
  fromCBORGroup = do
    decodeWord >>= \case
      0 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ Addr (KeyHashObj a) (StakeRefBase (KeyHashObj b))
      1 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ Addr (KeyHashObj a) (StakeRefBase (ScriptHashObj b))
      2 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ Addr (ScriptHashObj a) (StakeRefBase (KeyHashObj b))
      3 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure $ Addr (ScriptHashObj a) (StakeRefBase (ScriptHashObj b))
      4 -> do
        a <- fromCBOR
        x <- fromCBOR
        y <- fromCBOR
        z <- fromCBOR
        pure $ Addr (KeyHashObj a) (StakeRefPtr (Ptr x y z))
      5 -> do
        a <- fromCBOR
        x <- fromCBOR
        y <- fromCBOR
        z <- fromCBOR
        pure $ Addr (ScriptHashObj a) (StakeRefPtr (Ptr x y z))
      6 -> do
        a <- fromCBOR
        pure $ Addr (KeyHashObj a) StakeRefNull
      7 -> do
        a <- fromCBOR
        pure $ Addr (ScriptHashObj a) StakeRefNull
      8 -> do
        a <- fromCBOR
        pure $ AddrBootstrap a
      k -> invalidKey k

instance ToCBORGroup Ptr where
  toCBORGroup (Ptr sl txIx certIx) =
         toCBOR sl
      <> toCBOR (fromInteger (toInteger txIx) :: Word)
      <> toCBOR (fromInteger (toInteger certIx) :: Word)
  listLen _ = 3

instance FromCBORGroup Ptr where
  fromCBORGroup = Ptr <$> fromCBOR <*> fromCBOR <*> fromCBOR

instance ToCBOR PoolMetaData
 where
  toCBOR (PoolMetaData u h) =
    encodeListLen 2
      <> toCBOR u
      <> toCBOR h

instance FromCBOR PoolMetaData
  where
  fromCBOR = do
    enforceSize "PoolMetaData" 2
    u <- fromCBOR
    h <- fromCBOR
    pure $ PoolMetaData u h

instance
  (Crypto crypto)
  => ToCBORGroup (PoolParams crypto)
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
  listLen _ = 9

instance
  (Crypto crypto)
  => FromCBORGroup (PoolParams crypto)
 where
  fromCBORGroup = do
    vk <- fromCBOR
    vrf <- fromCBOR
    pledge <- fromCBOR
    cost <- fromCBOR
    margin <- fromCBOR
    ra <- fromCBOR
    owners <- decodeSet fromCBOR
    relays <- fromCBOR
    md <- decodeNullMaybe fromCBOR
    pure $ PoolParams
            { _poolPubKey = vk
            , _poolVrf    = vrf
            , _poolPledge = pledge
            , _poolCost   = cost
            , _poolMargin = margin
            , _poolRAcnt  = ra
            , _poolOwners = owners
            , _poolRelays = unwrapCborStrictSeq relays
            , _poolMD     = maybeToStrictMaybe md
            }

instance Relation (StakeCreds crypto) where
  type Domain (StakeCreds crypto) = Credential 'Staking crypto
  type Range (StakeCreds crypto)  = SlotNo

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
