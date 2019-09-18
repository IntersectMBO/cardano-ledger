{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module TxData
  where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeMapLen, encodeWord)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Lens.Micro.TH (makeLenses)

import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           BaseTypes (UnitInterval)
import           Coin (Coin)
import           Keys (AnyKeyHash, pattern AnyKeyHash, GenKeyHash, Hash, KeyHash, Sig, VKey,
                     VKeyGenesis, VerKeyVRF, hashAnyKey)
import           Ledger.Core (Relation (..))
import           Slot (Epoch, Slot)
import           Updates (Update, updateNull)

import           Serialization (CBORGroup (..), ToCBORGroup (..))

-- |The delegation of one stake key to another.
data Delegation crypto = Delegation
  { _delegator :: Credential crypto
  , _delegatee :: KeyHash crypto
  } deriving (Eq, Generic, Show)

instance NoUnexpectedThunks (Delegation crypto)

-- |A stake pool.
data PoolParams crypto =
  PoolParams
    { _poolPubKey  :: KeyHash crypto
    , _poolVrf     :: Hash (HASH crypto) (VerKeyVRF (VRF crypto))
    , _poolPledge  :: Coin
    , _poolCost    :: Coin
    , _poolMargin  :: UnitInterval
    , _poolRAcnt   :: RewardAcnt crypto
    , _poolOwners  :: Set (KeyHash crypto)
    } deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (PoolParams crypto)

-- |An account based address for rewards
newtype RewardAcnt crypto = RewardAcnt
  { getRwdCred :: StakeCredential crypto
  } deriving (Show, Eq, NoUnexpectedThunks, Ord)

-- | Script hash or key hash for a payment or a staking object.
data Credential crypto =
    ScriptHashObj (ScriptHash crypto)
  | KeyHashObj    (KeyHash crypto)
  | GenesisHashObj (GenKeyHash crypto)
    deriving (Show, Eq, Generic, Ord)
    deriving ToCBOR via (CBORGroup (Credential crypto))

instance NoUnexpectedThunks (Credential crypto)


-- |An address for UTxO.
data Addr crypto
  = AddrBase (Credential crypto) (Credential crypto)
  | AddrEnterprise (Credential crypto)
  | AddrPtr (Credential crypto) Ptr
  | AddrBootstrap (KeyHash crypto)
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (Addr crypto)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr Slot Ix Ix
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks Ptr

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
--
data MultiSig crypto =
       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   (AnyKeyHash crypto)

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [MultiSig crypto]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [MultiSig crypto]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [MultiSig crypto]
  deriving (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks (MultiSig crypto)

newtype ScriptHash crypto =
  ScriptHash (Hash (HASH crypto) (MultiSig crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (ScriptHash crypto)

type Wdrl crypto = Map (RewardAcnt crypto) Coin

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId crypto
  = TxId { _TxId :: Hash (HASH crypto) (TxBody crypto) }
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (TxId crypto)

-- |The input of a UTxO.
data TxIn crypto
  = TxIn (TxId crypto) Natural
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxIn crypto)

-- |The output of a UTxO.
data TxOut crypto
  = TxOut (Addr crypto) Coin
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (TxOut crypto)

type StakeCredential crypto = Credential crypto

-- | A heavyweight certificate.
data DCert crypto
    -- | A stake key registration certificate.
  = RegKey (StakeCredential crypto)
    -- | A stake key deregistration certificate.
  | DeRegKey (StakeCredential crypto)
    -- | A stake pool registration certificate.
  | RegPool (PoolParams crypto)
    -- | A stake pool retirement certificate.
  | RetirePool (KeyHash crypto) Epoch
    -- | A stake delegation certificate.
  | Delegate (Delegation crypto)
    -- | Genesis key delegation certificate
  | GenesisDelegate (GenKeyHash crypto, KeyHash crypto)
    -- | Move instantaneous rewards certificate
  | InstantaneousRewards (Map (Credential crypto) Coin)
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (DCert crypto)

-- |A raw transaction
data TxBody crypto
  = TxBody
      { _inputs   :: !(Set (TxIn crypto))
      , _outputs  :: [TxOut crypto]
      , _certs    :: Seq (DCert crypto)
      , _wdrls    :: Wdrl crypto
      , _txfee    :: Coin
      , _ttl      :: Slot
      , _txUpdate :: Update crypto
      } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (TxBody crypto)

-- |Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey crypto
  = WitVKey (VKey crypto) !(Sig crypto (TxBody crypto))
  | WitGVKey (VKeyGenesis crypto) !(Sig crypto (TxBody crypto))
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (WitVKey crypto)

witKeyHash
  :: forall crypto. ( Crypto crypto)
  => WitVKey crypto
  -> AnyKeyHash crypto
witKeyHash (WitVKey key _) = hashAnyKey key
witKeyHash (WitGVKey key _) = hashAnyKey key

instance forall crypto
  . ( Crypto crypto)
  => Ord (WitVKey crypto) where
    compare = comparing witKeyHash

-- |A fully formed transaction.
data Tx crypto
  = Tx
      { _body           :: !(TxBody crypto)
      , _witnessVKeySet :: !(Set (WitVKey crypto))
      , _witnessMSigMap ::
          Map (ScriptHash crypto) (MultiSig crypto)
      } deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (Tx crypto)

newtype StakeCreds crypto =
  StakeCreds (Map (StakeCredential crypto) Slot)
  deriving (Show, Eq, NoUnexpectedThunks)

addStakeCreds :: (StakeCredential crypto) -> Slot -> (StakeCreds crypto) -> StakeCreds crypto
addStakeCreds newCred s (StakeCreds creds) = StakeCreds $ Map.insert newCred s creds

newtype StakePools crypto =
  StakePools (Map (KeyHash crypto) Slot)
  deriving (Show, Eq, NoUnexpectedThunks)


-- CBOR

instance
  (Crypto crypto)
  => ToCBOR (DCert crypto)
 where
  toCBOR = \case
    RegKey vk ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR vk

    DeRegKey vk ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR vk

    RegPool poolParams ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR poolParams

    RetirePool vk epoch ->
      encodeListLen 3
        <> toCBOR (3 :: Word8)
        <> toCBOR vk
        <> toCBOR epoch

    Delegate delegation ->
      encodeListLen 2
        <> toCBOR (4 :: Word8)
        <> toCBOR delegation

    GenesisDelegate keys ->
      encodeListLen 2
        <> toCBOR (5 :: Word8)
        <> toCBOR keys

    InstantaneousRewards credCoinMap ->
      encodeListLen 2
        <> toCBOR (6 :: Word8)
        <> toCBOR credCoinMap

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxIn crypto)
 where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (TxOut crypto)
 where
  toCBOR (TxOut addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  Crypto crypto
  => ToCBOR (WitVKey crypto)
 where
  toCBOR (WitVKey vk sig) =
    encodeListLen 2
      <> toCBOR vk
      <> toCBOR sig
  toCBOR (WitGVKey vk sig) =
    encodeListLen 2
      <> toCBOR vk
      <> toCBOR sig


instance
  (Crypto crypto)
  => ToCBOR (Tx crypto)
 where
  toCBOR tx =
    encodeListLen 3
      <> toCBOR (_body tx)
      <> toCBOR (_witnessVKeySet tx)
      <> toCBOR (_witnessMSigMap tx)

instance
  (Crypto crypto)
  => ToCBOR (TxBody crypto)
 where
  toCBOR txbody =
    encodeMapLen 6
      <> encodeWord 0 <> toCBOR (_inputs txbody)
      <> encodeWord 1 <> toCBOR (_outputs txbody)
      <> encodeWord 2 <> toCBOR (_txfee txbody)
      <> encodeWord 3 <> toCBOR (_ttl txbody)
      <> if null cs then mempty else encodeWord 4 <> toCBOR cs
      <> if null ws then mempty else encodeWord 5 <> toCBOR ws
      <> if updateNull us then mempty else encodeWord 6 <> toCBOR us
    where
      cs = toList $ _certs txbody
      ws = _wdrls txbody
      us = _txUpdate txbody

instance ( Crypto crypto) =>
  ToCBOR (MultiSig crypto) where
  toCBOR (RequireSignature hk) =
    encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (RequireAllOf msigs) =
    encodeListLen 2 <> encodeWord 1 <> toCBOR msigs
  toCBOR (RequireAnyOf msigs) =
    encodeListLen 2 <> encodeWord 2 <> toCBOR msigs
  toCBOR (RequireMOf m msigs) =
    encodeListLen 3 <> encodeWord 3 <> toCBOR m <> toCBOR msigs

instance ( Crypto crypto) =>
  FromCBOR (MultiSig crypto) where
  fromCBOR = do
    _ <- decodeListLen
    ctor <- decodeWord
    case ctor of
      0 -> do
       hk <- AnyKeyHash <$> fromCBOR
       pure $ RequireSignature hk
      1 -> do
        msigs <- fromCBOR
        pure $ RequireAllOf msigs
      2 -> do
        msigs <- fromCBOR
        pure $ RequireAnyOf msigs
      3 -> do
        m     <- fromCBOR
        msigs <- fromCBOR
        pure $ RequireMOf m msigs
      _ -> error "pattern no supported"


instance (Typeable crypto, Crypto crypto)
  => ToCBORGroup (Credential crypto) where
  listLen _ = 2
  toCBORGroup = \case
    KeyHashObj     kh -> toCBOR (0 :: Word8) <> toCBOR kh
    ScriptHashObj  hs -> toCBOR (1 :: Word8) <> toCBOR hs
    GenesisHashObj kh -> toCBOR (2 :: Word8) <> toCBOR kh

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (Addr crypto)
 where
  toCBOR (AddrBase       (KeyHashObj a)    (KeyHashObj b))      =
    toCBOR (0 :: Word8)  <> toCBOR a       <> toCBOR b
  toCBOR (AddrBase       (KeyHashObj a)     (ScriptHashObj b))  =
    toCBOR (1 :: Word8)  <> toCBOR a       <> toCBOR b
  toCBOR (AddrBase       (ScriptHashObj a)  (KeyHashObj b))     =
    toCBOR (2 :: Word8)  <> toCBOR a       <> toCBOR b
  toCBOR (AddrBase       (ScriptHashObj a) (ScriptHashObj b))   =
    toCBOR (3 :: Word8)  <> toCBOR a       <> toCBOR b
  toCBOR (AddrPtr        (KeyHashObj a)    pointer)             =
    toCBOR (4 :: Word8)  <> toCBOR a       <> toCBORGroup pointer
  toCBOR (AddrPtr        (ScriptHashObj a)  pointer)            =
    toCBOR (5 :: Word8)  <> toCBOR a       <> toCBORGroup pointer
  toCBOR (AddrEnterprise (KeyHashObj a))                        =
    toCBOR (6 :: Word8)  <> toCBOR a
  toCBOR (AddrEnterprise (ScriptHashObj a))                     =
    toCBOR (7 :: Word8)  <> toCBOR a
  toCBOR (AddrBootstrap  a)                                     =
    toCBOR (8 :: Word8)  <> toCBOR a
  toCBOR _ = error "wat"


instance ToCBORGroup Ptr where
  toCBORGroup (Ptr sl txIx certIx) =
         toCBOR sl
      <> toCBOR (fromInteger (toInteger txIx) :: Word)
      <> toCBOR (fromInteger (toInteger certIx) :: Word)
  listLen _ = 3

instance Crypto crypto =>
  ToCBOR (Delegation crypto) where
  toCBOR delegation =
    encodeListLen 2
      <> toCBOR (_delegator delegation)
      <> toCBOR (_delegatee delegation)


instance
  (Crypto crypto)
  => ToCBOR (PoolParams crypto)
 where
  toCBOR poolParams =
    encodeListLen 7
      <> toCBOR (_poolPubKey poolParams)
      <> toCBOR (_poolVrf poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> toCBOR (_poolOwners poolParams)

instance Crypto crypto
  => ToCBOR (RewardAcnt crypto) where
  toCBOR (RewardAcnt ra) = toCBOR ra

instance Relation (StakeCreds crypto) where
  type Domain (StakeCreds crypto) = StakeCredential crypto
  type Range (StakeCreds crypto)  = Slot

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

-- Lenses

makeLenses ''TxBody

makeLenses ''Tx

makeLenses ''Delegation

makeLenses ''PoolParams
