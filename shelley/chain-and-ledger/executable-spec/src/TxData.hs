{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TxData
  where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeWord)

import           Lens.Micro.TH (makeLenses)

import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           Numeric.Natural (Natural)

import           BaseTypes
import           Coin
import           Keys
import           Slot
import           Updates

-- |The delegation of one stake key to another.
data Delegation hashAlgo dsignAlgo = Delegation
  { _delegator :: Credential hashAlgo dsignAlgo
  , _delegatee :: KeyHash hashAlgo dsignAlgo
  } deriving (Show, Eq, Ord)

-- |A stake pool.
data PoolParams hashAlgo dsignAlgo =
  PoolParams
    { _poolPubKey  :: VKey dsignAlgo
    , _poolPledge  :: Coin
    , _poolPledges :: Map (VKey dsignAlgo) Coin -- TODO not updated currently
    , _poolCost    :: Coin
    , _poolMargin  :: UnitInterval
    , _poolAltAcnt :: Maybe (KeyHash hashAlgo dsignAlgo)
    , _poolRAcnt   :: RewardAcnt hashAlgo dsignAlgo
    , _poolOwners  :: Set (KeyHash hashAlgo dsignAlgo)
    } deriving (Show, Eq, Ord)

-- |An account based address for a rewards
newtype RewardAcnt hashAlgo signAlgo = RewardAcnt
  { getRwdHK :: Credential hashAlgo signAlgo
  } deriving (Show, Eq, Ord)

-- | Script hash or key hash for a payment or a staking object.
data Credential hashAlgo dsignAlgo =
    ScriptHashObj { _validatorHash :: ScriptHash hashAlgo dsignAlgo }
  | KeyHashObj    { _vkeyHash      :: KeyHash hashAlgo dsignAlgo }
    deriving (Show, Eq, Ord)

-- |An address for UTxO.
data Addr hashAlgo dsignAlgo
  = AddrBase
      { _paymentObj :: Credential hashAlgo dsignAlgo
      , _stakingObj :: Credential hashAlgo dsignAlgo
      }
  | AddrEnterprise
    { _enterprisePayment :: Credential hashAlgo dsignAlgo }
  | AddrPtr
      { _stakePtr :: Ptr
      }
  deriving (Show, Eq, Ord)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr Slot Ix Ix
  deriving (Show, Eq, Ord)

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
data MultiSig hashAlgo dsignAlgo =
       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   (KeyHash hashAlgo dsignAlgo)

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [MultiSig hashAlgo dsignAlgo]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [MultiSig hashAlgo dsignAlgo]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [MultiSig hashAlgo dsignAlgo]
  deriving (Show, Eq, Ord)

newtype ScriptHash hashAlgo dsignAlgo =
  ScriptHash (Hash hashAlgo (MultiSig hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord, ToCBOR)

type Wdrl hashAlgo dsignAlgo = Map (RewardAcnt hashAlgo dsignAlgo) Coin

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId hashAlgo dsignAlgo
  = TxId { _TxId :: Hash hashAlgo (TxBody hashAlgo dsignAlgo) }
  deriving (Show, Eq, Ord, ToCBOR)

-- |The input of a UTxO.
data TxIn hashAlgo dsignAlgo
  = TxIn (TxId hashAlgo dsignAlgo) Natural
  deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut hashAlgo dsignAlgo
  = TxOut (Addr hashAlgo dsignAlgo) Coin
  deriving (Show, Eq, Ord)

type StakeCredential hashAlgo dsignAlgo = Credential hashAlgo dsignAlgo

-- | A heavyweight certificate.
data DCert hashAlgo dsignAlgo
    -- | A stake key registration certificate.
  = RegKey (StakeCredential hashAlgo dsignAlgo)
    -- | A stake key deregistration certificate.
  | DeRegKey (StakeCredential hashAlgo dsignAlgo)
    -- | A stake pool registration certificate.
  | RegPool (PoolParams hashAlgo dsignAlgo)
    -- | A stake pool retirement certificate.
  | RetirePool (KeyHash hashAlgo dsignAlgo) Epoch
    -- | A stake delegation certificate.
  | Delegate (Delegation hashAlgo dsignAlgo)
    -- | Genesis key delegation certificate
  | GenesisDelegate (VKeyGenesis dsignAlgo, VKey dsignAlgo)
  deriving (Show, Eq, Ord)

-- |A raw transaction
data TxBody hashAlgo dsignAlgo
  = TxBody
      { _inputs   :: !(Set (TxIn hashAlgo dsignAlgo))
      , _outputs  :: [TxOut hashAlgo dsignAlgo]
      , _certs    :: ![DCert hashAlgo dsignAlgo]
      , _wdrls    :: Wdrl hashAlgo dsignAlgo
      , _txfee    :: Coin
      , _ttl      :: Slot
      , _txUpdate :: Update dsignAlgo
      } deriving (Show, Eq, Ord)

-- |Proof/Witness that a transaction is authorized by the given key holder.
data WitVKey hashAlgo dsignAlgo
  = WitVKey (VKey dsignAlgo) !(Sig dsignAlgo (TxBody hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord)

-- |A fully formed transaction.
data Tx hashAlgo dsignAlgo
  = Tx
      { _body           :: !(TxBody hashAlgo dsignAlgo)
      , _witnessVKeySet :: !(Set (WitVKey hashAlgo dsignAlgo))
      , _witnessMSigMap ::
          Map (ScriptHash hashAlgo dsignAlgo) (MultiSig hashAlgo dsignAlgo)
      } deriving (Show, Eq, Ord)

newtype StakeKeys hashAlgo dsignAlgo =
  StakeKeys (Map (StakeCredential hashAlgo dsignAlgo) Slot)
  deriving (Show, Eq)

newtype StakePools hashAlgo dsignAlgo =
  StakePools (Map (KeyHash hashAlgo dsignAlgo) Slot)
  deriving (Show, Eq)


-- CBOR

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (DCert hashAlgo dsignAlgo)
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

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (TxIn hashAlgo dsignAlgo)
 where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (TxOut hashAlgo dsignAlgo)
 where
  toCBOR (TxOut addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

instance
  (Typeable hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (WitVKey hashAlgo dsignAlgo)
 where
  toCBOR (WitVKey vk sig) =
    encodeListLen 2
      <> toCBOR vk
      <> toCBOR sig


instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (Tx hashAlgo dsignAlgo)
 where
  toCBOR tx =
    encodeListLen 2
      <> toCBOR (_body tx)
      <> toCBOR (_witnessVKeySet tx)
      <> toCBOR (_witnessMSigMap tx)

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (TxBody hashAlgo dsignAlgo)
 where
  toCBOR txbody =
    encodeListLen 6
      <> toCBOR (_inputs txbody)
      <> toCBOR (_outputs txbody)
      <> toCBOR (_certs txbody)
      <> toCBOR (_wdrls txbody)
      <> toCBOR (_txfee txbody)
      <> toCBOR (_ttl txbody)
      <> toCBOR (_txUpdate txbody)

instance (DSIGNAlgorithm dsignAlgo, HashAlgorithm hashAlgo) =>
  ToCBOR (MultiSig hashAlgo dsignAlgo) where
  toCBOR (RequireSignature hk) =
    encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (RequireAllOf msigs) =
    encodeListLen 2 <> encodeWord 1 <> toCBOR msigs
  toCBOR (RequireAnyOf msigs) =
    encodeListLen 2 <> encodeWord 2 <> toCBOR msigs
  toCBOR (RequireMOf m msigs) =
    encodeListLen 3 <> encodeWord 3 <> toCBOR m <> toCBOR msigs

instance (DSIGNAlgorithm dsignAlgo, HashAlgorithm hashAlgo) =>
  FromCBOR (MultiSig hashAlgo dsignAlgo) where
  fromCBOR = do
    _ <- decodeListLen
    ctor <- decodeWord
    case ctor of
      0 -> do
       hk <- KeyHash <$> fromCBOR
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

instance (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (Credential hashAlgo dsignAlgo) where
  toCBOR = \case
    ScriptHashObj hs ->
      encodeListLen 2
      <> toCBOR (0 :: Word8)
      <> toCBOR hs
    KeyHashObj kh ->
      encodeListLen 2
      <> toCBOR (1 :: Word8)
      <> toCBOR kh

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (Addr hashAlgo dsignAlgo)
 where
  toCBOR = \case
    AddrBase pay stake ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR pay
        <> toCBOR stake
    AddrEnterprise pay ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR pay
    AddrPtr stakePtr ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> toCBOR stakePtr

instance ToCBOR Ptr where
  toCBOR (Ptr sl txIx certIx) =
    encodeListLen 3
      <> toCBOR sl
      <> toCBOR txIx
      <> toCBOR certIx

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  ToCBOR (Delegation hashAlgo dsignAlgo) where
  toCBOR delegation =
    encodeListLen 2
      <> toCBOR (_delegator delegation)
      <> toCBOR (_delegatee delegation)


instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (PoolParams hashAlgo dsignAlgo)
 where
  toCBOR poolParams =
    encodeListLen 8
      <> toCBOR (_poolPubKey poolParams)
      <> toCBOR (_poolPledge poolParams)
      <> toCBOR (_poolPledges poolParams)
      <> toCBOR (_poolCost poolParams)
      <> toCBOR (_poolMargin poolParams)
      <> toCBOR (_poolAltAcnt poolParams)
      <> toCBOR (_poolRAcnt poolParams)
      <> toCBOR (_poolOwners poolParams)

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (RewardAcnt hashAlgo dsignAlgo) where
  toCBOR rwdAcnt =
    encodeListLen 1
      <> toCBOR (getRwdHK rwdAcnt)

-- Lenses

makeLenses ''TxBody

makeLenses ''Tx

makeLenses ''Delegation

makeLenses ''PoolParams
