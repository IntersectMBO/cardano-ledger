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

import           Coin
import           Delegation.PoolParams
import           Keys
import           Slot
import           Updates

-- |An address for UTxO.
data Addr hashAlgo dsignAlgo
  = AddrVKey
      { _payHK   :: KeyHash hashAlgo dsignAlgo
      , _stakeHK :: KeyHash hashAlgo dsignAlgo
      }
  | AddrScr
    { _payScr   :: ScriptHash hashAlgo dsignAlgo
    , _stakeScr :: ScriptHash hashAlgo dsignAlgo
    }
  | AddrPtr
      { _stakePtr :: Ptr
      }
  deriving (Show, Eq, Ord)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr Slot Ix Ix
  deriving (Show, Eq, Ord)

data MultiSig hashAlgo dsignAlgo =
    SingleSig (KeyHash hashAlgo dsignAlgo)
  | MultiSig Int [MultiSig hashAlgo dsignAlgo]
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

data StakeObject hashAlgo dsignAlgo =
    KeyHashStake (KeyHash hashAlgo dsignAlgo)
  | ScriptHashStake (ScriptHash hashAlgo dsignAlgo)
  deriving (Show, Eq, Ord)

-- | A heavyweight certificate.
data DCert hashAlgo dsignAlgo
    -- | A stake key registration certificate.
  = RegKey (StakeObject hashAlgo dsignAlgo)
    -- | A stake key deregistration certificate.
  | DeRegKey (StakeObject hashAlgo dsignAlgo) --TODO this is actually KeyHash on page 13, is that what we want?
    -- | A stake pool registration certificate.
  | RegPool (PoolParams hashAlgo dsignAlgo)
    -- | A stake pool retirement certificate.
  | RetirePool (KeyHash hashAlgo dsignAlgo) Epoch
    -- | A stake delegation certificate.
  | Delegate (Delegation dsignAlgo)
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

makeLenses ''TxBody

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

makeLenses ''Tx

-- newtype StakePools hashAlgo dsignAlgo =
--   StakePools (Map (KeyHash hashAlgo dsignAlgo) Slot)
--   deriving (Show, Eq)

-- newtype StakeKeys hashAlgo dsignAlgo =
--   StakeKeys (Map (StakeObject hashAlgo dsignAlgo) Slot)
--   deriving (Show, Eq)

newtype StakeKeys hashAlgo dsignAlgo =
  StakeKeys (Map (KeyHash hashAlgo dsignAlgo) Slot)
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
  toCBOR (SingleSig hk) = encodeListLen 2 <> encodeWord 0 <> toCBOR hk
  toCBOR (MultiSig th msigs) =
    encodeListLen 3 <> encodeWord 1 <> toCBOR th <> toCBOR msigs

instance (DSIGNAlgorithm dsignAlgo, HashAlgorithm hashAlgo) =>
  FromCBOR (MultiSig hashAlgo dsignAlgo) where
  fromCBOR = do
    _ <- decodeListLen
    ctor <- decodeWord
    if ctor == 0
      then do
       hk <- KeyHash <$> fromCBOR
       pure $ SingleSig hk
      else do
       th <- fromCBOR
       msigs <- fromCBOR
       pure $ MultiSig th msigs


instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (Addr hashAlgo dsignAlgo)
 where
  toCBOR = \case
    AddrVKey payHK stakeHK ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR payHK
        <> toCBOR stakeHK
    AddrScr payScr stakeScr ->
      encodeListLen 3
        <> toCBOR (1 :: Word8)
        <> toCBOR payScr
        <> toCBOR stakeScr
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

instance (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (StakeObject hashAlgo dsignAlgo) where
  toCBOR = \case
     KeyHashStake kh ->
       encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> toCBOR kh
     ScriptHashStake sc ->
       encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR sc
