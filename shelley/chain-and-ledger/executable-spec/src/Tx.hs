{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Tx
  ( -- transaction
    Tx(..)
  , TxBody(..)
  , TxOut(..)
  , TxIn(..)
  , TxId(..)
  , txUpdate
  , inputs
  , outputs
  , certs
  , wdrls
  , txfee
  , ttl
  , body
  , witnessVKeySet
  , witnessMSigMap
    -- witness data
  , WitVKey(..)
  , MultiSignatureScript
  , validateScript
  , hashScript
  , txwitsVKey
  , txwitsScripts
  )
where


import           Keys

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, encodeWord, encodeWord8)

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashWithSerialiser)

import           Cardano.Crypto.DSIGN (DSIGNAlgorithm)

import           Data.Word (Word8)

import           Lens.Micro.TH (makeLenses)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)

import           Address
import           Coin (Coin (..))
import           Delegation.Certificates (DCert (..))
import           Delegation.PoolParams (RewardAcnt (..))
import           Slot (Slot (..))
import           Updates (Update)


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

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (TxIn hashAlgo dsignAlgo)
 where
  toCBOR (TxIn txId index) =
    encodeListLen 2
      <> toCBOR txId
      <> toCBOR index

-- |The output of a UTxO.
data TxOut hashAlgo dsignAlgo
  = TxOut (Addr hashAlgo dsignAlgo) Coin
  deriving (Show, Eq, Ord)

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (TxOut hashAlgo dsignAlgo)
 where
  toCBOR (TxOut addr coin) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR coin

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

instance
  (Typeable hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (WitVKey hashAlgo dsignAlgo)
 where
  toCBOR (WitVKey vk sig) =
    encodeListLen 2
      <> toCBOR vk
      <> toCBOR sig

-- |A fully formed transaction.
data Tx hashAlgo dsignAlgo
  = Tx
      { _body           :: !(TxBody hashAlgo dsignAlgo)
      , _witnessVKeySet :: !(Set (WitVKey hashAlgo dsignAlgo))
      , _witnessMSigMap ::
          Map (ScriptHash hashAlgo dsignAlgo) (MultiSig hashAlgo dsignAlgo)
      } deriving (Show, Eq, Ord)

makeLenses ''Tx

-- | Typeclass for multis-signature script data types. Allows for script
-- validation and hashing.
class (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, ToCBOR a) =>
  MultiSignatureScript a hashAlgo dsignAlgo where
  validateScript :: a -> Tx hashAlgo dsignAlgo -> Bool
  hashScript :: a -> ScriptHash hashAlgo dsignAlgo

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

-- | Script evaluator for native multi-signature scheme. 'vhks' is the set of
-- key hashes that signed the transaction to be validated.
evalNativeMultiSigScript
  :: MultiSig hashAlgo dsignAlgo
  -> Set (KeyHash hashAlgo dsignAlgo)
  -> Bool
evalNativeMultiSigScript (SingleSig hk) vhks = Set.member hk vhks
evalNativeMultiSigScript (MultiSig th msigs) vhks =
  th <= sum [if evalNativeMultiSigScript msig vhks then 1 else 0 | msig <- msigs]

-- | Script validator for native multi-signature scheme.
validateNativeMultiSigScript
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => MultiSig hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Bool
validateNativeMultiSigScript msig tx =
  evalNativeMultiSigScript msig vhks
  where witsSet = _witnessVKeySet tx
        vhks    = Set.map (\(WitVKey vk _) -> hashKey vk) witsSet

-- | Hashes native multi-signature script, appending the 'nativeMultiSigTag' in
-- front and then calling the script CBOR function.
hashNativeMultiSigScript
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => MultiSig hashAlgo dsignAlgo
  -> ScriptHash hashAlgo dsignAlgo
hashNativeMultiSigScript msig =
  ScriptHash $ hashWithSerialiser (\x -> encodeWord8 nativeMultiSigTag
                                          <> toCBOR x) msig

-- | Magic number representing the tag of the native multi-signature script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeMultiSigTag :: Word8
nativeMultiSigTag = 0

instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo) =>
  MultiSignatureScript (MultiSig hashAlgo dsignAlgo) hashAlgo dsignAlgo where
  validateScript = validateNativeMultiSigScript
  hashScript = hashNativeMultiSigScript

-- | Witness accessor function for Transactions
txwitsVKey
  :: (DSIGNAlgorithm dsignAlgo)
  => Tx hashAlgo dsignAlgo
  -> Map.Map (VKey dsignAlgo) (Sig dsignAlgo (TxBody hashAlgo dsignAlgo))
txwitsVKey tx =
  Map.fromList $ map (\(WitVKey vk sig) -> (vk, sig)) (Set.toList $ _witnessVKeySet tx)

-- | Multi-signature script witness accessor function for Transactions
txwitsScripts
  :: Tx hashAlgo dsignAlgo
  -> Map.Map (ScriptHash hashAlgo dsignAlgo) (MultiSig hashAlgo dsignAlgo)
txwitsScripts tx = _witnessMSigMap tx
