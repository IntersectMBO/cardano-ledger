{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : UTxO
Description : Simple UTxO Ledger

This module defines the types and functions for a simple UTxO Ledger
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

module UTxO
  (
  -- * Primitives
    TxId(..)
  , Addr(..)
  , Ptr(..)
  , Wdrl
  , Ix
  , mkRwdAcnt
  -- * Derived Types
  , TxIn(..)
  , TxOut(..)
  , UTxO(..)
  , TxBody(..)
  -- * Functions
  , txid
  , txins
  , txinLookup
  , txouts
  , txUpdate
  , balance
  , deposits
  , (<|)
  , (</|)
  , dom
  , union
  , makeWitnessVKey
  , makeWitnessesVKey
  , WitVKey(..)
  , txwitsVKey
  , Tx(..)
  -- lenses
    -- TxBody
  , inputs
  , outputs
  , certs
  , wdrls
  , txfee
  , ttl
  -- Tx
  , body
  , witnessSet
  , verifyWitVKey
  , txup
  ) where

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Typeable           (Typeable)
import           Data.Word               (Word8)
import           Numeric.Natural         (Natural)

import           Lens.Micro ((^.))
import           Lens.Micro.TH (makeLenses)

import           Cardano.Binary          (ToCBOR(toCBOR), encodeListLen)

import           Coin                    (Coin (..))
import           Keys
import           PParams                 (PParams(..))
import           Slot (Slot(..))
import           Updates                 (Update)

import           Delegation.Certificates (StakePools(..), DCert (..), dvalue)
import           Delegation.PoolParams (poolPubKey, RewardAcnt(..))

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId hashAlgo dsignAlgo
  = TxId { _TxId :: Hash hashAlgo (TxBody hashAlgo dsignAlgo) }
  deriving (Show, Eq, Ord, ToCBOR)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr
  = Ptr Slot Ix Ix
  deriving (Show, Eq, Ord)

instance ToCBOR Ptr where
  toCBOR (Ptr slot txIx certIx) =
    encodeListLen 3
      <> toCBOR slot
      <> toCBOR txIx
      <> toCBOR certIx

-- |An address for UTxO.
data Addr hashAlgo dsignAlgo
  = AddrTxin
      { _payHK :: KeyHash hashAlgo dsignAlgo
      , _stakeHK :: KeyHash hashAlgo dsignAlgo
      }
  | AddrPtr
      { _stakePtr :: Ptr
      }
  deriving (Show, Eq, Ord)

instance
  (Typeable dsignAlgo, HashAlgorithm hashAlgo)
  => ToCBOR (Addr hashAlgo dsignAlgo)
 where
  toCBOR = \case
    AddrTxin payHK stakeHK ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR payHK
        <> toCBOR stakeHK
    AddrPtr stakePtr ->
      encodeListLen 2
        <> toCBOR (1 :: Word8)
        <> toCBOR stakePtr

mkRwdAcnt
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     )
  => KeyPair dsignAlgo
  -> RewardAcnt hashAlgo dsignAlgo
mkRwdAcnt keys = RewardAcnt $ hashKey $ vKey keys

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

-- |The unspent transaction outputs.
newtype UTxO hashAlgo dsignAlgo
  = UTxO (Map (TxIn hashAlgo dsignAlgo) (TxOut hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord)

type Wdrl hashAlgo dsignAlgo = Map (RewardAcnt hashAlgo dsignAlgo) Coin

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

-- |Compute the id of a transaction.
txid
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxBody hashAlgo dsignAlgo
  -> TxId hashAlgo dsignAlgo
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: TxBody hashAlgo dsignAlgo -> Set (TxIn hashAlgo dsignAlgo)
txins = flip (^.) inputs

-- |Compute the transaction outputs of a transaction.
txouts
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxBody hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (tx ^. outputs) [0..]]
  where
    transId = txid tx

-- |Lookup a txin for a given UTxO collection
txinLookup
  :: TxIn hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
  -> Maybe (TxOut hashAlgo dsignAlgo)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

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

-- |Verify a transaction body witness
verifyWitVKey
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
  -> Bool
verifyWitVKey tx (WitVKey vkey sig) = verify vkey tx sig

-- |A fully formed transaction.
data Tx hashAlgo dsignAlgo
  = Tx
      { _body       :: !(TxBody hashAlgo dsignAlgo)
      , _witnessSet :: !(Set (WitVKey hashAlgo dsignAlgo))
      } deriving (Show, Eq, Ord)

makeLenses ''Tx

instance
  (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => ToCBOR (Tx hashAlgo dsignAlgo)
 where
  toCBOR tx =
    encodeListLen 2
      <> toCBOR (_body tx)
      <> toCBOR (_witnessSet tx)

-- |Create a witness for transaction
makeWitnessVKey
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> KeyPair dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
makeWitnessVKey tx keys = WitVKey (vKey keys) (sign (sKey keys) tx)

-- |Create witnesses for transaction
makeWitnessesVKey
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> [KeyPair dsignAlgo]
  -> Set (WitVKey hashAlgo dsignAlgo)
makeWitnessesVKey tx = Set.fromList . fmap (makeWitnessVKey tx)

-- | Witness accessor function for Transactions
txwitsVKey
  :: (DSIGNAlgorithm dsignAlgo)
  => Tx hashAlgo dsignAlgo
  -> Map.Map (VKey dsignAlgo) (Sig dsignAlgo (TxBody hashAlgo dsignAlgo))
txwitsVKey tx =
  Map.fromList $ map (\(WitVKey vk sig) -> (vk, sig)) (Set.toList $ _witnessSet tx)

-- |Domain restriction
(<|)
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
(</|)
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
ins </| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- | Domain of UTxO
dom :: UTxO hashAlgo dsignAlgo -> Set (TxIn hashAlgo dsignAlgo)
dom (UTxO utxo) = Map.keysSet utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
union
  :: UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO hashAlgo dsignAlgo -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where addCoins (TxOut _ a) b = a + b

-- |Determine the total deposit amount needed
deposits
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => PParams
  -> StakePools hashAlgo dsignAlgo
  -> [DCert hashAlgo dsignAlgo]
  -> Coin
deposits pc (StakePools stpools) cs = foldl f (Coin 0) cs'
  where
    f coin cert = coin + dvalue cert pc
    notRegisteredPool (RegPool pool) = Map.notMember (hashKey $ pool ^. poolPubKey) stpools
    notRegisteredPool _ = True
    cs' = filter notRegisteredPool cs

txup :: Tx hashAlgo dsignAlgo -> Update dsignAlgo
txup (Tx txbody _ ) = _txUpdate txbody
