{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

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
  , Tx(..)
  -- * Functions
  , txid
  , txins
  , txinLookup
  , txouts
  , balance
  , deposits
  , (<|)
  , (</|)
  , dom
  , union
  , makeWitness
  , makeWitnesses
  , Wit(..)
  , TxWits(..)
  -- lenses
    -- Tx
  , inputs
  , outputs
  , certs
  , wdrls
  , txfee
  , ttl
    -- TxWits
  , body
  , witnessSet
  , verifyWit
  ) where

import           Crypto.Hash             (Digest, SHA256, hash)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Char8   as BS
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Numeric.Natural         (Natural)

import           Lens.Micro ((^.))
import           Lens.Micro.TH (makeLenses)

import           Coin                    (Coin (..))
import           Keys
import           PParams                 (PParams(..))
import           Slot (Slot(..))

import           Delegation.Certificates (StakePools(..), DCert (..), dvalue)
import           Delegation.PoolParams (poolPubKey, RewardAcnt(..))

-- |A hash
type Hash = Digest SHA256

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { _TxId :: Hash }
  deriving (Show, Eq, Ord)

type Ix  = Natural

-- | Pointer to a slot, transaction index and index in certificate list.
data Ptr = Ptr Slot Ix Ix
         deriving (Show, Eq, Ord)

-- |An address for UTxO.
data Addr = AddrTxin { _payHK   :: HashKey, _stakeHK :: HashKey }
          | AddrPtr { _stakePtr :: Ptr }
          deriving (Show, Eq, Ord)

mkRwdAcnt :: KeyPair -> RewardAcnt
mkRwdAcnt keys = RewardAcnt $ hashKey $ vKey keys

-- |The input of a UTxO.
data TxIn = TxIn TxId Natural deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

type Wdrl = Map RewardAcnt Coin

-- |A raw transaction
data Tx = Tx { _inputs  :: !(Set TxIn)
             , _outputs :: [TxOut]
             , _certs   :: ![DCert]
             , _wdrls   :: Wdrl
             , _txfee   :: Coin
             , _ttl     :: Slot
             } deriving (Show, Eq, Ord)

makeLenses ''Tx

-- |Compute the id of a transaction.
txid :: Tx -> TxId
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> Set TxIn
txins = flip (^.) inputs

-- |Compute the transaction outputs of a transaction.
txouts :: Tx -> UTxO
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (tx ^. outputs) [0..]]
  where
    transId = txid tx

-- |Lookup a txin for a given UTxO collection
txinLookup :: TxIn -> UTxO -> Maybe TxOut
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey !(Sig Tx) deriving (Show, Eq, Ord)

-- |Verify a transaction body witness
verifyWit :: Tx -> Wit -> Bool
verifyWit tx (Wit vkey sig) = verify vkey tx sig

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
              { _body       :: !Tx
              , _witnessSet :: !(Set Wit)
              } deriving (Show, Eq, Ord)

makeLenses ''TxWits

-- |Create a witness for transaction
makeWitness :: Tx -> KeyPair -> Wit
makeWitness tx keys = Wit (vKey keys) (sign (sKey keys) tx)

-- |Create witnesses for transaction
makeWitnesses :: Tx -> [KeyPair] -> Set Wit
makeWitnesses tx = Set.fromList . fmap (makeWitness tx)

-- |Domain restriction
(<|) :: Set TxIn -> UTxO -> UTxO
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
(</|) :: Set TxIn -> UTxO -> UTxO
ins </| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- | Domain of UTxO
dom :: UTxO -> Set TxIn
dom (UTxO utxo) = Map.keysSet utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
union :: UTxO -> UTxO -> UTxO
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where addCoins (TxOut _ a) b = a + b

-- |Determine the total deposit amount needed
deposits :: PParams -> StakePools -> [DCert] -> Coin
deposits pc (StakePools stpools) cs = foldl f (Coin 0) cs'
  where
    f coin cert = coin + dvalue cert pc
    notRegisteredPool (RegPool pool) = Map.notMember (hashKey $ pool ^. poolPubKey) stpools
    notRegisteredPool _ = True
    cs' = filter notRegisteredPool cs

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show
