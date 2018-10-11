{-|
Module      : UTxO
Description : Simple UTxO Ledger

This module defines the types and functions for a simple UTxO Ledger
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

{-# LANGUAGE FlexibleInstances #-}

module UTxO
  (
  -- * Primitives
    Script(..)
  , TxId(..)
  , Coin(..)
  , Addr(..)
  -- * Derived Types
  , TxIn(..)
  , OutRef(..)
  , TxOut(..)
  , UTxO(..)
  , Tx(..)
  -- * Functions
  , txid
  , txins
  , txouts
  , balance
  , (◃)
  , (⋪)
  , verify
  , (∪)
  -- * Signing and Verifying
  , Owner(..)
  , SKey(..)
  , VKey(..)
  , KeyPair(..)
  , keyPair
  , Sig(..)
  , Ledger
  -- * Ledger State
  , LedgerState(..)
  , asStateTransition
  -- * Genesis State
  , genesisId
  , genesisState
  -- * Validation
  , ValidationError (..)
  ) where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Numeric.Natural       (Natural)

-- |An abstract script
data Script r = Validator (LedgerState r -> r -> Bool) String
              | Redeemer (LedgerState r -> r) String

instance Show (Script r) where
  show (Validator _ s) = s
  show (Redeemer _ s) = s

instance Eq (Script r) where
  (Validator _ s1) == (Validator _ s2) = s1 == s2
  (Redeemer _ s1) == (Redeemer _ s2) = s1 == s2
  _ == _ = False

instance Ord (Script r) where
  compare (Validator _ s1) (Validator _ s2) = compare s1 s2
  compare (Redeemer _ s1) (Redeemer _ s2) = compare s1 s2
  compare (Validator _ _) (Redeemer _ _) = LT
  compare (Redeemer _ _) (Validator _ _) = GT

-- |A hash
type Hash = Digest SHA256

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving (Show, Eq, Ord)

-- |The amount of value held by a transaction output.
newtype Coin = Coin Natural deriving (Show, Eq, Ord)

instance Semigroup Coin where
  (Coin a) <> (Coin b) = Coin (a + b)

instance Monoid Coin where
  mempty = Coin 0
  mappend = (<>)

-- |The address of a transaction output, used to identify the owner.
newtype Addr = Addr Hash deriving (Show, Eq, Ord)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn r = TxIn TxId Natural (Script r) (Script r) deriving (Show, Eq, Ord)

data OutRef = OutRef TxId Natural deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map OutRef TxOut) deriving (Show, Eq, Ord)

-- |A transaction
data Tx r = Tx { inputs  :: Set (TxIn r)
               , outputs :: [TxOut]
               } deriving (Show, Eq, Ord)

-- |Compute the id of a transaction.
txid :: Tx r -> TxId
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: Tx r -> Set (TxIn r)
txins = inputs

-- |Compute the transaction outputs of a transaction.
txouts :: Tx r -> UTxO
txouts tx = UTxO $
  Map.fromList [(OutRef transId idx, out) | (out, idx) <- zip (outputs tx) [0..]]
  where
    transId = txid tx

-- |Compute the output references of a transaction.
outRefs :: Tx r -> Set OutRef
outRefs tx = Set.fromList [OutRef transId idx | TxIn transId idx _ _ <- Set.toList (txins tx)]

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

-- |Key Pair.
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord)

-- |Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair owner = KeyPair (SKey owner) (VKey owner)

-- |A digital signature.
data Sig a = Sig a Owner deriving (Show, Eq, Ord)

-- |A ledger
type Ledger r = [Tx r]

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

-- |Domain restriction
(◃) :: Set OutRef -> UTxO -> UTxO
refs ◃ (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` refs) utxo

-- |Domain exclusion
(⋪) :: Set OutRef -> UTxO -> UTxO
refs ⋪ (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` refs) utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
(∪) :: UTxO -> UTxO -> UTxO
(UTxO a) ∪ (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

instance BA.ByteArrayAccess (Script r) where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess (Tx r) where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

-- |Validation errors represent the failures of a transaction to be valid
-- for a given ledger state.
data ValidationError =
                     -- | The transaction inputs are not valid.
                       BadInputs
                     -- | The transaction results in an increased total balance of the ledger.
                     | IncreasedTotalBalance
                     -- | The transaction is not authorized
                     | Unauthorized
                     -- | A transaction input does not match address
                     | WrongAddress
                     deriving (Show, Eq)

-- |The validity of a transaction, where an invalid transaction
-- is represented by list of errors.
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b                 = b
  a <> Valid                 = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)

-- |The state associated with a 'Ledger'.
newtype LedgerState r =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo        :: UTxO
  } deriving (Show, Eq)

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [])

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: [TxOut] -> LedgerState r
genesisState outs = LedgerState
  (UTxO (Map.fromList
    [(OutRef genesisId idx, out) | (idx, out) <- zip [0..] outs]
  ))

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs :: Tx r -> LedgerState r -> Validity
validInputs tx l =
  if outRefs tx `Set.isSubsetOf` unspentInputs (getUtxo l)
    then Valid
    else Invalid [BadInputs]
  where unspentInputs (UTxO utxo) = Map.keysSet utxo

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance :: Tx r -> LedgerState r -> Validity
preserveBalance tx l =
  if balance (txouts tx) <= balance (outRefs tx ◃ getUtxo l)
    then Valid
    else Invalid [IncreasedTotalBalance]

-- |Given a ledger state, determine if the inputs have the correct addresses
correctAddresses :: Tx r -> LedgerState r -> Validity
correctAddresses tx ls =
  if all (correctAddress (getUtxo ls)) (txins tx)
    then Valid
    else Invalid [WrongAddress]
    where
      correctAddress (UTxO utxo) (TxIn transId idx validator _) =
        case Map.lookup (OutRef transId idx) utxo of
          Just (TxOut (Addr scriptHash) _) -> hash validator == scriptHash
          _                                -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
authorized :: Tx r -> LedgerState r -> Validity
authorized tx ls =
  if all (validateInput ls) (txins tx)
    then Valid
    else Invalid [Unauthorized]
    where
      validateInput state (TxIn _ _ (Validator v _) (Redeemer r _)) = v state (r state)
      validateInput _ _ = False

valid :: Tx r -> LedgerState r -> Validity
valid tx ls =
  validInputs tx ls
    <> preserveBalance tx ls
    <> correctAddresses tx ls
    <> authorized tx ls

-- |Apply a transaction as a state transition function on the ledger state.
applyTx :: LedgerState r -> Tx r -> LedgerState r
applyTx ls tx = LedgerState $ outRefs tx ⋪ getUtxo ls ∪ txouts tx

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition :: LedgerState r -> Tx r -> Either [ValidationError] (LedgerState r)
asStateTransition ls tx =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTx ls tx
