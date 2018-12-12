{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies          #-}
module Ledger.UTxO where

import Control.Lens
import Control.State.Transition
import qualified Data.ByteArray as BA
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Ledger.Core
import Ledger.Signatures
import Numeric.Natural (Natural)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as BS

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving (Show, Eq, Ord)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Value deriving (Show, Eq)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq)

-- |A raw transaction
data Tx = Tx { inputs  :: Set TxIn
             , outputs :: [TxOut]
             } deriving (Show, Eq)

-- |Compute the id of a transaction.
txid :: HasHash Tx => Tx -> TxId
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> Set TxIn
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: HasHash Tx => Tx -> UTxO
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

txfee :: Tx -> Value
txfee = undefined

-- |Domain restriction
--
--     * __TODO__ - better symbol?
(◁) :: Set TxIn -> UTxO -> UTxO
ins ◁ (UTxO utxo) = UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
--
(⋪) :: Set TxIn -> UTxO -> UTxO
ins ⋪ (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
(∪) :: UTxO -> UTxO -> UTxO
(UTxO a) ∪ (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Value
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

instance Ledger.Core.HasHash Tx where
  hash = Crypto.hash

instance Ledger.Core.HasHash VKey where
  hash = Crypto.hash

instance BA.ByteArrayAccess VKey where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

data ProtocolConstants = ProtocolConstants
  { pcMinFee :: Tx -> Value }

-- | UTXO transition system
data UTXO

instance STS UTXO where
  type State UTXO = UTxO
  type Signal UTXO = Tx
  type Environment UTXO = ProtocolConstants
  data PredicateFailure UTXO
    = BadInputs
    | FeeTooLow
    | IncreasedTotalBalance
    deriving (Eq, Show)

  initialRules =
    [ return (UTxO Map.empty) ]
  transitionRules = [ utxoInductive ]

utxoInductive :: TransitionRule UTXO
utxoInductive = do
  TRC (pc, utxo, tx) <- judgmentContext
  balance (txouts tx)
    <> txfee tx
    == balance (txins tx ◁ utxo)
    ?! IncreasedTotalBalance
  pcMinFee pc tx <= txfee tx ?! FeeTooLow
  let unspentInputs (UTxO utxo) = Map.keysSet utxo
  txins tx `Set.isSubsetOf` unspentInputs utxo ?! BadInputs
  return $ (txins tx ⋪ utxo) ∪ txouts tx

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig Tx) deriving (Show, Eq)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
              { body     :: Tx
              , witneses :: Set Wit
              } deriving (Show, Eq)

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

-- | UTXO with witnessing
data UTXOW

instance STS UTXOW where
  type State UTXOW = UTxO
  type Signal UTXOW = TxWits
  type Environment UTXOW = ProtocolConstants
  data PredicateFailure UTXOW
    = UtxoFailure (PredicateFailure UTXO)
    | InsufficientWitnesses
    deriving (Eq, Show)

  initialRules = [ return $ UTxO Map.empty ]
  transitionRules =
    [ do
        TRC (pc, utxo, tw) <- judgmentContext
        witnessed tw utxo ?! InsufficientWitnesses
        res <- trans @UTXO $ TRC (pc, utxo, body tw)
        return res
    ]

instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> hash key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> UTxO -> Bool
witnessed (TxWits tx wits) utxo =
  Set.size wits == Set.size ins && all (hasWitness wits) ins
 where
  ins = inputs tx
  hasWitness witnesses input =
    isJust $ find (isWitness tx input utxo) witnesses
  isWitness tx' input unspent (Wit key sig) =
    verify key tx' sig && authTxin key input unspent
