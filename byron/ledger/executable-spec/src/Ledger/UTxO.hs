{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ledger.UTxO where

import Control.State.Transition
import qualified Crypto.Hash as Crypto
import Data.AbstractSize (HasTypeReps, typeReps, abstractSize)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence ((<|), empty)
import qualified Data.Set as Set
import Data.Typeable (typeOf)
import Numeric.Natural (Natural)
import Ledger.Core hiding ((<|))
import Ledger.GlobalParams (lovelaceCap)
import Ledger.Update (PParams (PParams), _factorA, _factorB)

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving (Show, Eq, Ord)

instance HasTypeReps TxId where
  typeReps x = typeOf x <| typeOf (getTxId x) <| empty

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn id = TxIn id Natural deriving (Show, Eq, Ord)

instance HasTypeReps i => HasTypeReps (TxIn i) where
  typeReps x@(TxIn i' n) = typeOf x <| typeOf i' <| typeOf n <| empty

-- |The output of a UTxO.
data TxOut = TxOut { addr  :: Addr
                   , value :: Lovelace
                   } deriving (Show, Eq, Ord)

-- |The unspent transaction outputs.
newtype UTxO id = UTxO
  { unUTxO :: Map (TxIn id) TxOut
  } deriving (Show, Eq, Relation)

-- | Construct a UTxO from initial TxOuts, using the supplied id generator
fromTxOuts :: Ord id => (TxOut -> id) -> [TxOut] -> UTxO id
fromTxOuts mkId = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))

-- | Construct a UTxO from initial TxOuts, using Owner as TxId
fromTxOutsNatural :: [TxOut] -> UTxO Natural
fromTxOutsNatural = fromTxOuts (unOwner . owner . addr)

-- |A raw transaction
data Tx id = Tx { txid    :: id
                , inputs  :: [TxIn id]
                , outputs :: [TxOut]
                } deriving (Show, Ord)

instance HasTypeReps i => HasTypeReps (Tx i) where
  typeReps x@(Tx i' inputs outputs)
    = typeOf x <| typeOf i' <| typeOf inputs <| typeOf outputs <| empty


instance Eq id => Eq (Tx id) where
  (Tx _ ins outs) == (Tx _ ins' outs') = ins == ins' && outs == outs'

-- |Compute the UTxO inputs of a transaction.
txins :: Tx id -> [TxIn id]
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: Ord id => Tx id -> UTxO id
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

-- TODO: Handle fees properly
txfee :: Tx id -> Lovelace
txfee _ = Lovelace 0

-- |Determine the total balance contained in the UTxO.
balance :: UTxO id -> Lovelace
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

instance Ledger.Core.HasHash (Tx TxId) where
  hash = Crypto.hash

instance Show id => BA.ByteArrayAccess (Tx id) where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

data UTxOEnv id = UTxOEnv { utxo0 :: UTxO id
                          , pps   :: PParams
                          }

pcMinFee :: forall id . HasTypeReps id => PParams -> Tx id -> Lovelace
pcMinFee PParams {_factorA = a, _factorB = b} tx
  = fromIntegral $ a * txsize tx + b

txsize :: forall id . HasTypeReps id => Tx id -> Int
txsize = abstractSize costs
  where costs = Map.fromList [ (typeOf (undefined :: TxIn id), 1)
                             , (typeOf (undefined :: TxOut)  , 1)
                             ]

-- | UTXO transition system
data UTXO id

data UTxOState id = UTxOState { utxo :: UTxO id
                              , reserves :: Lovelace
                              }

instance (Ord id, HasTypeReps id) => STS (UTXO id) where

  type Environment (UTXO id) = UTxOEnv id
  type State (UTXO id) = UTxOState id
  type Signal (UTXO id) = Tx id
  data PredicateFailure (UTXO id)
    = EmptyTxInputs
    | FeeTooLow
    | IncreasedTotalBalance
    | InputsNotInUTxO
    | NonPositiveOutputs
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC UTxOEnv {utxo0} <- judgmentContext
        return $ UTxOState { utxo     = utxo0
                           , reserves = lovelaceCap - balance utxo0
                           }
    ]
  transitionRules =
    [ do
        TRC ( UTxOEnv _ pps
            , UTxOState {utxo, reserves}
            , tx
            ) <- judgmentContext

        txins tx ⊆ dom utxo ?! InputsNotInUTxO

        let fee = balance (txins tx ◁ utxo) - balance (txouts tx)

        pcMinFee pps tx <= fee ?! FeeTooLow

        (not . null) (txins tx) ?! EmptyTxInputs

        let
          outputValues = fmap value $ Set.toList $ range (txouts tx)
        all (0<) outputValues ?! NonPositiveOutputs

        return $ UTxOState { utxo     = (txins tx ⋪ utxo) ∪ txouts tx
                           , reserves = reserves + fee
                           }
    ]

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit id = Wit VKey (Sig (Tx id)) deriving (Show, Eq, Ord)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits id = TxWits
                { body      :: Tx id
                , witnesses :: [Wit id]
                } deriving (Show, Eq)

instance HasTypeReps i => HasTypeReps (TxWits i) where
  typeReps (TxWits b w) = typeOf b <| typeOf w <| empty

instance Show id => BA.ByteArrayAccess [TxWits id] where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show

instance Show id => HasHash [TxWits id] where
  hash = Crypto.hash

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx id -> Wit id
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: Ord id => UTxO id -> Tx id -> TxWits id
makeTxWits (UTxO utxo) tx = TxWits
  { body      = tx
  , witnesses = wits
  }
 where
  getKey txin =
    let
      TxOut (Addr (VKey o)) _ =
        fromMaybe
            (error "makeTxWits: Missing output for transaction input")
          $ Map.lookup txin utxo
    in KeyPair (SKey o) (VKey o)
  keys = getKey <$> inputs tx
  wits = makeWitness <$> keys <*> pure tx

-- | UTXO with witnessing
data UTXOW id

instance (Ord id, HasTypeReps id) => STS (UTXOW id) where

  type Environment (UTXOW id) = UTxOEnv id
  type State (UTXOW id) = UTxOState id
  type Signal (UTXOW id) = TxWits id
  data PredicateFailure (UTXOW id)
    = UtxoFailure (PredicateFailure (UTXO id))
    | InsufficientWitnesses
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @(UTXO id) $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxoSt@UTxOState {utxo}, tw) <- judgmentContext
        witnessed tw utxo ?! InsufficientWitnesses
        utxoSt' <- trans @(UTXO id) $ TRC (env, utxoSt, body tw)
        return utxoSt'
    ]

instance (Ord id, HasTypeReps id) => Embed (UTXO id) (UTXOW id) where
  wrapFailed = UtxoFailure


data UTXOWS id

instance (Ord id, HasTypeReps id) => STS (UTXOWS id) where
  type State (UTXOWS id) = UTxOState id
  type Signal (UTXOWS id) = [TxWits id]
  type Environment (UTXOWS id) = UTxOEnv id
  data PredicateFailure (UTXOWS id)
    = UtxowFailure (PredicateFailure (UTXOW id))
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @(UTXOW id) $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxo, txWits) <- judgmentContext
        case (txWits :: [TxWits id]) of
          []     -> return utxo
          (tx:gamma) -> do
            utxo'  <- trans @(UTXOWS id) $ TRC (env, utxo, gamma)
            utxo'' <- trans @(UTXOW id)  $ TRC (env, utxo', tx)
            return utxo''
    ]

instance (Ord id, HasTypeReps id) => Embed (UTXOW id) (UTXOWS id) where
  wrapFailed = UtxowFailure


-- |Determine if a UTxO input is authorized by a given key.
authTxin :: Ord id => VKey -> TxIn id -> UTxO id -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: Ord id => TxWits id -> UTxO id -> Bool
witnessed (TxWits tx wits) utxo =
  length wits == length ins && all (hasWitness wits) ins
 where
  ins = inputs tx
  hasWitness ws input =
    isJust $ find (isWitness tx input utxo) ws
  isWitness tx' input unspent (Wit key sig) =
    verify key tx' sig && authTxin key input unspent
