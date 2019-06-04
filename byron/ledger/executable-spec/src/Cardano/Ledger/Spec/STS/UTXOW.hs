{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | UTXO transition system with witnessing

module Cardano.Ledger.Spec.STS.UTXOW where

import qualified Data.Map as Map

import Control.State.Transition
  ( Embed
  , Environment
  , IRC(IRC)
  , PredicateFailure
  , STS
  , Signal
  , State
  , TRC(TRC)
  , (?!)
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Control.State.Transition.Generator (HasTrace, initEnvGen, sigGen)
import Data.AbstractSize (HasTypeReps)

import Ledger.Core
  ( Addr(Addr)
  , KeyPair(KeyPair)
  , VKey
  , hash
  , keyPair
  , mkAddr
  , owner
  , sign
  , verify
  )
import qualified Ledger.Update.Generators as UpdateGen
import Ledger.UTxO
  ( Tx
  , TxId(TxId)
  , TxId(TxId)
  , TxIn
  , TxOut(TxOut)
  , TxWits(TxWits)
  , UTxO(UTxO)
  , Wit(Wit)
  , addr
  , body
  , fromTxOuts
  , inputs
  )
import qualified Ledger.UTxO.Generators as UTxOGen

import Cardano.Ledger.Spec.STS.UTXO

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
  length wits == length ins && all (isWitness tx utxo) (zip ins wits)
 where
  ins = inputs tx
  isWitness tx' unspent (input, Wit key sig) =
    verify key tx' sig && authTxin key input unspent


instance (Ord id, HasTypeReps id) => Embed (UTXO id) (UTXOW id) where
  wrapFailed = UtxoFailure

-- | Constant list of addresses intended to be used in the generators.
traceAddrs :: [Addr]
traceAddrs = mkAddr <$> [0 .. 10]

instance HasTrace (UTXOW TxId) where
  initEnvGen
    = UTxOEnv <$> genUTxO <*> UpdateGen.pparamsGen
    where
      genUTxO = do
        txOuts <- UTxOGen.genInitialTxOuts traceAddrs
        -- All the outputs in the initial UTxO need to refer to some
        -- transaction id. Since there are no transactions where these outputs
        -- come from we use the hash of the address as transaction id.
        pure $ fromTxOuts (TxId . hash . addr) txOuts

  sigGen _e st = do
    tx <- UTxOGen.tx traceAddrs (utxo st)
    let wits = witnessForTxIn tx (utxo st) <$> inputs tx
    pure $ TxWits tx wits

witnessForTxIn :: Ord id => Tx id -> UTxO id -> TxIn id -> Wit id
witnessForTxIn tx (UTxO utxo) txin =
  case Map.lookup txin utxo of
    Just (TxOut (Addr pay) _) ->
      witnessForTx (keyPair $ owner pay) tx
    Nothing                   ->
      error "The generators must ensure that we are spending unspent inputs"

witnessForTx :: KeyPair -> Tx id -> Wit id
witnessForTx (KeyPair sk vk) tx = Wit vk (sign sk tx)
