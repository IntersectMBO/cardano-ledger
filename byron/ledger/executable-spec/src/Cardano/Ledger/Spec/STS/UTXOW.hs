{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | UTXO transition system with witnessing

module Cardano.Ledger.Spec.STS.UTXOW where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import qualified Crypto.Hash as Crypto
import qualified Data.Map as Map
import Hedgehog (Gen)

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
  , hash
  , keyPair
  , mkAddr
  , owner
  , sign
  )
import Ledger.GlobalParams (lovelaceCap)
import qualified Ledger.Update.Generators as UpdateGen
import Ledger.UTxO
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

instance (Ord id, HasTypeReps id) => Embed (UTXO id) (UTXOW id) where
  wrapFailed = UtxoFailure

-- | Constant list of addresses intended to be used in the generators.
traceAddrs :: [Addr]
traceAddrs = mkAddr <$> [0 .. 10]

genTx :: [Addr] -> UTxO TxId -> Gen (Tx TxId)
genTx addrs utxo = do
  -- Use a dummy hash for now, as we will be replacing the transaction id
  -- after the call to UTxOGen.genTxFromUTxO.
  let dummyTxId = TxId $ Crypto.hash $ IOs ([], [])
  -- Generate a valid transaction from a given 'UTxO'
  tx@Tx {inputs, outputs} <- UTxOGen.genTxFromUTxO dummyTxId addrs utxo
  let
    txHash = Crypto.hash $ IOs (inputs, outputs)
  pure $ tx { txid = TxId txHash }

instance HasTrace (UTXOW TxId) where
  initEnvGen
    = UTxOEnv <$> genUTxO <*> UpdateGen.pps
    where
      genUTxO = do
        txOuts <- UTxOGen.genInitialTxOuts traceAddrs
        -- All the outputs in the initial UTxO need to refer to some
        -- transaction id. Since there are no transactions where these outputs
        -- come from we use the hash of the address as transaction id.
        pure $ fromTxOuts (TxId . hash . addr) txOuts

  sigGen _e st = do
    tx <- genTx traceAddrs (utxo st)
    let wits = witnessForTxIn tx (utxo st) <$> (inputs tx)
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

newtype IOs = IOs ([TxIn TxId], [TxOut])
  deriving (Show)

instance BA.ByteArrayAccess (IOs) where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show
