{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UTXO transition system with witnessing

module Cardano.Ledger.Spec.STS.UTXOW where

import           Data.Data (Data, Typeable)
import qualified Data.Map as Map
import           GHC.Stack (HasCallStack)
import           Hedgehog (Gen, MonadTest)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Internal.Property (CoverPercentage)
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Embed, Environment, IRC (IRC), PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, SignalGenerator, coverFailures,
                     envGen, sigGen, tinkerWithSigGen)

import           Ledger.Core (Addr (Addr), KeyPair (KeyPair), VKey, keyPair, mkAddr, owner, sign,
                     verify)
import qualified Ledger.Update.Generators as UpdateGen
import           Ledger.Util (mkGoblinGens)
import           Ledger.UTxO (Tx, TxIn, TxOut (TxOut), TxWits (TxWits), UTxO (UTxO), Wit (Wit),
                     body, fromTxOuts, inputs, pcMinFee)
import qualified Ledger.UTxO.Generators as UTxOGen

import           Cardano.Ledger.Spec.STS.UTXO

import           Test.Goblin (GoblinData, mkEmptyGoblin)


data UTXOW deriving (Data, Typeable)


instance STS UTXOW where

  type Environment UTXOW = UTxOEnv
  type State UTXOW = UTxOState
  type Signal UTXOW = TxWits

  -- | These `PredicateFailure`s are all throwable.
  data PredicateFailure UTXOW
    = UtxoFailure (PredicateFailure UTXO)
    | InsufficientWitnesses
    deriving (Eq, Show, Data, Typeable)

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @UTXO $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxoSt@UTxOState {utxo}, tw) <- judgmentContext
        witnessed tw utxo ?! InsufficientWitnesses
        utxoSt' <- trans @UTXO $ TRC (env, utxoSt, body tw)
        return utxoSt'
    ]

-- |Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> key == pay
  _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> UTxO -> Bool
witnessed (TxWits tx wits) utxo =
  length wits == length ins && all (isWitness tx utxo) (zip ins wits)
 where
  ins = inputs tx
  isWitness tx' unspent (input, Wit key sig) =
    verify key tx' sig && authTxin key input unspent


instance Embed UTXO UTXOW where
  wrapFailed = UtxoFailure

-- | Constant list of addresses intended to be used in the generators.
traceAddrs :: [Addr]
traceAddrs = mkAddr <$> [0 .. 10]

instance HasTrace UTXOW where
  envGen _
    = UTxOEnv <$> genUTxO <*> UpdateGen.pparamsGen
    where
      genUTxO = do
        txOuts <- UTxOGen.genInitialTxOuts traceAddrs
        -- All the outputs in the initial UTxO need to refer to some
        -- transaction id. Since there are no transactions where these outputs
        -- come from we use the hash of the address as transaction id.
        pure $ fromTxOuts txOuts

  sigGen UTxOEnv { pps } st = do
    tx <- UTxOGen.genTxFromUTxO traceAddrs (pcMinFee pps) (utxo st)
    let wits = witnessForTxIn tx (utxo st) <$> inputs tx
    pure $ TxWits tx wits

witnessForTxIn :: Tx -> UTxO -> TxIn -> Wit
witnessForTxIn tx (UTxO utxo) txin =
  case Map.lookup txin utxo of
    Just (TxOut (Addr pay) _) ->
      witnessForTx (keyPair $ owner pay) tx
    Nothing                   ->
      error "The generators must ensure that we are spending unspent inputs"

witnessForTx :: KeyPair -> Tx -> Wit
witnessForTx (KeyPair sk vk) tx = Wit vk (sign sk tx)


--------------------------------------------------------------------------------
-- GoblinData & goblin-tinkered SignalGenerators
--------------------------------------------------------------------------------

mkGoblinGens
  "UTXOW"
  [ "InsufficientWitnesses"
  , "UtxoFailure_EmptyTxInputs"
  , "UtxoFailure_EmptyTxOutputs"
  , "UtxoFailure_FeeTooLow"
  , "UtxoFailure_InputsNotInUTxO"
  , "UtxoFailure_NonPositiveOutputs"
  ]

tamperedTxWitsList :: UTxOEnv -> UTxOState -> Gen [TxWits]
tamperedTxWitsList env st = do
  gen <- Gen.element (map (\sg -> sg env st) goblinGensUTXOW)
  Gen.list (Range.linear 0 10) gen


--------------------------------------------------------------------------------
-- Hedgehog coverage checking
--------------------------------------------------------------------------------

-- | Check that all the relevant predicate failures are covered.
coverUtxoFailure
  :: forall m a
   .  ( MonadTest m
      , HasCallStack
      , Data a
      )
  => CoverPercentage
  -- ^ Minimum percentage that each failure must occur.
  -> a
  -- ^ Structure containing the failures
  -> m ()
coverUtxoFailure coverPercentage someData = do
  coverFailures
    coverPercentage
    [ InsufficientWitnesses
    ]
    someData

  coverFailures
    coverPercentage
    [ EmptyTxInputs
    , EmptyTxOutputs
    , FeeTooLow
    , IncreasedTotalBalance
    , InputsNotInUTxO
    , NonPositiveOutputs
    ]
    someData

