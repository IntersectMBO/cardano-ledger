{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UTXO transition system with witnessing
module Byron.Spec.Ledger.STS.UTXOW where

import Byron.Spec.Ledger.Core (Addr (Addr), VKey, mkAddr, verify)
import Byron.Spec.Ledger.STS.UTXO
import Byron.Spec.Ledger.UTxO (
  Tx (..),
  TxIn,
  TxOut (TxOut),
  UTxO (UTxO),
  Wit (Wit),
  fromTxOuts,
  inputs,
  pcMinFee,
 )
import qualified Byron.Spec.Ledger.UTxO.Generators as UTxOGen
import qualified Byron.Spec.Ledger.Update.Generators as UpdateGen
import Control.State.Transition (
  Embed,
  Environment,
  IRC (IRC),
  STS (..),
  Signal,
  State,
  TRC (TRC),
  initialRules,
  judgmentContext,
  trans,
  transitionRules,
  wrapFailed,
  (?!),
 )
import Data.Data (Data)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadTest)
import Hedgehog.Internal.Property (CoverPercentage)
import NoThunks.Class (NoThunks (..))
import Test.Control.State.Transition.Generator (
  HasTrace,
  coverFailures,
  envGen,
  sigGen,
 )

data UTXOW deriving (Data)

-- | These `PredicateFailure`s are all throwable.
data UtxowPredicateFailure
  = UtxoFailure (PredicateFailure UTXO)
  | InsufficientWitnesses
  deriving (Eq, Show, Data, Generic, NoThunks)

instance STS UTXOW where
  type Environment UTXOW = UTxOEnv
  type State UTXOW = UTxOState
  type Signal UTXOW = Tx
  type PredicateFailure UTXOW = UtxowPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @UTXO $ IRC env
    ]

  transitionRules =
    [ do
        TRC (env, utxoSt@UTxOState {utxo}, tw) <- judgmentContext
        witnessed tw utxo ?! InsufficientWitnesses
        utxoSt' <- trans @UTXO $ TRC (env, utxoSt, tw)
        return utxoSt'
    ]

-- | Determine if a UTxO input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) = case Map.lookup txin utxo of
  Just (TxOut (Addr pay) _) -> key == pay
  _ -> False

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are sufficient.
--  TODO - should we only check for one witness for each unique input address?
witnessed :: Tx -> UTxO -> Bool
witnessed (Tx tx wits) utxo =
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
  envGen _ =
    UTxOEnv <$> genUTxO <*> UpdateGen.pparamsGen
    where
      genUTxO = do
        txOuts <- UTxOGen.genInitialTxOuts traceAddrs
        -- All the outputs in the initial UTxO need to refer to some
        -- transaction id. Since there are no transactions where these outputs
        -- come from we use the hash of the address as transaction id.
        pure $ fromTxOuts txOuts

  sigGen UTxOEnv {pps} st =
    UTxOGen.genTxFromUTxO traceAddrs (pcMinFee pps) (utxo st)

--------------------------------------------------------------------------------
-- Hedgehog coverage checking
--------------------------------------------------------------------------------

-- | Check that all the relevant predicate failures are covered.
coverUtxoFailure ::
  forall m a.
  ( MonadTest m
  , HasCallStack
  , Data a
  ) =>
  -- | Minimum percentage that each failure must occur.
  CoverPercentage ->
  -- | Structure containing the failures
  a ->
  m ()
coverUtxoFailure coverPercentage someData = do
  coverFailures @_ @UTXOW
    coverPercentage
    [ InsufficientWitnesses
    ]
    someData

  coverFailures @_ @UTXO
    coverPercentage
    [ FeeTooLow
    , InputsNotInUTxO
    ]
    someData

-- We do not check coverage of `EmptyTxInputs` & `EmptyTxOutputs`, because
-- they such transactions are not constructible in `cardano-ledger`'s types,
-- due to usage of `NonEmpty` for the lists of `TxIn` and `TxOut`.
--
-- We do not check coverage of `NonPositiveOutputs` because it is not
-- possible to represent a non-positive Lovelace value in a `TxOut` since
-- there is bounds-checking on all constructions of `Lovelace` values.
--
-- We do not check coverage of `IncreasedTotalBalance` because it is not
-- throwable.
