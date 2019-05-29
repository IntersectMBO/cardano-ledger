{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.UTxO.Model
  ( tests
  )
where

import Cardano.Prelude hiding (trace)
import Test.Cardano.Prelude

import Control.Lens ((^.))
import qualified Data.Map.Strict as M

import Hedgehog (MonadTest, evalEither, forAll, property)

import Cardano.Chain.UTxO (updateUTxOTxWitness)
import qualified Cardano.Chain.UTxO as Concrete
import qualified Cardano.Chain.UTxO.UTxO as Concrete.UTxO
import Cardano.Crypto (hashDecoded)

import qualified Cardano.Ledger.Spec.STS.UTXO as Abstract
import Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import qualified Control.State.Transition as Abstract
import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition.Trace
  (Trace, TraceOrder(OldestFirst), preStatesAndSignals, traceEnv)
import qualified Ledger.UTxO as Abstract

import qualified Test.Cardano.Chain.Elaboration.UTxO as E
import Test.Options (TSGroup, TSProperty, withTestsTS)


tests :: TSGroup
tests = $$discoverPropArg


-- | Every abstract trace of transaction that was generated according to the
--   inference rules, after being elaborated must be validated by the concrete
--   UTxO validator.
ts_prop_generatedChainsAreValidated :: TSProperty
ts_prop_generatedChainsAreValidated =
  withTestsTS 200 $ property $ do
    tr <- forAll (trace @(UTXOW Abstract.TxId) 200)
    classifyTraceLength tr 200 50
    passConcreteValidation tr


passConcreteValidation :: MonadTest m => Trace (UTXOW Abstract.TxId) -> m ()
passConcreteValidation tr = void $ evalEither res
 where
  res = foldM (elaborateAndUpdate abstractEnv) initSt
    $ preStatesAndSignals OldestFirst tr

  abstractEnv = tr ^. traceEnv

  initSt      = elaborateInitialUTxO (Abstract.utxo0 abstractEnv)


-- | Create the initial concrete UTxO by elaborating the outputs and updating
--   the map from abstract TxIds to concrete TxIds
elaborateInitialUTxO
  :: Abstract.UTxO Abstract.TxId
  -> (Map Abstract.TxId Concrete.TxId, Concrete.UTxO)
elaborateInitialUTxO abstractUtxo = foldr
  txOutToUTxO
  (mempty, Concrete.UTxO.empty)
  (M.toList $ Abstract.unUTxO abstractUtxo)
 where
  txOutToUTxO
    :: (Abstract.TxIn Abstract.TxId, Abstract.TxOut)
    -> (Map Abstract.TxId Concrete.TxId, Concrete.UTxO)
    -> (Map Abstract.TxId Concrete.TxId, Concrete.UTxO)
  txOutToUTxO (Abstract.TxIn abstractTxId _, abstractTxOut) (txIdMap, utxo) =
    let
      singletonUtxo =
        Concrete.UTxO.fromTxOut (E.elaborateTxOut abstractTxOut)
      [(Concrete.TxInUtxo concreteTxId _, _)] =
        Concrete.UTxO.toList singletonUtxo
      Right utxo' = utxo `Concrete.UTxO.union` singletonUtxo
    in (M.insert abstractTxId concreteTxId txIdMap, utxo')


-- | Elaborate a single transaction, apply it to the UTxO, and update the TxId
--   map with the new concrete TxId
elaborateAndUpdate
  :: Abstract.UTxOEnv Abstract.TxId
  -> (Map Abstract.TxId Concrete.TxId, Concrete.UTxO)
  -> (Abstract.State (UTXOW Abstract.TxId), Abstract.TxWits Abstract.TxId)
  -> Either
       Concrete.UTxOValidationError
       (Map Abstract.TxId Concrete.TxId, Concrete.UTxO)
elaborateAndUpdate abstractEnv (txIdMap, utxo) (_abstractUtxo, abstractTxWits) =
  (txIdMap', )
    <$> updateUTxOTxWitness
          (E.elaborateUTxOEnv abstractEnv)
          utxo
          concreteTxWitness
 where
  concreteTxWitness = E.elaborateTxWitsBS
    ( const
    $ fromMaybe
        (panic
          "elaborateAndUpdate: Missing abstract TxId during elaboration"
        )
    . flip M.lookup txIdMap
    )
    (Abstract.utxo0 abstractEnv)
    abstractTxWits

  concreteTxId = hashDecoded $ Concrete.aTaTx concreteTxWitness

  txIdMap' =
    M.insert (Abstract.txid $ Abstract.body abstractTxWits) concreteTxId txIdMap
