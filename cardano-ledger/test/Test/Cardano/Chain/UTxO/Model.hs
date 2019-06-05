{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.UTxO.Model
  ( tests
  , elaborateAndUpdate
  , elaborateInitialUTxO
  , elaborateTxWitnesses
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
import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition.Trace
  (Trace, TraceOrder(OldestFirst), traceEnv, traceSignals)
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
    tr <- forAll $ trace @UTXOW 200
    classifyTraceLength tr 200 50
    passConcreteValidation tr


passConcreteValidation :: MonadTest m => Trace UTXOW -> m ()
passConcreteValidation tr = void $ evalEither res
 where
  res = foldM (elaborateAndUpdate abstractEnv) initSt
    $ traceSignals OldestFirst tr

  abstractEnv = tr ^. traceEnv

  initSt      = elaborateInitialUTxO (Abstract.utxo0 abstractEnv)


-- | Create the initial concrete UTxO by elaborating the outputs and updating
--   the map from abstract TxIds to concrete TxIds
elaborateInitialUTxO
  :: Abstract.UTxO
  -> (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
elaborateInitialUTxO abstractUtxo = foldr
  txOutToUTxO
  (Concrete.UTxO.empty, mempty)
  (M.toList $ Abstract.unUTxO abstractUtxo)
 where
  txOutToUTxO
    :: (Abstract.TxIn, Abstract.TxOut)
    -> (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
    -> (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
  txOutToUTxO (Abstract.TxIn abstractTxId _, abstractTxOut) (utxo, txIdMap) =
    let
      singletonUtxo =
        Concrete.UTxO.fromTxOut (E.elaborateTxOut abstractTxOut)
      [(Concrete.TxInUtxo concreteTxId _, _)] =
        Concrete.UTxO.toList singletonUtxo
      Right utxo' = utxo `Concrete.UTxO.union` singletonUtxo
    in (utxo', M.insert abstractTxId concreteTxId txIdMap)


-- | Elaborate a single transaction, apply it to the UTxO, and update the TxId
--   map with the new concrete TxId
elaborateAndUpdate
  :: Abstract.UTxOEnv
  -> (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
  -> Abstract.TxWits
  -> Either
       Concrete.UTxOValidationError
       (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
elaborateAndUpdate abstractEnv (utxo, txIdMap) abstractTxWits =
  (, txIdMap')
    <$> updateUTxOTxWitness
          (E.elaborateUTxOEnv abstractEnv)
          utxo
          concreteTxWitness
 where
  (concreteTxWitness, txIdMap') =
    elaborateTxWitsBSWithMap txIdMap abstractTxWits

elaborateTxWitnesses
  :: Map Abstract.TxId Concrete.TxId
  -> [Abstract.TxWits]
  -> ([Concrete.ATxAux ByteString], Map Abstract.TxId Concrete.TxId)
elaborateTxWitnesses txIdMap = first reverse . foldl' step ([], txIdMap)
  where step (acc, m) = first (: acc) . elaborateTxWitsBSWithMap m

elaborateTxWitsBSWithMap
  :: Map Abstract.TxId Concrete.TxId
  -> Abstract.TxWits
  -> (Concrete.ATxAux ByteString, Map Abstract.TxId Concrete.TxId)
elaborateTxWitsBSWithMap txIdMap abstractTxWits = (concreteTxWitness, txIdMap')
 where
  concreteTxWitness = E.elaborateTxWitsBS
    ( fromMaybe
        (panic
          "elaborateTxWitsBSWithMap: Missing abstract TxId during elaboration"
        )
    . flip M.lookup txIdMap
    )
    abstractTxWits

  concreteTxId = hashDecoded $ Concrete.aTaTx concreteTxWitness

  txIdMap'     = M.insert
    (Abstract.txid $ Abstract.body abstractTxWits)
    concreteTxId
    txIdMap
