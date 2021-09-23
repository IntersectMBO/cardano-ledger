{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Chain.UTxO.Model
  ( tests,
    elaborateAndUpdate,
    elaborateInitialUTxO,
    elaborateTxWitnesses,
    elaborateTxWitsBSWithMap,
  )
where

import qualified Byron.Spec.Ledger.STS.UTXO as Abstract
import Byron.Spec.Ledger.STS.UTXOW (UTXOW)
import qualified Byron.Spec.Ledger.UTxO as Abstract
import Cardano.Chain.Block (BlockValidationMode (BlockValidation))
import Cardano.Chain.UTxO
  ( TxValidationMode (TxValidation),
    updateUTxOTxWitness,
  )
import qualified Cardano.Chain.UTxO as Concrete
import qualified Cardano.Chain.UTxO.UTxO as Concrete.UTxO
import Cardano.Chain.ValidationMode (ValidationMode (..))
import Cardano.Crypto (hashDecoded, serializeCborHash)
import Cardano.Prelude hiding (trace, traceM, traceShow)
import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    traceEnv,
    traceSignals,
  )
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Hedgehog (MonadTest, evalEither, forAll, property)
import Lens.Micro ((^.))
import qualified Test.Cardano.Chain.Elaboration.UTxO as E
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)

tests :: TSGroup
tests = $$discoverPropArg

-- | Every abstract trace of transaction that was generated according to the
--   inference rules, after being elaborated must be validated by the concrete
--   UTxO validator.
ts_prop_generatedUTxOChainsAreValidated :: TSProperty
ts_prop_generatedUTxOChainsAreValidated =
  withTestsTS 300 $
    property $ do
      tr <- forAll $ trace @UTXOW () 500
      classifyTraceLength tr 200 50
      passConcreteValidation tr

passConcreteValidation :: MonadTest m => Trace UTXOW -> m ()
passConcreteValidation !tr = void $ evalEither res
  where
    res =
      foldM (elaborateAndUpdate abstractEnv) initSt $
        traceSignals OldestFirst tr

    abstractEnv = tr ^. traceEnv

    initSt = elaborateInitialUTxO (Abstract.utxo0 abstractEnv)

-- | Create the initial concrete UTxO by elaborating the outputs and updating
--   the map from abstract TxIds to concrete TxIds
elaborateInitialUTxO ::
  Abstract.UTxO ->
  (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
elaborateInitialUTxO abstractUtxo =
  foldr
    txOutToUTxO
    (Concrete.UTxO.empty, mempty)
    (M.toList $ Abstract.unUTxO abstractUtxo)
  where
    txOutToUTxO ::
      (Abstract.TxIn, Abstract.TxOut) ->
      (Concrete.UTxO, Map Abstract.TxId Concrete.TxId) ->
      (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
    txOutToUTxO (Abstract.TxIn abstractTxId _, abstractTxOut) (utxo, txIdMap) =
      let singletonUtxo =
            Concrete.UTxO.fromTxOut (E.elaborateTxOut abstractTxOut)
          [(Concrete.TxInUtxo concreteTxId _, _)] =
            Concrete.UTxO.toList singletonUtxo
          Right utxo' = utxo `Concrete.UTxO.union` singletonUtxo
       in (utxo', M.insert abstractTxId concreteTxId txIdMap)

-- | Elaborate a single transaction, apply it to the UTxO, and update the TxId
--   map with the new concrete TxId
elaborateAndUpdate ::
  Abstract.UTxOEnv ->
  (Concrete.UTxO, Map Abstract.TxId Concrete.TxId) ->
  Abstract.Tx ->
  Either
    Concrete.UTxOValidationError
    (Concrete.UTxO, Map Abstract.TxId Concrete.TxId)
elaborateAndUpdate abstractEnv (utxo, txIdMap) abstractTxWits =
  (,txIdMap')
    <$> runReaderT
      ( updateUTxOTxWitness
          (E.elaborateUTxOEnv abstractEnv)
          utxo
          concreteTxWitness
      )
      vMode
  where
    (concreteTxWitness, txIdMap') =
      elaborateTxWitsBSWithMap txIdMap abstractTxWits

    vMode =
      ValidationMode
        { blockValidationMode = BlockValidation,
          txValidationMode = TxValidation
        }

elaborateTxWitnesses ::
  Map Abstract.TxId Concrete.TxId ->
  [Abstract.Tx] ->
  ([Concrete.ATxAux ByteString], Map Abstract.TxId Concrete.TxId)
elaborateTxWitnesses txIdMap = first reverse . foldl' step ([], txIdMap)
  where
    step (acc, m) = first (: acc) . elaborateTxWitsBSWithMap m

elaborateTxWitsBSWithMap ::
  Map Abstract.TxId Concrete.TxId ->
  Abstract.Tx ->
  (Concrete.ATxAux ByteString, Map Abstract.TxId Concrete.TxId)
elaborateTxWitsBSWithMap txIdMap abstractTxWits = (concreteTxWitness, txIdMap')
  where
    concreteTxWitness =
      E.elaborateTxBS
        (elaborateTxId txIdMap)
        abstractTxWits

    concreteTxId = hashDecoded $ Concrete.aTaTx concreteTxWitness

    txIdMap' =
      M.insert
        (Abstract.txid $ Abstract.body abstractTxWits)
        concreteTxId
        txIdMap

-- | This function uses the supplied txIdMap to look up the concrete
-- counterpart to an abstract TxId. If the key is not present in the map,
-- though, it hashes the `show` Text of the TxId, which essentially creates
-- a new TxId outside of the UTxO (but which is unlikely to collide with
-- another TxId.
elaborateTxId ::
  Map Abstract.TxId Concrete.TxId ->
  (Abstract.TxId -> Concrete.TxId)
elaborateTxId txIdMap txId =
  case M.lookup txId txIdMap of
    Just concreteTxId -> concreteTxId
    Nothing -> coerce $ serializeCborHash (show txId :: Text)
