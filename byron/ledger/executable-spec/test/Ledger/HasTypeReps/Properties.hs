{-# LANGUAGE TypeApplications #-}

module Ledger.HasTypeReps.Properties
    (testTxHasTypeReps)
  where

import           Data.AbstractSize
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Sequence                      (empty, (<|), (><))
import qualified Data.Sequence                      as Seq
import           Data.Typeable                      (TypeRep, Typeable, typeOf)
import           Numeric.Natural                    (Natural)

import           Hedgehog                           (MonadTest, Property,
                                                     forAll, property,
                                                     withTests, (===))
import           Test.Tasty.Hedgehog

import           Control.State.Transition.Generator (trace)
import           Control.State.Transition.Trace     (TraceOrder (OldestFirst),
                                                     traceSignals)

import           Ledger.Core                        hiding ((<|))
import           Ledger.UTxO

import           Cardano.Ledger.Spec.STS.UTXOW      (UTXOW)

import           Test.Tasty                         (TestTree, testGroup)
import           Test.Tasty.HUnit                   (Assertion, testCase, (@?=))

--------------------------------------------------------------------------------
-- Example HasTypeReps.typeReps for TxIn, Tx
--------------------------------------------------------------------------------

aTx :: Tx
aTx = undefined

aTxId :: TxId
aTxId = TxId (hash aTx)

-- | 'TxIn' has a generic instance for 'HasTypeReps', which recursively adds
--   'typeReps' for all types within 'TxIn'.
exampleTypeRepsTxIn :: Assertion
exampleTypeRepsTxIn =
  let txIn = TxIn aTxId 0
  in (typeReps txIn) @?= typeOf (undefined::TxIn)
                         <| typeOf (undefined::TxId)
                         <| typeOf (undefined::Hash)
                         <| typeOf (undefined::Int)
                         <| typeOf (undefined::Natural)
                         <| empty

-- | A 'TxWits' term may contain multiple inputs/outputs/witnesses.
--   In this example, we have 2 inputs and show how the 'typeReps' for
--   'TxIn' is repeated twice.
exampleTypeRepsTx :: Assertion
exampleTypeRepsTx =
  let
    (in0,in1) = (TxIn aTxId 0, TxIn aTxId 1)
    outs = []
    wits = []
    tx = TxWits (Tx [in0, in1] outs) wits
  in (typeReps tx) @?= typeOf (undefined::TxWits)
                       <| typeOf (undefined::Tx)
                       <| typeOf (undefined::[TxIn])
                       <| typeReps in0
                       >< typeReps in1
                       >< (Seq.fromList [ typeOf (undefined::[TxOut])
                                        , typeOf (undefined::[Wit]) ])

--------------------------------------------------------------------------------
-- Properties of abstractSize of TxWits / TxIn /TxOut / Wit
--------------------------------------------------------------------------------

-- | Make a singleton cost of "1" for the given term's type
mkCost :: Typeable a => a -> Map TypeRep Size
mkCost term = Map.singleton (typeOf term) 1

txInCost :: Map TypeRep Size
txInCost = mkCost (undefined :: TxIn)

txIdCost :: Map TypeRep Size
txIdCost = mkCost (undefined :: TxId)

hashCost :: Map TypeRep Size
hashCost = mkCost (undefined :: Hash)

txOutCost :: Map TypeRep Size
txOutCost = mkCost (undefined :: TxOut)

addrCost :: Map TypeRep Size
addrCost = mkCost (undefined :: Addr)

lovelaceCost :: Map TypeRep Size
lovelaceCost = mkCost (undefined :: Lovelace)

witCost :: Map TypeRep Size
witCost = mkCost (undefined :: Wit)

sigCost :: Map TypeRep Size
sigCost = mkCost (undefined :: Sig Tx)

vKeyCost :: Map TypeRep Size
vKeyCost = mkCost (undefined :: VKey)

-- | Tests that the size of a 'TxWits' term, computed with the combined costs
--   of 'TxIn/TxOut/Wit', is the sum of costs of all 'TxIn/TxOut/Wit' contained
--   in the 'TxWits'.
propSumOfSizesTxWits
  :: MonadTest m => TxWits -> m ()
propSumOfSizesTxWits txw
  = (abstractSize (costsTxIn <> costsTxOut <> costsWit) txw)
         === abstractSize costsTxIn (body txw)
             + abstractSize costsTxOut (body txw)
             + abstractSize costsWit (witnesses txw)
  where
    costsTxIn = Map.unions [txInCost, txIdCost, hashCost]
    costsTxOut = Map.unions [ txOutCost, addrCost, vKeyCost, lovelaceCost ]
    costsWit = Map.unions [ witCost, vKeyCost, sigCost ]

-- | A TxWits contains multiple inputs, outputs and witnesses.
--   This property tests that
--   - the abstractSize of TxWits varies with the number of items
--   - the combined cost is the sum of individual costs
--   - types that are shared (e.g. VKey appears in both TxOut and Wit)
--     should be counted for each appearance
propMultipleOfSizes
  :: MonadTest m => TxWits -> m ()
propMultipleOfSizes txw =
  let
    body_ = (body txw)
    wits_ = witnesses txw
  in
    -- we should account for each TxIn/TxId/Hash in a TxWits's size
    abstractSize txInCost txw === length (inputs body_)
    >> abstractSize txIdCost txw === length (inputs body_)
    >> abstractSize hashCost txw === length (inputs body_)
    -- the combined cost is the sum of individual costs
    >> abstractSize (Map.unions [txInCost, txIdCost, hashCost]) txw
       === abstractSize txInCost txw
           + abstractSize txIdCost txw
           + abstractSize hashCost txw

    -- we should account for each TxOut/Addr/Lovelace in a TxWits's size
    >> abstractSize txOutCost txw === length (outputs body_)
    >> abstractSize addrCost txw === length (outputs body_)
    >> abstractSize lovelaceCost txw === length (outputs body_)
    -- the combined cost is the sum of individual costs
    >> abstractSize (Map.unions [ txOutCost, addrCost, lovelaceCost ]) txw
       === abstractSize txOutCost txw
           + abstractSize addrCost txw
           + abstractSize lovelaceCost txw

    -- we should account for each Wit/Sig in a TxWits's size
    >> abstractSize witCost txw === length wits_
    >> abstractSize sigCost txw === length wits_
    -- the combined cost is the sum of individual costs
    >> abstractSize (Map.unions [ witCost, sigCost ]) txw
       === abstractSize witCost txw
           + abstractSize sigCost txw

    -- since Vkey appears in each input _and_ each witness, the size of
    -- TxWits should be the total number of inputs and wits
    >> abstractSize vKeyCost txw
       === (length $ outputs body_) + (length $ wits_)

propTxAbstractSize :: Property
propTxAbstractSize
  = withTests 50 $ property $ do
    tr <- forAll (trace @UTXOW 100)
    let txs = traceSignals OldestFirst tr :: [TxWits]
    mapM_ propSize txs
  where
    propSize txw = propSumOfSizesTxWits txw >> propMultipleOfSizes txw

testTxHasTypeReps :: TestTree
testTxHasTypeReps = testGroup "Test HasTypeReps instances"
  [ testCase "exampleTypeRepsTxIn - HasTypeReps" exampleTypeRepsTxIn
  , testCase "exampleTypeRepsTx - HasTypeReps" exampleTypeRepsTx

  , testProperty "AbstractSize and HasTypeReps" propTxAbstractSize
  ]
