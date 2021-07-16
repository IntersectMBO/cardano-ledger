{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Byron.Spec.Ledger.AbstractSize.Properties (testTxHasTypeReps) where

import Byron.Spec.Ledger.Core hiding ((<|))
import Byron.Spec.Ledger.STS.UTXOW (UTXOW)
import Byron.Spec.Ledger.UTxO
import Control.State.Transition.Generator (trace)
import Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import Data.AbstractSize
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (empty, (<|), (><))
import qualified Data.Sequence as Seq
import Data.Typeable (TypeRep, Typeable, typeOf)
import Hedgehog (MonadTest, Property, forAll, property, withTests, (===))
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Example HasTypeReps.typeReps for TxIn, Tx
--------------------------------------------------------------------------------

aTx :: TxBody
aTx = undefined

aTxId :: TxId
aTxId = TxId (hash aTx)

-- | 'TxIn' has a generic instance for 'HasTypeReps', which recursively adds
--   'typeReps' for all types within 'TxIn'.
exampleTypeRepsTxIn :: Assertion
exampleTypeRepsTxIn =
  let txIn = TxIn aTxId 0
   in typeReps txIn @?= typeOf (undefined :: TxIn)
        <| typeOf (undefined :: TxId)
        <| typeOf (undefined :: Hash)
        <| typeOf (undefined :: Maybe Int)
        <| typeOf (undefined :: Int)
        <| typeOf (undefined :: Natural)
        <| empty

-- | A 'TxWits' term may contain multiple inputs/outputs/witnesses.
--   In this example, we have 2 inputs and show how the 'typeReps' for
--   'TxIn' is repeated twice.
exampleTypeRepsTx :: Assertion
exampleTypeRepsTx =
  let (in0, in1) = (TxIn aTxId 0, TxIn aTxId 1)
      outs = []
      wits = []
      tx = Tx (TxBody [in0, in1] outs) wits
   in typeReps tx @?= typeOf (undefined :: Tx)
        <| typeOf (undefined :: TxBody)
        <| typeOf (undefined :: [TxIn])
        <| typeReps in0
        >< typeReps in1
        >< ( Seq.fromList
               [ typeOf (undefined :: [TxOut]),
                 typeOf (undefined :: [Wit])
               ]
           )

--------------------------------------------------------------------------------
-- Properties of abstractSize of TxWits / TxIn /TxOut / Wit
--------------------------------------------------------------------------------

-- | Make a singleton cost of "1" for the given term's type
mkCost :: forall a. Typeable a => Map TypeRep Size
mkCost = Map.singleton (typeOf (undefined :: a)) 1

-- | Tests that the size of a 'TxWits' term, computed with the combined costs
--   of 'TxIn/TxOut/Wit', is the sum of costs of all 'TxIn/TxOut/Wit' contained
--   in the 'TxWits'.
propSumOfSizesTx ::
  MonadTest m => Tx -> m ()
propSumOfSizesTx txw =
  abstractSize (txInCosts <> txOutCosts <> witCosts) txw
    === abstractSize txInCosts (body txw)
      + abstractSize txOutCosts (body txw)
      + abstractSize witCosts (witnesses txw)
  where
    txInCosts :: Map TypeRep Size
    txInCosts = Map.unions [mkCost @TxIn, mkCost @TxId, mkCost @Hash]

    txOutCosts :: Map TypeRep Size
    txOutCosts = Map.unions [mkCost @TxOut, mkCost @Addr, mkCost @VKey, mkCost @Lovelace]

    witCosts :: Map TypeRep Size
    witCosts = Map.unions [mkCost @Wit, mkCost @VKey, mkCost @(Sig TxBody)]

-- | A TxWits contains multiple inputs, outputs and witnesses.
--   This property tests that
--   - the abstractSize of TxWits varies with the number of items
--   - the combined cost is the sum of individual costs
--   - types that are shared (e.g. VKey appears in both TxOut and Wit)
--     should be counted for each appearance
propMultipleOfSizes ::
  MonadTest m => Tx -> m ()
propMultipleOfSizes txw =
  let body_ = body txw
      wits_ = witnesses txw
   in do
        -- we should account for each TxIn/TxId/Hash in a TxWits's size
        abstractSize (mkCost @TxIn) txw === length (inputs body_)
        abstractSize (mkCost @TxId) txw === length (inputs body_)
        abstractSize (mkCost @Hash) txw === length (inputs body_)
        -- the combined cost is the sum of individual costs
        abstractSize (Map.unions [mkCost @TxIn, mkCost @TxId, mkCost @Hash]) txw
          === abstractSize (mkCost @TxIn) txw
            + abstractSize (mkCost @TxId) txw
            + abstractSize (mkCost @Hash) txw

        -- we should account for each TxOut/Addr/Lovelace in a TxWits's size
        abstractSize (mkCost @TxOut) txw === length (outputs body_)
        abstractSize (mkCost @Addr) txw === length (outputs body_)
        abstractSize (mkCost @Lovelace) txw === length (outputs body_)
        -- the combined cost is the sum of individual costs
        abstractSize (Map.unions [mkCost @TxOut, mkCost @Addr, mkCost @Lovelace]) txw
          === abstractSize (mkCost @TxOut) txw
            + abstractSize (mkCost @Addr) txw
            + abstractSize (mkCost @Lovelace) txw

        -- we should account for each Wit/Sig in a TxWits's size
        abstractSize (mkCost @Wit) txw === length wits_
        abstractSize (mkCost @(Sig TxBody)) txw === length wits_
        -- the combined cost is the sum of individual costs
        abstractSize (Map.unions [mkCost @Wit, mkCost @(Sig TxBody)]) txw
          === abstractSize (mkCost @Wit) txw
            + abstractSize (mkCost @(Sig TxBody)) txw

        -- since Vkey appears in each input _and_ each witness, the size of
        -- TxWits should be the total number of inputs and wits
        abstractSize (mkCost @VKey) txw
          === length (outputs body_) + length wits_

propTxAbstractSize :: Property
propTxAbstractSize =
  withTests 50 $
    property $ do
      tr <- forAll (trace @UTXOW () 100)
      let txs = traceSignals OldestFirst tr :: [Tx]
      mapM_ propSize txs
  where
    propSize txw = propSumOfSizesTx txw >> propMultipleOfSizes txw

testTxHasTypeReps :: TestTree
testTxHasTypeReps =
  testGroup
    "Test HasTypeReps instances"
    [ testCase "AbstractSize - example - TxIn" exampleTypeRepsTxIn,
      testCase "AbstractSize - example - Tx" exampleTypeRepsTx,
      testProperty "AbstractSize and HasTypeReps - Tx*" propTxAbstractSize
    ]
