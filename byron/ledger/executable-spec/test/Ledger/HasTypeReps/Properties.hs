{-# LANGUAGE TypeApplications #-}

module Ledger.HasTypeReps.Properties
    (testTxHasTypeReps)
  where

import           Data.AbstractSize

import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Sequence                      (empty, (<|), (><))
import qualified Data.Sequence                      as Seq
import           Data.Typeable                      (TypeRep, typeOf)
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

-- | Since 'VKey' appears in both Wit and TxOut (as part of Addr), we
--   need to use the same "cost" for this type in the following tests.
vKeyCost :: Size
vKeyCost = 17

-- | Example costs for types in a 'TxIn'.
costsTxIn :: Map TypeRep Size
costsTxIn = Map.fromList [ (typeOf (undefined :: TxIn), 2)
                         , (typeOf (undefined :: TxId), 3)
                         , (typeOf (undefined :: Hash), 5) ]

-- | Example costs for types in a 'TxOut'.
costsTxOut :: Map TypeRep Size
costsTxOut = Map.fromList [ (typeOf (undefined :: TxOut), 11)
                          , (typeOf (undefined :: Addr), 13)
                          , (typeOf (undefined :: VKey), vKeyCost)
                          , (typeOf (undefined :: Lovelace), 23) ]

-- | Example costs for types in a 'Wit'.
costsWit :: Map TypeRep Size
costsWit = Map.fromList [ (typeOf (undefined :: Wit), 31)
                        , (typeOf (undefined :: VKey), vKeyCost)
                        , (typeOf (undefined :: Sig Tx), 41)]

-- | This property tests that 'abstractSize' accounts for multiple occurences
--   of any type represented in the given "cost map".
--
--   'abstractSize' uses a "cost map" to compute the size of a term.
--   For types that contain multiple occurrences of other types, 'abstractSize'
--   should count each occurence (e.g. a 'Tx' may contain multiple 'TxIn' and
--   when computing the size of Tx, we should account for the size of each
--   'TxIn' occurence).
--
--  Precondition: this test assumes that the "cost map" only accounts for types
--  that occur within the "inner" type of multiples that we are targeting, e.g.
--  if we include a type in the cost map that occurs in 'TxIn' _and_ 'Tx',
--  this property would have to be weakened to
--  abstractSize costs x - n * unitCost > 0
propMultiplesOfSize
  :: (MonadTest m, HasTypeReps a)
  => Map TypeRep Size
  -- ^ a "cost map" for some type that may occur multiple times in the
  --   given term of type 'a'
  -> a
  -- ^ term for which we will compute 'abstractSize'
  -> Int
  -- ^ the number of occurences of the "costed types" in given the term
  -> m ()
propMultiplesOfSize costs x n
  = abstractSize costs x === n * unitCost
  where
    unitCost = sum (Map.elems costs)

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

-- | A combination of several properties
--   (1) If we "cost" just the 'TxIn' type, then the abstract size of a 'Tx'
--       term is a multiple of the unit cost for a 'TxIn'
--   (2) Similarly, for 'Tx' and 'TxOut'
--   (3) Similarly, for 'Tx' and 'Wit'
--   (4) The abstract size of a 'TxWits' term is the sum of sizes of
--       the 'TxIn/TxOut/Wit' parts.
propTxAbstractSize :: Property
propTxAbstractSize
  = withTests 50 $ property $ do
    tr <- forAll (trace @UTXOW 100)
    let txs = traceSignals OldestFirst tr :: [TxWits]
    mapM_ propSize txs
  where
    propSize txw
      = let
          body_ = (body txw)
          wits_ = witnesses txw
        in propMultiplesOfSize costsTxIn body_ (length $ inputs body_) -- (1)
           >> propMultiplesOfSize costsTxOut body_ (length $ outputs body_) -- (2)
           >> propMultiplesOfSize costsWit wits_ (length wits_) -- (3)
           >> propSumOfSizesTxWits txw -- (4)

testTxHasTypeReps :: TestTree
testTxHasTypeReps = testGroup "Test HasTypeReps instances"
  [ testCase "exampleTypeRepsTxIn - HasTypeReps" exampleTypeRepsTxIn
  , testCase "exampleTypeRepsTx - HasTypeReps" exampleTypeRepsTx

  , testProperty "AbstractSize and HasTypeReps" propTxAbstractSize
  ]
