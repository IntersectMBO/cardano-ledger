{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module PropertyTests (propertyTests) where

import qualified Data.Map                as Map
import           Data.MultiSet           (unions, fromSet, occur, filter, size)
import qualified Data.Set                as Set
import           Data.Text               (pack)

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hedgehog.Coverage

import           Hedgehog

import           Generator

import           Coin
import           LedgerState             (LedgerState(..), LedgerValidation(..))
import           UTxO


-- | Take 'addr |-> c' pair from 'TxOut' and insert into map or add 'c' to value
-- already present. Used to fold over 'UTxO' to accumulate funds per address.
insertOrUpdate :: TxOut -> Map.Map Addr Coin -> Map.Map Addr Coin
insertOrUpdate (TxOut a c) m =
    Map.insert a (if Map.member a m
                  then c <> (m Map.! a)
                  else c) m

-- | Return True if at least half of the keys have non-trivial coin values to
-- spent, i.e., at least 2 coins per 50% of addresses.
isNotDustDist :: UTxO -> UTxO -> Bool
isNotDustDist initUtxo utxo =
    utxoSize initUtxo <=
           2 * (Map.size $ Map.filter (> Coin 1) coinMap)
        where coinMap = Map.foldr insertOrUpdate Map.empty (utxoMap utxo)

-- | This property states that a non-empty UTxO set in the genesis state has a
-- non-zero balance.
propPositiveBalance:: Property
propPositiveBalance =
    property $ do
      initialState <- forAll genNonemptyGenesisState
      utxoSize (getUtxo initialState) /== 0
      Coin 0 /== balance (getUtxo initialState)

-- | This property states that the balance of the initial genesis state equals
-- the balance of the end ledger state plus the collected fees.
propPreserveBalanceInitTx :: Cover
propPreserveBalanceInitTx =
    withCoverage $ do
      (_, steps, fees, ls, _, next)  <- forAll genNonEmptyAndAdvanceTx
      classify (steps > 1) "non-trivial number of steps"
      case next of
        Left _    -> failure
        Right ls' -> do
              classify (isNotDustDist (getUtxo ls) (getUtxo ls'))
                           "non-trivial wealth dist"
              balance (getUtxo ls) === balance (getUtxo ls') <> fees

-- | Property 7.2 (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Cover
propBalanceTxInTxOut = withCoverage $ do
  (l, steps, txfee, txwits, l')  <- forAll genValidStateTx
  let tx                       = body txwits
  let inps                     = txins tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  (balance $ inps <| (getUtxo l)) === ((balance $ txouts tx) <> txfee)

-- | Property 7.3 (Preserve Outputs of Transaction)
propPreserveOutputs :: Cover
propPreserveOutputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l')

-- | Property 7.4 (Eliminate Inputs of Transaction)
propEliminateInputs :: Cover
propEliminateInputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ getUtxo l') (txins tx)

-- | Property 7.5 (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Cover
propUniqueTxIds = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = body txwits
  let origTxIds             = collectIds <$> (Map.keys $ utxoMap (getUtxo l))
  let newTxIds              = collectIds <$> (Map.keys $ utxoMap (txouts tx))
  let txId                  = txid tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (getUtxo l) (getUtxo l')) "non-trivial wealth dist"
  True === ((all (== txId) newTxIds) &&
            (not $ any (== txId) origTxIds) &&
            Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ getUtxo l'))
         where collectIds (TxIn txId _) = txId

-- | Property checks no double spend occurs in the currently generated 'TxWits'
-- transactions. Note: this is more a property of the current generator.
propNoDoubleSpend :: Cover
propNoDoubleSpend = Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
      (_, _, _, _, txs, next)  <- forAll genNonEmptyAndAdvanceTx
      case next of
        Left _  -> failure
        Right _ -> do
          let inputIndicesSet = unions $ map (\txwit -> fromSet $ inputs $ body txwit) txs
          0 === (Data.MultiSet.size $ Data.MultiSet.filter
                     (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                     inputIndicesSet)

-- | Classify mutated transaction into double-spends (validated and
-- non-validated). This is a property of the validator, i.e., no validated
-- transaction should ever be able to do a double spend.
classifyInvalidDoubleSpend :: Cover
classifyInvalidDoubleSpend = Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
      (_, _, _, _, txs, LedgerValidation validationErrors _)
          <- forAll genNonEmptyAndAdvanceTx'
      let inputIndicesSet  = unions $ map (\txwit -> fromSet $ inputs $ body txwit) txs
      let multiSpentInputs = (Data.MultiSet.size $ Data.MultiSet.filter
                                   (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                                   inputIndicesSet)
      let isMultiSpend = 0 < multiSpentInputs
      classify (isMultiSpend && validationErrors == []) "multi-spend, validation OK"
      classify (isMultiSpend && validationErrors /= []) "multi-spend, validation KO"
      classify (isMultiSpend) "multi-spend"
      True === ((not isMultiSpend) || validationErrors /= [])

-- | 'TestTree' of property-based testing properties.
propertyTests :: TestTree
propertyTests = testGroup "Property-Based Testing"
                [ testGroup "Ledger Genesis State"
                  [testProperty
                    "non-empty genesis ledger state has non-zero balance"
                    propPositiveBalance
                  , testPropertyCoverage
                    "several transaction added to genesis ledger state"
                    propPreserveBalanceInitTx]
                , testGroup "Property tests starting from valid ledger state"
                  [testPropertyCoverage
                    "preserve balance restricted to TxIns in Balance of outputs"
                    propBalanceTxInTxOut
                  , testPropertyCoverage
                    "Preserve outputs of transaction"
                    propPreserveOutputs
                  , testPropertyCoverage
                    "Eliminate Inputs of Transaction"
                    propEliminateInputs
                  , testPropertyCoverage
                    "Completeness and Collision-Freeness of new TxIds"
                    propUniqueTxIds
                  , testPropertyCoverage
                    "No Double Spend in valid ledger states"
                    propNoDoubleSpend
                  ]
                , testGroup "Property tests with mutated transactions"
                  [testPropertyCoverage
                   "preserve balance of change in UTxO"
                   propBalanceTxInTxOut'
                  , testPropertyCoverage
                    "Classify double spend"
                    classifyInvalidDoubleSpend
                  ]
                ]

-- | Mutations for Property 7.2
propBalanceTxInTxOut' :: Cover
propBalanceTxInTxOut' =
  Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
  (l, _, txfee, txwits, lv)  <- forAll genStateTx
  let tx                       = body txwits
  let inps                     = txins tx
  let getErrors (LedgerValidation valErrors _) = valErrors
  let balanceSource            = balance $ inps <| (getUtxo l)
  let balanceTarget            = (balance $ txouts tx)
  let valErrors                = getErrors lv
  let nonTrivial               =  balanceSource /= Coin 0
  let balanceOk                = balanceSource == balanceTarget <> txfee
  classify (valErrors /= [] && balanceOk && nonTrivial) "non-valid, OK"
  if valErrors /= [] && balanceOk && nonTrivial
  then label (pack (  "inputs: "       ++ (show $ Set.size (inputs tx))
                     ++ " outputs: "   ++ (show $ length (outputs tx))
                     ++ " balance l "  ++ (show balanceSource)
                     ++ " balance l' " ++ (show balanceTarget)
                     ++ " txfee " ++ show txfee
                     ++ "\n  validationErrors: " ++ show valErrors))
  else (if valErrors /= [] && balanceOk
        then label ("non-validated, OK, trivial")
        else (if valErrors /= []
              then label ("non-validated, KO")
              else label ("validated")
        ))
  success
