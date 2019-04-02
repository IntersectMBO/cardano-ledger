{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse   #-}

module PropertyTests (propertyTests) where

import qualified Data.Map.Strict         as Map
import           Data.MultiSet           (unions, fromSet, occur, filter, size)
import qualified Data.Set                as Set
import           Data.Text               (pack)

import           Lens.Micro              ((^.), (&), (%~), (.~))

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hedgehog.Coverage

import           Hedgehog
import qualified Hedgehog.Gen    as Gen

import           Generator

import           Coin
import           LedgerState
import           Slot
import           UTxO


-- | Take 'addr |-> c' pair from 'TxOut' and insert into map or add 'c' to value
-- already present. Used to fold over 'UTxO' to accumulate funds per address.
insertOrUpdate :: TxOut -> Map.Map Addr Coin -> Map.Map Addr Coin
insertOrUpdate (TxOut a c) m =
    Map.insert a (if Map.member a m
                  then c + (m Map.! a)
                  else c) m

-- | Return True if at least half of the keys have non-trivial coin values to
-- spent, i.e., at least 2 coins per 50% of addresses.
isNotDustDist :: UTxO -> UTxO -> Bool
isNotDustDist initUtxo utxo' =
    utxoSize initUtxo <=
           2 * (Map.size $ Map.filter (> Coin 1) coinMap)
        where coinMap = Map.foldr insertOrUpdate Map.empty (utxoMap utxo')

-- | This property states that a non-empty UTxO set in the genesis state has a
-- non-zero balance.
propPositiveBalance:: Property
propPositiveBalance =
    property $ do
      initialState <- forAll genNonemptyGenesisState
      utxoSize (initialState ^. utxoState . utxo) /== 0
      Coin 0 /== balance (initialState ^. utxoState . utxo)

-- | This property states that the balance of the initial genesis state equals
-- the balance of the end ledger state plus the collected fees.
propPreserveBalanceInitTx :: Cover
propPreserveBalanceInitTx =
    withCoverage $ do
      (_, steps, fee, ls, _, next)  <- forAll genNonEmptyAndAdvanceTx
      classify (steps > 1) "non-trivial number of steps"
      case next of
        Left _    -> failure
        Right ls' -> do
              classify (isNotDustDist (ls ^. utxoState . utxo) (ls' ^. utxoState . utxo)) "non-trivial wealth dist"
              balance (ls ^. utxoState . utxo) === balance (ls' ^. utxoState . utxo) + fee

-- | Property (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Cover
propBalanceTxInTxOut = withCoverage $ do
  (l, steps, fee, txwits, l')  <- forAll genValidStateTx
  let tx                       = txwits ^. body
  let inps                     = txins tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo)) "non-trivial wealth dist"
  (balance $ inps <| (l ^. utxoState . utxo)) === ((balance $ txouts tx) + fee)

-- | Property (Preserve Outputs of Transaction)
propPreserveOutputs :: Cover
propPreserveOutputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo)) "non-trivial wealth dist"
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ l' ^. utxoState . utxo)

-- | Property (Eliminate Inputs of Transaction)
propEliminateInputs :: Cover
propEliminateInputs = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo)) "non-trivial wealth dist"
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ l' ^. utxoState . utxo) (txins tx)

-- | Property (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Cover
propUniqueTxIds = withCoverage $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  let origTxIds             = collectIds <$> (Map.keys $ utxoMap (l ^. utxoState . utxo))
  let newTxIds              = collectIds <$> (Map.keys $ utxoMap (txouts tx))
  let txId                  = txid tx
  classify (steps > 1) "non-trivial valid ledger state"
  classify (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo)) "non-trivial wealth dist"
  True === ((all (== txId) newTxIds) &&
            (not $ any (== txId) origTxIds) &&
            Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ l' ^. utxoState . utxo))
         where collectIds (TxIn txId _) = txId

-- | Property checks no double spend occurs in the currently generated 'TxWits'
-- transactions. Note: this is more a property of the current generator.
propNoDoubleSpend :: Cover
propNoDoubleSpend = Test.Tasty.Hedgehog.Coverage.withTests 1000 $ withCoverage $ do
      (_, _, _, _, txs, next)  <- forAll genNonEmptyAndAdvanceTx
      case next of
        Left _  -> failure
        Right _ -> do
          let inputIndicesSet = unions $ map (\txwit -> fromSet $ txwit ^. body . inputs) txs
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
      let inputIndicesSet  = unions $ map (\txwit -> fromSet $ txwit ^. body . inputs) txs
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
                  , testPropertyCoverage
                    "changing witness set"
                    propCheckMinimalWitnessSet
                  , testPropertyCoverage
                    "using subset of witness set"
                    propCheckMissingWitness
                  , testPropertyCoverage
                    "Correctly preserve balance"
                    propPreserveBalance
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
  (l, _, fee, txwits, lv)  <- forAll genStateTx
  let tx                       = _body txwits
  let inps                     = txins tx
  let getErrors (LedgerValidation valErrors _) = valErrors
  let balanceSource            = balance $ inps <| (l ^. utxoState . utxo)
  let balanceTarget            = (balance $ txouts tx)
  let valErrors                = getErrors lv
  let nonTrivial               =  balanceSource /= Coin 0
  let balanceOk                = balanceSource == balanceTarget + fee
  classify (valErrors /= [] && balanceOk && nonTrivial) "non-valid, OK"
  if valErrors /= [] && balanceOk && nonTrivial
  then label (pack (  "inputs: "       ++ (show $ Set.size $ tx ^. inputs)
                     ++ " outputs: "   ++ (show $ length $ tx ^. outputs)
                     ++ " balance l "  ++ (show balanceSource)
                     ++ " balance l' " ++ (show balanceTarget)
                     ++ " txfee " ++ show fee
                     ++ "\n  validationErrors: " ++ show valErrors))
  else (if valErrors /= [] && balanceOk
        then label ("non-validated, OK, trivial")
        else (if valErrors /= []
              then label ("non-validated, KO")
              else label ("validated")
        ))
  success

-- | Check that we correctly report redundant witnesses. We get the list of the
-- keys from the generator and use one to generate a new witness. If that key
-- was used to sign the transaction, then the transaction must validate. If the
-- witness was not already used to sign the transaction, a `UnneededWitnesses`
-- validation error must be reported.
propCheckMinimalWitnessSet :: Cover
propCheckMinimalWitnessSet = withCoverage $ do
  (l, steps, _, txwits, _, keyPairs)  <- forAll genValidStateTxKeys
  let keyPair                  = fst $ head keyPairs
  let tx                       = txwits ^. body
  let witness                  = makeWitness tx keyPair
  let txwits'                  = txwits & witnessSet %~ (Set.insert witness)
  let l''                      = asStateTransition (Slot (steps)) l txwits'
  classify (not $ witness `Set.member` (txwits ^. witnessSet))
               "unneeded signature added"
  case l'' of
    Left [UnneededWitnesses]  ->
        (witness `Set.member` (txwits ^. witnessSet)) === False
    Right _                    ->
        (witness `Set.member` (txwits ^. witnessSet)) === True
    _                          -> failure

-- | Check that we correctly report missing witnesses.
propCheckMissingWitness :: Cover
propCheckMissingWitness = withCoverage $ do
  (l, steps, _, txwits, _) <- forAll genValidStateTx
  witnessList              <- forAll (Gen.subsequence $
                                        Set.toList (txwits ^. witnessSet))
  let witnessSet''          = txwits ^. witnessSet
  let witnessSet'           = Set.fromList witnessList
  let l'                    = asStateTransition (Slot steps) l (txwits & witnessSet .~ witnessSet')
  let isRealSubset          = witnessSet' `Set.isSubsetOf` witnessSet'' &&
                              witnessSet' /= witnessSet''
  classify (isRealSubset) "real subset"
  label (pack ("witnesses:" ++ show (Set.size witnessSet'')))
  case l' of
    Left [MissingWitnesses] -> isRealSubset === True
    Right _                 -> (witnessSet' == witnessSet'') === True
    _                       -> failure

-- | Property (Preserve Balance)
propPreserveBalance :: Cover
propPreserveBalance = withCoverage $ do
  (l, _, fee, tx, l') <- forAll genValidStateTx
  let destroyed =
           (balance (l ^. utxoState . utxo))
        + (keyRefunds (l ^. pcs) (l ^. delegationState . dstate . stKeys) $ tx ^. body)
  let created =
           (balance (l' ^. utxoState . utxo))
        + fee
        + (deposits (l' ^. pcs) (l' ^. delegationState . pstate . stPools) $ tx ^.body . certs)
  destroyed === created
