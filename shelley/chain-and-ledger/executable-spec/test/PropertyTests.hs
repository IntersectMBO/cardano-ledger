{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module PropertyTests (propertyTests) where

import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import           Data.MultiSet (filter, fromSet, occur, size, unions)
import qualified Data.Set as Set

import           Lens.Micro ((%~), (&), (.~), (^.))

import           Hedgehog.Internal.Property (LabelName (..))
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Coin
import           Ledger.Core ((<|))
import           LedgerState hiding (dms)
import           PParams
import           Slot
import           Tx (pattern TxIn, pattern TxOut, body, certs, inputs, outputs, witnessVKeySet,
                     _body, _witnessVKeySet)
import           UTxO (balance, deposits, makeWitnessVKey, txid, txins, txouts, verifyWitVKey)

import           Generator
import           MockTypes


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
           2 *Map.size (Map.filter (> Coin 1) coinMap)
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
propPreserveBalanceInitTx :: Property
propPreserveBalanceInitTx =
    property $ do
      (_, steps, fee, ls, _, next)  <- forAll genNonEmptyAndAdvanceTx
      classify "non-trivial number of steps" (steps > 1)
      case next of
        Left _    -> failure
        Right ls' -> do
              classify "non-trivial wealth dist"
                (isNotDustDist (ls ^. utxoState . utxo) (ls' ^. utxoState . utxo))
              balance (ls ^. utxoState . utxo) === balance (ls' ^. utxoState . utxo) + fee

-- | Property (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Property
propBalanceTxInTxOut = property $ do
  (l, steps, fee, txwits, l')  <- forAll genValidStateTx
  let tx                       = txwits ^. body
  let inps                     = txins tx
  classify "non-trivial valid ledger state" (steps > 1)
  classify "non-trivial wealth dist"
    (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo))
  (balance $ inps <| (l ^. utxoState . utxo)) === (balance (txouts tx) + fee)

-- | Property (Preserve Outputs of Transaction)
propPreserveOutputs :: Property
propPreserveOutputs = property $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  classify "non-trivial valid ledger state" (steps > 1)
  classify "non-trivial wealth dist"
    (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo))
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ l' ^. utxoState . utxo)

-- | Property (Eliminate Inputs of Transaction)
propEliminateInputs :: Property
propEliminateInputs = property $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  classify "non-trivial valid ledger state" (steps > 1)
  classify "non-trivial wealth dist"
    (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo))
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ l' ^. utxoState . utxo) (txins tx)

-- | Property (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Property
propUniqueTxIds = property $ do
  (l, steps, _, txwits, l') <- forAll genValidStateTx
  let tx                    = txwits ^. body
  let origTxIds             = collectIds <$> Map.keys (utxoMap (l ^. utxoState . utxo))
  let newTxIds              = collectIds <$> Map.keys (utxoMap (txouts tx))
  let txId                  = txid tx
  classify "non-trivial valid ledger state" (steps > 1)
  classify "non-trivial wealth dist"
    (isNotDustDist (l ^. utxoState . utxo) (l' ^. utxoState . utxo))
  True === (all (== txId) newTxIds &&
             notElem txId origTxIds &&
            Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ l' ^. utxoState . utxo))
         where collectIds (TxIn txId _) = txId

-- | Property checks no double spend occurs in the currently generated 'TxWits'
-- transactions. Note: this is more a property of the current generator.
propNoDoubleSpend :: Property
propNoDoubleSpend = withTests 1000 $ property $ do
      (_, _, _, _, txs, next)  <- forAll genNonEmptyAndAdvanceTx
      case next of
        Left _  -> failure
        Right _ -> do
          let inputIndicesSet = unions $ map (\txwit -> fromSet $ txwit ^. body . inputs) txs
          0 === Data.MultiSet.size (Data.MultiSet.filter
                     (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                     inputIndicesSet)

-- | Classify mutated transaction into double-spends (validated and
-- non-validated). This is a property of the validator, i.e., no validated
-- transaction should ever be able to do a double spend.
classifyInvalidDoubleSpend :: Property
classifyInvalidDoubleSpend = withTests 1000 $ property $ do
      (_, _, _, _, txs, LedgerValidation validationErrors _)
          <- forAll genNonEmptyAndAdvanceTx'
      let inputIndicesSet  = unions $ map (\txwit -> fromSet $ txwit ^. body . inputs) txs
      let multiSpentInputs = Data.MultiSet.size $ Data.MultiSet.filter
                                   (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
                                   inputIndicesSet
      let isMultiSpend = 0 < multiSpentInputs
      classify "multi-spend, validation OK" (null validationErrors)
      classify "multi-spend, validation KO" (isMultiSpend && validationErrors /= [])
      classify "multi-spend" isMultiSpend
      True === (not isMultiSpend || validationErrors /= [])

-- | 'TestTree' of property-based testing properties.
propertyTests :: TestTree
propertyTests = testGroup "Property-Based Testing"
                [ testGroup "Ledger Genesis State"
                  [testProperty
                    "non-empty genesis ledger state has non-zero balance"
                    propPositiveBalance
                  , testProperty
                    "several transaction added to genesis ledger state"
                    propPreserveBalanceInitTx]
                , testGroup "Property tests starting from valid ledger state"
                  [testProperty
                    "preserve balance restricted to TxIns in Balance of outputs"
                    propBalanceTxInTxOut
                  , testProperty
                    "Preserve outputs of transaction"
                    propPreserveOutputs
                  , testProperty
                    "Eliminate Inputs of Transaction"
                    propEliminateInputs
                  , testProperty
                    "Completeness and Collision-Freeness of new TxIds"
                    propUniqueTxIds
                  , testProperty
                    "No Double Spend in valid ledger states"
                    propNoDoubleSpend
                  , testProperty
                    "adding redundant witness"
                    propCheckRedundantWitnessSet
                  , testProperty
                    "using subset of witness set"
                    propCheckMissingWitness
                  , testProperty
                    "Correctly preserve balance"
                    propPreserveBalance
                  ]
                , testGroup "Property tests with mutated transactions"
                  [testProperty
                   "preserve balance of change in UTxO"
                   propBalanceTxInTxOut'
                  , testProperty
                    "Classify double spend"
                    classifyInvalidDoubleSpend
                  ]
                ]

-- | Mutations for Property 7.2
propBalanceTxInTxOut' :: Property
propBalanceTxInTxOut' =
  withTests 1000 $ property $ do
  (l, _, fee, txwits, lv)  <- forAll genStateTx
  let tx                       = _body txwits
  let inps                     = txins tx
  let getErrors (LedgerValidation valErrors _) = valErrors
  let balanceSource            = balance $ inps <| (l ^. utxoState . utxo)
  let balanceTarget            = balance $ txouts tx
  let valErrors                = getErrors lv
  let nonTrivial               =  balanceSource /= Coin 0
  let balanceOk                = balanceSource == balanceTarget + fee
  classify "non-valid, OK" (valErrors /= [] && balanceOk && nonTrivial)
  if valErrors /= [] && balanceOk && nonTrivial

  then label $ LabelName (   "inputs: "     ++ show (show $ Set.size $ tx ^. inputs)
              ++ " outputs: "   ++ show (show $ length $ tx ^. outputs)
              ++ " balance l "  ++ show balanceSource
              ++ " balance l' " ++ show balanceTarget
              ++ " txfee " ++ show fee
              ++ "\n  validationErrors: " ++ show valErrors)
  else (if valErrors /= [] && balanceOk
        then label "non-validated, OK, trivial"
        else (if valErrors /= []
              then label "non-validated, KO"
              else label "validated"
        ))
  success

-- | Check that we correctly test redundant witnesses. We get the list of the
-- keys from the generator and use one to generate a new witness. If that key
-- was used to sign the transaction, then the transaction must validate. If a
-- new, redundant witness signature is added, the transaction must still
-- validate.
propCheckRedundantWitnessSet :: Property
propCheckRedundantWitnessSet = property $ do
  (l, steps, _, txwits, _, keyPairs)  <- forAll genValidStateTxKeys
  let keyPair                  = fst $ head keyPairs
  let tx                       = txwits ^. body
  let witness                  = makeWitnessVKey tx keyPair
  let txwits'                  = txwits & witnessVKeySet %~ Set.insert witness
  let dms                      = _dms $ _dstate $ _delegationState l
  let l''                      = asStateTransition (Slot steps) emptyPParams l txwits' dms
  classify "unneeded signature added"
    (not $ witness `Set.member` (txwits ^. witnessVKeySet))
  case l'' of
    Right _                    ->
        True === Set.null (
         Set.filter (not . verifyWitVKey tx) (_witnessVKeySet txwits'))
    _                          -> failure

-- | Check that we correctly report missing witnesses.
propCheckMissingWitness :: Property
propCheckMissingWitness = property $ do
  (l, steps, _, txwits, _) <- forAll genValidStateTx
  witnessList              <- forAll (Gen.subsequence $
                                        Set.toList (txwits ^. witnessVKeySet))
  let witnessVKeySet''          = txwits ^. witnessVKeySet
  let witnessVKeySet'           = Set.fromList witnessList
  let dms                   = _dms $ _dstate $ _delegationState l
  let l'                    = asStateTransition (Slot steps) emptyPParams l (txwits & witnessVKeySet .~ witnessVKeySet') dms
  let isRealSubset          = witnessVKeySet' `Set.isSubsetOf` witnessVKeySet'' &&
                              witnessVKeySet' /= witnessVKeySet''
  classify "real subset" isRealSubset
  label $ LabelName ("witnesses:" ++ show (Set.size witnessVKeySet''))
  case l' of
    Left [MissingWitnesses] -> isRealSubset === True
    Right _                 -> (witnessVKeySet' == witnessVKeySet'') === True
    _                       -> failure

-- | Property (Preserve Balance)
propPreserveBalance :: Property
propPreserveBalance = property $ do
  (l, _, fee, tx, l') <- forAll genValidStateTx
  let destroyed =
           balance (l ^. utxoState . utxo)
        + (keyRefunds emptyPParams (l ^. delegationState . dstate . stKeys) $ tx ^. body)
  let created =
           balance (l' ^. utxoState . utxo)
        + fee
        + (deposits emptyPParams (l' ^. delegationState . pstate . stPools) $ toList $ tx ^.body . certs)
  destroyed === created
