{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Shelley.Spec.Ledger.NonTraceProperties.PropertyTests (nonTracePropertyTests) where

import Byron.Spec.Ledger.Core ((<|))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MultiSet (filter, fromSet, occur, size, unions)
import qualified Data.Set as Set
import Hedgehog
  ( (/==),
    (===),
    Property,
    classify,
    failure,
    label,
    property,
    success,
    withTests,
  )
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (LabelName (..))
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
  ( _body,
    _certs,
    _inputs,
    _outputs,
    _witnessVKeySet,
    pattern TxIn,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.UTxO
  ( balance,
    hashTxBody,
    makeWitnessVKey,
    totalDeposits,
    txid,
    txins,
    txouts,
    verifyWitVKey,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.NonTraceProperties.Generator
import Test.Shelley.Spec.Ledger.NonTraceProperties.Validity
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Take 'addr |-> c' pair from 'TxOut' and insert into map or add 'c' to value
-- already present. Used to fold over 'UTxO' to accumulate funds per address.
insertOrUpdate :: TxOut -> Map Addr Coin -> Map Addr Coin
insertOrUpdate (TxOut a c) m =
  Map.insert
    a
    ( if Map.member a m
        then c + (m Map.! a)
        else c
    )
    m

-- | Return True if at least half of the keys have non-trivial coin values to
-- spent, i.e., at least 2 coins per 50% of addresses.
isNotDustDist :: UTxO -> UTxO -> Bool
isNotDustDist initUtxo utxo' =
  utxoSize initUtxo
    <= 2 * Map.size (Map.filter (> Coin 1) coinMap)
  where
    coinMap = Map.foldr insertOrUpdate Map.empty (utxoMap utxo')

-- | This property states that a non-empty UTxO set in the genesis state has a
-- non-zero balance.
propPositiveBalance :: Property
propPositiveBalance =
  property $ do
    initialState <- Hedgehog.forAll genNonemptyGenesisState
    utxoSize ((_utxo . _utxoState) initialState) /== 0
    Coin 0 /== balance ((_utxo . _utxoState) initialState)

-- | This property states that the balance of the initial genesis state equals
-- the balance of the end ledger state plus the collected fees.
propPreserveBalanceInitTx :: Property
propPreserveBalanceInitTx =
  property $ do
    (_, steps, fee, ls, _, next) <- Hedgehog.forAll genNonEmptyAndAdvanceTx
    classify "non-trivial number of steps" (steps > 1)
    case next of
      Left _ -> failure
      Right ls' -> do
        classify
          "non-trivial wealth dist"
          (isNotDustDist ((_utxo . _utxoState) ls) ((_utxo . _utxoState) ls'))
        balance ((_utxo . _utxoState) ls) === balance ((_utxo . _utxoState) ls') + fee

-- | Property (Preserve Balance Restricted to TxIns in Balance of TxOuts)
propBalanceTxInTxOut :: Property
propBalanceTxInTxOut = property $ do
  (l, steps, fee, txwits, l') <- Hedgehog.forAll genValidStateTx
  let tx = _body txwits
  let inps = txins tx
  classify "non-trivial valid ledger state" (steps > 1)
  classify
    "non-trivial wealth dist"
    (isNotDustDist ((_utxo . _utxoState) l) ((_utxo . _utxoState) l'))
  (balance $ inps <| ((_utxo . _utxoState) l)) === (balance (txouts tx) + fee)

-- | Property (Preserve Outputs of Transaction)
propPreserveOutputs :: Property
propPreserveOutputs = property $ do
  (l, steps, _, txwits, l') <- Hedgehog.forAll genValidStateTx
  let tx = _body txwits
  classify "non-trivial valid ledger state" (steps > 1)
  classify
    "non-trivial wealth dist"
    (isNotDustDist ((_utxo . _utxoState) l) ((_utxo . _utxoState) l'))
  True === Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ (_utxo . _utxoState) l')

-- | Property (Eliminate Inputs of Transaction)
propEliminateInputs :: Property
propEliminateInputs = property $ do
  (l, steps, _, txwits, l') <- Hedgehog.forAll genValidStateTx
  let tx = _body txwits
  classify "non-trivial valid ledger state" (steps > 1)
  classify
    "non-trivial wealth dist"
    (isNotDustDist ((_utxo . _utxoState) l) ((_utxo . _utxoState) l'))
  -- no element of 'txins tx' is a key in the 'UTxO' of l'
  Map.empty === Map.restrictKeys (utxoMap $ (_utxo . _utxoState) l') (txins tx)

-- | Property (Completeness and Collision-Freeness of new TxIds)
propUniqueTxIds :: Property
propUniqueTxIds = property $ do
  (l, steps, _, txwits, l') <- Hedgehog.forAll genValidStateTx
  let tx = _body txwits
  let origTxIds = collectIds <$> Map.keys (utxoMap ((_utxo . _utxoState) l))
  let newTxIds = collectIds <$> Map.keys (utxoMap (txouts tx))
  let txId = txid tx
  classify "non-trivial valid ledger state" (steps > 1)
  classify
    "non-trivial wealth dist"
    (isNotDustDist ((_utxo . _utxoState) l) ((_utxo . _utxoState) l'))
  True
    === ( all (== txId) newTxIds
            && notElem txId origTxIds
            && Map.isSubmapOf (utxoMap $ txouts tx) (utxoMap $ (_utxo . _utxoState) l')
        )
  where
    collectIds (TxIn txId _) = txId

-- | Property checks no double spend occurs in the currently generated 'TxWits'
-- transactions. Note: this is more a property of the current generator.
propNoDoubleSpend :: Property
propNoDoubleSpend = withTests 1000 $ property $ do
  (_, _, _, _, txs, next) <- Hedgehog.forAll genNonEmptyAndAdvanceTx
  case next of
    Left _ -> failure
    Right _ -> do
      let inputIndicesSet = unions $ map (\txwit -> fromSet $ (_inputs . _body) txwit) txs
      0
        === Data.MultiSet.size
          ( Data.MultiSet.filter
              (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
              inputIndicesSet
          )

-- | Classify mutated transaction into double-spends (validated and
-- non-validated). This is a property of the validator, i.e., no validated
-- transaction should ever be able to do a double spend.
classifyInvalidDoubleSpend :: Property
classifyInvalidDoubleSpend = withTests 1000 $ property $ do
  (_, _, _, _, txs, LedgerValidation validationErrors _) <-
    Hedgehog.forAll genNonEmptyAndAdvanceTx'
  let inputIndicesSet = unions $ map (\txwit -> fromSet $ (_inputs . _body) txwit) txs
  let multiSpentInputs =
        Data.MultiSet.size $
          Data.MultiSet.filter
            (\idx -> 1 < Data.MultiSet.occur idx inputIndicesSet)
            inputIndicesSet
  let isMultiSpend = 0 < multiSpentInputs
  classify "multi-spend, validation OK" (null validationErrors)
  classify "multi-spend, validation KO" (isMultiSpend && validationErrors /= [])
  classify "multi-spend" isMultiSpend
  True === (not isMultiSpend || validationErrors /= [])

propNonNegativeTxOuts :: Property
propNonNegativeTxOuts =
  withTests 100000 . property $ do
    (_, _, _, tx, _) <- Hedgehog.forAll genStateTx
    all (\(TxOut _ (Coin x)) -> x >= 0) (_outputs . _body $ tx) === True

-- | Mutations for Property 7.2
propBalanceTxInTxOut' :: Property
propBalanceTxInTxOut' =
  withTests 1000 $ property $ do
    (l, _, fee, txwits, lv) <- Hedgehog.forAll genStateTx
    let tx = _body txwits
    let inps = txins tx
    let getErrors (LedgerValidation valErrors _) = valErrors
    let balanceSource = balance $ inps <| ((_utxo . _utxoState) l)
    let balanceTarget = balance $ txouts tx
    let valErrors = getErrors lv
    let nonTrivial = balanceSource /= Coin 0
    let balanceOk = balanceSource == balanceTarget + fee
    classify "non-valid, OK" (valErrors /= [] && balanceOk && nonTrivial)
    if valErrors /= [] && balanceOk && nonTrivial
      then
        label $
          LabelName
            ( "inputs: " ++ show (show $ Set.size $ _inputs tx)
                ++ " outputs: "
                ++ show (show $ length $ _outputs tx)
                ++ " balance l "
                ++ show balanceSource
                ++ " balance l' "
                ++ show balanceTarget
                ++ " txfee "
                ++ show fee
                ++ "\n  validationErrors: "
                ++ show valErrors
            )
      else
        ( if valErrors /= [] && balanceOk
            then label "non-validated, OK, trivial"
            else
              ( if valErrors /= []
                  then label "non-validated, KO"
                  else label "validated"
              )
        )
    success

-- | Check that we correctly test redundant witnesses. We get the list of the
-- keys from the generator and use one to generate a new witness. If that key
-- was used to sign the transaction, then the transaction must validate. If a
-- new, redundant witness signature is added, the transaction must still
-- validate.
propCheckRedundantWitnessSet :: Property
propCheckRedundantWitnessSet = property $ do
  (l, steps, _, txwits, _, keyPairs) <- Hedgehog.forAll genValidStateTxKeys
  let keyPair = fst $ head keyPairs
  let tx = _body txwits
  let witness = makeWitnessVKey (hashTxBody tx) keyPair
  let txwits' = txwits {_witnessVKeySet = (Set.insert witness (_witnessVKeySet txwits))}
  let l'' = asStateTransition (SlotNo $ fromIntegral steps) emptyPParams l txwits' (AccountState 0 0)
  classify
    "unneeded signature added"
    (not $ witness `Set.member` (_witnessVKeySet txwits))
  case l'' of
    Right _ ->
      True
        === Set.null
          ( Set.filter (not . verifyWitVKey (hashTxBody tx)) (_witnessVKeySet txwits')
          )
    _ -> failure

-- | Check that we correctly report missing witnesses.
propCheckMissingWitness :: Property
propCheckMissingWitness = property $ do
  (l, steps, _, txwits, _) <- Hedgehog.forAll genValidStateTx
  witnessList <-
    Hedgehog.forAll
      ( Gen.subsequence $
          Set.toList (_witnessVKeySet txwits)
      )
  let witnessVKeySet'' = _witnessVKeySet txwits
  let witnessVKeySet' = Set.fromList witnessList
  let l' =
        asStateTransition
          (SlotNo $ fromIntegral steps)
          emptyPParams
          l
          (txwits {_witnessVKeySet = witnessVKeySet'})
          (AccountState 0 0)
  let isRealSubset =
        witnessVKeySet' `Set.isSubsetOf` witnessVKeySet''
          && witnessVKeySet' /= witnessVKeySet''
  classify "real subset" isRealSubset
  label $ LabelName ("witnesses:" ++ show (Set.size witnessVKeySet''))
  case l' of
    Left [MissingWitnesses] -> isRealSubset === True
    Right _ -> (witnessVKeySet' == witnessVKeySet'') === True
    _ -> failure

-- | Property (Preserve Balance)
propPreserveBalance :: Property
propPreserveBalance = property $ do
  (l, _, fee, tx, l') <- Hedgehog.forAll genValidStateTx
  let destroyed =
        balance ((_utxo . _utxoState) l)
          + (keyRefunds emptyPParams ((_stkCreds . _dstate . _delegationState) l) $ _body tx)
  let created =
        balance ((_utxo . _utxoState) l')
          + fee
          + (totalDeposits emptyPParams ((_stPools . _pstate . _delegationState) l') $ toList $ (_certs . _body) tx)
  destroyed === created

-- | 'TestTree' of property-based testing properties.
nonTracePropertyTests :: TestTree
nonTracePropertyTests =
  testGroup
    "Non-Trace Property-Based Testing"
    [ testGroup
        "Ledger Genesis State"
        [ testProperty
            "non-empty genesis ledger state has non-zero balance"
            propPositiveBalance,
          testProperty
            "several transaction added to genesis ledger state"
            propPreserveBalanceInitTx
        ],
      testGroup
        "Property tests starting from valid ledger state"
        [ testProperty
            "preserve balance restricted to TxIns in Balance of outputs"
            propBalanceTxInTxOut,
          testProperty
            "Preserve outputs of transaction"
            propPreserveOutputs,
          testProperty
            "Eliminate Inputs of Transaction"
            propEliminateInputs,
          testProperty
            "Completeness and Collision-Freeness of new TxIds"
            propUniqueTxIds,
          testProperty
            "No Double Spend in valid ledger states"
            propNoDoubleSpend,
          testProperty
            "adding redundant witness"
            propCheckRedundantWitnessSet,
          testProperty
            "using subset of witness set"
            propCheckMissingWitness,
          testProperty
            "Correctly preserve balance"
            propPreserveBalance
        ],
      testGroup
        "Property tests with mutated transactions"
        [ testProperty
            "preserve balance of change in UTxO"
            propBalanceTxInTxOut',
          testProperty
            "Classify double spend"
            classifyInvalidDoubleSpend,
          testProperty
            "NonNegative TxOuts"
            propNonNegativeTxOuts
        ]
    ]
