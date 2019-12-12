{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Generator
    ( utxoSize
    , utxoMap
    , genNonEmptyAndAdvanceTx
    , genNonEmptyAndAdvanceTx'
    , genNonemptyGenesisState
    , genStateTx
    , genValidStateTx
    , genValidStateTxKeys
    , genDelegationData
    , genDelegation
    , genDCertDelegate
    , genKeyPairs
    , asStateTransition
    , asStateTransition'
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Lens.Micro ((^.))

import           Numeric.Natural

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


import           Coin
import           Control.State.Transition.Extended (TRC (..), applySTS)
import           Keys (pattern KeyPair, hashKey, vKey)
import           LedgerState (pattern LedgerValidation, applyTxBody, dstate, genesisCoins,
                     genesisState, stkCreds, utxo, utxoState, validTx, _delegationState, _dstate,
                     _genDelegs, _txSlotIx, _utxoState)
import           PParams (PParams (..), emptyPParams)
import           Slot
import           STS.Delegs (pattern DelegateeNotRegisteredDELEG, PredicateFailure (..),
                     pattern WithrawalsNotInRewardsDELEGS)
import           STS.Ledger (LedgerEnv (..), PredicateFailure (..))
import           STS.Utxo (pattern BadInputsUTxO, pattern ExpiredUTxO, pattern FeeTooSmallUTxO,
                     pattern InputSetEmptyUTxO, pattern ValueNotConservedUTxO)
import           STS.Utxow (pattern InvalidWitnessesUTXOW, pattern MissingScriptWitnessesUTXOW,
                     pattern MissingVKeyWitnessesUTXOW, PredicateFailure (..))
import           Tx (pattern Tx, pattern TxBody, pattern TxOut, body)
import           TxData (pattern AddrBase, pattern DeRegKey, pattern Delegate, pattern Delegation,
                     pattern KeyHashObj, pattern RegKey, pattern RetirePool, StakeCreds (..))
import           Updates
import           UTxO (pattern UTxO, balance, makeWitnessVKey)
import           Validation (ValidationError (..), Validity (..))
import           Test.Utils

import           MockTypes
import           Mutator

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: Addr -> KeyPairs -> KeyPair
findPayKeyPair (AddrBase (KeyHashObj addr) _) keyList =
    case matches of
      []    -> error "findPayKeyPair: could not find a match for the given address"
      (x:_) -> fst x
    where
      matches = filter (\(pay, _) -> addr == hashKey (vKey pay)) keyList
findPayKeyPair _ _ = error "findPayKeyPair: expects only AddrBase addresses"

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

-- | Generator for a natural number between 'lower' and 'upper'
genWord64 :: Word64 -> Word64 -> Gen Word64
genWord64 lower upper = Gen.integral $ Range.linear lower upper

-- | Returns the number of entries of the UTxO set.
utxoSize :: UTxO -> Int
utxoSize (UTxO m) = Map.size m

-- | Extract the map in an 'UTxO'.
utxoMap :: UTxO -> Map TxIn TxOut
utxoMap (UTxO m) = m

-- | Generates a list of '(pay, stake)' key pairs.
genKeyPairs :: Int -> Int -> Gen KeyPairs
genKeyPairs lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.linear (1 :: Natural) 1000)
  return
    $ fmap
      (\n ->
         ( KeyPair (fromIntegral $ 2*n) (fromIntegral $ 2*n)
         , KeyPair (fromIntegral $ 2*n+1) (fromIntegral $ 2*n+1)
         )
      )
      xs

-- | Hashes all pairs of pay, stake key pairs of a list into a list of pairs of
-- hashed keys
hashKeyPairs :: KeyPairs -> [(Credential, Credential)]
hashKeyPairs keyPairs =
    (\(a, b) -> (KeyHashObj . hashKey $ vKey a, KeyHashObj . hashKey $ vKey b)) <$> keyPairs

-- | Transforms list of keypairs into 'Addr' types of the form 'AddrTxin pay
-- stake'
addrTxins :: KeyPairs -> [Addr]
addrTxins keyPairs = uncurry AddrBase <$> hashKeyPairs keyPairs

-- | Generator for List of 'Coin' values. Generates between 'lower' and 'upper'
-- coins, with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.exponential minCoin maxCoin)
  return (Coin <$> xs)

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Coin
-- value is generated.
genTxOut :: [Addr] -> Gen [TxOut]
genTxOut addrs = do
  ys <- genCoinList 100 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- TODO generate sensible protocol constants
defPCs :: PParams
defPCs = emptyPParams

-- | Generator of a non-empty genesis ledger state, i.e., at least one valid
-- address and non-zero UTxO.
genNonemptyGenesisState :: Gen LedgerState
genNonemptyGenesisState = do
  keyPairs <- genKeyPairs 1 10
  (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)

-- | Generator for a new 'Tx' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- coins, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTx :: KeyPairs -> UTxO -> SlotNo -> Gen (Coin, Tx)
genTx keyList (UTxO m) cslot = do
  -- select payer
  selectedInputs <- Gen.shuffle utxoInputs
  let !selectedAddr    = addr $ head selectedInputs
  let !selectedUTxO    = Map.filter (\(TxOut a _) -> a == selectedAddr) m
  let !selectedKeyPair = findPayKeyPair selectedAddr keyList
  let !selectedBalance = balance $ UTxO selectedUTxO

  -- select receipients, distribute balance of selected UTxO set
  n <- genNatural 1 10 -- (fromIntegral $ length keyList) -- TODO make this variable, but uses too much RAM atm
  receipients <- take (fromIntegral n) <$> Gen.shuffle keyList
  let realN                = length receipients
  let (perReceipient, txfee') = splitCoin selectedBalance (fromIntegral realN)
  let !receipientAddrs      = fmap
          (\(p, d) -> AddrBase (KeyHashObj . hashKey $ vKey p) (KeyHashObj . hashKey $ vKey d)) receipients
  txttl <- genWord64 1 100
  let !txbody = TxBody
           (Map.keysSet selectedUTxO)
           ((`TxOut` perReceipient) <$> receipientAddrs)
           Empty
           Map.empty -- TODO generate witdrawals
           txfee'
           (cslot + SlotNo txttl)
           emptyUpdate
  let !txwit = makeWitnessVKey txbody selectedKeyPair
  pure (txfee', Tx txbody (Set.fromList [txwit]) Map.empty)
            where utxoInputs = Map.keys m
                  addr inp   = getTxOutAddr $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx :: KeyPairs -> SlotNo -> LedgerState ->
                    Gen (Coin, Tx, Either [ValidationError] LedgerState)
genLedgerStateTx keyList (SlotNo _slot) sourceState = do
  let utxo' = sourceState ^. utxoState . utxo
  slot' <- genWord64 _slot (_slot + 100)
  (txfee', tx) <- genTx keyList utxo' (SlotNo slot')
  pure (txfee', tx, asStateTransition (SlotNo slot') defPCs sourceState tx (Coin 0))

-- | Generator of a non-emtpy ledger genesis state and a random number of
-- transactions applied to it. Returns the amount of accumulated fees, the
-- initial ledger state and the final ledger state or the validation error if an
-- invalid transaction has been generated.
genNonEmptyAndAdvanceTx
  :: Gen (KeyPairs, Natural, Coin, LedgerState, [Tx], Either [ValidationError] LedgerState)
genNonEmptyAndAdvanceTx = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, ls') <- repeatCollectTx steps keyPairs (SlotNo 1) (Coin 0) ls []
  pure (keyPairs, steps, fees, ls, txs, ls')

-- | Mutated variant of above, collects validation errors in 'LedgerValidation'.
genNonEmptyAndAdvanceTx'
  :: Gen (KeyPairs, Natural, Coin, LedgerState, [Tx], LedgerValidation)
genNonEmptyAndAdvanceTx' = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, lv') <- repeatCollectTx' steps keyPairs (Coin 0) ls [] []
  pure (keyPairs, steps, fees, ls, txs, lv')

-- | Generator for a fixed number of 'n' transaction step executions, using the
-- list of pairs of key pairs, the 'fees' coin accumulator, initial ledger state
-- 'ls' and returns the result of the repeated generation and application of
-- transactions.
repeatCollectTx
    :: Natural
    -> KeyPairs
    -> SlotNo
    -> Coin
    -> LedgerState
    -> [Tx]
    -> Gen (Coin, [Tx], Either [ValidationError] LedgerState)
repeatCollectTx 0 _ _ fees ls txs = pure (fees, reverse txs, Right ls)
repeatCollectTx n keyPairs (SlotNo _slot) fees ls txs = do
  (txfee', tx, next) <- genLedgerStateTx keyPairs (SlotNo _slot) ls
  case next of
    Left _    -> pure (fees, txs, next)
    Right ls' -> repeatCollectTx (n - 1) keyPairs (SlotNo $ _slot + 1) (txfee' + fees) ls' (tx:txs)

-- | Mutated variant of `repeatCollectTx'`, stops at recursion depth or after
-- exhausting the UTxO set to prevent calling 'head' on empty input list.
repeatCollectTx'
    :: Natural
    -> KeyPairs
    -> Coin
    -> LedgerState
    -> [Tx]
    -> [ValidationError]
    -> Gen (Coin, [Tx], LedgerValidation)
repeatCollectTx' n keyPairs fees ls txs validationErrors
 | n == 0 || (utxoSize $ ls ^. utxoState . utxo) == 0 =
     pure (fees, reverse txs, LedgerValidation validationErrors ls)
 | otherwise = do
    (txfee', tx, LedgerValidation errors' ls') <- genLedgerStateTx' keyPairs ls
    repeatCollectTx' (n - 1) keyPairs (txfee' + fees) ls' (tx:txs) (validationErrors ++ errors')

-- | Find first matching key pair for stake key in 'AddrTxin'.
findStakeKeyPair :: StakeCredential -> KeyPairs -> KeyPair
findStakeKeyPair (KeyHashObj hk) keyList =
    snd $ head $ filter (\(_, stake) -> hk == hashKey (vKey stake)) keyList
findStakeKeyPair _ _ = undefined -- TODO treat script case

-- | Returns the hashed 'addr' part of a 'TxOut'.
getTxOutAddr :: TxOut -> Addr
getTxOutAddr (TxOut addr _) = addr

-- | Generator for arbitrary valid ledger state, discarding any generated
-- invalid one.
genValidLedgerState :: Gen (KeyPairs, Natural, [Tx], LedgerState)
genValidLedgerState = do
  (keyPairs, steps, _, _, txs, newState) <- genNonEmptyAndAdvanceTx
  case newState of
    Left _   -> Gen.discard
    Right ls -> pure (keyPairs, steps, txs, ls)

genValidSuccessorState :: KeyPairs -> SlotNo -> LedgerState ->
  Gen (Coin, Tx, LedgerState)
genValidSuccessorState keyPairs _slot sourceState = do
  (txfee', entry, next) <- genLedgerStateTx keyPairs _slot sourceState
  case next of
    Left _   -> Gen.discard
    Right ls -> pure (txfee', entry, ls)

genValidStateTx :: Gen (LedgerState, Natural, Coin, Tx, LedgerState)
genValidStateTx = do
  (ls, steps, txfee', entry, ls', _) <- genValidStateTxKeys
  pure (ls, steps, txfee', entry, ls')

genValidStateTxKeys :: Gen (LedgerState, Natural, Coin, Tx, LedgerState, KeyPairs)
genValidStateTxKeys = do
  (keyPairs, steps, _, ls) <- genValidLedgerState
  (txfee', entry, ls')     <- genValidSuccessorState keyPairs (SlotNo $ fromIntegral steps + 1) ls
  pure (ls, steps, txfee', entry, ls', keyPairs)

genStateTx :: Gen (LedgerState, Natural, Coin, Tx, LedgerValidation)
genStateTx = do
  (keyPairs, steps, _, ls) <- genValidLedgerState
  (txfee', entry, lv)      <- genLedgerStateTx' keyPairs ls
  pure (ls, steps, txfee', entry, lv)

genLedgerStateTx' :: KeyPairs -> LedgerState ->
                    Gen (Coin, Tx, LedgerValidation)
genLedgerStateTx' keyList sourceState = do
  let utxo' = sourceState ^. utxoState . utxo
  _slot <- genWord64 0 1000
  (txfee', tx) <- genTx keyList utxo' (SlotNo _slot)
  tx'          <- mutateTx tx
  pure (txfee'
       , tx'
       , asStateTransition' (SlotNo _slot) defPCs (LedgerValidation [] sourceState) tx' (Coin 0))

-- Generators for 'DelegationData'

genDelegationData :: KeyPairs -> EpochNo -> Gen DCert
genDelegationData keys epoch =
    Gen.choice [ genDCertRegKey keys
               , genDCertDeRegKey keys
               , genDCertRetirePool keys epoch]

genDCertRegKey :: KeyPairs -> Gen DCert
genDCertRegKey keys =
  RegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertDeRegKey :: KeyPairs -> Gen DCert
genDCertDeRegKey keys =
    DeRegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertRetirePool :: KeyPairs -> EpochNo -> Gen DCert
genDCertRetirePool keys epoch = do
  key <- getAnyStakeKey keys
  pure $ RetirePool (hashKey key) epoch

genDelegation :: KeyPairs -> DPState -> Gen Delegation
genDelegation keys d = do
  poolKey      <- Gen.element $ Map.keys stkCreds'
  delegatorKey <- getAnyStakeKey keys
  pure $ Delegation (KeyHashObj $ hashKey delegatorKey) $ (hashKey $ vKey $ findStakeKeyPair poolKey keys)
       where (StakeCreds stkCreds') = d ^. dstate . stkCreds

genDCertDelegate :: KeyPairs -> DPState -> Gen DCert
genDCertDelegate keys ds = Delegate <$> genDelegation keys ds

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: SlotNo
  -> PParams
  -> LedgerState
  -> Tx
  -> Coin
  -> Either [ValidationError] LedgerState
asStateTransition _slot pp ls tx res =
  let next = runShelleyBase $ applySTS @LEDGER
              (TRC ((LedgerEnv _slot (_txSlotIx ls) pp res)
              , (_utxoState ls, _delegationState ls)
              , tx))
  in
  case next of
    Left pfs -> Left $ convertPredicateFailuresToValidationErrors pfs
    Right (u, d)  -> Right $ ls { _utxoState = u
                                , _delegationState = d
                                , _txSlotIx = 1 + _txSlotIx ls
                                }

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  -- :: ( Crypto crypto
  --    , Signable (DSIGN crypto) (TxBody crypto)
  --    )
  -- =>
  :: SlotNo
  -> PParams
  -> LedgerValidation
  -> Tx
  -> Coin
  -> LedgerValidation
asStateTransition' _slot pp (LedgerValidation valErrors ls) tx _ =
    let ls' = applyTxBody ls pp (tx ^. body)
        d'  = (_genDelegs . _dstate . _delegationState) ls
    in
    case validTx tx d' _slot pp ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

convertPredicateFailuresToValidationErrors :: [[PredicateFailure LEDGER]] -> [ValidationError]
convertPredicateFailuresToValidationErrors pfs =
  map predicateFailureToValidationError $ foldr (++) [] pfs

predicateFailureToValidationError :: PredicateFailure LEDGER -> ValidationError

predicateFailureToValidationError (UtxowFailure (MissingVKeyWitnessesUTXOW))
  = MissingWitnesses
predicateFailureToValidationError (UtxowFailure (MissingScriptWitnessesUTXOW))
  = MissingWitnesses

predicateFailureToValidationError (UtxowFailure (InvalidWitnessesUTXOW))
  = InvalidWitness

predicateFailureToValidationError (UtxowFailure (UtxoFailure InputSetEmptyUTxO))
  = InputSetEmpty

predicateFailureToValidationError (UtxowFailure (UtxoFailure (ExpiredUTxO a b)))
  = Expired a b

predicateFailureToValidationError (UtxowFailure (UtxoFailure BadInputsUTxO))
  = BadInputs

predicateFailureToValidationError (UtxowFailure (UtxoFailure (FeeTooSmallUTxO a b)))
  = FeeTooSmall a b

predicateFailureToValidationError (UtxowFailure (UtxoFailure (ValueNotConservedUTxO a b)))
  = ValueNotConserved a b

predicateFailureToValidationError (DelegsFailure DelegateeNotRegisteredDELEG)
  = StakeDelegationImpossible

predicateFailureToValidationError (DelegsFailure WithrawalsNotInRewardsDELEGS)
  = IncorrectRewards

predicateFailureToValidationError _ = UnknownValidationError
