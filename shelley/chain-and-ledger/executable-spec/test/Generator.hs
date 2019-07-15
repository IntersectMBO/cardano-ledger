{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Generator
    (
      utxoSize
    , utxoMap
    , genBool
    , genNatural
    , genNonEmptyAndAdvanceTx
    , genNonEmptyAndAdvanceTx'
    , genNonemptyGenesisState
    , genStateTx
    , genValidStateTx
    , genValidStateTxKeys
    , genDelegationData
    , genDelegation
    , genStakePool
    , genDCertRegPool
    , genDCertDelegate
    , genKeyPairs
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.Ratio

import           Lens.Micro              ((^.))

import           Numeric.Natural

import           Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range

import           Address (pattern AddrTxin)
import           BaseTypes
import           Coin
import           Keys (pattern KeyPair, hashKey, vKey)
import           LedgerState (pattern LedgerValidation, ValidationError (..),
                     _delegationState, _dstate, asStateTransition,
                     asStateTransition', genesisState, DState(..), utxoState,
                     utxo, dstate, stKeys)
import           Slot
import           Updates
import           Tx(pattern Tx, pattern TxBody, pattern TxOut)
import           UTxO (pattern UTxO, balance, makeWitnessVKey)
import           PParams (PParams(..), emptyPParams)
import           Delegation.Certificates (pattern Delegate, pattern DeRegKey,
                     pattern RegKey, pattern RegPool, pattern RetirePool,
                     StakeKeys(..))
import           Delegation.PoolParams (pattern Delegation, pattern PoolParams,
                     RewardAcnt(..))

import           MockTypes
import           Mutator

-- | Returns the number of entries of the UTxO set.
utxoSize :: UTxO -> Int
utxoSize (UTxO m) = Map.size m

-- | Extract the map in an 'UTxO'.
utxoMap :: UTxO -> Map.Map TxIn TxOut
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
hashKeyPairs :: KeyPairs -> [(KeyHash, KeyHash)]
hashKeyPairs keyPairs =
    (\(a, b) -> (hashKey $ vKey a, hashKey $ vKey b)) <$> keyPairs

-- | Transforms list of keypairs into 'Addr' types of the form 'AddrTxin pay
-- stake'
addrTxins :: KeyPairs -> [Addr]
addrTxins keyPairs = uncurry AddrTxin <$> hashKeyPairs keyPairs

genBool :: Gen Bool
genBool = Gen.enumBounded

-- | Generator for a natural number between 'lower' and 'upper'.
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

genInteger :: Integer -> Integer -> Gen Integer
genInteger lower upper = Gen.integral $ Range.linear lower upper

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
  (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)

-- | Generator for a new 'Tx' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- coins, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTx :: KeyPairs -> UTxO -> Slot -> Gen (Coin, Tx)
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
          (\(p, d) -> AddrTxin (hashKey $ vKey p) (hashKey $ vKey d)) receipients
  txttl <- genNatural 1 100
  let !txbody = TxBody
           (Map.keysSet selectedUTxO)
           ((\r -> TxOut r perReceipient) <$> receipientAddrs)
           []
           Map.empty -- TODO generate witdrawals
           txfee'
           (cslot + (Slot txttl))
           emptyUpdate
  let !txwit = makeWitnessVKey txbody selectedKeyPair
  pure (txfee', Tx txbody (Set.fromList [txwit]) Map.empty)
            where utxoInputs = Map.keys m
                  addr inp   = getTxOutAddr $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx :: KeyPairs -> Slot -> LedgerState ->
                    Gen (Coin, Tx, Either [ValidationError] LedgerState)
genLedgerStateTx keyList (Slot _slot) sourceState = do
  let utxo' = sourceState ^. utxoState . utxo
  let dms' = _dms $ _dstate $ _delegationState sourceState
  slot' <- genNatural _slot (_slot + 100)
  (txfee', tx) <- genTx keyList utxo' (Slot slot')
  pure (txfee', tx, asStateTransition (Slot slot') defPCs sourceState tx dms')

-- | Generator of a non-emtpy ledger genesis state and a random number of
-- transactions applied to it. Returns the amount of accumulated fees, the
-- initial ledger state and the final ledger state or the validation error if an
-- invalid transaction has been generated.
genNonEmptyAndAdvanceTx
  :: Gen (KeyPairs, Natural, Coin, LedgerState, [Tx], Either [ValidationError] LedgerState)
genNonEmptyAndAdvanceTx = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, ls') <- repeatCollectTx steps keyPairs (Slot 1) (Coin 0) ls []
  pure (keyPairs, steps, fees, ls, txs, ls')

-- | Mutated variant of above, collects validation errors in 'LedgerValidation'.
genNonEmptyAndAdvanceTx'
  :: Gen (KeyPairs, Natural, Coin, LedgerState, [Tx], LedgerValidation)
genNonEmptyAndAdvanceTx' = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, lv') <- repeatCollectTx' steps keyPairs (Coin 0) ls [] []
  pure (keyPairs, steps, fees, ls, txs, lv')

-- | Generator for a fixed number of 'n' transaction step executions, using the
-- list of pairs of key pairs, the 'fees' coin accumulator, initial ledger state
-- 'ls' and returns the result of the repeated generation and application of
-- transactions.
repeatCollectTx
    :: Natural
    -> KeyPairs
    -> Slot
    -> Coin
    -> LedgerState
    -> [Tx]
    -> Gen (Coin, [Tx], Either [ValidationError] LedgerState)
repeatCollectTx 0 _ _ fees ls txs = pure (fees, reverse txs, Right ls)
repeatCollectTx n keyPairs (Slot _slot) fees ls txs = do
  (txfee', tx, next) <- genLedgerStateTx keyPairs (Slot _slot) ls
  case next of
    Left _    -> pure (fees, txs, next)
    Right ls' -> repeatCollectTx (n - 1) keyPairs (Slot $ _slot + 1) (txfee' + fees) ls' (tx:txs)

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

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: Addr -> KeyPairs -> KeyPair
findPayKeyPair (AddrTxin addr _) keyList =
    fst $ head $ filter (\(pay, _) -> addr == (hashKey $ vKey pay)) keyList
findPayKeyPair _ _ = error "currently no such keys should be generated"

-- | Find first matching key pair for stake key in 'AddrTxin'.
findStakeKeyPair :: KeyHash -> KeyPairs -> KeyPair
findStakeKeyPair addr keyList =
    snd $ head $ filter (\(_, stake) -> addr == (hashKey $ vKey stake)) keyList

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

genValidSuccessorState :: KeyPairs -> Slot -> LedgerState ->
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
  (txfee', entry, ls')     <- genValidSuccessorState keyPairs (Slot $ steps + 1) ls
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
  let dms' = _dms $ _dstate $ _delegationState sourceState
  _slot <- genNatural 0 1000
  (txfee', tx) <- genTx keyList utxo' (Slot _slot)
  tx'          <- mutateTx tx
  pure (txfee'
       , tx'
       , asStateTransition' (Slot _slot) defPCs (LedgerValidation [] sourceState) tx' dms')

-- Generators for 'DelegationData'

genDelegationData :: KeyPairs -> Epoch -> Gen DCert
genDelegationData keys epoch =
    Gen.choice [ genDCertRegKey keys
               , genDCertDeRegKey keys
               , genDCertRetirePool keys epoch]

genDCertRegKey :: KeyPairs -> Gen DCert
genDCertRegKey keys =
  RegKey <$> getAnyStakeKey keys

genDCertDeRegKey :: KeyPairs -> Gen DCert
genDCertDeRegKey keys =
    DeRegKey <$> getAnyStakeKey keys

genDCertRetirePool :: KeyPairs -> Epoch -> Gen DCert
genDCertRetirePool keys epoch = do
  key <- getAnyStakeKey keys
  pure $ RetirePool key epoch

genStakePool :: KeyPairs -> Gen PoolParams
genStakePool keys = do
  poolKey       <- getAnyStakeKey keys
  cost          <- Coin <$> genInteger 1 100
  pledge        <- Coin <$> genInteger 1 100
  marginPercent <- genNatural 0 100
  acntKey       <- getAnyStakeKey keys
  let interval = case mkUnitInterval $ fromIntegral marginPercent % 100 of
                   Just i  -> i
                   Nothing -> interval0
  pure $ PoolParams poolKey pledge Map.empty cost interval Nothing (RewardAcnt $ hashKey acntKey) Set.empty

genDelegation :: KeyPairs -> DPState -> Gen Delegation
genDelegation keys d = do
  poolKey      <- Gen.element $ Map.keys stKeys'
  delegatorKey <- getAnyStakeKey keys
  pure $ Delegation delegatorKey $ (vKey $ findStakeKeyPair poolKey keys)
       where (StakeKeys stKeys') = d ^. dstate . stKeys

genDCertRegPool :: KeyPairs -> Gen DCert
genDCertRegPool keys = RegPool <$> genStakePool keys

genDCertDelegate :: KeyPairs -> DPState -> Gen DCert
genDCertDelegate keys ds = Delegate <$> genDelegation keys ds
