{-# LANGUAGE BangPatterns #-}

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
    , genDelegationData
    , genDelegation
    , genStakePool
    , genDCertRegPool
    , genDCertDelegate
    ) where

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import Data.Ratio

import           Numeric.Natural

import           Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range

import           Lovelace
import           Keys
import           LedgerState     (LedgerState (..),
                                  LedgerValidation(..),
                                  ValidationError (..), asStateTransition,
                                  asStateTransition',
                                  genesisState, DelegationState(..),
                                  KeyPairs
                                 )
import           Slot
import           UTxO
import           PrtlConsts              (PrtlConsts(..))
import           Delegation.Certificates  (DCert(..))
import           Delegation.StakePool  (StakePool(..), Delegation(..))

import           Mutator

-- | Returns the number of entries of the UTxO set.
utxoSize :: UTxO -> Int
utxoSize (UTxO m) = Map.size m

-- | Extract the map in an 'UTxO'.
utxoMap :: UTxO -> Map.Map TxIn TxOut
utxoMap (UTxO m) = m

-- | Generator for '(Owner, Owner)' pairs, 'fst even', 'snd' is 'fst + 1'
genOwnerList :: Int -> Int -> Gen [(Owner, Owner)]
genOwnerList lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.linear (1 :: Natural) 1000)
  return $ fmap (\n -> (Owner $ 2*n, Owner $2*n+1)) xs

-- | Generates a list of '(pay, stake)' key pairs.
genKeyPairs :: Int -> Int -> Gen KeyPairs
genKeyPairs lower upper =
    fmap (\(a, b) -> (keyPair a, keyPair b))
             <$> genOwnerList lower upper

-- | Hashes all pairs of pay, stake key pairs of a list into a list of pairs of
-- hashed keys
hashKeyPairs :: KeyPairs -> [(HashKey, HashKey)]
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

-- | Generator for List of 'Lovelace' values. Generates between 'lower' and 'upper'
-- lovelace, with values between 'minLovelace' and 'maxLovelace'.
genLovelaceList :: Natural -> Natural -> Int -> Int -> Gen [Lovelace]
genLovelaceList minLovelace maxLovelace lower upper = do
  xs <- Gen.list (Range.linear lower upper)
        $ Gen.integral (Range.exponential minLovelace maxLovelace)
  return (Lovelace <$> xs)

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Lovelace
-- value is generated.
genTxOut :: [Addr] -> Gen [TxOut]
genTxOut addrs = do
  ys <- genLovelaceList 100 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- TODO generate sensible protocol constants
defPCs :: PrtlConsts
defPCs = PrtlConsts 0 0 100 100 0 0

-- | Generator of a non-empty genesis ledger state, i.e., at least one valid
-- address and non-zero UTxO.
genNonemptyGenesisState :: Gen LedgerState
genNonemptyGenesisState = do
  keyPairs <- genKeyPairs 1 10
  (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)

-- | Generator for a new 'TxWits' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- lovelace, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTxWits :: KeyPairs -> UTxO -> Slot -> Gen (Lovelace, TxWits)
genTxWits keyList (UTxO m) cslot = do
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
  let (perReceipient, txfee) = splitLovelace selectedBalance (fromIntegral realN)
  let !receipientAddrs      = fmap
          (\(p, d) -> AddrTxin (hashKey $ vKey p) (hashKey $ vKey d)) receipients
  txttl <- genNatural 1 100
  let !txbody = Tx
           (Map.keysSet selectedUTxO)
           ((\r -> TxOut r perReceipient) <$> receipientAddrs)
           Set.empty
           txfee
           (cslot + (Slot txttl))
  let !txwit = makeWitness selectedKeyPair txbody
  pure (txfee, TxWits txbody $ Set.fromList [txwit])
            where utxoInputs = Map.keys m
                  addr inp   = getTxOutAddr $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx :: KeyPairs -> LedgerState ->
                    Gen (Lovelace, TxWits, Either [ValidationError] LedgerState)
genLedgerStateTx keyList sourceState = do
  let utxo = getUtxo sourceState
  slot <- genNatural 0 1000
  (txfee, tx) <- genTxWits keyList utxo (Slot slot)
  pure (txfee, tx, asStateTransition (Slot slot) sourceState tx)

-- | Generator of a non-emtpy ledger genesis state and a random number of
-- transactions applied to it. Returns the amount of accumulated fees, the
-- initial ledger state and the final ledger state or the validation error if an
-- invalid transaction has been generated.
genNonEmptyAndAdvanceTx
  :: Gen (KeyPairs, Natural, Lovelace, LedgerState, [TxWits], Either [ValidationError] LedgerState)
genNonEmptyAndAdvanceTx = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, ls') <- repeatCollectTx steps keyPairs (Lovelace 0) ls []
  pure (keyPairs, steps, fees, ls, txs, ls')

-- | Mutated variant of above, collects validation errors in 'LedgerValidation'.
genNonEmptyAndAdvanceTx'
  :: Gen (KeyPairs, Natural, Lovelace, LedgerState, [TxWits], LedgerValidation)
genNonEmptyAndAdvanceTx' = do
  keyPairs    <- genKeyPairs 1 10
  steps       <- genNatural 1 10
  ls          <- (genesisState defPCs) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, lv') <- repeatCollectTx' steps keyPairs (Lovelace 0) ls [] []
  pure (keyPairs, steps, fees, ls, txs, lv')

-- | Generator for a fixed number of 'n' transaction step executions, using the
-- list of pairs of key pairs, the 'fees' lovelace accumulator, initial ledger state
-- 'ls' and returns the result of the repeated generation and application of
-- transactions.
repeatCollectTx
    :: Natural
    -> KeyPairs
    -> Lovelace
    -> LedgerState
    -> [TxWits]
    -> Gen (Lovelace, [TxWits], Either [ValidationError] LedgerState)
repeatCollectTx 0 _ fees ls txs = pure (fees, reverse txs, Right ls)
repeatCollectTx n keyPairs fees ls txs = do
  (txfee, tx, next) <- genLedgerStateTx keyPairs ls
  case next of
    Left _    -> pure (fees, txs, next)
    Right ls' -> repeatCollectTx (n - 1) keyPairs (txfee <> fees) ls' (tx:txs)

-- | Mutated variant of `repeatCollectTx'`, stops at recursion depth or after
-- exhausting the UTxO set to prevent calling 'head' on empty input list.
repeatCollectTx'
    :: Natural
    -> KeyPairs
    -> Lovelace
    -> LedgerState
    -> [TxWits]
    -> [ValidationError]
    -> Gen (Lovelace, [TxWits], LedgerValidation)
repeatCollectTx' n keyPairs fees ls txs validationErrors
 | n == 0 || (utxoSize $ getUtxo ls) == 0 =
     pure (fees, reverse txs, LedgerValidation validationErrors ls)
 | otherwise = do
    (txfee, tx, LedgerValidation errors' ls') <- genLedgerStateTx' keyPairs ls
    repeatCollectTx' (n - 1) keyPairs (txfee <> fees) ls' (tx:txs) (validationErrors ++ errors')

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: Addr -> KeyPairs -> KeyPair
findPayKeyPair (AddrTxin addr _) keyList =
    fst $ head $ filter (\(pay, _) -> addr == (hashKey $ vKey pay)) keyList

-- | Find first matching key pair for stake key in 'AddrTxin'.
findStakeKeyPair :: HashKey -> KeyPairs -> KeyPair
findStakeKeyPair addr keyList =
    snd $ head $ filter (\(_, stake) -> addr == (hashKey $ vKey stake)) keyList

-- | Returns the hashed 'addr' part of a 'TxOut'.
getTxOutAddr :: TxOut -> Addr
getTxOutAddr (TxOut addr _) = addr

-- | Generator for arbitrary valid ledger state, discarding any generated
-- invalid one.
genValidLedgerState :: Gen (KeyPairs, Natural, [TxWits], LedgerState)
genValidLedgerState = do
  (keyPairs, steps, _, _, txs, newState) <- genNonEmptyAndAdvanceTx
  case newState of
    Left _   -> Gen.discard
    Right ls -> pure (keyPairs, steps, txs, ls)

genValidSuccessorState :: KeyPairs -> LedgerState ->
  Gen (Lovelace, TxWits, LedgerState)
genValidSuccessorState keyPairs sourceState = do
  (txfee, entry, next) <- genLedgerStateTx keyPairs sourceState
  case next of
    Left _   -> Gen.discard
    Right ls -> pure (txfee, entry, ls)

genValidStateTx :: Gen (LedgerState, Natural, Lovelace, TxWits, LedgerState)
genValidStateTx = do
  (keyPairs, steps, _, ls) <- genValidLedgerState
  (txfee, entry, ls')        <- genValidSuccessorState keyPairs ls
  pure (ls, steps, txfee, entry, ls')

genStateTx :: Gen (LedgerState, Natural, Lovelace, TxWits, LedgerValidation)
genStateTx = do
  (keyPairs, steps, _, ls) <- genValidLedgerState
  (txfee, entry, lv)         <- genLedgerStateTx' keyPairs ls
  pure (ls, steps, txfee, entry, lv)

genLedgerStateTx' :: KeyPairs -> LedgerState ->
                    Gen (Lovelace, TxWits, LedgerValidation)
genLedgerStateTx' keyList sourceState = do
  let utxo = getUtxo sourceState
  slot <- genNatural 0 1000
  (txfee, tx) <- genTxWits keyList utxo (Slot slot)
  tx'       <- mutateTxWits tx
  pure (txfee
       , tx'
       , asStateTransition' (Slot slot) (LedgerValidation [] sourceState) tx')

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

genStakePool :: KeyPairs -> Gen StakePool
genStakePool keys = do
  poolKey       <- getAnyStakeKey keys
  cost          <- Lovelace <$> genNatural 1 100
  marginPercent <- genNatural 0 100
  pure $ StakePool poolKey Map.empty cost (marginPercent % 100) Nothing

genDelegation :: KeyPairs -> DelegationState -> Gen Delegation
genDelegation keys dstate = do
  poolKey      <- Gen.element (Map.keys $ getStKeys dstate)
  delegatorKey <- getAnyStakeKey keys
  pure $ Delegation delegatorKey $ (vKey $ findStakeKeyPair poolKey keys)

genDCertRegPool :: KeyPairs -> Gen DCert
genDCertRegPool keys = RegPool <$> genStakePool keys

genDCertDelegate :: KeyPairs -> DelegationState -> Gen DCert
genDCertDelegate keys dstate = Delegate <$> genDelegation keys dstate
