{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.NonTraceProperties.Generator
  ( utxoSize,
    utxoMap,
    genNonEmptyAndAdvanceTx,
    genNonEmptyAndAdvanceTx',
    genNonemptyGenesisState,
    genStateTx,
    genValidStateTx,
    genValidStateTxKeys,
    genDelegationData,
    genDelegation,
    genDCertDelegate,
    genKeyPairs,
    asStateTransition,
    asStateTransition',
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural
import Shelley.Spec.Ledger.Address (pattern Addr)
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential (pattern KeyHashObj, pattern StakeRefBase)
import Shelley.Spec.Ledger.Keys (KeyRole (..), hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    _delegationState,
    _dstate,
    _genDelegs,
    _stkCreds,
    _utxo,
    _utxoState,
    applyTxBody,
    genesisCoins,
    genesisState,
  )
import Shelley.Spec.Ledger.PParams (PParams, emptyPParams)
import Shelley.Spec.Ledger.STS.Delegs
  ( PredicateFailure (..),
    pattern DelegateeNotRegisteredDELEG,
    pattern WithdrawalsNotInRewardsDELEGS,
  )
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..), PredicateFailure (..))
import Shelley.Spec.Ledger.STS.Utxo
  ( pattern BadInputsUTxO,
    pattern ExpiredUTxO,
    pattern FeeTooSmallUTxO,
    pattern InputSetEmptyUTxO,
    pattern ValueNotConservedUTxO,
  )
import Shelley.Spec.Ledger.STS.Utxow
  ( PredicateFailure (..),
    pattern InvalidWitnessesUTXOW,
    pattern MissingScriptWitnessesUTXOW,
    pattern MissingVKeyWitnessesUTXOW,
  )
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
  ( WitnessSetHKD (..),
    _body,
    pattern Tx,
    pattern TxBody,
    pattern TxOut,
  )
import Shelley.Spec.Ledger.TxData
  ( StakeCreds (..),
    Wdrl (..),
    pattern DCertDeleg,
    pattern DCertPool,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
    pattern RegKey,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.UTxO (balance, hashTxBody, makeWitnessVKey, pattern UTxO)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Shelley.Spec.Ledger.NonTraceProperties.Mutator
import Test.Shelley.Spec.Ledger.NonTraceProperties.Validity
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
import Unsafe.Coerce

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: HashAlgorithm h => Addr h -> KeyPairs h -> KeyPair h 'Payment
findPayKeyPair (Addr _ (KeyHashObj addr) _) keyList =
  case matches of
    [] -> error "findPayKeyPair: could not find a match for the given address"
    (x : _) -> fst x
  where
    matches = filter (\(pay, _) -> addr == hashKey (vKey pay)) keyList
findPayKeyPair _ _ = error "findPayKeyPair: expects only KeyHash addresses"

-- | Generator for a natural number between 'lower' and 'upper'
genNatural :: Natural -> Natural -> Gen Natural
genNatural lower upper = Gen.integral $ Range.linear lower upper

-- | Generator for a natural number between 'lower' and 'upper'
genWord64 :: Word64 -> Word64 -> Gen Word64
genWord64 lower upper = Gen.integral $ Range.linear lower upper

-- | Returns the number of entries of the UTxO set.
utxoSize :: UTxO h -> Int
utxoSize (UTxO m) = Map.size m

-- | Extract the map in an 'UTxO'.
utxoMap :: UTxO h -> Map (TxIn h) (TxOut h)
utxoMap (UTxO m) = m

-- | Generates a list of '(pay, stake)' key pairs.
genKeyPairs :: Int -> Int -> Gen (KeyPairs h)
genKeyPairs lower upper = do
  xs <-
    Gen.list (Range.linear lower upper) $
      Gen.integral (Range.linear (1 :: Natural) 1000)
  return $
    fmap
      ( \n ->
          ( KeyPair (fromIntegral $ 2 * n) (fromIntegral $ 2 * n),
            KeyPair (fromIntegral $ 2 * n + 1) (fromIntegral $ 2 * n + 1)
          )
      )
      xs

-- | Hashes all pairs of pay, stake key pairs of a list into a list of pairs of
-- hashed keys
hashKeyPairs :: HashAlgorithm h => KeyPairs h -> [(Credential h 'Payment, StakeReference h)]
hashKeyPairs keyPairs =
  ( \(a, b) ->
      ( KeyHashObj . hashKey $ vKey a,
        StakeRefBase . KeyHashObj . hashKey $ vKey b
      )
  )
    <$> keyPairs

-- | Transforms list of keypairs into 'Addr' types of the form 'AddrTxin pay
-- stake'
addrTxins :: HashAlgorithm h => KeyPairs h -> [Addr h]
addrTxins keyPairs = uncurry (Addr Testnet) <$> hashKeyPairs keyPairs

-- | Generator for List of 'Coin' values. Generates between 'lower' and 'upper'
-- coins, with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  xs <-
    Gen.list (Range.linear lower upper) $
      Gen.integral (Range.exponential minCoin maxCoin)
  return (Coin <$> xs)

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Coin
-- value is generated.
genTxOut :: [Addr h] -> Gen [TxOut h]
genTxOut addrs = do
  ys <- genCoinList 100 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- TODO generate sensible protocol constants
defPCs :: PParams
defPCs = emptyPParams

-- | Generator of a non-empty genesis ledger state, i.e., at least one valid
-- address and non-zero UTxO.
genNonemptyGenesisState :: HashAlgorithm h => proxy h -> Gen (LedgerState h)
genNonemptyGenesisState _ = do
  keyPairs <- genKeyPairs 1 10
  (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)

-- | Generator for a new 'Tx' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- coins, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTx :: HashAlgorithm h => KeyPairs h -> UTxO h -> SlotNo -> Gen (Coin, Tx h)
genTx keyList (UTxO m) cslot = do
  -- select payer
  selectedInputs <- Gen.shuffle utxoInputs
  let !selectedAddr = addr $ head selectedInputs
  let !selectedUTxO = Map.filter (\(TxOut a _) -> a == selectedAddr) m
  let !selectedKeyPair = findPayKeyPair selectedAddr keyList
  let !selectedBalance = balance $ UTxO selectedUTxO

  -- select receipients, distribute balance of selected UTxO set
  n <- genNatural 1 10 -- (fromIntegral $ length keyList) -- TODO make this variable, but uses too much RAM atm
  receipients <- Seq.fromList . take (fromIntegral n) <$> Gen.shuffle keyList
  let realN = length receipients
  let (perReceipient, txfee') = splitCoin selectedBalance (fromIntegral realN)
  let !receipientAddrs =
        fmap
          ( \(p, d) ->
              Addr
                Testnet
                (KeyHashObj . hashKey $ vKey p)
                (StakeRefBase . KeyHashObj . hashKey $ vKey d)
          )
          receipients
  txttl <- genWord64 1 100
  let !txbody =
        TxBody
          (Map.keysSet selectedUTxO)
          (StrictSeq.toStrict ((`TxOut` perReceipient) <$> receipientAddrs))
          StrictSeq.Empty
          (Wdrl Map.empty) -- TODO generate witdrawals
          txfee'
          (cslot + SlotNo txttl)
          SNothing
          SNothing
  let !txbHash = hashTxBody txbody
  let !txwit = makeWitnessVKey txbHash selectedKeyPair
  pure (txfee', Tx txbody mempty {addrWits = Set.fromList [txwit]} SNothing)
  where
    utxoInputs = Map.keys m
    addr inp = getTxOutAddr $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx ::
  HashAlgorithm h =>
  KeyPairs h ->
  SlotNo ->
  LedgerState h ->
  Gen (Coin, Tx h, Either [ValidationError] (LedgerState h))
genLedgerStateTx keyList (SlotNo _slot) sourceState = do
  let utxo' = (_utxo . _utxoState) sourceState
  slot' <- genWord64 _slot (_slot + 100)
  (txfee', tx) <- genTx keyList utxo' (SlotNo slot')
  pure (txfee', tx, asStateTransition (SlotNo slot') defPCs sourceState tx (AccountState 0 0))

-- | Generator of a non-emtpy ledger genesis state and a random number of
-- transactions applied to it. Returns the amount of accumulated fees, the
-- initial ledger state and the final ledger state or the validation error if an
-- invalid transaction has been generated.
genNonEmptyAndAdvanceTx ::
  HashAlgorithm h => proxy h -> Gen (KeyPairs h, Natural, Coin, LedgerState h, [Tx h], Either [ValidationError] (LedgerState h))
genNonEmptyAndAdvanceTx _ = do
  keyPairs <- genKeyPairs 1 10
  steps <- genNatural 1 10
  ls <- (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, ls') <- repeatCollectTx steps keyPairs (SlotNo 1) (Coin 0) ls []
  pure (keyPairs, steps, fees, ls, txs, ls')

-- | Mutated variant of above, collects validation errors in 'LedgerValidation'.
genNonEmptyAndAdvanceTx' ::
  HashAlgorithm h => proxy h -> Gen (KeyPairs h, Natural, Coin, LedgerState h, [Tx h], LedgerValidation h)
genNonEmptyAndAdvanceTx' _ = do
  keyPairs <- genKeyPairs 1 10
  steps <- genNatural 1 10
  ls <- (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, lv') <- repeatCollectTx' steps keyPairs (Coin 0) ls [] []
  pure (keyPairs, steps, fees, ls, txs, lv')

-- | Generator for a fixed number of 'n' transaction step executions, using the
-- list of pairs of key pairs, the 'fees' coin accumulator, initial ledger state
-- 'ls' and returns the result of the repeated generation and application of
-- transactions.
repeatCollectTx ::
  HashAlgorithm h =>
  Natural ->
  KeyPairs h ->
  SlotNo ->
  Coin ->
  LedgerState h ->
  [Tx h] ->
  Gen (Coin, [Tx h], Either [ValidationError] (LedgerState h))
repeatCollectTx 0 _ _ fees ls txs = pure (fees, reverse txs, Right ls)
repeatCollectTx n keyPairs (SlotNo _slot) fees ls txs = do
  (txfee', tx, next) <- genLedgerStateTx keyPairs (SlotNo _slot) ls
  case next of
    Left _ -> pure (fees, txs, next)
    Right ls' -> repeatCollectTx (n - 1) keyPairs (SlotNo $ _slot + 1) (txfee' + fees) ls' (tx : txs)

-- | Mutated variant of `repeatCollectTx'`, stops at recursion depth or after
-- exhausting the UTxO set to prevent calling 'head' on empty input list.
repeatCollectTx' ::
  HashAlgorithm h =>
  Natural ->
  KeyPairs h ->
  Coin ->
  LedgerState h ->
  [Tx h] ->
  [ValidationError] ->
  Gen (Coin, [Tx h], LedgerValidation h)
repeatCollectTx' n keyPairs fees ls txs validationErrors
  | n == 0 || (utxoSize $ (_utxo . _utxoState) ls) == 0 =
    pure (fees, reverse txs, LedgerValidation validationErrors ls)
  | otherwise = do
    (txfee', tx, LedgerValidation errors' ls') <- genLedgerStateTx' keyPairs ls
    repeatCollectTx' (n - 1) keyPairs (txfee' + fees) ls' (tx : txs) (validationErrors ++ errors')

-- | Find first matching key pair for stake key in 'AddrTxin'.
findStakeKeyPair :: HashAlgorithm h => Credential h 'Staking -> KeyPairs h -> KeyPair h 'Staking
findStakeKeyPair (KeyHashObj hk) keyList =
  snd $ head $ filter (\(_, stake) -> hk == hashKey (vKey stake)) keyList
findStakeKeyPair _ _ = undefined -- TODO treat script case

-- | Returns the hashed 'addr' part of a 'TxOut'.
getTxOutAddr :: TxOut h -> Addr h
getTxOutAddr (TxOut addr _) = addr

-- | Generator for arbitrary valid ledger state, discarding any generated
-- invalid one.
genValidLedgerState :: HashAlgorithm h => proxy h -> Gen (KeyPairs h, Natural, [Tx h], LedgerState h)
genValidLedgerState p = do
  (keyPairs, steps, _, _, txs, newState) <- genNonEmptyAndAdvanceTx p
  case newState of
    Left _ -> Gen.discard
    Right ls -> pure (keyPairs, steps, txs, ls)

genValidSuccessorState ::
  HashAlgorithm h =>
  KeyPairs h ->
  SlotNo ->
  LedgerState h ->
  Gen (Coin, Tx h, LedgerState h)
genValidSuccessorState keyPairs _slot sourceState = do
  (txfee', entry, next) <- genLedgerStateTx keyPairs _slot sourceState
  case next of
    Left _ -> Gen.discard
    Right ls -> pure (txfee', entry, ls)

genValidStateTx :: HashAlgorithm h => proxy h -> Gen (LedgerState h, Natural, Coin, Tx h, LedgerState h)
genValidStateTx p = do
  (ls, steps, txfee', entry, ls', _) <- genValidStateTxKeys p
  pure (ls, steps, txfee', entry, ls')

genValidStateTxKeys :: HashAlgorithm h => proxy h -> Gen (LedgerState h, Natural, Coin, Tx h, LedgerState h, KeyPairs h)
genValidStateTxKeys p = do
  (keyPairs, steps, _, ls) <- genValidLedgerState p
  (txfee', entry, ls') <- genValidSuccessorState keyPairs (SlotNo $ fromIntegral steps + 1) ls
  pure (ls, steps, txfee', entry, ls', keyPairs)

genStateTx :: HashAlgorithm h => proxy h -> Gen (LedgerState h, Natural, Coin, Tx h, LedgerValidation h)
genStateTx p = do
  (keyPairs, steps, _, ls) <- genValidLedgerState p
  (txfee', entry, lv) <- genLedgerStateTx' keyPairs ls
  pure (ls, steps, txfee', entry, lv)

genLedgerStateTx' ::
  HashAlgorithm h =>
  KeyPairs h ->
  LedgerState h ->
  Gen (Coin, Tx h, LedgerValidation h)
genLedgerStateTx' keyList sourceState = do
  let utxo' = (_utxo . _utxoState) sourceState
  _slot <- genWord64 0 1000
  (txfee', tx) <- genTx keyList utxo' (SlotNo _slot)
  tx' <- mutateTx tx
  pure
    ( txfee',
      tx',
      asStateTransition' (SlotNo _slot) defPCs (LedgerValidation [] sourceState) tx' (AccountState 0 0)
    )

-- Generators for 'DelegationData'

genDelegationData :: HashAlgorithm h => KeyPairs h -> EpochNo -> Gen (DCert h)
genDelegationData keys epoch =
  Gen.choice
    [ genDCertRegKey keys,
      genDCertDeRegKey keys,
      genDCertRetirePool keys epoch
    ]

genDCertRegKey :: HashAlgorithm h => KeyPairs h -> Gen (DCert h)
genDCertRegKey keys =
  DCertDeleg . RegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertDeRegKey :: HashAlgorithm h => KeyPairs h -> Gen (DCert h)
genDCertDeRegKey keys =
  DCertDeleg . DeRegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertRetirePool :: HashAlgorithm h => KeyPairs h -> EpochNo -> Gen (DCert h)
genDCertRetirePool keys epoch = do
  key <- getAnyStakeKey keys
  pure $ DCertPool $ RetirePool (unsafeCoerce $ hashKey key) epoch

genDelegation :: HashAlgorithm h => KeyPairs h -> DPState h -> Gen (Delegation h)
genDelegation keys d = do
  poolKey <- Gen.element $ Map.keys stkCreds'
  delegatorKey <- getAnyStakeKey keys
  pure $ Delegation (KeyHashObj $ hashKey delegatorKey) $ (unsafeCoerce . hashKey $ vKey $ findStakeKeyPair poolKey keys)
  where
    (StakeCreds stkCreds') = (_stkCreds . _dstate) d

genDCertDelegate :: HashAlgorithm h => KeyPairs h -> DPState h -> Gen (DCert h)
genDCertDelegate keys ds = (DCertDeleg . Delegate) <$> genDelegation keys ds

-- | In the case where a transaction is valid for a given ledger state,
--  apply the transaction as a state transition function on the ledger state.
--  Otherwise, return a list of validation errors.
asStateTransition ::
  forall h.
  HashAlgorithm h =>
  SlotNo ->
  PParams ->
  LedgerState h ->
  Tx h ->
  AccountState ->
  Either [ValidationError] (LedgerState h)
asStateTransition _slot pp ls tx acnt =
  let next =
        runShelleyBase $
          applySTS @(LEDGER h)
            ( TRC
                ( (LedgerEnv _slot 0 pp acnt),
                  (_utxoState ls, _delegationState ls),
                  tx
                )
            )
   in case next of
        Left pfs -> Left $ convertPredicateFailuresToValidationErrors pfs
        Right (u, d) ->
          Right $
            ls
              { _utxoState = u,
                _delegationState = d
              }

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition' ::
  -- :: ( Crypto crypto
  --    , DSignable crypto (TxBody crypto)
  --    )
  -- =>
  HashAlgorithm h =>
  SlotNo ->
  PParams ->
  LedgerValidation h ->
  Tx h ->
  AccountState ->
  LedgerValidation h
asStateTransition' _slot pp (LedgerValidation valErrors ls) tx _ =
  let ls' = applyTxBody ls pp (_body tx)
      d' = (_genDelegs . _dstate . _delegationState) ls
   in case validTx tx d' _slot pp ls of
        Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
        Valid -> LedgerValidation valErrors ls'

convertPredicateFailuresToValidationErrors :: [[PredicateFailure (LEDGER h)]] -> [ValidationError]
convertPredicateFailuresToValidationErrors pfs =
  map predicateFailureToValidationError $ foldr (++) [] pfs

predicateFailureToValidationError :: PredicateFailure (LEDGER h) -> ValidationError
predicateFailureToValidationError (UtxowFailure (MissingVKeyWitnessesUTXOW _)) =
  MissingWitnesses
predicateFailureToValidationError (UtxowFailure (MissingScriptWitnessesUTXOW _)) =
  MissingWitnesses
predicateFailureToValidationError (UtxowFailure (InvalidWitnessesUTXOW ([], []))) =
  InvalidWitness
predicateFailureToValidationError (UtxowFailure (UtxoFailure InputSetEmptyUTxO)) =
  InputSetEmpty
predicateFailureToValidationError (UtxowFailure (UtxoFailure (ExpiredUTxO a b))) =
  Expired a b
predicateFailureToValidationError (UtxowFailure (UtxoFailure (BadInputsUTxO _))) =
  BadInputs
predicateFailureToValidationError (UtxowFailure (UtxoFailure (FeeTooSmallUTxO a b))) =
  FeeTooSmall a b
predicateFailureToValidationError (UtxowFailure (UtxoFailure (ValueNotConservedUTxO a b))) =
  ValueNotConserved a b
predicateFailureToValidationError (DelegsFailure (DelegateeNotRegisteredDELEG _)) =
  StakeDelegationImpossible
predicateFailureToValidationError (DelegsFailure (WithdrawalsNotInRewardsDELEGS _)) =
  IncorrectRewards
predicateFailureToValidationError _ = UnknownValidationError
