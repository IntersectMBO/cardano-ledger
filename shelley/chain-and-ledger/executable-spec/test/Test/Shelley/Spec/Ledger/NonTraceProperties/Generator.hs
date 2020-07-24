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

import Control.State.Transition.Extended (TRC (..))
import Data.ByteString.Char8 (pack)

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
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys (KeyPair (..))
import Shelley.Spec.Ledger.Keys (KeyRole (..), hashKey, vKey)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    genesisState,
    _delegationState,
    _dstate,
    _genDelegs,
    _rewards,
    _utxo,
    _utxoState,
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
    pattern ForgingAda,
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
  ( Wdrl (..),
    getAddress,
    pattern DCertDeleg,
    pattern DCertPool,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
    pattern RegKey,
    pattern RetirePool,
  )
import Shelley.Spec.Ledger.UTxO (balance, makeWitnessVKey, pattern UTxO)
import Shelley.Spec.Ledger.Value
  ( coinToValue,
    splitValueFee,
    zeroV,
  )
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    Credential,
    DCert,
    DPState,
    Delegation,
    KeyPairs,
    LEDGER,
    LedgerState,
    Mock,
    StakeReference,
    Tx,
    TxOut,
    UTxO,
    Value,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
  ( applyTxBody,
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.NonTraceProperties.Mutator
import Test.Shelley.Spec.Ledger.NonTraceProperties.Validity
import Test.Shelley.Spec.Ledger.Orphans ()
import Test.Shelley.Spec.Ledger.Utils
import Unsafe.Coerce

-- | Find first matching key pair for address. Returns the matching key pair
-- where the first element of the pair matched the hash in 'addr'.
findPayKeyPair :: Crypto c => Addr c -> KeyPairs c -> KeyPair 'Payment c
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

-- | Generates a list of '(pay, stake)' key pairs.
genKeyPairs :: Mock c => Int -> Int -> Gen (KeyPairs c)
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
hashKeyPairs :: Crypto c => KeyPairs c -> [(Credential c 'Payment, StakeReference c)]
hashKeyPairs keyPairs =
  ( \(a, b) ->
      ( KeyHashObj . hashKey $ vKey a,
        StakeRefBase . KeyHashObj . hashKey $ vKey b
      )
  )
    <$> keyPairs

-- | Transforms list of keypairs into 'Addr' types of the form 'AddrTxin pay
-- stake'
addrTxins :: Crypto c => KeyPairs c -> [Addr c]
addrTxins keyPairs = uncurry (Addr Testnet) <$> hashKeyPairs keyPairs

-- | Generator for List of 'Coin' values. Generates between 'lower' and 'upper'
-- coins, with values between 'minCoin' and 'maxCoin'.
genCoinList :: Integer -> Integer -> Int -> Int -> Gen [Coin]
genCoinList minCoin maxCoin lower upper = do
  xs <-
    Gen.list (Range.linear lower upper) $
      Gen.integral (Range.exponential minCoin maxCoin)
  return (Coin <$> xs)

--TODO make correct
genValueList :: Integer -> Integer -> Int -> Int -> Gen [Value c]
genValueList minCoin maxCoin lower upper = do
  xs <- genCoinList minCoin maxCoin lower upper
  return (fmap coinToValue xs)

-- | Generator for a list of 'TxOut' where for each 'Addr' of 'addrs' one Coin
-- value is generated.
genTxOut :: Crypto c => [Addr c] -> Gen [TxOut c]
genTxOut addrs = do
  ys <- genValueList 100 10000 (length addrs) (length addrs)
  return (uncurry TxOut <$> zip addrs ys)

-- TODO generate sensible protocol constants
defPCs :: PParams
defPCs = emptyPParams

-- | Generator of a non-empty genesis ledger state, i.e., at least one valid
-- address and non-zero UTxO.
genNonemptyGenesisState :: Mock c => proxy c -> Gen (LedgerState c)
genNonemptyGenesisState _ = do
  keyPairs <- genKeyPairs 1 10
  (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)

-- TODO make witnesses for forges -- TODO use the one from Utxo
-- generate value properly
genValue :: Integer -> Integer -> Gen (Value c)
genValue _ _ = do
  pure zeroV

-- | Generator for a new 'Tx' and fee value for executing the
-- transaction. Selects one valid input from the UTxO, sums up all funds of the
-- address associated to that input, selects a random subsequence of other valid
-- addresses and spends the UTxO. If 'n' addresses are selected to spent 'b'
-- coins, the amount spent to each address is 'div b n' and the fees are set to
-- 'rem b n'.
genTx :: Mock c => KeyPairs c -> UTxO c -> SlotNo -> Gen (Coin, Tx c)
genTx keyList (UTxO m) cslot = do
  -- pick a forge value
  txforge <- genValue 20 20
  -- select payer
  selectedInputs <- Gen.shuffle utxoInputs
  let !selectedAddr    = addr $ head selectedInputs
  let !selectedUTxO    = Map.filter (\out -> getAddress out == selectedAddr) m
  let !selectedKeyPair = findPayKeyPair selectedAddr keyList
  let !selectedBalance = (balance $ UTxO selectedUTxO) <> txforge

  -- select receipients, distribute balance of selected UTxO set
  -- TODO whats up with coin vs value here
  n <- genNatural 1 10 -- (fromIntegral $ length keyList) -- TODO make this variable, but uses too much RAM atm
  receipients <- Seq.fromList . take (fromIntegral n) <$> Gen.shuffle keyList
  let realN = length receipients
  let (perReceipient, txfee') = splitValueFee selectedBalance (fromIntegral realN)
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
          txforge
          (Wdrl Map.empty) -- TODO generate witdrawals
          txfee'
          (cslot + SlotNo txttl)
          SNothing
          SNothing
  let !txbHash = hashAnnotated txbody
  let !txwit = makeWitnessVKey txbHash selectedKeyPair
  pure (txfee', Tx txbody mempty {addrWits = Set.fromList [txwit]} SNothing)
  where
    utxoInputs = Map.keys m
    addr inp = getAddress $ m Map.! inp

-- | Generator for new transaction state transition, starting from a
-- 'LedgerState' and using a list of pairs of 'KeyPair'. Returns either the
-- accumulated fees and a resulting ledger state or the 'ValidationError'
-- information in case of an invalid transaction.
genLedgerStateTx ::
  Mock c =>
  KeyPairs c ->
  SlotNo ->
  LedgerState c ->
  Gen (Coin, Tx c, Either [ValidationError] (LedgerState c))
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
  Mock c => proxy c -> Gen (KeyPairs c, Natural, Coin, LedgerState c, [Tx c], Either [ValidationError] (LedgerState c))
genNonEmptyAndAdvanceTx _ = do
  keyPairs <- genKeyPairs 1 10
  steps <- genNatural 1 10
  ls <- (genesisState Map.empty . genesisCoins) <$> genTxOut (addrTxins keyPairs)
  (fees, txs, ls') <- repeatCollectTx steps keyPairs (SlotNo 1) (Coin 0) ls []
  pure (keyPairs, steps, fees, ls, txs, ls')

-- | Mutated variant of above, collects validation errors in 'LedgerValidation'.
genNonEmptyAndAdvanceTx' ::
  Mock c => proxy c -> Gen (KeyPairs c, Natural, Coin, LedgerState c, [Tx c], LedgerValidation c)
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
  Mock c =>
  Natural ->
  KeyPairs c ->
  SlotNo ->
  Coin ->
  LedgerState c ->
  [Tx c] ->
  Gen (Coin, [Tx c], Either [ValidationError] (LedgerState c))
repeatCollectTx 0 _ _ fees ls txs = pure (fees, reverse txs, Right ls)
repeatCollectTx n keyPairs (SlotNo _slot) fees ls txs = do
  (txfee', tx, next) <- genLedgerStateTx keyPairs (SlotNo _slot) ls
  case next of
    Left _ -> pure (fees, txs, next)
    Right ls' -> repeatCollectTx (n - 1) keyPairs (SlotNo $ _slot + 1) (txfee' + fees) ls' (tx : txs)

-- | Mutated variant of `repeatCollectTx'`, stops at recursion depth or after
-- exhausting the UTxO set to prevent calling 'head' on empty input list.
repeatCollectTx' ::
  Mock c =>
  Natural ->
  KeyPairs c ->
  Coin ->
  LedgerState c ->
  [Tx c] ->
  [ValidationError] ->
  Gen (Coin, [Tx c], LedgerValidation c)
repeatCollectTx' n keyPairs fees ls txs validationErrors
  | n == 0 || (utxoSize $ (_utxo . _utxoState) ls) == 0 =
    pure (fees, reverse txs, LedgerValidation validationErrors ls)
  | otherwise = do
    (txfee', tx, LedgerValidation errors' ls') <- genLedgerStateTx' keyPairs ls
    repeatCollectTx' (n - 1) keyPairs (txfee' + fees) ls' (tx : txs) (validationErrors ++ errors')

-- | Find first matching key pair for stake key in 'AddrTxin'.
findStakeKeyPair :: Crypto c => Credential c 'Staking -> KeyPairs c -> KeyPair 'Staking c
findStakeKeyPair (KeyHashObj hk) keyList =
  snd $ head $ filter (\(_, stake) -> hk == hashKey (vKey stake)) keyList
findStakeKeyPair _ _ = undefined -- TODO treat script case

-- -- | Returns the hashed 'addr' part of a 'TxOut'.
-- getTxOutAddr :: HashAlgorithm h => TxOut h -> Addr h
-- getTxOutAddr (TxOut addr _) = addr

-- | Generator for arbitrary valid ledger state, discarding any generated
-- invalid one.
genValidLedgerState :: Mock c => proxy c -> Gen (KeyPairs c, Natural, [Tx c], LedgerState c)
genValidLedgerState p = do
  (keyPairs, steps, _, _, txs, newState) <- genNonEmptyAndAdvanceTx p
  case newState of
    Left _ -> Gen.discard
    Right ls -> pure (keyPairs, steps, txs, ls)

genValidSuccessorState ::
  Mock c =>
  KeyPairs c ->
  SlotNo ->
  LedgerState c ->
  Gen (Coin, Tx c, LedgerState c)
genValidSuccessorState keyPairs _slot sourceState = do
  (txfee', entry, next) <- genLedgerStateTx keyPairs _slot sourceState
  case next of
    Left _ -> Gen.discard
    Right ls -> pure (txfee', entry, ls)

genValidStateTx :: Mock c => proxy c -> Gen (LedgerState c, Natural, Coin, Tx c, LedgerState c)
genValidStateTx p = do
  (ls, steps, txfee', entry, ls', _) <- genValidStateTxKeys p
  pure (ls, steps, txfee', entry, ls')

genValidStateTxKeys :: Mock c => proxy c -> Gen (LedgerState c, Natural, Coin, Tx c, LedgerState c, KeyPairs c)
genValidStateTxKeys p = do
  (keyPairs, steps, _, ls) <- genValidLedgerState p
  (txfee', entry, ls') <- genValidSuccessorState keyPairs (SlotNo $ fromIntegral steps + 1) ls
  pure (ls, steps, txfee', entry, ls', keyPairs)

genStateTx :: Mock c => proxy c -> Gen (LedgerState c, Natural, Coin, Tx c, LedgerValidation c)
genStateTx p = do
  (keyPairs, steps, _, ls) <- genValidLedgerState p
  (txfee', entry, lv) <- genLedgerStateTx' keyPairs ls
  pure (ls, steps, txfee', entry, lv)

genLedgerStateTx' ::
  Mock c =>
  KeyPairs c ->
  LedgerState c ->
  Gen (Coin, Tx c, LedgerValidation c)
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

genDelegationData :: Crypto c => KeyPairs c -> EpochNo -> Gen (DCert c)
genDelegationData keys epoch =
  Gen.choice
    [ genDCertRegKey keys,
      genDCertDeRegKey keys,
      genDCertRetirePool keys epoch
    ]

genDCertRegKey :: Crypto c => KeyPairs c -> Gen (DCert c)
genDCertRegKey keys =
  DCertDeleg . RegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertDeRegKey :: Crypto c => KeyPairs c -> Gen (DCert c)
genDCertDeRegKey keys =
  DCertDeleg . DeRegKey . KeyHashObj . hashKey <$> getAnyStakeKey keys

genDCertRetirePool :: Crypto c => KeyPairs c -> EpochNo -> Gen (DCert c)
genDCertRetirePool keys epoch = do
  key <- getAnyStakeKey keys
  pure $ DCertPool $ RetirePool (unsafeCoerce $ hashKey key) epoch

genDelegation :: Crypto c => KeyPairs c -> DPState c -> Gen (Delegation c)
genDelegation keys d = do
  poolKey <- Gen.element $ Map.keys $ _rewards . _dstate $ d
  delegatorKey <- getAnyStakeKey keys
  pure $ Delegation (KeyHashObj $ hashKey delegatorKey) $ (unsafeCoerce . hashKey $ vKey $ findStakeKeyPair poolKey keys)

genDCertDelegate :: Crypto c => KeyPairs c -> DPState c -> Gen (DCert c)
genDCertDelegate keys ds = (DCertDeleg . Delegate) <$> genDelegation keys ds

-- | In the case where a transaction is valid for a given ledger state,
--  apply the transaction as a state transition function on the ledger state.
--  Otherwise, return a list of validation errors.
asStateTransition ::
  forall c.
  Mock c =>
  SlotNo ->
  PParams ->
  LedgerState c ->
  Tx c ->
  AccountState ->
  Either [ValidationError] (LedgerState c)
asStateTransition _slot pp ls tx acnt =
  let next =
        runShelleyBase $
          applySTSTest @(LEDGER c)
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
  Mock c =>
  SlotNo ->
  PParams ->
  LedgerValidation c ->
  Tx c ->
  AccountState ->
  LedgerValidation c
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
predicateFailureToValidationError (UtxowFailure (InvalidWitnessesUTXOW [])) =
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
  ValueNotConserved (pack $ show a) (pack $ show b)
predicateFailureToValidationError (UtxowFailure (UtxoFailure (ForgingAda _))) =
  UserForgingAda
predicateFailureToValidationError (DelegsFailure (DelegateeNotRegisteredDELEG _)) =
  StakeDelegationImpossible
predicateFailureToValidationError (DelegsFailure (WithdrawalsNotInRewardsDELEGS _)) =
  IncorrectRewards
predicateFailureToValidationError _ = UnknownValidationError
