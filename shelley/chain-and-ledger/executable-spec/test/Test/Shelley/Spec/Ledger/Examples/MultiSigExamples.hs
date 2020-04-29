{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Examples.MultiSigExamples
  ( applyTxWithScript
  , aliceOnly
  , bobOnly
  , aliceAndBob
  , aliceOrBob
  , aliceAndBobOrCarl
  , aliceAndBobOrCarlAndDaria
  , aliceAndBobOrCarlOrDaria
  )
  where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList)
import           Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq

import qualified Data.Set as Set (fromList)

import           Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe)
import           Shelley.Spec.Ledger.Coin
import           Shelley.Spec.Ledger.Keys (KeyRole(..), asWitness)
import           Shelley.Spec.Ledger.LedgerState (genesisCoins, genesisId, genesisState, _utxoState)
import           Shelley.Spec.Ledger.MetaData (MetaData)
import           Shelley.Spec.Ledger.PParams (PParams, emptyPParams, _maxTxSize)
import           Shelley.Spec.Ledger.Scripts (pattern RequireAllOf, pattern RequireAnyOf,
                     pattern RequireMOf, pattern RequireSignature)
import           Shelley.Spec.Ledger.Slot (SlotNo (..))
import           Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import           Shelley.Spec.Ledger.Tx (pattern Tx, hashScript, _body)
import           Shelley.Spec.Ledger.TxData (pattern Addr, pattern KeyHashObj,
                     pattern ScriptHashObj, pattern StakeCreds, pattern StakePools,
                     pattern StakeRefBase, pattern TxBody, pattern TxIn, pattern TxOut,
                     pattern Wdrl, unWdrl)
import           Shelley.Spec.Ledger.UTxO (makeWitnessesVKey, txid)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Addr, KeyPair, LedgerState, MultiSig,
                     ScriptHash, Tx, TxBody, TxId, TxIn, UTXOW, UTxOState, Wdrl, pattern GenDelegs)
import           Test.Shelley.Spec.Ledger.Examples.Examples (aliceAddr, alicePay, bobAddr, bobPay,
                     carlAddr, dariaAddr)
import           Test.Shelley.Spec.Ledger.Utils

-- Multi-Signature tests

-- Multi-signature scripts
singleKeyOnly :: Addr -> MultiSig
singleKeyOnly (Addr (KeyHashObj pk) _ ) = RequireSignature $ asWitness pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: MultiSig
aliceOnly = singleKeyOnly aliceAddr

bobOnly :: MultiSig
bobOnly = singleKeyOnly bobAddr

aliceOrBob :: MultiSig
aliceOrBob = RequireAnyOf [aliceOnly, singleKeyOnly bobAddr]

aliceAndBob :: MultiSig
aliceAndBob = RequireAllOf [aliceOnly, singleKeyOnly bobAddr]

aliceAndBobOrCarl :: MultiSig
aliceAndBobOrCarl = RequireMOf 1 [aliceAndBob, singleKeyOnly carlAddr]

aliceAndBobOrCarlAndDaria :: MultiSig
aliceAndBobOrCarlAndDaria =
  RequireAnyOf [aliceAndBob,
                RequireAllOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]

aliceAndBobOrCarlOrDaria :: MultiSig
aliceAndBobOrCarlOrDaria =
  RequireMOf 1 [aliceAndBob,
                RequireAnyOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]]

initTxBody :: [(Addr, Coin)] -> TxBody
initTxBody addrs = TxBody
        (Set.fromList [TxIn genesisId 0, TxIn genesisId 1])
        (StrictSeq.fromList $ map (uncurry TxOut) addrs)
        Empty
        (Wdrl Map.empty)
        (Coin 0)
        (SlotNo 0)
        SNothing
        SNothing

makeTxBody :: [TxIn] -> [(Addr, Coin)] -> Wdrl -> TxBody
makeTxBody inp addrCs wdrl =
  TxBody
    (Set.fromList inp)
    (StrictSeq.fromList [uncurry TxOut addrC | addrC <- addrCs])
    Empty
    wdrl
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeTx :: TxBody -> [KeyPair 'Witness] -> Map ScriptHash MultiSig -> Maybe MetaData -> Tx
makeTx txBody keyPairs msigs =
  Tx txBody (makeWitnessesVKey txBody keyPairs) msigs . maybeToStrictMaybe

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

genesis :: LedgerState
genesis = genesisState genDelegs0 utxo0
  where
    genDelegs0 = Map.empty
    utxo0 = genesisCoins
              [ TxOut aliceAddr aliceInitCoin
              , TxOut bobAddr bobInitCoin]

initPParams :: PParams
initPParams = emptyPParams {_maxTxSize = 1000}

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState
  :: Coin
  -> [(MultiSig, Coin)]
  -> (TxId, Either [[PredicateFailure UTXOW]] UTxOState)
initialUTxOState aliceKeep msigs =
  let addresses =
        [(aliceAddr, aliceKeep) | aliceKeep > 0] ++
        map (\(msig, c) ->
               (Addr
                (ScriptHashObj $ hashScript msig)
                (StakeRefBase $ ScriptHashObj $ hashScript msig), c)) msigs
  in
  let tx = makeTx (initTxBody addresses)
                  (asWitness <$> [alicePay, bobPay])
                  Map.empty Nothing in
  (txid $ _body tx, runShelleyBase $ applySTS @UTXOW (TRC( UtxoEnv
                                           (SlotNo 0)
                                           initPParams
                                           (StakeCreds Map.empty)
                                           (StakePools Map.empty)
                                           (GenDelegs Map.empty)
                                         , _utxoState genesis
                                         , tx)))

-- | Start from genesis, consume Alice's and Bob's coins, create an output
-- spendable by Alice if 'aliceKeep' is greater than 0. For each pair of script
-- and coin value in 'lockScripts' create an output of that value locked by the
-- script. Sign the transaction with keys in 'signers'. Then create an
-- transaction that uses the scripts in 'unlockScripts' (and the output for
-- 'aliceKeep' in the case of it being non-zero) to spend all funds back to
-- Alice. Return resulting UTxO state or collected errors
applyTxWithScript
  :: [(MultiSig, Coin)]
  -> [MultiSig]
  -> Wdrl
  -> Coin
  -> [KeyPair 'Witness]
  -> Either [[PredicateFailure UTXOW]] UTxOState
applyTxWithScript lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
        utxoSt = case initUtxo of
                   Right utxoSt'' -> utxoSt''
                   _                      -> error ("must fail test before: "
                                                   ++ show initUtxo)
        txbody = makeTxBody inputs
          [(aliceAddr, aliceInitCoin + bobInitCoin + sum (unWdrl wdrl))] wdrl
        inputs = [TxIn txId (fromIntegral n) | n <-
                     [0..length lockScripts - (if aliceKeep > 0 then 0 else 1)]]
                                 -- alice? + scripts
        tx = makeTx
              txbody
              signers
              (Map.fromList $ map (\scr -> (hashScript scr, scr)) unlockScripts)
              Nothing
        utxoSt' = runShelleyBase $ applySTS @UTXOW (TRC( UtxoEnv
                                          (SlotNo 0)
                                          initPParams
                                          (StakeCreds Map.empty)
                                          (StakePools Map.empty)
                                          (GenDelegs Map.empty)
                                      , utxoSt
                                      , tx))
