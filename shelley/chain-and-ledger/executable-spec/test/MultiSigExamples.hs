{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module MultiSigExamples
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
import           Data.Sequence (Seq (..))

import qualified Data.Set as Set (fromList)

import           Coin
import           Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import           Keys (pattern GenDelegs, undiscriminateKeyHash)
import           LedgerState (genesisCoins, genesisId, genesisState, _utxoState)
import           MockTypes (Addr, KeyPair, LedgerState, MultiSig, ScriptHash, Tx, TxBody, TxId,
                     TxIn, UTXOW, UTxOState, Wdrl)
import           PParams (PParams (..), emptyPParams)
import           Slot (SlotNo (..))
import           STS.Utxo (UtxoEnv (..))
import           Test.Utils
import           Tx (hashScript)
import           TxData (pattern AddrBase, pattern KeyHashObj, pattern RequireAllOf,
                     pattern RequireAnyOf, pattern RequireMOf, pattern RequireSignature,
                     pattern ScriptHashObj, pattern StakeCreds, pattern StakePools, pattern Tx,
                     pattern TxBody, pattern TxIn, pattern TxOut, _body)
import           Updates (emptyUpdate)
import           UTxO (makeWitnessesVKey, txid)

import           Examples (aliceAddr, alicePay, bobAddr, bobPay, carlAddr, dariaAddr)

-- Multi-Signature tests

-- Multi-signature scripts
singleKeyOnly :: Addr -> MultiSig
singleKeyOnly (AddrBase (KeyHashObj pk) _ ) = RequireSignature $ undiscriminateKeyHash pk
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
        (map (uncurry TxOut) addrs)
        Empty
        Map.empty
        (Coin 0)
        (SlotNo 0)
        emptyUpdate

makeTxBody :: [TxIn] -> [(Addr, Coin)] -> Wdrl -> TxBody
makeTxBody inp addrCs wdrl =
  TxBody
    (Set.fromList inp)
    [uncurry TxOut addrC | addrC <- addrCs]
    Empty
    wdrl
    (Coin 0)
    (SlotNo 10)
    emptyUpdate

makeTx :: TxBody -> [KeyPair] -> Map ScriptHash MultiSig -> Tx
makeTx txBody keyPairs =
  Tx txBody (makeWitnessesVKey txBody keyPairs)

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
               (AddrBase
                (ScriptHashObj $ hashScript msig)
                (ScriptHashObj $ hashScript msig), c)) msigs
  in
  let tx = makeTx (initTxBody addresses)
                  [alicePay, bobPay]
                  Map.empty in
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
  -> [KeyPair]
  -> Either [[PredicateFailure UTXOW]] UTxOState
applyTxWithScript lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
        utxoSt = case initUtxo of
                   Right utxoSt'' -> utxoSt''
                   _                      -> error ("must fail test before: "
                                                   ++ show initUtxo)
        txbody = makeTxBody inputs [(aliceAddr, aliceInitCoin + bobInitCoin + sum wdrl)] wdrl
        inputs = [TxIn txId (fromIntegral n) | n <-
                     [0..length lockScripts - (if aliceKeep > 0 then 0 else 1)]]
                                 -- alice? + scripts
        tx = makeTx
              txbody
              signers
              (Map.fromList $ map (\scr -> (hashScript scr, scr)) unlockScripts)
        utxoSt' = runShelleyBase $ applySTS @UTXOW (TRC( UtxoEnv
                                          (SlotNo 0)
                                          initPParams
                                          (StakeCreds Map.empty)
                                          (StakePools Map.empty)
                                          (GenDelegs Map.empty)
                                      , utxoSt
                                      , tx))
