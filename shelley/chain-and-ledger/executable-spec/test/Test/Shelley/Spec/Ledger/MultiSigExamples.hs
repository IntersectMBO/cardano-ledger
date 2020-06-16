{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.MultiSigExamples
  ( applyTxWithScript,
    aliceOnly,
    bobOnly,
    aliceAndBob,
    aliceOrBob,
    aliceAndBobOrCarl,
    aliceAndBobOrCarlAndDaria,
    aliceAndBobOrCarlOrDaria,
  )
where

import Cardano.Crypto.Hash (HashAlgorithm)
import Control.State.Transition.Extended (PredicateFailure, TRC (..), applySTS)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set (fromList)
import Shelley.Spec.Ledger.Address (pattern Addr)
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential
  ( pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
  )
import Shelley.Spec.Ledger.Keys (KeyRole (..), asWitness)
import Shelley.Spec.Ledger.LedgerState (_utxoState, genesisCoins, genesisId, genesisState)
import Shelley.Spec.Ledger.MetaData (MetaData)
import Shelley.Spec.Ledger.PParams (PParams, _maxTxSize, emptyPParams)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.Scripts
  ( pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (..), _body, hashScript, pattern Tx)
import Shelley.Spec.Ledger.TxData
  ( unWdrl,
    pattern StakePools,
    pattern TxBody,
    pattern TxIn,
    pattern TxOut,
    pattern Wdrl,
  )
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessesVKey, txid)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Addr,
    KeyPair,
    LedgerState,
    MultiSig,
    ScriptHash,
    Tx,
    TxBody,
    TxId,
    TxIn,
    UTXOW,
    UTxOState,
    Wdrl,
    pattern GenDelegs,
  )
import Test.Shelley.Spec.Ledger.Examples
  ( aliceAddr,
    alicePay,
    bobAddr,
    bobPay,
    carlAddr,
    dariaAddr,
  )
import Test.Shelley.Spec.Ledger.Utils

-- Multi-Signature tests

-- Multi-signature scripts
singleKeyOnly :: HashAlgorithm h => Addr h -> MultiSig h
singleKeyOnly (Addr _ (KeyHashObj pk) _) = RequireSignature $ asWitness pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: HashAlgorithm h => proxy h -> MultiSig h
aliceOnly _ = singleKeyOnly aliceAddr

bobOnly :: HashAlgorithm h => proxy h -> MultiSig h
bobOnly _ = singleKeyOnly bobAddr

aliceOrBob :: HashAlgorithm h => proxy h -> MultiSig h
aliceOrBob p = RequireAnyOf [aliceOnly p, singleKeyOnly bobAddr]

aliceAndBob :: HashAlgorithm h => proxy h -> MultiSig h
aliceAndBob p = RequireAllOf [aliceOnly p, singleKeyOnly bobAddr]

aliceAndBobOrCarl :: HashAlgorithm h => proxy h -> MultiSig h
aliceAndBobOrCarl p = RequireMOf 1 [aliceAndBob p, singleKeyOnly carlAddr]

aliceAndBobOrCarlAndDaria :: HashAlgorithm h => proxy h -> MultiSig h
aliceAndBobOrCarlAndDaria p =
  RequireAnyOf
    [ aliceAndBob p,
      RequireAllOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]
    ]

aliceAndBobOrCarlOrDaria :: HashAlgorithm h => proxy h -> MultiSig h
aliceAndBobOrCarlOrDaria p =
  RequireMOf
    1
    [ aliceAndBob p,
      RequireAnyOf [singleKeyOnly carlAddr, singleKeyOnly dariaAddr]
    ]

initTxBody :: HashAlgorithm h => [(Addr h, Coin)] -> TxBody h
initTxBody addrs =
  TxBody
    (Set.fromList [TxIn genesisId 0, TxIn genesisId 1])
    (StrictSeq.fromList $ map (uncurry TxOut) addrs)
    Empty
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 0)
    SNothing
    SNothing

makeTxBody :: HashAlgorithm h => [TxIn h] -> [(Addr h, Coin)] -> Wdrl h -> TxBody h
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

makeTx :: HashAlgorithm h => TxBody h -> [KeyPair h 'AWitness] -> Map (ScriptHash h) (MultiSig h) -> Maybe MetaData -> Tx h
makeTx txBody keyPairs msigs = Tx txBody wits . maybeToStrictMaybe
  where
    wits =
      mempty
        { addrWits = makeWitnessesVKey (hashTxBody txBody) keyPairs,
          msigWits = msigs
        }

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

genesis :: HashAlgorithm h => LedgerState h
genesis = genesisState genDelegs0 utxo0
  where
    genDelegs0 = Map.empty
    utxo0 =
      genesisCoins
        [ TxOut aliceAddr aliceInitCoin,
          TxOut bobAddr bobInitCoin
        ]

initPParams :: PParams
initPParams = emptyPParams {_maxTxSize = 1000}

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState ::
  forall h.
  HashAlgorithm h =>
  Coin ->
  [(MultiSig h, Coin)] ->
  (TxId h, Either [[PredicateFailure (UTXOW h)]] (UTxOState h))
initialUTxOState aliceKeep msigs =
  let addresses =
        [(aliceAddr, aliceKeep) | aliceKeep > 0]
          ++ map
            ( \(msig, c) ->
                ( Addr
                    Testnet
                    (ScriptHashObj $ hashScript msig)
                    (StakeRefBase $ ScriptHashObj $ hashScript msig),
                  c
                )
            )
            msigs
   in let tx =
            makeTx
              (initTxBody addresses)
              (asWitness <$> [alicePay, bobPay])
              Map.empty
              Nothing
       in ( txid $ _body tx,
            runShelleyBase $
              applySTS @(UTXOW h)
                ( TRC
                    ( UtxoEnv
                        (SlotNo 0)
                        initPParams
                        (StakePools Map.empty)
                        (GenDelegs Map.empty),
                      _utxoState genesis,
                      tx
                    )
                )
          )

-- | Start from genesis, consume Alice's and Bob's coins, create an output
-- spendable by Alice if 'aliceKeep' is greater than 0. For each pair of script
-- and coin value in 'lockScripts' create an output of that value locked by the
-- script. Sign the transaction with keys in 'signers'. Then create an
-- transaction that uses the scripts in 'unlockScripts' (and the output for
-- 'aliceKeep' in the case of it being non-zero) to spend all funds back to
-- Alice. Return resulting UTxO state or collected errors
applyTxWithScript ::
  forall proxy h.
  HashAlgorithm h =>
  proxy h ->
  [(MultiSig h, Coin)] ->
  [MultiSig h] ->
  Wdrl h ->
  Coin ->
  [KeyPair h 'AWitness] ->
  Either [[PredicateFailure (UTXOW h)]] (UTxOState h)
applyTxWithScript _ lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where
    (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
    utxoSt = case initUtxo of
      Right utxoSt'' -> utxoSt''
      _ ->
        error
          ( "must fail test before: "
              ++ show initUtxo
          )
    txbody =
      makeTxBody
        inputs
        [(aliceAddr, aliceInitCoin + bobInitCoin + sum (unWdrl wdrl))]
        wdrl
    inputs =
      [ TxIn txId (fromIntegral n)
        | n <-
            [0 .. length lockScripts - (if aliceKeep > 0 then 0 else 1)]
      ]
    -- alice? + scripts
    tx =
      makeTx
        txbody
        signers
        (Map.fromList $ map (\scr -> (hashScript scr, scr)) unlockScripts)
        Nothing
    utxoSt' =
      runShelleyBase $
        applySTS @(UTXOW h)
          ( TRC
              ( UtxoEnv
                  (SlotNo 0)
                  initPParams
                  (StakePools Map.empty)
                  (GenDelegs Map.empty),
                utxoSt,
                tx
              )
          )
