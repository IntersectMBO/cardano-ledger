{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.State.Transition.Extended (PredicateFailure, TRC (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set (fromList)
import Shelley.Spec.Ledger.API
  ( ScriptHash,
    UTXOW,
  )
import Shelley.Spec.Ledger.Address (pattern Addr)
import Shelley.Spec.Ledger.Address
  ( Addr,
  )
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
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyPair,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( LedgerState (..),
    UTxOState,
    genesisState,
  )
import Shelley.Spec.Ledger.MetaData (MetaData)
import Shelley.Spec.Ledger.PParams (PParams, emptyPParams, _maxTxSize)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    TxId,
    WitnessSetHKD (..),
    hashScript,
  )
import Shelley.Spec.Ledger.TxData
  ( TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey, txid)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Generator.Core
  ( genesisCoins,
    genesisId,
  )
import Test.Shelley.Spec.Ledger.Utils

-- Multi-Signature tests

-- Multi-signature scripts
singleKeyOnly :: Crypto c => Addr c -> MultiSig c
singleKeyOnly (Addr _ (KeyHashObj pk) _) = RequireSignature $ asWitness pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: Crypto c => proxy c -> MultiSig c
aliceOnly _ = singleKeyOnly Cast.aliceAddr

bobOnly :: Crypto c => proxy c -> MultiSig c
bobOnly _ = singleKeyOnly Cast.bobAddr

aliceOrBob :: Crypto c => proxy c -> MultiSig c
aliceOrBob p = RequireAnyOf [aliceOnly p, singleKeyOnly Cast.bobAddr]

aliceAndBob :: Crypto c => proxy c -> MultiSig c
aliceAndBob p = RequireAllOf [aliceOnly p, singleKeyOnly Cast.bobAddr]

aliceAndBobOrCarl :: Crypto c => proxy c -> MultiSig c
aliceAndBobOrCarl p = RequireMOf 1 [aliceAndBob p, singleKeyOnly Cast.carlAddr]

aliceAndBobOrCarlAndDaria :: Crypto c => proxy c -> MultiSig c
aliceAndBobOrCarlAndDaria p =
  RequireAnyOf
    [ aliceAndBob p,
      RequireAllOf [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr]
    ]

aliceAndBobOrCarlOrDaria :: Crypto c => proxy c -> MultiSig c
aliceAndBobOrCarlOrDaria p =
  RequireMOf
    1
    [ aliceAndBob p,
      RequireAnyOf [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr]
    ]

initTxBody :: Crypto c => [(Addr c, Coin)] -> TxBody c
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

makeTxBody :: Crypto c => [TxIn c] -> [(Addr c, Coin)] -> Wdrl c -> TxBody c
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

makeTx :: Mock c => TxBody c -> [KeyPair 'Witness c] -> Map (ScriptHash c) (MultiSig c) -> Maybe MetaData -> Tx c
makeTx txBody keyPairs msigs = Tx txBody wits . maybeToStrictMaybe
  where
    wits =
      mempty
        { addrWits = makeWitnessesVKey (hashAnnotated txBody) keyPairs,
          msigWits = msigs
        }

aliceInitCoin :: Coin
aliceInitCoin = 10000

bobInitCoin :: Coin
bobInitCoin = 1000

genesis :: Crypto c => LedgerState c
genesis = genesisState genDelegs0 utxo0
  where
    genDelegs0 = Map.empty
    utxo0 =
      genesisCoins
        [ TxOut Cast.aliceAddr aliceInitCoin,
          TxOut Cast.bobAddr bobInitCoin
        ]

initPParams :: PParams
initPParams = emptyPParams {_maxTxSize = 1000}

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState ::
  forall c.
  Mock c =>
  Coin ->
  [(MultiSig c, Coin)] ->
  (TxId c, Either [[PredicateFailure (UTXOW c)]] (UTxOState c))
initialUTxOState aliceKeep msigs =
  let addresses =
        [(Cast.aliceAddr, aliceKeep) | aliceKeep > 0]
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
              (asWitness <$> [Cast.alicePay, Cast.bobPay])
              Map.empty
              Nothing
       in ( txid $ _body tx,
            runShelleyBase $
              applySTSTest @(UTXOW c)
                ( TRC
                    ( UtxoEnv
                        (SlotNo 0)
                        initPParams
                        Map.empty
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
  forall proxy c.
  Mock c =>
  proxy c ->
  [(MultiSig c, Coin)] ->
  [MultiSig c] ->
  Wdrl c ->
  Coin ->
  [KeyPair 'Witness c] ->
  Either [[PredicateFailure (UTXOW c)]] (UTxOState c)
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
        [(Cast.aliceAddr, aliceInitCoin + bobInitCoin + sum (unWdrl wdrl))]
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
        applySTSTest @(UTXOW c)
          ( TRC
              ( UtxoEnv
                  (SlotNo 0)
                  initPParams
                  Map.empty
                  (GenDelegs Map.empty),
                utxoSt,
                tx
              )
          )
