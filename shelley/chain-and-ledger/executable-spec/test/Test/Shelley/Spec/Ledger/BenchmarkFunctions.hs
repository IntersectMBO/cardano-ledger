{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
  )
where

import Control.State.Transition.Extended (TRC (..), applySTS)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Keys (asWitness)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    emptyDPState,
    genesisCoins,
    genesisId,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..), emptyPPPUpdates)
import Shelley.Spec.Ledger.STS.Ledger (pattern LedgerEnv)
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (..), pattern Tx)
import Shelley.Spec.Ledger.TxData
  ( pattern TxBody,
    pattern TxIn,
    pattern TxOut,
    pattern Wdrl,
  )
import Shelley.Spec.Ledger.UTxO (hashTxBody, makeWitnessesVKey)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( DPState,
    LEDGER,
    LedgerEnv,
    Tx,
    TxBody,
    TxOut,
    UTxOState,
  )
import Test.Shelley.Spec.Ledger.Examples
  ( aliceAddr,
    alicePay,
    ppsEx1,
  )
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)

coins :: Integer -> [TxOut]
coins n = fmap (\_ -> TxOut aliceAddr (Coin $ 100)) [0 .. n]

utxoState :: Integer -> UTxOState
utxoState n =
  UTxOState
    (genesisCoins (coins n))
    (Coin 0)
    (Coin 0)
    emptyPPPUpdates

ppsBench :: PParams
ppsBench = ppsEx1 {_minUTxOValue = 10}

ledgerEnv :: LedgerEnv
ledgerEnv = LedgerEnv (SlotNo 0) 0 ppsBench (AccountState 0 0)

testLEDGER ::
  (UTxOState, DPState) ->
  Tx ->
  LedgerEnv ->
  Bool
testLEDGER initSt tx env = do
  let st = runShelleyBase $ applySTS @LEDGER (TRC (env, initSt, tx))
  case st of
    Right _ -> True
    Left _ -> False

txbSpendOneUTxO :: TxBody
txbSpendOneUTxO =
  TxBody
    (Set.fromList [TxIn genesisId 0])
    (StrictSeq.fromList [TxOut aliceAddr (Coin 10), TxOut aliceAddr (89)])
    StrictSeq.empty
    (Wdrl Map.empty)
    (Coin 1)
    (SlotNo 10)
    SNothing
    SNothing

txSpendOneUTxO :: Tx
txSpendOneUTxO =
  Tx
    txbSpendOneUTxO
    mempty
      { addrWits = makeWitnessesVKey (hashTxBody txbSpendOneUTxO) [asWitness alicePay]
      }
    SNothing

ledgerSpendOneUTxO :: Integer -> Bool
ledgerSpendOneUTxO n = testLEDGER (utxoState n, emptyDPState) txSpendOneUTxO ledgerEnv
