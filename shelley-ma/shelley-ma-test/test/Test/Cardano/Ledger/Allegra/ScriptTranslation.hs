
{-# LANGUAGE TypeApplications #-}


module Test.Cardano.Ledger.Allegra.ScriptTranslation
  ( testScriptPostTranslation
  ) where

import qualified Shelley.Spec.Ledger.API as S

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import Shelley.Spec.Ledger.PParams (emptyPParams)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Cardano.Ledger.Val as Val
import Shelley.Spec.Ledger.UTxO (txid)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase, applySTSTest)
import Shelley.Spec.Ledger.LedgerState ()
import Control.State.Transition.Extended (TRC (..))
import Control.Monad.Except (runExcept)
import Shelley.Spec.Ledger.Tx (hashScript, scriptWits)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Era (TranslateEra (..))
import Test.Cardano.Ledger.EraBuffet
  ( AllegraEra,
    ShelleyEra,
    StandardCrypto,
  )
import Data.Default.Class (def)

type Allegra = AllegraEra StandardCrypto

type Shelley = ShelleyEra StandardCrypto

bootstrapTxId :: S.TxId StandardCrypto
bootstrapTxId = txid @Shelley txb
  where
    txb =
      S.TxBody
        mempty
        StrictSeq.empty
        StrictSeq.empty
        (S.Wdrl mempty)
        (S.Coin 0)
        (SlotNo 0)
        S.SNothing
        S.SNothing

fromRight :: Either e a -> a
fromRight (Right x) = x
fromRight _ = undefined

script :: S.MultiSig StandardCrypto
script = S.RequireAllOf []

scriptHash :: S.ScriptHash StandardCrypto
scriptHash = hashScript @Shelley script

testScriptPostTranslation :: TestTree
testScriptPostTranslation = testCase
  "we should still be able to spend a translated script" $
  let
    addr =
      S.Addr
        S.Testnet
        (S.ScriptHashObj scriptHash)
        S.StakeRefNull
    utxo =
      S.UTxO $
        Map.singleton
          (S.TxIn bootstrapTxId 0)
          (S.TxOut addr (Val.inject (S.Coin 1)))
    env = S.LedgerEnv (SlotNo 0) 0 emptyPParams (S.AccountState (S.Coin 0) (S.Coin 0))
    utxoStShelley = def {S._utxo = utxo}
    utxoStAllegra = fromRight . runExcept $ translateEra @Allegra () utxoStShelley
    txb =
      S.TxBody
        (Set.singleton $ S.TxIn bootstrapTxId 0)
        StrictSeq.empty
        StrictSeq.empty
        (S.Wdrl mempty)
        (S.Coin 1)
        (SlotNo 1)
        S.SNothing
        S.SNothing
    wits = mempty { scriptWits = Map.singleton scriptHash script }
    txs = S.Tx txb wits S.SNothing
    txa = fromRight . runExcept $ translateEra @Allegra () txs
    result = runShelleyBase $
      applySTSTest @(S.LEDGER Allegra)
        (TRC (env, (utxoStAllegra, def), txa))
    in
    case result of
      Left e -> error $ show e
      Right _ -> pure ()
