{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.ScriptTranslation (
  testScriptPostTranslation,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  pattern RequireAllOf,
 )
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Except (runExcept)
import Control.State.Transition.Extended (TRC (..))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

bootstrapTxId :: S.TxId StandardCrypto
bootstrapTxId = txIdTxBody @Shelley mkBasicTxBody

fromRight :: HasCallStack => Either e a -> a
fromRight (Right x) = x
fromRight _ = error "Expected Right"

script :: MultiSig Shelley
script = RequireAllOf StrictSeq.empty

scriptHash :: S.ScriptHash StandardCrypto
scriptHash = hashScript @Shelley script

testScriptPostTranslation :: TestTree
testScriptPostTranslation =
  testCase
    "we should still be able to spend a translated script"
    $ let addr =
            S.Addr
              S.Testnet
              (S.ScriptHashObj scriptHash)
              S.StakeRefNull
          utxo =
            S.UTxO $
              Map.singleton
                (S.TxIn bootstrapTxId minBound)
                (S.ShelleyTxOut addr (Val.inject (S.Coin 1)))
          env = S.LedgerEnv (SlotNo 0) minBound emptyPParams (S.AccountState (S.Coin 0) (S.Coin 0)) False
          utxoStShelley = def {S.utxosUtxo = utxo}
          utxoStAllegra = fromRight . runExcept $ translateEra @Allegra NoGenesis utxoStShelley
          txb =
            S.ShelleyTxBody
              (Set.singleton $ S.TxIn bootstrapTxId minBound)
              StrictSeq.empty
              StrictSeq.empty
              (S.Withdrawals mempty)
              (S.Coin 1)
              (SlotNo 1)
              S.SNothing
              S.SNothing
          wits = mkBasicTxWits & scriptTxWitsL .~ Map.singleton scriptHash script
          txs = S.ShelleyTx txb wits S.SNothing
          txa = fromRight . runExcept $ translateEra @Allegra NoGenesis txs
          result =
            runShelleyBase $
              applySTSTest @(S.ShelleyLEDGER Allegra)
                (TRC (env, LedgerState utxoStAllegra def, txa))
       in case result of
            Left e -> error $ show e
            Right _ -> pure ()
