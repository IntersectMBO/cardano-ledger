

module Test.Shelley.Spec.Ledger.Pretty where

import Prettyprinter.Util(putDocW)
import Cardano.Ledger.Pretty
import Shelley.Spec.Ledger.TxBody(TxBody)
import Shelley.Spec.Ledger.Tx(Tx)
import Shelley.Spec.Ledger.LedgerState(LedgerState) -- ,EpochState)
import Shelley.Spec.Ledger.UTxO(UTxO)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(C)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.QuickCheck
  ( Arbitrary(..),
    generate,
    Gen,
  )

txbody :: Gen(TxBody C)
txbody = (arbitrary :: Gen(TxBody C))

tx :: Gen(Tx C)
tx = (arbitrary :: Gen(Tx C))

utxo :: Gen(UTxO C)
utxo = (arbitrary :: Gen(UTxO C))

ls :: Gen(LedgerState C)
ls = (arbitrary :: Gen(LedgerState C))

-- es = (arbitrary :: Gen(EpochState C))

main4, main5, main6, main7 :: Int -> IO ()

main4 n = do
  b <- generate txbody
  let doc = ppTxBody b
  putDocW n doc
  putStrLn ""


main5 n = do
  b <- generate tx
  let doc = ppTx b
  putDocW n doc
  putStrLn ""

main6 n = do
  b <- generate utxo
  let doc = ppUTxO b
  putDocW n doc
  putStrLn ""

main7 n = do
  b <- generate ls
  let doc = ppLedgerState b
  putDocW n doc
  putStrLn ""
