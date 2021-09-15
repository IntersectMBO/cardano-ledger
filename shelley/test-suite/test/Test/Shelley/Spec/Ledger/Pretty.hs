module Test.Shelley.Spec.Ledger.Pretty (testwidth, prettyTest) where

import Cardano.Ledger.Pretty
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter.Util (putDocW)
import Shelley.Spec.Ledger.LedgerState (LedgerState) -- ,EpochState)
import Shelley.Spec.Ledger.Tx (Tx)
import Shelley.Spec.Ledger.TxBody (TxBody)
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, generate, testProperty, withMaxSuccess, (===))

-- ====================================================
-- a few generators to generate random UTxO, TxBody, Tx and LedgerState

txbody :: Gen (TxBody C)
txbody = (arbitrary :: Gen (TxBody C))

tx :: Gen (Tx C)
tx = (arbitrary :: Gen (Tx C))

utxo :: Gen (UTxO C)
utxo = (arbitrary :: Gen (UTxO C))

ls :: Gen (LedgerState C)
ls = (arbitrary :: Gen (LedgerState C))

-- | Used to test pretty printing things with different widths
--   for example: testwidth 120 ls ppLedgerState
--   prints a random LedgerState with a max width of 120 columns
--   one can use this to observe the how "pretty" a printer is at different widths
testwidth :: Int -> Gen a -> (a -> PDoc) -> IO ()
testwidth n gen pp = do
  b <- generate gen
  let doc = pp b
  putDocW n doc
  putStrLn ""

-- | The idea is to see that the pretty printer actually produces some output
testPP :: String -> (t -> PDoc) -> Gen t -> TestTree
testPP name ppT gen = testProperty name (do t <- gen; pure (withMaxSuccess 25 (toText (ppT t) === toText (ppT t))))
  where
    toText doc = renderStrict (layoutPretty defaultLayoutOptions doc)

prettyTest :: TestTree
prettyTest =
  testGroup
    "Pretty printer tests"
    [ testPP "UTxO" ppUTxO utxo,
      testPP "TxBody" ppTxBody txbody,
      testPP "Tx" ppTx tx,
      testPP "LedgerState" ppLedgerState ls
    ]
