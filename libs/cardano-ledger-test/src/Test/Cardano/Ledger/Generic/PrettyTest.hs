module Test.Cardano.Ledger.Generic.PrettyTest (testwidth, prettyTest) where

import Cardano.Ledger.Core (Tx, TxBody)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.UTxO (UTxO)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Prettyprinter.Util (putDocW)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Generic.Proof (Proof (Shelley))
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, generate, testProperty, withMaxSuccess, (===))

-- ====================================================
-- a few generators to generate random UTxO, TxBody, Tx and LedgerState

txbody :: Gen (TxBody Shelley)
txbody = arbitrary

tx :: Gen (Tx Shelley)
tx = arbitrary

utxo :: Gen (UTxO Shelley)
utxo = arbitrary

ls :: Gen (LedgerState Shelley)
ls = arbitrary

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
    [ testPP "UTxO" (pcUTxO proof) utxo
    , testPP "TxBody" (pcTxBody proof) txbody
    , testPP "Tx" (pcTx proof) tx
    , testPP "LedgerState" (pcLedgerState proof) ls
    ]
  where
    proof = Shelley
