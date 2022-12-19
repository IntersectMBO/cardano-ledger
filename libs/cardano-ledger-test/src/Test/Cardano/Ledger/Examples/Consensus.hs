{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Examples.Consensus (genericConsensusTest) where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Pretty (PDoc, ppString, text)
import Prettyprinter (vsep)
import Test.Cardano.Ledger.Generic.Consensus (ledgerExamples, oldLedgerExamples)
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..), Reflect (..))
import Test.Cardano.Ledger.Generic.Same (Same (..))
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

-- ===============================================================

oneTest :: (Reflect era, EraCrypto era ~ StandardCrypto) => Proof era -> Assertion
oneTest proof = assertBool (displayResults results) (null results)
  where
    old = oldLedgerExamples proof
    new = ledgerExamples proof
    equal =
      case proof of
        Shelley _ -> old == new
        Allegra _ -> old == new
        Mary _ -> old == new
        Alonzo _ -> old == new
        Babbage _ -> old == new
        Conway _ -> old == new
    -- A result is bad, if the snd component is a Just (i.e. contains a description of an inequality)
    bad (_, Nothing) = False
    bad (_, Just _) = True
    results = if equal then [] else filter bad (same proof old new)
    displayResults :: [(String, Maybe PDoc)] -> String
    displayResults = show . vsep . map f
      where
        f (_label, Nothing) = mempty
        f (label, Just doc) =
          vsep
            [ text "======================",
              ppString ("path to unequal parts = " ++ label),
              text "------",
              doc,
              text " "
            ]

genericConsensusTest :: TestTree
genericConsensusTest =
  testGroup
    "Generic Consensus examples agree with non-generic ones."
    [ testCase "Shelley" (oneTest (Shelley Standard)),
      testCase "Allegra" (oneTest (Allegra Standard)),
      testCase "Mary" (oneTest (Mary Standard)),
      testCase "Alonzo" (oneTest (Alonzo Standard)),
      testCase "Babbage" (oneTest (Babbage Standard))
      -- testCase "Conway" (oneTest (Conway Standard)) TODO
    ]
