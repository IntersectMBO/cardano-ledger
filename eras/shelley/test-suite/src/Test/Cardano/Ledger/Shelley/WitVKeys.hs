module Test.Cardano.Ledger.Shelley.WitVKeys (tests) where

import Cardano.Ledger.Core
import Data.List (nub, sort)
import Data.Set as Set (fromList, singleton)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Utils (RawSeed, mkKeyPair')
import Test.QuickCheck (conjoin, (===), (==>))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import qualified Test.Tasty.QuickCheck as TQC

tests :: TestTree
tests = testProperty "WitVKey does not brake containers due to invalid Ord" witVKeysProp

witVKeysProp ::
  RawSeed ->
  SafeHash EraIndependentTxBody ->
  SafeHash EraIndependentTxBody ->
  TQC.Property
witVKeysProp seed h1 h2 =
  let kp = mkKeyPair' seed
      w1 = mkWitnessVKey h1 kp
      w2 = mkWitnessVKey h2 kp
   in conjoin
        [ sort [w1, w2] === sort [w2, w1]
        , length (nub [w1, w2]) === length (Set.fromList [w1, w2])
        , w1 /= w2 ==> length (Set.singleton w1 <> Set.singleton w2) === 2
        ]
