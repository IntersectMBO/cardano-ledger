module Test.Cardano.Ledger.Conway.Spec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Tx (tierRefScriptFee)
import Test.Cardano.Ledger.Common

spec :: Spec
spec = do
  describe "Various tests for functions defined in Conway" $ do
    prop "tierRefScriptFee is a linear function when growth is 1" $ \(Positive sizeIncrement) baseFee (NonNegative size) ->
      tierRefScriptFee 1 sizeIncrement baseFee size
        === Coin (floor (fromIntegral size * baseFee))
    it "tierRefScriptFee" $ do
      let step = 25600
      map (tierRefScriptFee 1.5 step 15) [0, step .. 204800]
        `shouldBe` map Coin [0, 384000, 960000, 1824000, 3120000, 5064000, 7980000, 12354000, 18915000]
