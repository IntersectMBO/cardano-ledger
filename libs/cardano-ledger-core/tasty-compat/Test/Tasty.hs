module Test.Tasty where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Tasty.QuickCheck (QuickCheckMaxRatio (..))

type TestName = String

type TestTree = Spec

defaultMain :: Spec -> IO ()
defaultMain = hspec

testGroup :: HasCallStack => String -> [SpecWith a] -> SpecWith a
testGroup s = describe s . sequence_

localOption :: QuickCheckMaxRatio -> SpecWith a -> SpecWith a
localOption (QuickCheckMaxRatio i) = modifyMaxDiscardRatio (const i)
