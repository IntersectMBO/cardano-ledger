{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Tools (spec) where

import Cardano.Ledger.Tools
import qualified Data.ByteString as BS
import Test.Cardano.Ledger.Common
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = describe "Tools" $ do
  prop "`integralToByteStringN . byteStringToNum` roundtrips" $ do
    bs <- arbitrary
    pure $ integralToByteStringN (BS.length bs) (byteStringToNum @Integer bs) === bs
  prop "`byteStringToNum . integralToByteStringN` roundtrips" $ do
    Positive (n :: Integer) <- arbitrary
    pure $ byteStringToNum (integralToByteStringN 64 n) === n
