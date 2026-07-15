module Main (
  main,
) where

import System.Environment (lookupEnv)
import Test.Cardano.Ledger.CanonicalState.Reference (loadAllReferenceCDDLs)
import qualified Test.Cardano.Ledger.CanonicalState.Spec
import Test.Cardano.Ledger.Common

main :: IO ()
main = do
  mReferenceCDDLs <- loadAllReferenceCDDLs

  sampleCount <- lookupEnv "CONFORMANCE_SAMPLES" >>= \m -> pure $ maybe 1000 read m

  ledgerTestMain $
    Test.Cardano.Ledger.CanonicalState.Spec.spec mReferenceCDDLs sampleCount
