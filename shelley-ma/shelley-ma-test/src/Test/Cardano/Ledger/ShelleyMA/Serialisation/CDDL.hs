{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.CDDL 
  ( cddlTests,
  )
where


import Test.Shelley.Spec.Ledger.Serialisation.CDDLUtils
  ( cddlTest,
    cddlTest',
  )


import qualified Cardano.Ledger.Core as Core
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Allegra (AllegraEra)

import Test.Tasty (TestTree, withResource, testGroup)
import qualified Data.ByteString.Lazy as BSL

type A = AllegraEra C_Crypto
type M = MaryEra C_Crypto

cddlTests :: Int -> TestTree
cddlTests n = withResource combinedCDDL (const (pure ())) $ \cddl ->
  testGroup "CDDL roundtrip tests" $
    [ cddlTest @(Core.Value A) n "coin"
    --, cddlTest @(Core.Value M) n "value"
    -- , cddlTest' @(Core.TxBody M) n "transaction_body"
    -- , cddlTest' @(Core.TxBody A) n "transaction_body"
    , cddlTest' @(Core.Script M) n "native_script"
    , cddlTest' @(Core.Script A) n "native_script"
    ]
      <*> pure cddl

combinedCDDL :: IO BSL.ByteString
combinedCDDL = do
  base <- BSL.readFile "cddl-files/shelley-ma.cddl"
  crypto <- BSL.readFile "cddl-files/mock/crypto.cddl"
  extras <- BSL.readFile "cddl-files/mock/extras.cddl"
  pure $ base <> crypto <> extras
