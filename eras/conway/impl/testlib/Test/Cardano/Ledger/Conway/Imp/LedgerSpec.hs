{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.LedgerSpec (spec) where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..), maxRefScriptSizePerTx)
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.SafeHash (originalBytesSize)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysFailsWithDatum)

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec =
  it "TxRefScriptsSizeTooBig" $ do
    -- we use here the largest script we currently have as many times as necessary to
    -- trigger the predicate failure
    Just plutusScript <- pure $ mkPlutusScript @era $ alwaysFailsWithDatum SPlutusV3
    let script :: Script era
        script = fromPlutusScript plutusScript
        size = originalBytesSize script
        (q, r) = maxRefScriptSizePerTx `quotRem` size
        n = q + signum r
    txIns <- replicateM n (produceRefScript script)
    let tx :: Tx era
        tx = mkBasicTx (mkBasicTxBody & referenceInputsTxBodyL .~ Set.fromList txIns)
    submitFailingTx
      tx
      [ injectFailure $ ConwayTxRefScriptsSizeTooBig (size * n) maxRefScriptSizePerTx
      ]
