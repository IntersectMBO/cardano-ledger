{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.BinarySpec (spec) where

import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Mary
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Cardano.Ledger.Core (Era(PreviousEra, eraName))

-- Note that we unfold this here rather than using 'specUpgrade' since
-- 'ShelleyTxWits' does not implement 'Memoizable' and as such cannot be tested
-- in the same way as other types - we need to manually unpack the relevant
-- parts for comparison.
spec :: Spec
spec =
  describe ("Upgrade from " ++ eraName @(PreviousEra Mary) ++ " to " ++ eraName @Mary) $ do
    specTxOutUpgrade @Mary
    specTxCertUpgrade @Mary
    specTxAuxDataUpgrade @Mary @AllegraTxAuxData
    -- specTxWitsUpgrade @Mary @txWits
    specTxBodyUpgrade @Mary @MaryTxBody
    when False $ specTxUpgrade @Mary @ShelleyTx
    specScriptUpgrade @Mary
