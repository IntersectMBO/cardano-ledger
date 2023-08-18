{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.BinarySpec (spec) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core (Era (..))
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody (AllegraTxBody)
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec

-- Note that we unfold this here rather than using 'specUpgrade' since
-- 'ShelleyTxWits' does not implement 'Memoizable' and as such cannot be tested
-- in the same way as other types - we need to manually unpack the relevant
-- parts for comparison.
spec :: Spec
spec =
  describe ("Upgrade from " ++ eraName @(PreviousEra Allegra) ++ " to " ++ eraName @Allegra) $ do
    specTxOutUpgrade @Allegra
    specTxCertUpgrade @Allegra
    specTxAuxDataUpgrade @Allegra @AllegraTxAuxData
    -- specTxWitsUpgrade @Allegra @txWits
    specTxBodyUpgrade @Allegra @AllegraTxBody
    when False $ specTxUpgrade @Allegra @ShelleyTx
    specScriptUpgrade @Allegra
