{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.Plutus.Context (
  ConwayEraPlutusTxInfo (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Core (PParamsUpdate)
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified PlutusLedgerApi.V3 as PV3

-- | A class to compute the changed parameters in the TxInfo
-- given a ToPlutusData instance for PParamsUpdate
class
  EraPlutusTxInfo l era =>
  ConwayEraPlutusTxInfo (l :: Language) era
  where
  toPlutusChangedParameters :: proxy l -> PParamsUpdate era -> PV3.ChangedParameters
