{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.Plutus.Context (
  BabelEraPlutusTxInfo (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Core (
  PParamsUpdate,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import qualified PlutusLedgerApi.V4 as PV4

-- ===========================================================
-- A class to compute the changed parameters in the TxInfo
-- given a ToPlutusData instance for PParamsUpdate

class
  ( ToPlutusData (PParamsUpdate era)
  , EraPlutusTxInfo l era
  ) =>
  BabelEraPlutusTxInfo (l :: Language) era
  where
  toBabelPlutusChangedParameters :: proxy l -> PParamsUpdate era -> PV4.ChangedParameters
