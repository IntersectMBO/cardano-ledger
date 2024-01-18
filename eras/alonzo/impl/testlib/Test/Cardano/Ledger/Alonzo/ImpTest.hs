{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.ImpTest (
  module ImpTest,
  initAlonzoImpNES,
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxValSizeL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  nesEsL,
 )
import Data.Default.Class (Default)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.ImpTest as ImpTest

initAlonzoImpNES ::
  ( AlonzoEraPParams era
  , Default (StashedAVVMAddresses era)
  , ShelleyEraImp era
  ) =>
  Coin ->
  NewEpochState era
initAlonzoImpNES rootCoin =
  initShelleyImpNES rootCoin
    & nesEsL . curPParamsEpochStateL . ppMaxValSizeL .~ 1_000_000_000

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AlonzoEra c)
  where
  initImpNES = initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
