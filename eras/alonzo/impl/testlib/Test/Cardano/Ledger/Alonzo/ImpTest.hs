{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.ImpTest (
  module ImpTest,
  emptyAlonzoImpNES,
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxValSizeL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraIndependentTxBody, EraTxOut)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  StashedAVVMAddresses,
  curPParamsEpochStateL,
  nesEsL,
 )
import Data.Default.Class (Default)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Shelley.ImpTest as ImpTest

emptyAlonzoImpNES ::
  ( EraGov era
  , EraTxOut era
  , AlonzoEraPParams era
  , Default (StashedAVVMAddresses era)
  ) =>
  Coin ->
  NewEpochState era
emptyAlonzoImpNES rootCoin =
  emptyShelleyImpNES rootCoin
    & nesEsL . curPParamsEpochStateL . ppMaxValSizeL .~ 1_000_000_000

instance
  ( Crypto c
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AlonzoEra c)
  where
  emptyImpNES = emptyAlonzoImpNES

  impWitsVKeyNeeded = shelleyImpWitsVKeyNeeded
