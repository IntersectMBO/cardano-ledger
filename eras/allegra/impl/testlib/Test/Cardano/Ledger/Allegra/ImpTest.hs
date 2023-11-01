{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.ImpTest () where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Crypto (Crypto (..))
import Test.Cardano.Ledger.Shelley.ImpTest (
  ShelleyEraImp (..),
  emptyShelleyImpNES,
  shelleyImpWitsVKeyNeeded,
 )

instance
  ( Crypto c
  , Signable
      (DSIGN c)
      (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AllegraEra c)
  where
  emptyImpNES = emptyShelleyImpNES

  impWitsVKeyNeeded = shelleyImpWitsVKeyNeeded
