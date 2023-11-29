{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest () where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Crypto (Crypto (..))
import Test.Cardano.Ledger.Alonzo.ImpTest (emptyAlonzoImpNES)
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Shelley.ImpTest (
  ShelleyEraImp (..),
  shelleyImpWitsVKeyNeeded,
 )

instance
  ( Crypto c
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (BabbageEra c)
  where
  emptyImpNES = emptyAlonzoImpNES

  impWitsVKeyNeeded = shelleyImpWitsVKeyNeeded
