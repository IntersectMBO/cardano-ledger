{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ImpTest () where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Core (EraIndependentTxBody)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Mary (MaryEra)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.TreeDiff ()
import Test.Cardano.Ledger.Shelley.ImpTest (
  ShelleyEraImp (..),
  initShelleyImpNES,
 )

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (MaryEra c)
  where
  initImpNES = initShelleyImpNES
