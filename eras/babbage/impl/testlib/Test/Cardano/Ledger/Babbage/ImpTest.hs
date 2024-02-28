{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest () where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (..))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.ImpTest (
  alonzoFixupTx,
  initAlonzoImpNES,
 )
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp (..))

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (BabbageEra c)
  where
  initImpNES = initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx
