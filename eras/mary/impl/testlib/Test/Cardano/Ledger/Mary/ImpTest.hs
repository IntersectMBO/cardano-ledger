{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ImpTest (
  MaryEraImp,
  module Test.Cardano.Ledger.Allegra.ImpTest,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.TreeDiff ()

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (MaryEra c)
  where
  initImpTestState = pure ()
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = shelleyFixupTx

class
  ( ShelleyEraImp era
  , MaryEraTxBody era
  , NativeScript era ~ Timelock era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  MaryEraImp era

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  MaryEraImp (MaryEra c)
