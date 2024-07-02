{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest (
  module Test.Cardano.Ledger.Alonzo.ImpTest,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Lens.Micro.Mtl ((%=))
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp LedgerState (BabbageEra c)
  where
  initImpTestState = impNESL %= initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx

instance ShelleyEraImp ls (BabbageEra c) => MaryEraImp ls (BabbageEra c)

instance ShelleyEraImp ls (BabbageEra c) => AlonzoEraImp ls (BabbageEra c) where
  scriptTestContexts = plutusTestScripts SPlutusV1 <> plutusTestScripts SPlutusV2
