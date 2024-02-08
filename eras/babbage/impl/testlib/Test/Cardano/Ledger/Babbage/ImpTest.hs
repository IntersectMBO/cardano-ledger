{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest (babbageFixupTx) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (
  AlonzoEraTxWits (..),
  BabbageEraTxBody (..),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.UTxO (EraUTxO (..))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.ImpTest (
  ImpTestM,
  addCollateralInput,
  addNativeScriptTxWits,
  addRootTxIn,
  fixupDatums,
  fixupFees,
  fixupPPHash,
  fixupPlutusScripts,
  initAlonzoImpNES,
  updateAddrTxWits,
 )
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp (..))

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (BabbageEra c)
  where
  initImpNES = initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = babbageFixupTx

babbageFixupTx ::
  ( ShelleyEraImp era
  , AlonzoEraTxWits era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BabbageEraTxBody era
  , AlonzoEraUTxO era
  ) =>
  Tx era ->
  ImpTestM era (Tx era)
babbageFixupTx =
  addNativeScriptTxWits
    >=> addCollateralInput
    >=> addRootTxIn
    >=> fixupPlutusScripts
    >=> fixupDatums
    >=> fixupPPHash
    >=> fixupFees
    >=> updateAddrTxWits
