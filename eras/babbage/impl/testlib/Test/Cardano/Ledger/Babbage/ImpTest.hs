{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest (
  module Test.Cardano.Ledger.Alonzo.ImpTest,
  produceRefScript,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Tools (setMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl ((%=))
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (txInAt)

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (BabbageEra c)
  where
  initImpTestState = impNESL %= initAlonzoImpNES
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = alonzoFixupTx

instance ShelleyEraImp (BabbageEra c) => MaryEraImp (BabbageEra c)

instance ShelleyEraImp (BabbageEra c) => AlonzoEraImp (BabbageEra c) where
  scriptTestContexts = plutusTestScripts SPlutusV1 <> plutusTestScripts SPlutusV2

produceRefScript ::
  (ShelleyEraImp era, BabbageEraTxOut era) =>
  Script era ->
  ImpTestM era (TxIn (EraCrypto era))
produceRefScript script = do
  addr <- freshKeyAddr_
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let txOutZero = mkBasicTxOut addr mempty & referenceScriptTxOutL .~ SJust script
      txOut = setMinCoinTxOut pp txOutZero
      txBody = mkBasicTxBody & outputsTxBodyL .~ SSeq.singleton txOut
  txInAt (0 :: Int) <$> submitTx (mkBasicTx txBody)
