{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ < 900
{-# LANGUAGE IncoherentInstances #-}
#endif
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.ImpTest (
  module Test.Cardano.Ledger.Alonzo.ImpTest,
  produceRefScript,
  produceRefScripts,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.Tools (setMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import Control.Monad (forM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~), (<>~))
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Plutus (testingCostModels)

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (BabbageEra c)
  where
  initNewEpochState =
    defaultInitNewEpochState
      (nesEsL . curPParamsEpochStateL . ppCostModelsL <>~ testingCostModels [PlutusV2])
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
  txIn :| [] <- produceRefScripts $ script :| []
  pure txIn

produceRefScripts ::
  (ShelleyEraImp era, BabbageEraTxOut era) =>
  NonEmpty (Script era) ->
  ImpTestM era (NonEmpty (TxIn (EraCrypto era)))
produceRefScripts scripts = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  txOuts <- forM scripts $ \script -> do
    addr <- freshKeyAddr_
    let txOutZero =
          mkBasicTxOut addr mempty & referenceScriptTxOutL .~ SJust script
    pure $ setMinCoinTxOut pp txOutZero
  let txBody = mkBasicTxBody & outputsTxBodyL .~ SSeq.fromList (NE.toList txOuts)
  txId <- txIdTx <$> submitTx (mkBasicTx txBody)
  pure $ NE.zipWith (\_ -> mkTxInPartial txId) scripts (0 :| [1 ..])
