{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Imp (
  spec,
  Shelley.shelleyToBabbageSpec,
  alonzoToConwaySpec,
  alonzoOnlySpec,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusTxInfo (..),
  LedgerLevelTxInfo (..),
  PlutusTxInfoResult (..),
 )
import Cardano.Ledger.BaseTypes (Globals (..), Inject (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Event)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Alonzo.Era (mkTestLedgerTxInfo)
import qualified Test.Cardano.Ledger.Alonzo.Imp.BbodySpec as BBODY
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as UTXOS
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as UTXOW
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as Mary
import qualified Test.Cardano.Ledger.Shelley.Imp as Shelley

spec ::
  ( AlonzoEraImp era
  , Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Mary.spec era
  describe "AlonzoEra Onwards" $ withImpInitEachEraVersion era $ do
    BBODY.spec
    UTXO.spec
    UTXOS.spec
    UTXOW.spec

alonzoToConwaySpec ::
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  proxy era ->
  Spec
alonzoToConwaySpec era = withImpInitEachEraVersion era $ do
  describe "AlonzoEra Specific" $ do
    UTXOW.alonzoToConwaySpec

alonzoOnlySpec :: Spec
alonzoOnlySpec =
  withImpInitEachEraVersion (Proxy @AlonzoEra) $
    describe "PlutusV1" $ do
      it "toPlutusTxInfo does not fail when Byron scripts are present in TxOuts" $ do
        pv <- getProtVer
        Globals {epochInfo, systemStart} <- use impGlobalsL
        (_, shelleyAddr) <- freshKeyAddr
        byronAddr <- AddrBootstrap <$> freshBootstrapAddress
        shelleyTxIn <- sendCoinTo shelleyAddr mempty
        utxo <- getUTxO
        let
          byronTxOut = mkBasicTxOut byronAddr . inject $ Coin 1
          tx :: Tx TopTx AlonzoEra
          tx =
            mkBasicTx @AlonzoEra mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton shelleyTxIn
              & bodyTxL . outputsTxBodyL .~ SSeq.singleton byronTxOut
          lti = mkTestLedgerTxInfo pv epochInfo systemStart utxo tx $ LedgerTopTxInfo mempty
        void $ expectRight $ unPlutusTxInfoResult $ toPlutusTxInfo SPlutusV1 lti
      it "toPlutusTxInfo does not fail when Byron scripts are present in TxIns" $ do
        pv <- getProtVer
        Globals {epochInfo, systemStart} <- use impGlobalsL
        (_, shelleyAddr) <- freshKeyAddr
        byronAddr <- AddrBootstrap <$> freshBootstrapAddress
        byronTxIn <- sendCoinTo byronAddr mempty
        utxo <- getUTxO
        let
          shelleyTxOut = mkBasicTxOut shelleyAddr . inject $ Coin 1
          tx :: Tx TopTx AlonzoEra
          tx =
            mkBasicTx @AlonzoEra mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton byronTxIn
              & bodyTxL . outputsTxBodyL .~ SSeq.singleton shelleyTxOut
          lti = mkTestLedgerTxInfo pv epochInfo systemStart utxo tx $ LedgerTopTxInfo mempty
        void $ expectRight $ unPlutusTxInfoResult $ toPlutusTxInfo SPlutusV1 lti
