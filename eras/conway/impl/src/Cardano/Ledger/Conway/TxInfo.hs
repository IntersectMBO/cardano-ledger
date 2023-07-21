{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxInfo (conwayTxInfo) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.TxInfo (
  EraPlutusContext,
  TranslationError (..),
  VersionedTxInfo (..),
  unTxCertV3,
 )
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Babbage.TxBody (
  AllegraEraTxBody (..),
  AlonzoEraTxBody (..),
  BabbageEraTxBody (..),
  MaryEraTxBody (..),
 )
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfoV1, babbageTxInfoV2)
import qualified Cardano.Ledger.Babbage.TxInfo as B
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.Monad (zipWithM)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Lens.Micro
import qualified PlutusLedgerApi.V3 as PV3

-- TODO implement this once Plutus releases an API with new certs

instance (Crypto c, EraTxCert (ConwayEra c)) => EraPlutusContext 'PlutusV1 (ConwayEra c) where
  transTxCert = undefined

instance (Crypto c, EraTxCert (ConwayEra c)) => EraPlutusContext 'PlutusV2 (ConwayEra c) where
  transTxCert = undefined

instance (Crypto c, EraTxCert (ConwayEra c)) => EraPlutusContext 'PlutusV3 (ConwayEra c) where
  transTxCert = undefined

conwayTxInfo ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusContext 'PlutusV1 era
  , EraPlutusContext 'PlutusV2 era
  , EraPlutusContext 'PlutusV3 era
  ) =>
  PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
conwayTxInfo pp lang ei sysS utxo tx = do
  timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
  case lang of
    PlutusV1 -> babbageTxInfoV1 timeRange tx utxo
    PlutusV2 -> babbageTxInfoV2 timeRange tx utxo
    PlutusV3 -> conwayTxInfoV3 timeRange tx utxo
  where
    interval = tx ^. bodyTxL ^. vldtTxBodyL

conwayTxInfoV3 ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusContext 'PlutusV3 era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PV3.POSIXTimeRange ->
  Tx era ->
  UTxO era ->
  Either (TranslationError (EraCrypto era)) VersionedTxInfo
conwayTxInfoV3 timeRange tx utxo = do
  inputs <- mapM (B.txInfoInV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
  refInputs <- mapM (B.txInfoInV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
  outputs <-
    zipWithM
      (B.txInfoOutV2 . Alonzo.TxOutFromOutput)
      [minBound ..]
      (foldr (:) [] outs)
  rdmrs' <- mapM (B.transRedeemerPtr txBody) rdmrs
  pure . TxInfoPV3 $
    PV3.TxInfo -- TODO Add relevant CIP-1694 data to PV3.TxInfo
      { PV3.txInfoInputs = inputs
      , PV3.txInfoOutputs = outputs
      , PV3.txInfoReferenceInputs = refInputs
      , PV3.txInfoFee = Alonzo.transValue (inject @(MaryValue (EraCrypto era)) fee)
      , -- Note that this translation is different from previous Plutus versions, sine we no
        -- longer add a zero ADA value to the mint field during translation:
        PV3.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
      , PV3.txInfoDCert = toList $ fmap (unTxCertV3 . Alonzo.transTxCert) (txBody ^. certsTxBodyL)
      , PV3.txInfoWdrl = PV3.fromList $ Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
      , PV3.txInfoValidRange = timeRange
      , PV3.txInfoSignatories =
          map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
      , PV3.txInfoRedeemers = PV3.fromList rdmrs'
      , PV3.txInfoData = PV3.fromList $ map Alonzo.transDataPair datpairs
      , PV3.txInfoId = PV3.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
      }
  where
    txBody = tx ^. bodyTxL
    witnesses = tx ^. witsTxL
    outs = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)
    rdmrs = Map.toList (unRedeemers $ witnesses ^. rdmrsTxWitsL)
