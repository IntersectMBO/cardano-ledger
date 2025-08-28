{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxInfo (
  transPlutusPurposeV1V2,
  transPlutusPurposeV3,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxCert,
  PlutusTxInfo,
  SupportedLanguage (..),
  toPlutusWithContext,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem)
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (Inject (..), ProtVer (..), strictMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxInfo (ConwayContextError (..), ConwayEraPlutusTxInfo (..))
import qualified Cardano.Ledger.Conway.TxInfo as Conway
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..), PlutusScript (..))
import Cardano.Ledger.Dijkstra.TxCert ()
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Plutus (
  Language (..),
  PlutusArgs (..),
  SLanguage (..),
  TxOutSource (..),
  transCoinToLovelace,
  transCoinToValue,
  transDatum,
 )
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Control.Monad (zipWithM)
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

instance EraPlutusContext DijkstraEra where
  type ContextError DijkstraEra = ConwayContextError DijkstraEra

  data TxInfoResult DijkstraEra
    = DijkstraTxInfoResult -- Fields must be kept lazy
        (Either (ContextError DijkstraEra) (PlutusTxInfo 'PlutusV1))
        (Either (ContextError DijkstraEra) (PlutusTxInfo 'PlutusV2))
        (Either (ContextError DijkstraEra) (PlutusTxInfo 'PlutusV3))
        (Either (ContextError DijkstraEra) (PlutusTxInfo 'PlutusV4))

  mkSupportedLanguage = \case
    PlutusV1 -> Just $ SupportedLanguage SPlutusV1
    PlutusV2 -> Just $ SupportedLanguage SPlutusV2
    PlutusV3 -> Just $ SupportedLanguage SPlutusV3
    PlutusV4 -> Just $ SupportedLanguage SPlutusV4

  mkTxInfoResult lti =
    DijkstraTxInfoResult
      (toPlutusTxInfo SPlutusV1 lti)
      (toPlutusTxInfo SPlutusV2 lti)
      (toPlutusTxInfo SPlutusV3 lti)
      (toPlutusTxInfo SPlutusV4 lti)

  lookupTxInfoResult SPlutusV1 (DijkstraTxInfoResult tirPlutusV1 _ _ _) = tirPlutusV1
  lookupTxInfoResult SPlutusV2 (DijkstraTxInfoResult _ tirPlutusV2 _ _) = tirPlutusV2
  lookupTxInfoResult SPlutusV3 (DijkstraTxInfoResult _ _ tirPlutusV3 _) = tirPlutusV3
  lookupTxInfoResult SPlutusV4 (DijkstraTxInfoResult _ _ _ tirPlutusV4) = tirPlutusV4

  mkPlutusWithContext = \case
    DijkstraPlutusV1 p -> toPlutusWithContext $ Left p
    DijkstraPlutusV2 p -> toPlutusWithContext $ Left p
    DijkstraPlutusV3 p -> toPlutusWithContext $ Left p
    DijkstraPlutusV4 p -> toPlutusWithContext $ Left p

transPlutusPurposeV1V2 ::
  forall l era proxy.
  ( EraPlutusTxInfo l era
  , PlutusTxCert l ~ PV2.DCert
  , Inject (ConwayPlutusPurpose AsItem era) (PlutusPurpose AsItem era)
  , Inject (DijkstraPlutusPurpose AsItem era) (PlutusPurpose AsItem era)
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  proxy l ->
  ProtVer ->
  DijkstraPlutusPurpose AsItem era ->
  Either (ContextError era) PV2.ScriptPurpose
transPlutusPurposeV1V2 proxy pv = \case
  DijkstraSpending txIn -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwaySpending txIn
  DijkstraMinting policyId -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwayMinting policyId
  DijkstraCertifying txCert -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwayCertifying txCert
  DijkstraRewarding rewardAccount -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwayRewarding rewardAccount
  DijkstraVoting voting -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwayVoting voting
  DijkstraProposing proposing -> Conway.transPlutusPurposeV1V2 proxy pv $ ConwayProposing proposing
  purpose -> Left $ inject $ PlutusPurposeNotSupported @era $ inject purpose

transPlutusPurposeV3 ::
  forall era.
  ( ConwayEraPlutusTxInfo PlutusV3 era
  , Inject (ConwayContextError era) (ContextError era)
  , Inject (DijkstraPlutusPurpose AsIxItem era) (PlutusPurpose AsIxItem era)
  ) =>
  ProtVer ->
  DijkstraPlutusPurpose AsIxItem era ->
  Either (ContextError era) PV3.ScriptPurpose
transPlutusPurposeV3 pv = \case
  DijkstraSpending txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwaySpending txIn
  DijkstraMinting txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwayMinting txIn
  DijkstraCertifying txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwayCertifying txIn
  DijkstraRewarding txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwayRewarding txIn
  DijkstraVoting txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwayVoting txIn
  DijkstraProposing txIn -> Conway.transPlutusPurposeV3 @PlutusV3 @era Proxy pv $ ConwayProposing txIn
  purpose ->
    Left $ inject $ PlutusPurposeNotSupported @era . hoistPlutusPurpose @era toAsItem $ inject purpose

instance EraPlutusTxInfo 'PlutusV1 DijkstraEra where
  toPlutusTxCert _ _ = Conway.transTxCertV1V2

  toPlutusScriptPurpose proxy pv =
    transPlutusPurposeV1V2 proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    Conway.guardConwayFeaturesForPlutusV1V2 ltiTx
    timeRange <- Conway.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Conway.transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    mapM_ (Conway.transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Conway.transTxOutV1 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txCerts
        , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV1.txInfoData = Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV1.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = Alonzo.toPlutusV1Args

instance EraPlutusTxInfo 'PlutusV2 DijkstraEra where
  toPlutusTxCert _ _ = Conway.transTxCertV1V2

  toPlutusScriptPurpose proxy pv = transPlutusPurposeV1V2 proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    Conway.guardConwayFeaturesForPlutusV1V2 ltiTx
    timeRange <-
      Conway.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Babbage.transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (Babbage.transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer ltiTx
    pure
      PV2.TxInfo
        { PV2.txInfoInputs = inputs
        , PV2.txInfoOutputs = outputs
        , PV2.txInfoReferenceInputs = refInputs
        , PV2.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
        , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV2.txInfoDCert = txCerts
        , PV2.txInfoWdrl = PV2.unsafeFromList $ Alonzo.transTxBodyWithdrawals txBody
        , PV2.txInfoValidRange = timeRange
        , PV2.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV2.txInfoRedeemers = plutusRedeemers
        , PV2.txInfoData = PV2.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV2.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = Babbage.toPlutusV2Args

instance EraPlutusTxInfo 'PlutusV3 DijkstraEra where
  toPlutusTxCert _ pv = pure . Conway.transTxCert pv

  toPlutusScriptPurpose _ = transPlutusPurposeV3

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    timeRange <-
      Conway.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    let
      txInputs = txBody ^. inputsTxBodyL
      refInputs = txBody ^. referenceInputsTxBodyL
    inputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList txInputs)
    refInputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList refInputs)
    let
      commonInputs = txInputs `Set.intersection` refInputs
    case toList commonInputs of
      (x : xs) -> Left $ ReferenceInputsNotDisjointFromInputs $ x :| xs
      _ -> Right ()
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer ltiTx
    pure
      PV3.TxInfo
        { PV3.txInfoInputs = inputsInfo
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputsInfo
        , PV3.txInfoFee = transCoinToLovelace (txBody ^. feeTxBodyL)
        , PV3.txInfoMint = Conway.transMintValue (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txCerts
        , PV3.txInfoWdrl = Conway.transTxBodyWithdrawals txBody
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV3.txInfoRedeemers = plutusRedeemers
        , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV3.txInfoId = Conway.transTxBodyId txBody
        , PV3.txInfoVotes = Conway.transVotingProcedures (txBody ^. votingProceduresTxBodyL)
        , PV3.txInfoProposalProcedures =
            map (Conway.transProposal proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
        , PV3.txInfoCurrentTreasuryAmount =
            strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
        , PV3.txInfoTreasuryDonation =
            case txBody ^. treasuryDonationTxBodyL of
              Coin 0 -> Nothing
              coin -> Just $ transCoinToLovelace coin
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = Conway.toPlutusV3Args

instance ConwayEraPlutusTxInfo 'PlutusV3 DijkstraEra where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))

instance ConwayEraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))

instance EraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusTxCert _ pv = pure . Conway.transTxCert pv

  toPlutusScriptPurpose _ = error "stub: PlutusV4 not yet implemented"

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    timeRange <-
      Conway.transValidityInterval ltiTx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    let
      txInputs = txBody ^. inputsTxBodyL
      refInputs = txBody ^. referenceInputsTxBodyL
    inputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList txInputs)
    refInputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList refInputs)
    let
      commonInputs = txInputs `Set.intersection` refInputs
    case toList commonInputs of
      (x : xs) -> Left $ ReferenceInputsNotDisjointFromInputs $ x :| xs
      _ -> Right ()
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer ltiTx
    pure
      PV3.TxInfo
        { PV3.txInfoInputs = inputsInfo
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputsInfo
        , PV3.txInfoFee = transCoinToLovelace (txBody ^. feeTxBodyL)
        , PV3.txInfoMint = Conway.transMintValue (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txCerts
        , PV3.txInfoWdrl = Conway.transTxBodyWithdrawals txBody
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV3.txInfoRedeemers = plutusRedeemers
        , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV3.txInfoId = Conway.transTxBodyId txBody
        , PV3.txInfoVotes = Conway.transVotingProcedures (txBody ^. votingProceduresTxBodyL)
        , PV3.txInfoProposalProcedures =
            map (Conway.transProposal proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
        , PV3.txInfoCurrentTreasuryAmount =
            strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
        , PV3.txInfoTreasuryDonation =
            case txBody ^. treasuryDonationTxBodyL of
              Coin 0 -> Nothing
              coin -> Just $ transCoinToLovelace coin
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = toPlutusV4Args

toPlutusV4Args ::
  EraPlutusTxInfo 'PlutusV4 era =>
  proxy 'PlutusV4 ->
  ProtVer ->
  PV3.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Maybe (Data era) ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV4)
toPlutusV4Args proxy pv txInfo plutusPurpose maybeSpendingData redeemerData = do
  scriptPurpose <- toPlutusScriptPurpose proxy pv plutusPurpose
  let scriptInfo =
        Conway.scriptPurposeToScriptInfo scriptPurpose (transDatum <$> maybeSpendingData)
  pure $
    PlutusV4Args $
      PV3.ScriptContext
        { PV3.scriptContextTxInfo = txInfo
        , PV3.scriptContextRedeemer = Babbage.transRedeemer redeemerData
        , PV3.scriptContextScriptInfo = scriptInfo
        }
