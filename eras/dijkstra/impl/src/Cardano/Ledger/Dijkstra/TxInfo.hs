{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxInfo (
  DijkstraContextError (..),
  transFailSubTxIsNotSupported,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfo,
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  toPlutusWithContext,
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (Inject (..), ProtVer (..), kindObject, strictMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Conway.TxInfo (
  ConwayContextError (..),
  ConwayEraPlutusTxInfo (..),
  transTxInInfoV1,
  transTxInInfoV3,
 )
import qualified Cardano.Ledger.Conway.TxInfo as Conway
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (
  PlutusScript (..),
  pattern GuardingPurpose,
 )
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCert)
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Plutus (
  Language (..),
  PlutusArgs (..),
  SLanguage (..),
  TxOutSource (..),
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transDatum,
  transEpochNo,
  transKeyHash,
 )
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.State (StakePoolParams (..))
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Control.Monad (zipWithM)
import Data.Aeson (KeyValue (..), ToJSON (..))
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

data DijkstraContextError era
  = ConwayContextError !(ConwayContextError era)
  | PointerPresentInOutput !(NonEmpty (TxOut era))
  | -- | Attempt to use PlutusV1-V3 in a sub-transaction will result in this failure
    SubTxIsNotSupported !TxId
  deriving (Generic)

deriving instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  ) =>
  Eq (DijkstraContextError era)

deriving instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  ) =>
  Show (DijkstraContextError era)

instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  ) =>
  NFData (DijkstraContextError era)

instance
  ( ToJSON (TxCert era)
  , ToJSON (PlutusPurpose AsIx era)
  , ToJSON (PlutusPurpose AsItem era)
  , ToJSON (TxOut era)
  , EraPParams era
  ) =>
  ToJSON (DijkstraContextError era)
  where
  toJSON = \case
    ConwayContextError x -> toJSON x
    PointerPresentInOutput x -> kindObject "PointerPresentInOutput" ["txOut" .= toJSON x]
    SubTxIsNotSupported txId -> kindObject "SubTxIsNotSupported" ["txId" .= toJSON txId]

instance
  ( EraPParams era
  , DecCBOR (TxCert era)
  , DecCBOR (TxOut era)
  , DecCBOR (PlutusPurpose AsIx era)
  , DecCBOR (PlutusPurpose AsItem era)
  ) =>
  DecCBOR (DijkstraContextError era)
  where
  decCBOR = decode $ Summands "ContextError" $ \case
    16 -> SumD ConwayContextError <! From
    17 -> SumD PointerPresentInOutput <! From
    18 -> SumD SubTxIsNotSupported <! From
    k -> Invalid k

instance
  ( EraPParams era
  , EncCBOR (TxCert era)
  , EncCBOR (TxOut era)
  , EncCBOR (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsItem era)
  ) =>
  EncCBOR (DijkstraContextError era)
  where
  encCBOR =
    encode . \case
      ConwayContextError x -> Sum ConwayContextError 16 !> To x
      PointerPresentInOutput x -> Sum PointerPresentInOutput 17 !> To x
      SubTxIsNotSupported txId -> Sum SubTxIsNotSupported 18 !> To txId

instance Inject (ConwayContextError era) (DijkstraContextError era) where
  inject = ConwayContextError

instance Inject (Babbage.BabbageContextError era) (DijkstraContextError era) where
  inject = ConwayContextError . inject

instance Inject (Alonzo.AlonzoContextError era) (DijkstraContextError era) where
  inject = ConwayContextError . inject

instance EraPlutusContext DijkstraEra where
  type ContextError DijkstraEra = DijkstraContextError DijkstraEra

  data TxInfoResult DijkstraEra
    = DijkstraTxInfoResult -- Fields must be kept lazy
        (PlutusTxInfoResult 'PlutusV1 DijkstraEra)
        (PlutusTxInfoResult 'PlutusV2 DijkstraEra)
        (PlutusTxInfoResult 'PlutusV3 DijkstraEra)
        (PlutusTxInfoResult 'PlutusV4 DijkstraEra)

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

instance EraPlutusTxInfo 'PlutusV1 DijkstraEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose = Conway.transPlutusPurposeV1V2

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailSubTxIsNotSupported $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      timeRange <- Conway.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputs <- mapM (Conway.transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
      mapM_ (Conway.transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
      outputs <-
        zipWithM
          (Conway.transTxOutV1 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      -- It is important for memoization for `txInfo` to be a let binding
      let
        txInfo =
          PV1.TxInfo
            { PV1.txInfoInputs = inputs
            , PV1.txInfoOutputs = outputs
            , PV1.txInfoFee = transCoinToValue (txBody ^. feeTxBodyL)
            , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
            , PV1.txInfoDCert = txCerts
            , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
            , PV1.txInfoValidRange = timeRange
            , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
            , PV1.txInfoData = Alonzo.transTxWitsDatums (tx ^. witsTxL)
            , PV1.txInfoId = Alonzo.transTxBodyId txBody
            }
      Right $ \_ -> Right txInfo

  toPlutusArgs = Alonzo.toPlutusV1Args

  toPlutusTxInInfo _ = transTxInInfoV1

transTxCertV1V2 ::
  ( ConwayEraTxCert era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  TxCert era ->
  Either (ContextError era) PV1.DCert
transTxCertV1V2 = \case
  RegDepositTxCert stakeCred _deposit ->
    Right $ PV1.DCertDelegRegKey (PV1.StakingHash (transCred stakeCred))
  UnRegDepositTxCert stakeCred _refund ->
    Right $ PV1.DCertDelegDeRegKey (PV1.StakingHash (transCred stakeCred))
  DelegTxCert stakeCred (DelegStake keyHash) ->
    Right $ PV1.DCertDelegDelegate (PV1.StakingHash (transCred stakeCred)) (transKeyHash keyHash)
  RegPoolTxCert (StakePoolParams {sppId, sppVrf}) ->
    Right $
      PV1.DCertPoolRegister
        (transKeyHash sppId)
        (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes (unVRFVerKeyHash sppVrf))))
  RetirePoolTxCert poolId retireEpochNo ->
    Right $ PV1.DCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  txCert -> Left $ inject $ CertificateNotSupported txCert

instance EraPlutusTxInfo 'PlutusV2 DijkstraEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose = Conway.transPlutusPurposeV1V2

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailSubTxIsNotSupported $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      timeRange <-
        Conway.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputs <- mapM (Babbage.transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
      refInputs <- mapM (Babbage.transTxInInfoV2 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
      outputs <-
        zipWithM
          (Babbage.transTxOutV2 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer tx
      -- It is important for memoization for `txInfo` to be a let binding
      let
        txInfo =
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
            , PV2.txInfoData = PV2.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
            , PV2.txInfoId = Alonzo.transTxBodyId txBody
            }
      Right $ \_ -> Right txInfo

  toPlutusArgs = Babbage.toPlutusV2Args

  toPlutusTxInInfo _ = Babbage.transTxInInfoV2

instance EraPlutusTxInfo 'PlutusV3 DijkstraEra where
  toPlutusTxCert _ _ = pure . transTxCert

  toPlutusScriptPurpose = Conway.transPlutusPurposeV3

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailSubTxIsNotSupported $ \tx -> PlutusTxInfoResult $ do
      let
        txBody = tx ^. bodyTxL
        txInputs = txBody ^. inputsTxBodyL
        refInputs = txBody ^. referenceInputsTxBodyL
      timeRange <-
        Conway.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList txInputs)
      refInputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList refInputs)
      Conway.checkReferenceInputsNotDisjointFromInputs txBody
      outputs <-
        zipWithM
          (Babbage.transTxOutV2 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer tx
      -- It is important for memoization for `txInfo` to be a let binding
      let
        txInfo =
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
            , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
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
      Right $ \_ -> Right txInfo

  toPlutusArgs = Conway.toPlutusV3Args

  toPlutusTxInInfo _ = transTxInInfoV3

transFailSubTxIsNotSupported ::
  forall l era.
  (EraTx era, Inject (DijkstraContextError era) (ContextError era)) =>
  Tx SubTx era -> PlutusTxInfoResult l era
transFailSubTxIsNotSupported tx =
  PlutusTxInfoResult $ Left $ inject $ SubTxIsNotSupported @era (txIdTx tx)

transTxCert ::
  (ConwayEraTxCert era, TxCert era ~ DijkstraTxCert era) => TxCert era -> PV3.TxCert
transTxCert = \case
  RegPoolTxCert StakePoolParams {sppId, sppVrf} ->
    PV3.TxCertPoolRegister
      (transKeyHash sppId)
      (PV3.PubKeyHash (PV3.toBuiltin (hashToBytes (unVRFVerKeyHash sppVrf))))
  RetirePoolTxCert poolId retireEpochNo ->
    PV3.TxCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  RegDepositTxCert stakeCred deposit ->
    PV3.TxCertRegStaking (transCred stakeCred) (Just $ transCoinToLovelace deposit)
  UnRegDepositTxCert stakeCred refund ->
    PV3.TxCertUnRegStaking (transCred stakeCred) (Just $ transCoinToLovelace refund)
  DelegTxCert stakeCred delegatee ->
    PV3.TxCertDelegStaking (transCred stakeCred) (Conway.transDelegatee delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV3.TxCertRegDeleg
      (transCred stakeCred)
      (Conway.transDelegatee delegatee)
      (transCoinToLovelace deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV3.TxCertAuthHotCommittee
      (Conway.transColdCommitteeCred coldCred)
      (Conway.transHotCommitteeCred hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV3.TxCertResignColdCommittee (Conway.transColdCommitteeCred coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV3.TxCertRegDRep (Conway.transDRepCred drepCred) (transCoinToLovelace deposit)
  UnRegDRepTxCert drepCred refund ->
    PV3.TxCertUnRegDRep (Conway.transDRepCred drepCred) (transCoinToLovelace refund)
  UpdateDRepTxCert drepCred _anchor ->
    PV3.TxCertUpdateDRep (Conway.transDRepCred drepCred)
  _ -> error "Impossible: All TxCerts should have been accounted for"

instance ConwayEraPlutusTxInfo 'PlutusV3 DijkstraEra where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))

instance ConwayEraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))

instance EraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusTxCert _ _ = pure . transTxCert

  toPlutusScriptPurpose _ = error "stub: PlutusV4 not yet implemented"

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    withBothTxLevels ltiTx mkTopTxInfo mkSubTxInfo
    where
      mkTopTxInfo tx = PlutusTxInfoResult $ do
        txInfo <- mkAnyLevelTxInfo tx
        let topTxInfo = txInfo {PV3.txInfoFee = transCoinToLovelace (tx ^. bodyTxL . feeTxBodyL)}
        Right $ \case
          GuardingPurpose AsPurpose ->
            -- TODO: Add Sub transactions
            Right topTxInfo
          _ -> Right topTxInfo
      mkSubTxInfo tx = PlutusTxInfoResult $ do
        txInfo <- mkAnyLevelTxInfo tx
        Right $ \_ -> Right txInfo
      mkAnyLevelTxInfo ::
        Tx l DijkstraEra ->
        Either (ContextError DijkstraEra) (PlutusTxInfo 'PlutusV4)
      mkAnyLevelTxInfo tx = do
        let
          txBody = tx ^. bodyTxL
          txInputs = txBody ^. inputsTxBodyL
          refInputs = txBody ^. referenceInputsTxBodyL
        timeRange <-
          Conway.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
        inputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList txInputs)
        refInputsInfo <- mapM (Conway.transTxInInfoV3 ltiUTxO) (Set.toList refInputs)
        Conway.checkReferenceInputsNotDisjointFromInputs txBody
        let ledgerOutputs = txBody ^. outputsTxBodyL
        outputs <-
          zipWithM
            (Babbage.transTxOutV2 . TxOutFromOutput)
            [minBound ..]
            (F.toList ledgerOutputs)
        let
          outputHasPtr txOut = case txOut ^. addrTxOutL of
            Addr _ _ (StakeRefPtr _) -> True
            _ -> False
        case NE.nonEmpty (filter outputHasPtr $ toList ledgerOutputs) of
          Nothing -> pure ()
          Just ptrOutputs -> Left $ PointerPresentInOutput ptrOutputs
        txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
        plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer tx
        Right $
          PV3.TxInfo
            { PV3.txInfoInputs = inputsInfo
            , PV3.txInfoOutputs = outputs
            , PV3.txInfoReferenceInputs = refInputsInfo
            , PV3.txInfoFee = 0
            , PV3.txInfoMint = Conway.transMintValue (txBody ^. mintTxBodyL)
            , PV3.txInfoTxCerts = txCerts
            , PV3.txInfoWdrl = Conway.transTxBodyWithdrawals txBody
            , PV3.txInfoValidRange = timeRange
            , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
            , PV3.txInfoRedeemers = plutusRedeemers
            , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
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

  toPlutusArgs = toPlutusV4Args

  toPlutusTxInInfo _ = transTxInInfoV3

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
