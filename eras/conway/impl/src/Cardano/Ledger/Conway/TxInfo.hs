{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxInfo (
  ConwayContextError (..),
  ConwayEraPlutusTxInfo (..),
  transTxBodyWithdrawals,
  transTxCert,
  transDRepCred,
  transColdCommitteeCred,
  transHotCommitteeCred,
  transDelegatee,
  transDRep,
  transScriptPurpose,
  transMap,
  transTxInInfoV1,
  transTxOutV1,
  toPlutusV3Args,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxCert,
  PlutusTxInfo,
  toPlutusWithContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), TxOutSource (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), toAsItem)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..), transTxOutV2)
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (
  Inject (..),
  ProtVer (..),
  StrictMaybe (..),
  getVersion64,
  isSJust,
  kindObject,
  strictMaybe,
  txIxToInt,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Constitution (..),
  GovAction (..),
  GovActionId (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  unGovActionIx,
 )
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..), PlutusScript (..))
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.Language (Language (..), PlutusArgs (..), SLanguage (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.Plutus.TxInfo (
  transBoundedRational,
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transDatum,
  transEpochNo,
  transKeyHash,
  transRewardAccount,
  transSafeHash,
  transScriptHash,
 )
import qualified Cardano.Ledger.Plutus.TxInfo as TxInfo
import Cardano.Ledger.PoolParams
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.State (UTxO)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Control.Arrow (ArrowChoice (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Aeson (ToJSON (..), (.=))
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Set as Set
import GHC.Generics hiding (to)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V3.MintValue as PV3

instance EraPlutusContext ConwayEra where
  type ContextError ConwayEra = ConwayContextError ConwayEra

  data TxInfoResult ConwayEra
    = ConwayTxInfoResult -- Fields must be kept lazy
        (Either (ContextError ConwayEra) (PlutusTxInfo 'PlutusV1))
        (Either (ContextError ConwayEra) (PlutusTxInfo 'PlutusV2))
        (Either (ContextError ConwayEra) (PlutusTxInfo 'PlutusV3))

  mkTxInfoResult lti =
    ConwayTxInfoResult
      (toPlutusTxInfo SPlutusV1 lti)
      (toPlutusTxInfo SPlutusV2 lti)
      (toPlutusTxInfo SPlutusV3 lti)

  lookupTxInfoResult SPlutusV1 (ConwayTxInfoResult tirPlutusV1 _ _) = tirPlutusV1
  lookupTxInfoResult SPlutusV2 (ConwayTxInfoResult _ tirPlutusV2 _) = tirPlutusV2
  lookupTxInfoResult SPlutusV3 (ConwayTxInfoResult _ _ tirPlutusV3) = tirPlutusV3

  mkPlutusWithContext = \case
    ConwayPlutusV1 p -> toPlutusWithContext $ Left p
    ConwayPlutusV2 p -> toPlutusWithContext $ Left p
    ConwayPlutusV3 p -> toPlutusWithContext $ Left p

data ConwayContextError era
  = BabbageContextError !(BabbageContextError era)
  | CertificateNotSupported !(TxCert era)
  | PlutusPurposeNotSupported !(PlutusPurpose AsItem era)
  | CurrentTreasuryFieldNotSupported !Coin
  | VotingProceduresFieldNotSupported !(VotingProcedures era)
  | ProposalProceduresFieldNotSupported !(OSet.OSet (ProposalProcedure era))
  | TreasuryDonationFieldNotSupported !Coin
  deriving (Generic)

deriving instance
  ( Eq (BabbageContextError era)
  , Eq (TxCert era)
  , Eq (PlutusPurpose AsItem era)
  , Eq (PlutusPurpose AsIx era)
  , EraPParams era
  ) =>
  Eq (ConwayContextError era)

deriving instance
  ( Show (BabbageContextError era)
  , Show (TxCert era)
  , Show (PlutusPurpose AsItem era)
  , Show (PlutusPurpose AsIx era)
  , EraPParams era
  ) =>
  Show (ConwayContextError era)

instance Inject (BabbageContextError era) (ConwayContextError era) where
  inject = BabbageContextError

instance Inject (AlonzoContextError era) (ConwayContextError era) where
  inject = BabbageContextError . inject

instance
  ( NoThunks (TxCert era)
  , NoThunks (PlutusPurpose AsIx era)
  , NoThunks (PlutusPurpose AsItem era)
  , EraPParams era
  ) =>
  NoThunks (ConwayContextError era)

instance
  ( EraPParams era
  , NFData (TxCert era)
  , NFData (PlutusPurpose AsIx era)
  , NFData (PlutusPurpose AsItem era)
  ) =>
  NFData (ConwayContextError era)

instance
  ( EraPParams era
  , EncCBOR (TxCert era)
  , EncCBOR (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsItem era)
  ) =>
  EncCBOR (ConwayContextError era)
  where
  encCBOR = \case
    -- We start at tag 8, just in case to avoid clashes with previous eras.
    BabbageContextError babbageContextError ->
      encode $ Sum BabbageContextError 8 !> To babbageContextError
    CertificateNotSupported txCert ->
      encode $ Sum CertificateNotSupported 9 !> To txCert
    PlutusPurposeNotSupported purpose ->
      encode $ Sum PlutusPurposeNotSupported 10 !> To purpose
    CurrentTreasuryFieldNotSupported scoin ->
      encode $ Sum CurrentTreasuryFieldNotSupported 11 !> To scoin
    VotingProceduresFieldNotSupported votingProcedures ->
      encode $ Sum VotingProceduresFieldNotSupported 12 !> To votingProcedures
    ProposalProceduresFieldNotSupported proposalProcedures ->
      encode $ Sum ProposalProceduresFieldNotSupported 13 !> To proposalProcedures
    TreasuryDonationFieldNotSupported coin ->
      encode $ Sum TreasuryDonationFieldNotSupported 14 !> To coin

instance
  ( EraPParams era
  , DecCBOR (TxCert era)
  , DecCBOR (PlutusPurpose AsIx era)
  , DecCBOR (PlutusPurpose AsItem era)
  ) =>
  DecCBOR (ConwayContextError era)
  where
  decCBOR = decode $ Summands "ContextError" $ \case
    8 -> SumD BabbageContextError <! From
    9 -> SumD CertificateNotSupported <! From
    10 -> SumD PlutusPurposeNotSupported <! From
    11 -> SumD CurrentTreasuryFieldNotSupported <! From
    12 -> SumD VotingProceduresFieldNotSupported <! From
    13 -> SumD ProposalProceduresFieldNotSupported <! From
    14 -> SumD TreasuryDonationFieldNotSupported <! From
    n -> Invalid n

instance
  ( ToJSON (TxCert era)
  , ToJSON (PlutusPurpose AsIx era)
  , ToJSON (PlutusPurpose AsItem era)
  , EraPParams era
  ) =>
  ToJSON (ConwayContextError era)
  where
  toJSON = \case
    BabbageContextError err -> toJSON err
    CertificateNotSupported txCert ->
      kindObject "CertificateNotSupported" ["certificate" .= toJSON txCert]
    PlutusPurposeNotSupported purpose ->
      kindObject "PlutusPurposeNotSupported" ["purpose" .= toJSON purpose]
    CurrentTreasuryFieldNotSupported scoin ->
      kindObject
        "CurrentTreasuryFieldNotSupported"
        ["current_treasury_value" .= toJSON scoin]
    VotingProceduresFieldNotSupported votingProcedures ->
      kindObject
        "VotingProceduresFieldNotSupported"
        ["voting_procedures" .= toJSON votingProcedures]
    ProposalProceduresFieldNotSupported proposalProcedures ->
      kindObject
        "ProposalProceduresFieldNotSupported"
        ["proposal_procedures" .= toJSON proposalProcedures]
    TreasuryDonationFieldNotSupported coin ->
      kindObject
        "TreasuryDonationFieldNotSupported"
        ["treasury_donation" .= toJSON coin]

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Babbage features, return Left.
transTxOutV1 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  TxOutSource ->
  TxOut era ->
  Either (ContextError era) PV1.TxOut
transTxOutV1 txOutSource txOut = do
  when (isSJust (txOut ^. dataTxOutL)) $ do
    Left $ inject $ InlineDatumsNotSupported @era txOutSource
  case Alonzo.transTxOut txOut of
    Nothing -> Left $ inject $ ByronTxOutInContext @era txOutSource
    Just plutusTxOut -> Right plutusTxOut

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
transTxInInfoV1 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (TxInfo.transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V3 context
transTxInInfoV3 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV3.TxInInfo
transTxInInfoV3 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV3.TxInInfo (transTxIn txIn) plutusTxOut)

guardConwayFeaturesForPlutusV1V2 ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  Tx era ->
  Either (ContextError era) ()
guardConwayFeaturesForPlutusV1V2 tx = do
  let txBody = tx ^. bodyTxL
      currentTreasuryValue = txBody ^. currentTreasuryValueTxBodyL
      votingProcedures = txBody ^. votingProceduresTxBodyL
      proposalProcedures = txBody ^. proposalProceduresTxBodyL
      treasuryDonation = txBody ^. treasuryDonationTxBodyL
  unless (null $ unVotingProcedures votingProcedures) $
    Left $
      inject $
        VotingProceduresFieldNotSupported @era votingProcedures
  unless (null proposalProcedures) $
    Left $
      inject $
        ProposalProceduresFieldNotSupported @era proposalProcedures
  unless (treasuryDonation == Coin 0) $
    Left $
      inject $
        TreasuryDonationFieldNotSupported @era treasuryDonation
  case currentTreasuryValue of
    SNothing -> Right ()
    SJust treasury ->
      Left $ inject $ CurrentTreasuryFieldNotSupported @era treasury

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
  txCert
    | Just dCert <- Alonzo.transTxCertCommon txCert -> Right dCert
    | otherwise -> Left $ inject $ CertificateNotSupported txCert

instance EraPlutusTxInfo 'PlutusV1 ConwayEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose proxy pv = transPlutusPurposeV1V2 proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    guardConwayFeaturesForPlutusV1V2 ltiTx
    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiProtVer ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    mapM_ (transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV1 . TxOutFromOutput)
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

instance EraPlutusTxInfo 'PlutusV2 ConwayEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose proxy pv = transPlutusPurposeV1V2 proxy pv . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    guardConwayFeaturesForPlutusV1V2 ltiTx
    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiProtVer ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
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

instance EraPlutusTxInfo 'PlutusV3 ConwayEra where
  toPlutusTxCert _ pv = pure . transTxCert pv

  toPlutusScriptPurpose = transScriptPurpose

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} = do
    timeRange <-
      Alonzo.transValidityInterval ltiTx ltiProtVer ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV3 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV3 ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy ltiProtVer ltiTx
    pure
      PV3.TxInfo
        { PV3.txInfoInputs = inputs
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputs
        , PV3.txInfoFee = transCoinToLovelace (txBody ^. feeTxBodyL)
        , PV3.txInfoMint = transMintValue (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txCerts
        , PV3.txInfoWdrl = transTxBodyWithdrawals txBody
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV3.txInfoRedeemers = plutusRedeemers
        , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
        , PV3.txInfoId = transTxBodyId txBody
        , PV3.txInfoVotes = transVotingProcedures (txBody ^. votingProceduresTxBodyL)
        , PV3.txInfoProposalProcedures =
            map (transProposal proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
        , PV3.txInfoCurrentTreasuryAmount =
            strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
        , PV3.txInfoTreasuryDonation =
            case txBody ^. treasuryDonationTxBodyL of
              Coin 0 -> Nothing
              coin -> Just $ transCoinToLovelace coin
        }
    where
      txBody = ltiTx ^. bodyTxL

  toPlutusArgs = toPlutusV3Args

transTxId :: TxId -> PV3.TxId
transTxId txId = PV3.TxId (transSafeHash (unTxId txId))

transTxBodyId :: EraTxBody era => TxBody era -> PV3.TxId
transTxBodyId txBody = PV3.TxId (transSafeHash (hashAnnotated txBody))

transTxIn :: TxIn -> PV3.TxOutRef
transTxIn (TxIn txid txIx) = PV3.TxOutRef (transTxId txid) (toInteger (txIxToInt txIx))

transMintValue :: MultiAsset -> PV3.MintValue
transMintValue = PV3.UnsafeMintValue . PV1.getValue . Alonzo.transMultiAsset

-- | Translate all `Withdrawal`s from within a `TxBody`
transTxBodyWithdrawals :: EraTxBody era => TxBody era -> PV3.Map PV3.Credential PV3.Lovelace
transTxBodyWithdrawals txBody =
  transMap transRewardAccount transCoinToLovelace (unWithdrawals $ txBody ^. withdrawalsTxBodyL)

-- | In protocol version 9, a bug in `RegTxCert` and `UnRegTxCert` pattern definitions was causing
-- the deposit in `RegDepositTxCert` and `UnRegDepositTxCert` to be omitted.  We need to keep this
-- behavior for version 9, so, now that the bug in the patterns has been fixed, we are explicitly
-- omitting the deposit in these cases. It has been confirmed that this buggy behavior for protocol
-- version 9 has been exercised on Mainnet, therefore this conditional translation can never be
-- removed for Conway era (#4863)
transTxCert :: ConwayEraTxCert era => ProtVer -> TxCert era -> PV3.TxCert
transTxCert pv = \case
  RegPoolTxCert PoolParams {ppId, ppVrf} ->
    PV3.TxCertPoolRegister
      (transKeyHash ppId)
      (PV3.PubKeyHash (PV3.toBuiltin (hashToBytes (unVRFVerKeyHash ppVrf))))
  RetirePoolTxCert poolId retireEpochNo ->
    PV3.TxCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  RegTxCert stakeCred ->
    PV3.TxCertRegStaking (transCred stakeCred) Nothing
  UnRegTxCert stakeCred ->
    PV3.TxCertUnRegStaking (transCred stakeCred) Nothing
  RegDepositTxCert stakeCred deposit ->
    let transDeposit
          | HF.bootstrapPhase pv = Nothing
          | otherwise = Just (transCoinToLovelace deposit)
     in PV3.TxCertRegStaking (transCred stakeCred) transDeposit
  UnRegDepositTxCert stakeCred refund ->
    let transRefund
          | HF.bootstrapPhase pv = Nothing
          | otherwise = Just (transCoinToLovelace refund)
     in PV3.TxCertUnRegStaking (transCred stakeCred) transRefund
  DelegTxCert stakeCred delegatee ->
    PV3.TxCertDelegStaking (transCred stakeCred) (transDelegatee delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV3.TxCertRegDeleg (transCred stakeCred) (transDelegatee delegatee) (transCoinToLovelace deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV3.TxCertAuthHotCommittee (transColdCommitteeCred coldCred) (transHotCommitteeCred hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV3.TxCertResignColdCommittee (transColdCommitteeCred coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV3.TxCertRegDRep (transDRepCred drepCred) (transCoinToLovelace deposit)
  UnRegDRepTxCert drepCred refund ->
    PV3.TxCertUnRegDRep (transDRepCred drepCred) (transCoinToLovelace refund)
  UpdateDRepTxCert drepCred _anchor ->
    PV3.TxCertUpdateDRep (transDRepCred drepCred)

transDRepCred :: Credential 'DRepRole -> PV3.DRepCredential
transDRepCred = PV3.DRepCredential . transCred

transColdCommitteeCred :: Credential 'ColdCommitteeRole -> PV3.ColdCommitteeCredential
transColdCommitteeCred = PV3.ColdCommitteeCredential . transCred

transHotCommitteeCred :: Credential 'HotCommitteeRole -> PV3.HotCommitteeCredential
transHotCommitteeCred = PV3.HotCommitteeCredential . transCred

transDelegatee :: Delegatee -> PV3.Delegatee
transDelegatee = \case
  DelegStake poolId -> PV3.DelegStake (transKeyHash poolId)
  DelegVote drep -> PV3.DelegVote (transDRep drep)
  DelegStakeVote poolId drep -> PV3.DelegStakeVote (transKeyHash poolId) (transDRep drep)

transDRep :: DRep -> PV3.DRep
transDRep = \case
  DRepCredential drepCred -> PV3.DRep (transDRepCred drepCred)
  DRepAlwaysAbstain -> PV3.DRepAlwaysAbstain
  DRepAlwaysNoConfidence -> PV3.DRepAlwaysNoConfidence

-- | In Conway we have `Anchor`s in some certificates and all proposals. However, because
-- we do not translate anchors to plutus context, it is not always possible to deduce
-- which item the script purpose is responsible for, without also including the index for
-- that item. For this reason starting with PlutusV3, besides the item, `PV3.Certifying`
-- and `PV3.Proposing` also have an index. Moreover, other script purposes rely on Ledger
-- `Ord` instances for types that dictate the order, so it might not be a good idea to pass
-- that information to Plutus for those purposes.
transScriptPurpose ::
  (ConwayEraPlutusTxInfo l era, PlutusTxCert l ~ PV3.TxCert) =>
  proxy l ->
  ProtVer ->
  ConwayPlutusPurpose AsIxItem era ->
  Either (ContextError era) PV3.ScriptPurpose
transScriptPurpose proxy pv = \case
  ConwaySpending (AsIxItem _ txIn) -> pure $ PV3.Spending (transTxIn txIn)
  ConwayMinting (AsIxItem _ policyId) -> pure $ PV3.Minting (Alonzo.transPolicyID policyId)
  ConwayCertifying (AsIxItem ix txCert) ->
    PV3.Certifying (toInteger ix) <$> toPlutusTxCert proxy pv txCert
  ConwayRewarding (AsIxItem _ rewardAccount) -> pure $ PV3.Rewarding (transRewardAccount rewardAccount)
  ConwayVoting (AsIxItem _ voter) -> pure $ PV3.Voting (transVoter voter)
  ConwayProposing (AsIxItem ix proposal) ->
    pure $ PV3.Proposing (toInteger ix) (transProposal proxy proposal)

transVoter :: Voter -> PV3.Voter
transVoter = \case
  CommitteeVoter cred -> PV3.CommitteeVoter $ PV3.HotCommitteeCredential $ transCred cred
  DRepVoter cred -> PV3.DRepVoter $ PV3.DRepCredential $ transCred cred
  StakePoolVoter keyHash -> PV3.StakePoolVoter $ transKeyHash keyHash

transGovActionId :: GovActionId -> PV3.GovernanceActionId
transGovActionId GovActionId {gaidTxId, gaidGovActionIx} =
  PV3.GovernanceActionId
    { PV3.gaidTxId = transTxId gaidTxId
    , PV3.gaidGovActionIx = toInteger $ unGovActionIx gaidGovActionIx
    }

transGovAction :: ConwayEraPlutusTxInfo l era => proxy l -> GovAction era -> PV3.GovernanceAction
transGovAction proxy = \case
  ParameterChange pGovActionId ppu govPolicy ->
    PV3.ParameterChange
      (transPrevGovActionId pGovActionId)
      (toPlutusChangedParameters proxy ppu)
      (transGovPolicy govPolicy)
  HardForkInitiation pGovActionId protVer ->
    PV3.HardForkInitiation
      (transPrevGovActionId pGovActionId)
      (transProtVer protVer)
  TreasuryWithdrawals withdrawals govPolicy ->
    PV3.TreasuryWithdrawals
      (transMap transRewardAccount transCoinToLovelace withdrawals)
      (transGovPolicy govPolicy)
  NoConfidence pGovActionId -> PV3.NoConfidence (transPrevGovActionId pGovActionId)
  UpdateCommittee pGovActionId ccToRemove ccToAdd threshold ->
    PV3.UpdateCommittee
      (transPrevGovActionId pGovActionId)
      (map (PV3.ColdCommitteeCredential . transCred) $ Set.toList ccToRemove)
      (transMap (PV3.ColdCommitteeCredential . transCred) transEpochNo ccToAdd)
      (transBoundedRational threshold)
  NewConstitution pGovActionId constitution ->
    PV3.NewConstitution
      (transPrevGovActionId pGovActionId)
      (transConstitution constitution)
  InfoAction -> PV3.InfoAction
  where
    transGovPolicy = \case
      SJust govPolicy -> Just (transScriptHash govPolicy)
      SNothing -> Nothing
    transConstitution (Constitution _ govPolicy) =
      PV3.Constitution (transGovPolicy govPolicy)
    transPrevGovActionId = \case
      SJust (GovPurposeId gaId) -> Just (transGovActionId gaId)
      SNothing -> Nothing

transMap :: (t1 -> k) -> (t2 -> v) -> Map.Map t1 t2 -> PV3.Map k v
transMap transKey transValue =
  PV3.unsafeFromList . map (\(k, v) -> (transKey k, transValue v)) . Map.toList

transVotingProcedures ::
  VotingProcedures era -> PV3.Map PV3.Voter (PV3.Map PV3.GovernanceActionId PV3.Vote)
transVotingProcedures =
  transMap transVoter (transMap transGovActionId (transVote . vProcVote)) . unVotingProcedures

transVote :: Vote -> PV3.Vote
transVote = \case
  VoteNo -> PV3.VoteNo
  VoteYes -> PV3.VoteYes
  Abstain -> PV3.Abstain

transProposal ::
  ConwayEraPlutusTxInfo l era =>
  proxy l ->
  ProposalProcedure era ->
  PV3.ProposalProcedure
transProposal proxy ProposalProcedure {pProcDeposit, pProcReturnAddr, pProcGovAction} =
  PV3.ProposalProcedure
    { PV3.ppDeposit = transCoinToLovelace pProcDeposit
    , PV3.ppReturnAddr = transRewardAccount pProcReturnAddr
    , PV3.ppGovernanceAction = transGovAction proxy pProcGovAction
    }

transPlutusPurposeV1V2 ::
  ( PlutusTxCert l ~ PV2.DCert
  , PlutusPurpose AsItem era ~ ConwayPlutusPurpose AsItem era
  , EraPlutusTxInfo l era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  proxy l ->
  ProtVer ->
  ConwayPlutusPurpose AsItem era ->
  Either (ContextError era) PV2.ScriptPurpose
transPlutusPurposeV1V2 proxy pv = \case
  ConwaySpending txIn -> Alonzo.transPlutusPurpose proxy pv $ AlonzoSpending txIn
  ConwayMinting policyId -> Alonzo.transPlutusPurpose proxy pv $ AlonzoMinting policyId
  ConwayCertifying txCert -> Alonzo.transPlutusPurpose proxy pv $ AlonzoCertifying txCert
  ConwayRewarding rewardAccount -> Alonzo.transPlutusPurpose proxy pv $ AlonzoRewarding rewardAccount
  purpose -> Left $ inject $ PlutusPurposeNotSupported purpose

transProtVer :: ProtVer -> PV3.ProtocolVersion
transProtVer (ProtVer major minor) =
  PV3.ProtocolVersion (toInteger (getVersion64 major)) (toInteger minor)

toPlutusV3Args ::
  EraPlutusTxInfo 'PlutusV3 era =>
  proxy 'PlutusV3 ->
  ProtVer ->
  PV3.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Maybe (Data era) ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV3)
toPlutusV3Args proxy pv txInfo plutusPurpose maybeSpendingData redeemerData = do
  scriptPurpose <- toPlutusScriptPurpose proxy pv plutusPurpose
  let scriptInfo =
        scriptPurposeToScriptInfo
          scriptPurpose
          (transDatum <$> maybeSpendingData)
  pure $
    PlutusV3Args $
      PV3.ScriptContext
        { PV3.scriptContextTxInfo = txInfo
        , PV3.scriptContextRedeemer = Babbage.transRedeemer redeemerData
        , PV3.scriptContextScriptInfo = scriptInfo
        }

scriptPurposeToScriptInfo :: PV3.ScriptPurpose -> Maybe PV1.Datum -> PV3.ScriptInfo
scriptPurposeToScriptInfo sp maybeSpendingData =
  case sp of
    PV3.Spending txIn -> PV3.SpendingScript txIn maybeSpendingData
    PV3.Minting policyId -> PV3.MintingScript policyId
    PV3.Certifying ix txCert -> PV3.CertifyingScript ix txCert
    PV3.Rewarding rewardAccount -> PV3.RewardingScript rewardAccount
    PV3.Voting voter -> PV3.VotingScript voter
    PV3.Proposing ix proposal -> PV3.ProposingScript ix proposal

-- | A class to compute the changed parameters in the TxInfo
-- given a ToPlutusData instance for PParamsUpdate
class
  EraPlutusTxInfo l era =>
  ConwayEraPlutusTxInfo (l :: Language) era
  where
  toPlutusChangedParameters :: proxy l -> PParamsUpdate era -> PV3.ChangedParameters

instance ConwayEraPlutusTxInfo 'PlutusV3 ConwayEra where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))
