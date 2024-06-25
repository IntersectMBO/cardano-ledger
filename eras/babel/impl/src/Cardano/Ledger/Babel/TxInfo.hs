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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.TxInfo (
  BabelContextError (..),
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
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  PlutusTxCert,
  mkPlutusLanguageContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), TxOutSource (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), toAsItem)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..), transTxOutV2)
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Plutus.Context
import Cardano.Ledger.Babel.Scripts (BabelPlutusPurpose (..), PlutusScript (..))
import Cardano.Ledger.Babel.Tx ()
import Cardano.Ledger.Babel.TxCert
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
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
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
import Cardano.Ledger.Conway.Plutus.Context (
  ConwayEraPlutusTxInfo (toPlutusChangedParameters),
  conwayPParamMap,
  pparamUpdateFromData,
  pparamUpdateToData,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.Plutus.TxInfo (
  transBoundedRational,
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transEpochNo,
  transKeyHash,
  transRewardAccount,
  transSafeHash,
  transScriptHash,
 )
import qualified Cardano.Ledger.Plutus.TxInfo as TxInfo
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO)
import Control.Arrow (ArrowChoice (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Aeson (ToJSON (..), (.=))
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OSet.Strict as OSet
import qualified Data.Set as Set
import GHC.Generics hiding (to)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4
import qualified PlutusTx.AssocMap as PMap
import qualified PlutusTx.Eq

instance Crypto c => EraPlutusContext (BabelEra c) where
  type ContextError (BabelEra c) = BabelContextError (BabelEra c)

  mkPlutusScriptContext = \case
    BabelPlutusV1 p -> mkPlutusLanguageContext p
    BabelPlutusV2 p -> mkPlutusLanguageContext p
    BabelPlutusV3 p -> mkPlutusLanguageContext p
    BabelPlutusV4 p -> mkPlutusLanguageContext p

data BabelContextError era
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
  Eq (BabelContextError era)

deriving instance
  ( Show (BabbageContextError era)
  , Show (TxCert era)
  , Show (PlutusPurpose AsItem era)
  , Show (PlutusPurpose AsIx era)
  , EraPParams era
  ) =>
  Show (BabelContextError era)

instance Inject (BabelContextError era) (BabelContextError era)

instance Inject (BabbageContextError era) (BabelContextError era) where
  inject = BabbageContextError

instance Inject (AlonzoContextError era) (BabelContextError era) where
  inject = BabbageContextError . inject

instance
  ( NoThunks (TxCert era)
  , NoThunks (PlutusPurpose AsIx era)
  , NoThunks (PlutusPurpose AsItem era)
  , EraPParams era
  ) =>
  NoThunks (BabelContextError era)

instance
  ( EraPParams era
  , NFData (TxCert era)
  , NFData (PlutusPurpose AsIx era)
  , NFData (PlutusPurpose AsItem era)
  ) =>
  NFData (BabelContextError era)

instance
  ( EraPParams era
  , EncCBOR (TxCert era)
  , EncCBOR (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsItem era)
  ) =>
  EncCBOR (BabelContextError era)
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
  DecCBOR (BabelContextError era)
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
  ToJSON (BabelContextError era)
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
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  TxOutSource (EraCrypto era) ->
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
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (ContextError era) PV1.TxInInfo
transTxInInfoV1 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV1 (TxOutFromInput txIn) txOut
  Right (PV1.TxInInfo (TxInfo.transTxIn txIn) plutusTxOut)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V3 context
transTxInInfoV3 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (ContextError era) PV3.TxInInfo
transTxInInfoV3 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV3.TxInInfo (transTxIn txIn) plutusTxOut)

guardBabelFeaturesForPlutusV1V2 ::
  forall era.
  ( EraTx era
  , BabelEraTxBody era
  , Inject (BabelContextError era) (ContextError era)
  ) =>
  Tx era ->
  Either (ContextError era) ()
guardBabelFeaturesForPlutusV1V2 tx = do
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
  ( BabelEraTxCert era
  , Inject (BabelContextError era) (ContextError era)
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

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (BabelEra c) where
  toPlutusTxCert _ = transTxCertV1V2

  toPlutusScriptPurpose proxy = transPlutusPurposeV1V2 proxy . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    guardBabelFeaturesForPlutusV1V2 tx
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (transTxInInfoV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    mapM_ (transTxInInfoV1 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (transTxOutV1 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
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
        , PV1.txInfoData = Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV1.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (BabelEra c) where
  toPlutusTxCert _ = transTxCertV1V2

  toPlutusScriptPurpose proxy = transPlutusPurposeV1V2 proxy . hoistPlutusPurpose toAsItem

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    guardBabelFeaturesForPlutusV1V2 tx
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy tx
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
        , PV2.txInfoData = PV2.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV2.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV2.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV3 (BabelEra c) where
  toPlutusTxCert _ = pure . transTxCert

  toPlutusScriptPurpose = transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    -- TODO WG: realizedInputs. Add realizedFulfills here. Put them in PV4 TxInfo.
    inputs <- mapM (transTxInInfoV3 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV3 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy tx
    pure
      PV3.TxInfo
        { PV3.txInfoInputs = inputs
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputs
        , PV3.txInfoFee = transCoinToLovelace (txBody ^. feeTxBodyL)
        , PV3.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txCerts
        , PV3.txInfoWdrl = transTxBodyWithdrawals txBody
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV3.txInfoRedeemers = plutusRedeemers
        , PV3.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
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
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose = do
    let redeemers :: PMap.Map PV3.ScriptPurpose PV3.Redeemer = PV3.txInfoRedeemers txInfo
    purpose <- toPlutusScriptPurpose proxy scriptPurpose
    let redeemer = fromJust $ PMap.lookup purpose redeemers -- TODO WG obviously partial
    pure $ PV3.ScriptContext txInfo redeemer (fromScriptPurpose purpose)

fromScriptPurpose :: PV3.ScriptPurpose -> PV3.ScriptInfo
fromScriptPurpose = \case
  PV3.Minting cs -> PV3.MintingScript cs
  PV3.Spending txOutRef -> PV3.SpendingScript txOutRef Nothing
  PV3.Rewarding cred -> PV3.RewardingScript cred
  PV3.Certifying index txCert -> PV3.CertifyingScript index txCert
  PV3.Voting voter -> PV3.VotingScript voter
  PV3.Proposing index proposal -> PV3.ProposingScript index proposal

transTxId :: TxId c -> PV3.TxId
transTxId txId = PV3.TxId (transSafeHash (unTxId txId))

transTxBodyId :: EraTxBody era => TxBody era -> PV3.TxId
transTxBodyId txBody = PV3.TxId (transSafeHash (hashAnnotated txBody))

transTxIn :: TxIn c -> PV3.TxOutRef
transTxIn (TxIn txid txIx) = PV3.TxOutRef (transTxId txid) (toInteger (txIxToInt txIx))

-- | Translate all `Withdrawal`s from within a `TxBody`
transTxBodyWithdrawals :: EraTxBody era => TxBody era -> PV3.Map PV3.Credential PV3.Lovelace
transTxBodyWithdrawals txBody =
  transMap transRewardAccount transCoinToLovelace (unWithdrawals $ txBody ^. withdrawalsTxBodyL)

transTxCert :: BabelEraTxCert era => TxCert era -> PV3.TxCert
transTxCert = \case
  RegPoolTxCert PoolParams {ppId, ppVrf} ->
    PV3.TxCertPoolRegister (transKeyHash ppId) (PV3.PubKeyHash (PV3.toBuiltin (hashToBytes ppVrf)))
  RetirePoolTxCert poolId retireEpochNo ->
    PV3.TxCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  RegTxCert stakeCred ->
    PV3.TxCertRegStaking (transCred stakeCred) Nothing
  UnRegTxCert stakeCred ->
    PV3.TxCertUnRegStaking (transCred stakeCred) Nothing
  RegDepositTxCert stakeCred deposit ->
    PV3.TxCertRegStaking (transCred stakeCred) (Just (transCoinToLovelace deposit))
  UnRegDepositTxCert stakeCred refund ->
    PV3.TxCertUnRegStaking (transCred stakeCred) (Just (transCoinToLovelace refund))
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

transDRepCred :: Credential 'DRepRole c -> PV3.DRepCredential
transDRepCred = PV3.DRepCredential . transCred

transColdCommitteeCred :: Credential 'ColdCommitteeRole c -> PV3.ColdCommitteeCredential
transColdCommitteeCred = PV3.ColdCommitteeCredential . transCred

transHotCommitteeCred :: Credential 'HotCommitteeRole c -> PV3.HotCommitteeCredential
transHotCommitteeCred = PV3.HotCommitteeCredential . transCred

transDelegatee :: Delegatee c -> PV3.Delegatee
transDelegatee = \case
  DelegStake poolId -> PV3.DelegStake (transKeyHash poolId)
  DelegVote drep -> PV3.DelegVote (transDRep drep)
  DelegStakeVote poolId drep -> PV3.DelegStakeVote (transKeyHash poolId) (transDRep drep)

transDRep :: DRep c -> PV3.DRep
transDRep = \case
  DRepCredential drepCred -> PV3.DRep (transDRepCred drepCred)
  DRepAlwaysAbstain -> PV3.DRepAlwaysAbstain
  DRepAlwaysNoConfidence -> PV3.DRepAlwaysNoConfidence

-- | In Babel we have `Anchor`s in some certificates and all proposals. However, because
-- we do not translate anchors to plutus context, it is not always possible to deduce
-- which item the script purpose is responsible for, without also including the index for
-- that item. For this reason starting with PlutusV3, besides the item, `PV3.Certifying`
-- and `PV3.Proposing` also have an index. Moreover, other script purposes rely on Ledger
-- `Ord` instances for types that dictate the order, so it might not be a good idea to pass
-- that information to Plutus for those purposes.
transScriptPurpose ::
  (ConwayEraPlutusTxInfo l era, PlutusTxCert l ~ PV3.TxCert) =>
  proxy l ->
  BabelPlutusPurpose AsIxItem era ->
  Either (ContextError era) PV3.ScriptPurpose
transScriptPurpose proxy = \case
  BabelSpending (AsIxItem _ txIn) -> pure $ PV3.Spending (transTxIn txIn)
  BabelMinting (AsIxItem _ policyId) -> pure $ PV3.Minting (Alonzo.transPolicyID policyId)
  BabelCertifying (AsIxItem ix txCert) ->
    PV3.Certifying (toInteger ix) <$> toPlutusTxCert proxy txCert
  BabelRewarding (AsIxItem _ rewardAccount) -> pure $ PV3.Rewarding (transRewardAccount rewardAccount)
  BabelVoting (AsIxItem _ voter) -> pure $ PV3.Voting (transVoter voter)
  BabelProposing (AsIxItem ix proposal) ->
    pure $ PV3.Proposing (toInteger ix) (transProposal proxy proposal)

transVoter :: Voter c -> PV3.Voter
transVoter = \case
  CommitteeVoter cred -> PV3.CommitteeVoter $ PV3.HotCommitteeCredential $ transCred cred
  DRepVoter cred -> PV3.DRepVoter $ PV3.DRepCredential $ transCred cred
  StakePoolVoter keyHash -> PV3.StakePoolVoter $ transKeyHash keyHash

transGovActionId :: GovActionId c -> PV3.GovernanceActionId
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
  , PlutusPurpose AsItem era ~ BabelPlutusPurpose AsItem era
  , EraPlutusTxInfo l era
  , Inject (BabelContextError era) (ContextError era)
  ) =>
  proxy l ->
  BabelPlutusPurpose AsItem era ->
  Either (ContextError era) PV2.ScriptPurpose
transPlutusPurposeV1V2 proxy = \case
  BabelSpending txIn -> Alonzo.transPlutusPurpose proxy $ AlonzoSpending txIn
  BabelMinting policyId -> Alonzo.transPlutusPurpose proxy $ AlonzoMinting policyId
  BabelCertifying txCert -> Alonzo.transPlutusPurpose proxy $ AlonzoCertifying txCert
  BabelRewarding rewardAccount -> Alonzo.transPlutusPurpose proxy $ AlonzoRewarding rewardAccount
  purpose -> Left $ inject $ PlutusPurposeNotSupported purpose

transProtVer :: ProtVer -> PV3.ProtocolVersion
transProtVer (ProtVer major minor) =
  PV3.ProtocolVersion (toInteger (getVersion64 major)) (toInteger minor)

instance Crypto c => EraPlutusTxInfo 'PlutusV4 (BabelEra c) where
  toPlutusTxCert _ = pure . transTxCertV4

  toPlutusScriptPurpose = transScriptPurposeV4

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    -- TODO WG: realizedInputs. Add realizedFulfills here. Put them in PV4 TxInfo.
    inputs <- mapM (transTxInInfoV4 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (transTxInInfoV4 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy tx
    pure
      PV4.TxInfo
        { PV4.txInfoInputs = inputs
        , PV4.txInfoOutputs = outputs
        , PV4.txInfoReferenceInputs = refInputs
        , PV4.txInfoFee = transCoinToLovelace (txBody ^. feeTxBodyL)
        , PV4.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
        , PV4.txInfoTxCerts = txCerts
        , PV4.txInfoWdrl = transTxBodyWithdrawals txBody
        , PV4.txInfoValidRange = timeRange
        , PV4.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV4.txInfoRedeemers = plutusRedeemers
        , PV4.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV4.txInfoId = transTxBodyId txBody
        , PV4.txInfoVotes = transVotingProceduresV4 (txBody ^. votingProceduresTxBodyL)
        , PV4.txInfoProposalProcedures =
            map (transProposalV4 proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
        , PV4.txInfoCurrentTreasuryAmount =
            strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
        , PV4.txInfoTreasuryDonation =
            case txBody ^. treasuryDonationTxBodyL of
              Coin 0 -> Nothing
              coin -> Just $ transCoinToLovelace coin
        , PV4.txInfoFulfills = undefined
        , PV4.txInfoRequests = undefined
        , PV4.txInfoRequiredTxs = undefined
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose = do
    let redeemers :: PMap.Map PV4.ScriptPurpose PV4.Redeemer = PV4.txInfoRedeemers txInfo
    purpose <- toPlutusScriptPurpose proxy scriptPurpose
    let redeemer = fromJust $ PMap.lookup purpose redeemers -- TODO WG obviously partial
    pure $ PV4.ScriptContext txInfo redeemer (fromScriptPurposeV4 purpose)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V4 context
transTxInInfoV4 ::
  forall era.
  ( Inject (BabbageContextError era) (ContextError era)
  , Value era ~ MaryValue (EraCrypto era)
  , BabbageEraTxOut era
  ) =>
  UTxO era ->
  TxIn (EraCrypto era) ->
  Either (ContextError era) PV4.TxInInfo
transTxInInfoV4 utxo txIn = do
  txOut <- left (inject . AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV2 (TxOutFromInput txIn) txOut
  Right (PV4.TxInInfo (transTxIn txIn) plutusTxOut)

fromScriptPurposeV4 :: PV4.ScriptPurpose -> PV4.ScriptInfo
fromScriptPurposeV4 = \case
  PV4.Minting cs -> PV4.MintingScript cs
  PV4.Spending txOutRef -> PV4.SpendingScript txOutRef Nothing
  PV4.Rewarding cred -> PV4.RewardingScript cred
  PV4.Certifying index txCert -> PV4.CertifyingScript index txCert
  PV4.Voting voter -> PV4.VotingScript voter
  PV4.Proposing index proposal -> PV4.ProposingScript index proposal

transTxCertV4 :: BabelEraTxCert era => TxCert era -> PV4.TxCert
transTxCertV4 = \case
  RegPoolTxCert PoolParams {ppId, ppVrf} ->
    PV4.TxCertPoolRegister (transKeyHash ppId) (PV4.PubKeyHash (PV4.toBuiltin (hashToBytes ppVrf)))
  RetirePoolTxCert poolId retireEpochNo ->
    PV4.TxCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  RegTxCert stakeCred ->
    PV4.TxCertRegStaking (transCred stakeCred) Nothing
  UnRegTxCert stakeCred ->
    PV4.TxCertUnRegStaking (transCred stakeCred) Nothing
  RegDepositTxCert stakeCred deposit ->
    PV4.TxCertRegStaking (transCred stakeCred) (Just (transCoinToLovelace deposit))
  UnRegDepositTxCert stakeCred refund ->
    PV4.TxCertUnRegStaking (transCred stakeCred) (Just (transCoinToLovelace refund))
  DelegTxCert stakeCred delegatee ->
    PV4.TxCertDelegStaking (transCred stakeCred) (transDelegateeV4 delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV4.TxCertRegDeleg (transCred stakeCred) (transDelegateeV4 delegatee) (transCoinToLovelace deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV4.TxCertAuthHotCommittee (transColdCommitteeCredV4 coldCred) (transHotCommitteeCredV4 hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV4.TxCertResignColdCommittee (transColdCommitteeCredV4 coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV4.TxCertRegDRep (transDRepCredV4 drepCred) (transCoinToLovelace deposit)
  UnRegDRepTxCert drepCred refund ->
    PV4.TxCertUnRegDRep (transDRepCredV4 drepCred) (transCoinToLovelace refund)
  UpdateDRepTxCert drepCred _anchor ->
    PV4.TxCertUpdateDRep (transDRepCredV4 drepCred)

transDRepCredV4 :: Credential 'DRepRole c -> PV4.DRepCredential
transDRepCredV4 = PV4.DRepCredential . transCred

transColdCommitteeCredV4 :: Credential 'ColdCommitteeRole c -> PV4.ColdCommitteeCredential
transColdCommitteeCredV4 = PV4.ColdCommitteeCredential . transCred

transHotCommitteeCredV4 :: Credential 'HotCommitteeRole c -> PV4.HotCommitteeCredential
transHotCommitteeCredV4 = PV4.HotCommitteeCredential . transCred

transDelegateeV4 :: Delegatee c -> PV4.Delegatee
transDelegateeV4 = \case
  DelegStake poolId -> PV4.DelegStake (transKeyHash poolId)
  DelegVote drep -> PV4.DelegVote (transDRepV4 drep)
  DelegStakeVote poolId drep -> PV4.DelegStakeVote (transKeyHash poolId) (transDRepV4 drep)

transDRepV4 :: DRep c -> PV4.DRep
transDRepV4 = \case
  DRepCredential drepCred -> PV4.DRep (transDRepCredV4 drepCred)
  DRepAlwaysAbstain -> PV4.DRepAlwaysAbstain
  DRepAlwaysNoConfidence -> PV4.DRepAlwaysNoConfidence

-- | In Babel we have `Anchor`s in some certificates and all proposals. However, because
-- we do not translate anchors to plutus context, it is not always possible to deduce
-- which item the script purpose is responsible for, without also including the index for
-- that item. For this reason starting with PlutusV3, besides the item, `PV3.Certifying`
-- and `PV3.Proposing` also have an index. Moreover, other script purposes rely on Ledger
-- `Ord` instances for types that dictate the order, so it might not be a good idea to pass
-- that information to Plutus for those purposes.
transScriptPurposeV4 ::
  (BabelEraPlutusTxInfo l era, PlutusTxCert l ~ PV4.TxCert) =>
  proxy l ->
  BabelPlutusPurpose AsIxItem era ->
  Either (ContextError era) PV4.ScriptPurpose
transScriptPurposeV4 proxy = \case
  BabelSpending (AsIxItem _ txIn) -> pure $ PV4.Spending (transTxIn txIn)
  BabelMinting (AsIxItem _ policyId) -> pure $ PV4.Minting (Alonzo.transPolicyID policyId)
  BabelCertifying (AsIxItem ix txCert) ->
    PV4.Certifying (toInteger ix) <$> toPlutusTxCert proxy txCert
  BabelRewarding (AsIxItem _ rewardAccount) -> pure $ PV4.Rewarding (transRewardAccount rewardAccount)
  BabelVoting (AsIxItem _ voter) -> pure $ PV4.Voting (transVoterV4 voter)
  BabelProposing (AsIxItem ix proposal) ->
    pure $ PV4.Proposing (toInteger ix) (transProposalV4 proxy proposal)

transVoterV4 :: Voter c -> PV4.Voter
transVoterV4 = \case
  CommitteeVoter cred -> PV4.CommitteeVoter $ PV4.HotCommitteeCredential $ transCred cred
  DRepVoter cred -> PV4.DRepVoter $ PV4.DRepCredential $ transCred cred
  StakePoolVoter keyHash -> PV4.StakePoolVoter $ transKeyHash keyHash

transGovActionIdV4 :: GovActionId c -> PV4.GovernanceActionId
transGovActionIdV4 GovActionId {gaidTxId, gaidGovActionIx} =
  PV4.GovernanceActionId
    { PV4.gaidTxId = transTxId gaidTxId
    , PV4.gaidGovActionIx = toInteger $ unGovActionIx gaidGovActionIx
    }

transGovActionV4 ::
  BabelEraPlutusTxInfo l era => proxy l -> GovAction era -> PV4.GovernanceAction
transGovActionV4 proxy = \case
  ParameterChange pGovActionId ppu govPolicy ->
    PV4.ParameterChange
      (transPrevGovActionId pGovActionId)
      (toBabelPlutusChangedParameters proxy ppu)
      (transGovPolicy govPolicy)
  HardForkInitiation pGovActionId protVer ->
    PV4.HardForkInitiation
      (transPrevGovActionId pGovActionId)
      (transProtVerV4 protVer)
  TreasuryWithdrawals withdrawals govPolicy ->
    PV4.TreasuryWithdrawals
      (transMap transRewardAccount transCoinToLovelace withdrawals)
      (transGovPolicy govPolicy)
  NoConfidence pGovActionId -> PV4.NoConfidence (transPrevGovActionId pGovActionId)
  UpdateCommittee pGovActionId ccToRemove ccToAdd threshold ->
    PV4.UpdateCommittee
      (transPrevGovActionId pGovActionId)
      (map (PV4.ColdCommitteeCredential . transCred) $ Set.toList ccToRemove)
      (transMap (PV4.ColdCommitteeCredential . transCred) transEpochNo ccToAdd)
      (transBoundedRational threshold)
  NewConstitution pGovActionId constitution ->
    PV4.NewConstitution
      (transPrevGovActionId pGovActionId)
      (transConstitution constitution)
  InfoAction -> PV4.InfoAction
  where
    transGovPolicy = \case
      SJust govPolicy -> Just (transScriptHash govPolicy)
      SNothing -> Nothing
    transConstitution (Constitution _ govPolicy) =
      PV4.Constitution (transGovPolicy govPolicy)
    transPrevGovActionId = \case
      SJust (GovPurposeId gaId) -> Just (transGovActionIdV4 gaId)
      SNothing -> Nothing

transVotingProceduresV4 ::
  VotingProcedures era -> PV4.Map PV4.Voter (PV4.Map PV4.GovernanceActionId PV4.Vote)
transVotingProceduresV4 =
  transMap transVoterV4 (transMap transGovActionIdV4 (transVoteV4 . vProcVote)) . unVotingProcedures

transVoteV4 :: Vote -> PV4.Vote
transVoteV4 = \case
  VoteNo -> PV4.VoteNo
  VoteYes -> PV4.VoteYes
  Abstain -> PV4.Abstain

transProposalV4 ::
  BabelEraPlutusTxInfo l era =>
  proxy l ->
  ProposalProcedure era ->
  PV4.ProposalProcedure
transProposalV4 proxy ProposalProcedure {pProcDeposit, pProcReturnAddr, pProcGovAction} =
  PV4.ProposalProcedure
    { PV4.ppDeposit = transCoinToLovelace pProcDeposit
    , PV4.ppReturnAddr = transRewardAccount pProcReturnAddr
    , PV4.ppGovernanceAction = transGovActionV4 proxy pProcGovAction
    }

transProtVerV4 :: ProtVer -> PV4.ProtocolVersion
transProtVerV4 (ProtVer major minor) =
  PV4.ProtocolVersion (toInteger (getVersion64 major)) (toInteger minor)

-- ==========================
-- Instances

instance PlutusTx.Eq.Eq PV4.ScriptPurpose where
  (==) = undefined -- TODO WG (I don't want to go and recalculate the hashes of my forked Plutus repo)

instance Crypto c => ToPlutusData (PParamsUpdate (BabelEra c)) where
  toPlutusData = pparamUpdateToData conwayPParamMap
  fromPlutusData = pparamUpdateFromData conwayPParamMap

instance Crypto c => ConwayEraPlutusTxInfo 'PlutusV3 (BabelEra c) where
  toPlutusChangedParameters _ x = PV3.ChangedParameters (PV3.dataToBuiltinData (toPlutusData x))

instance Crypto c => BabelEraPlutusTxInfo 'PlutusV4 (BabelEra c) where
  toBabelPlutusChangedParameters _ x = PV4.ChangedParameters (PV4.dataToBuiltinData (toPlutusData x))
