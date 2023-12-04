{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxInfo (
  ContextError (..),
  transWithdrawals,
  transTxCert,
  transDRepCred,
  transColdCommitteeCred,
  transHotCommitteeCred,
  transDelegatee,
  transDRep,
  transScriptPurpose,
) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  mkPlutusLanguageContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo hiding (ContextError (..))
import Cardano.Ledger.Alonzo.Tx (Data, ScriptPurpose (..), rdptrInv)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), RdmrPtr, unRedeemers, unTxDats)
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage hiding (ContextError (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..), isSJust)
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
import Cardano.Ledger.Conway.Scripts (PlutusScript (..))
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (
  PlutusScriptPurpose,
  PlutusTxCert,
  transCoin,
  transCred,
  transKeyHash,
  txInfoIn',
 )
import Cardano.Ledger.PoolParams
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when, zipWithM)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- | Given a TxOut, translate it for V2 and return (Right transalation).
-- If the transaction contains any Byron addresses or Conway features, return Left.
txInfoOutV1 ::
  forall c.
  Crypto c =>
  TxOutSource c ->
  TxOut (ConwayEra c) ->
  Either (ContextError (ConwayEra c)) PV1.TxOut
txInfoOutV1 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = txOut ^. referenceScriptTxOutL
  when (isSJust referenceScript) $ Left $ ReferenceScriptsNotSupported os
  dataHash <-
    case txOut ^. datumTxOutF of
      NoDatum -> Right Nothing
      DatumHash dh -> Right $ Just $ Alonzo.transDataHash dh
      Datum _ -> Left $ InlineDatumsNotSupported os
  addr <-
    case Alonzo.transTxOutAddr txOut of
      Nothing -> Left (ByronTxOutInContext os)
      Just addr -> Right addr
  Right (PV1.TxOut addr (Alonzo.transValue @c val) dataHash)

-- | Given a TxOut, translate it for V2 and return (Right transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Left.
txInfoOutV2 ::
  forall c.
  Crypto c =>
  TxOutSource c ->
  TxOut (ConwayEra c) ->
  Either (ContextError (ConwayEra c)) PV2.TxOut
txInfoOutV2 os txOut = do
  let val = txOut ^. valueTxOutL
      referenceScript = Babbage.transReferenceScript $ txOut ^. referenceScriptTxOutL
      datum =
        case txOut ^. datumTxOutF of
          NoDatum -> PV2.NoOutputDatum
          DatumHash dh -> PV2.OutputDatumHash $ Alonzo.transDataHash dh
          Datum binaryData ->
            PV2.OutputDatum
              . PV2.Datum
              . PV2.dataToBuiltinData
              . getPlutusData
              . binaryDataToData
              $ binaryData
  case Alonzo.transTxOutAddr txOut of
    Nothing -> Left (ByronTxOutInContext os)
    Just ad ->
      Right (PV2.TxOut ad (Alonzo.transValue @c val) datum referenceScript)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V1 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV1 ::
  Crypto c =>
  UTxO (ConwayEra c) ->
  TxIn c ->
  Either (ContextError (ConwayEra c)) PV1.TxInInfo
txInfoInV1 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV1 (TxOutFromInput txin) txout
      Right (PV1.TxInInfo (Alonzo.txInfoIn' txin) out)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it to the V2 context
--   and return (Just translation). If does not exist in the UTxO, return Nothing.
txInfoInV2 ::
  Crypto c =>
  UTxO (ConwayEra c) ->
  TxIn c ->
  Either (ContextError (ConwayEra c)) PV2.TxInInfo
txInfoInV2 (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Left (TranslationLogicMissingInput txin)
    Just txout -> do
      out <- txInfoOutV2 (TxOutFromInput txin) txout
      Right (PV2.TxInInfo (Alonzo.txInfoIn' txin) out)

transRedeemerPtr ::
  EraPlutusTxInfo l (ConwayEra c) =>
  proxy l ->
  TxBody (ConwayEra c) ->
  (RdmrPtr, (Data (ConwayEra c), ExUnits)) ->
  Either (ContextError (ConwayEra c)) (PlutusScriptPurpose l, PV2.Redeemer)
transRedeemerPtr proxy txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left (RdmrPtrPointsToNothing ptr)
    SJust sp -> do
      plutusScriptPurpose <- toPlutusScriptPurpose proxy sp
      Right (plutusScriptPurpose, Babbage.transRedeemer d)

instance Crypto c => EraPlutusContext (ConwayEra c) where
  data ContextError (ConwayEra c)
    = ByronTxOutInContext !(TxOutSource c)
    | TranslationLogicMissingInput !(TxIn c)
    | RdmrPtrPointsToNothing !RdmrPtr
    | InlineDatumsNotSupported !(TxOutSource c)
    | ReferenceScriptsNotSupported !(TxOutSource c)
    | ReferenceInputsNotSupported !(Set.Set (TxIn c))
    | TimeTranslationPastHorizon !Text
    | CertificateNotSupported (TxCert (ConwayEra c))
    deriving (Eq, Show, Generic)

  mkPlutusScriptContext = \case
    ConwayPlutusV1 p -> mkPlutusLanguageContext p
    ConwayPlutusV2 p -> mkPlutusLanguageContext p
    ConwayPlutusV3 p -> mkPlutusLanguageContext p

instance NoThunks (ContextError (ConwayEra c))

instance Crypto c => NFData (ContextError (ConwayEra c))

instance Crypto c => EncCBOR (ContextError (ConwayEra c)) where
  encCBOR = \case
    ByronTxOutInContext txOutSource ->
      encode $ Sum ByronTxOutInContext 0 !> To txOutSource
    TranslationLogicMissingInput txIn ->
      encode $ Sum TranslationLogicMissingInput 1 !> To txIn
    RdmrPtrPointsToNothing ptr ->
      encode $ Sum RdmrPtrPointsToNothing 2 !> To ptr
    InlineDatumsNotSupported txOutSource ->
      encode $ Sum InlineDatumsNotSupported 4 !> To txOutSource
    ReferenceScriptsNotSupported txOutSource ->
      encode $ Sum ReferenceScriptsNotSupported 5 !> To txOutSource
    ReferenceInputsNotSupported txIns ->
      encode $ Sum ReferenceInputsNotSupported 6 !> To txIns
    TimeTranslationPastHorizon err ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err
    CertificateNotSupported txCert ->
      encode $ Sum CertificateNotSupported 7 !> To txCert

instance Crypto c => DecCBOR (ContextError (ConwayEra c)) where
  decCBOR = decode $ Summands "ContextError" $ \case
    0 -> SumD ByronTxOutInContext <! From
    1 -> SumD TranslationLogicMissingInput <! From
    2 -> SumD RdmrPtrPointsToNothing <! From
    4 -> SumD InlineDatumsNotSupported <! From
    5 -> SumD ReferenceScriptsNotSupported <! From
    6 -> SumD ReferenceInputsNotSupported <! From
    7 -> SumD TimeTranslationPastHorizon <! From
    8 -> SumD CertificateNotSupported <! From
    n -> Invalid n

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (ConwayEra c) where
  toPlutusTxCert _ = fmap Alonzo.transTxCert . downgradeConwayTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp ei sysS utxo tx = do
    let refInputs = txBody ^. referenceInputsTxBodyL
        interval = tx ^. bodyTxL . vldtTxBodyL
    timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
    unless (Set.null refInputs) $ Left (ReferenceInputsNotSupported refInputs)
    inputs <- mapM (txInfoInV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    outputs <-
      zipWithM
        (txInfoOutV1 . TxOutFromOutput)
        [minBound ..]
        (foldr (:) [] outs)
    txInfoCerts <- mapM (toPlutusTxCert proxy) $ toList $ (txBody ^. certsTxBodyL)
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txInfoCerts
        , PV1.txInfoWdrl = Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories =
            map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
        , PV1.txInfoData = map Alonzo.transDataPair datpairs
        , PV1.txInfoId = PV1.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
        }
    where
      txBody = tx ^. bodyTxL
      txWits = tx ^. witsTxL
      outs = txBody ^. outputsTxBodyL
      datpairs = Map.toList (unTxDats $ txWits ^. datsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (ConwayEra c) where
  toPlutusTxCert _ = fmap Alonzo.transTxCert . downgradeConwayTxCert

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp ei sysS utxo tx = do
    let interval = tx ^. bodyTxL . vldtTxBodyL
    timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
    inputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (txInfoOutV2 . TxOutFromOutput)
        [minBound ..]
        (foldr (:) [] outs)
    rdmrs' <- mapM (transRedeemerPtr proxy txBody) rdmrs
    txInfoCerts <- mapM (toPlutusTxCert proxy) $ toList $ (txBody ^. certsTxBodyL)
    pure
      PV2.TxInfo
        { PV2.txInfoInputs = inputs
        , PV2.txInfoOutputs = outputs
        , PV2.txInfoReferenceInputs = refInputs
        , PV2.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV2.txInfoDCert = txInfoCerts
        , PV2.txInfoWdrl =
            PV2.fromList $ Map.toList (Alonzo.transWithdrawals (txBody ^. withdrawalsTxBodyL))
        , PV2.txInfoValidRange = timeRange
        , PV2.txInfoSignatories =
            map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
        , PV2.txInfoRedeemers = PV2.fromList rdmrs'
        , PV2.txInfoData = PV2.fromList $ map Alonzo.transDataPair datpairs
        , PV2.txInfoId = PV2.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
        }
    where
      txBody = tx ^. bodyTxL
      txWits = tx ^. witsTxL
      outs = txBody ^. outputsTxBodyL
      datpairs = Map.toList (unTxDats $ txWits ^. datsTxWitsL)
      rdmrs = Map.toList (unRedeemers $ txWits ^. rdmrsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV2.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV3 (ConwayEra c) where
  toPlutusTxCert _ = pure . transTxCert -- fmap Alonzo.transTxCert . downgradeConwayTxCert

  toPlutusScriptPurpose = transScriptPurpose

  toPlutusTxInfo proxy pp ei sysS utxo tx = do
    let interval = tx ^. bodyTxL . vldtTxBodyL
    timeRange <- left TimeTranslationPastHorizon $ Alonzo.transVITime pp ei sysS interval
    inputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (txInfoInV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (txInfoOutV2 . Alonzo.TxOutFromOutput)
        [minBound ..]
        (foldr (:) [] outs)
    rdmrs' <- mapM (transRedeemerPtr proxy txBody) rdmrs
    txInfoCerts <- mapM (toPlutusTxCert proxy) $ toList $ (txBody ^. certsTxBodyL)
    pure
      PV3.TxInfo -- TODO Add relevant CIP-1694 data to PV3.TxInfo
        { PV3.txInfoInputs = inputs
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputs
        , PV3.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , -- Note that this translation is different from previous Plutus versions, since we no
          -- longer add a zero ADA value to the mint field during translation:
          PV3.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txInfoCerts
        , PV3.txInfoWdrl =
            PV3.fromList $ Map.toList (transWithdrawals (txBody ^. withdrawalsTxBodyL))
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories =
            map Alonzo.transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
        , PV3.txInfoRedeemers = PV3.fromList rdmrs'
        , PV3.txInfoData = PV3.fromList $ map Alonzo.transDataPair datpairs
        , PV3.txInfoId = PV3.TxId (Alonzo.transSafeHash (hashAnnotated txBody))
        , -- FIXME: implement for plutus v3
          PV3.txInfoVotes = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoProposalProcedures = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoCurrentTreasuryAmount = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoTreasuryDonation = error "Unimplemented"
        }
    where
      txBody = tx ^. bodyTxL
      txWits = tx ^. witsTxL
      outs = txBody ^. outputsTxBodyL
      datpairs = Map.toList (unTxDats $ txWits ^. datsTxWitsL)
      rdmrs = Map.toList (unRedeemers $ txWits ^. rdmrsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV3.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

-- -- | This is a temporary version that only translates certificates from previous eras,
-- -- none of them are Conway specific.
-- conwayTransTxCert :: ShelleyEraTxCert era => TxCert era -> PV3.TxCert
-- conwayTransTxCert = \case
--   RegTxCert stakeCred ->
--     PV3.TxCertRegStaking (Alonzo.transCred stakeCred) (error "unimplemented")
--   UnRegTxCert stakeCred ->
--     PV3.TxCertUnRegStaking (Alonzo.transCred stakeCred) (error "unimplemented")
--   DelegStakeTxCert stakeCred _keyHash ->
--     PV3.TxCertDelegStaking
--       (Alonzo.transCred stakeCred)
--       (error "unimplemented")
--   _ -> error "Translation of Conway Certificate is not implemented yet"

-- | Adapted from `Alonzo.transWithdrawals` so as to drop the 'StakingCredential' constructor.
transWithdrawals :: Withdrawals c -> Map.Map PV3.Credential Integer
transWithdrawals (Withdrawals mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) =
      Map.insert (Alonzo.transCred cred) n ans

-- transRedeemerPtr ::
--   ( MaryEraTxBody era
--   , EraPlutusContext 'PlutusV3 era
--   ) =>
--   TxBody era ->
--   (RdmrPtr, (Data era, ExUnits)) ->
--   Either (TranslationError (EraCrypto era)) (PV3.ScriptPurpose, PV3.Redeemer)
-- transRedeemerPtr txb (ptr, (d, _)) =
--   case rdptrInv txb ptr of
--     SNothing -> Left (RdmrPtrPointsToNothing ptr)
--     SJust sp -> Right (transScriptPurpose sp, B.transRedeemer d)

transTxCert :: ConwayEraTxCert era => TxCert era -> PV3.TxCert
transTxCert = \case
  RegPoolTxCert (PoolParams {}) ->
    error "Unimplemented"
  --   PV3.TxCertPoolRegister (transKeyHash ppId) (PV3.PubKeyHash (PV3.toBuiltin (hashToBytes ppVrf)))
  RetirePoolTxCert _poolId (EpochNo _i) ->
    error "Unimplemented"
  --   PV3.TxCertPoolRetire (transKeyHash poolId) (toInteger i)
  RegTxCert stakeCred ->
    PV3.TxCertRegStaking (transCred stakeCred) Nothing
  UnRegTxCert stakeCred ->
    PV3.TxCertUnRegStaking (transCred stakeCred) Nothing
  RegDepositTxCert stakeCred deposit ->
    PV3.TxCertRegStaking (transCred stakeCred) (Just (transCoin deposit))
  UnRegDepositTxCert stakeCred refund ->
    PV3.TxCertUnRegStaking (transCred stakeCred) (Just (transCoin refund))
  DelegTxCert stakeCred delegatee ->
    PV3.TxCertDelegStaking (transCred stakeCred) (transDelegatee delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV3.TxCertRegDeleg (transCred stakeCred) (transDelegatee delegatee) (transCoin deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV3.TxCertAuthHotCommittee (transColdCommitteeCred coldCred) (transHotCommitteeCred hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV3.TxCertResignColdCommittee (transColdCommitteeCred coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV3.TxCertRegDRep (transDRepCred drepCred) (transCoin deposit)
  UnRegDRepTxCert drepCred refund ->
    PV3.TxCertUnRegDRep (transDRepCred drepCred) (transCoin refund)
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

transScriptPurpose ::
  (EraPlutusTxInfo l era, PlutusTxCert l ~ PV3.TxCert) =>
  proxy l ->
  ScriptPurpose era ->
  Either (ContextError era) PV3.ScriptPurpose
transScriptPurpose proxy = \case
  Minting policyId -> pure $ PV3.Minting (Alonzo.transPolicyID policyId)
  Spending txIn -> pure $ PV3.Spending (txInfoIn' txIn)
  Rewarding (RewardAcnt _networkId cred) ->
    pure $ PV3.Rewarding (transCred cred)
  Certifying txCert -> PV3.Certifying <$> toPlutusTxCert proxy txCert

-- FIXME: Add support for PV3, add voting , proposing

downgradeConwayTxCert ::
  Crypto c =>
  TxCert (ConwayEra c) ->
  Either (ContextError (ConwayEra c)) (TxCert (BabbageEra c))
downgradeConwayTxCert = \case
  RegPoolTxCert poolParams -> Right $ RegPoolTxCert poolParams
  RetirePoolTxCert poolId epochNo -> Right $ RetirePoolTxCert poolId epochNo
  RegTxCert cred -> Right $ RegTxCert cred
  UnRegTxCert cred -> Right $ UnRegTxCert cred
  DelegStakeTxCert cred poolId -> Right $ DelegStakeTxCert cred poolId
  txCert -> Left $ CertificateNotSupported txCert
