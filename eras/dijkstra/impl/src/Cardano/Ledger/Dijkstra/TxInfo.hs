{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 910
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27342
{-# OPTIONS_GHC -fno-spec-eval #-}
#endif

module Cardano.Ledger.Dijkstra.TxInfo (
  DijkstraContextError (..),
  guardDijkstraFeaturesForPlutusV1toV3,
  transFailUnsupportedScriptInSubTx,
  transRedeemerPointerV4,
  transValidityInterval,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusScriptPurpose,
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  SupportedPlutusRunnable (..),
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (transPolicyID, transValue)
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (
  Exclusive (..),
  Inclusive (..),
  Inject (..),
  StrictMaybe (..),
  TxIx (TxIx),
  kindObjectValue,
  strictMaybe,
  strictMaybeToMaybe,
  txIxToInt,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Conway.TxInfo (
  ConwayContextError (..),
  ConwayEraPlutusTxInfo (..),
  transColdCommitteeCred,
  transDRepCred,
  transDelegatee,
  transHotCommitteeCred,
  transMap,
  transProposal,
  transSlotToPOSIXTime,
  transTxInInfoV1,
  transTxInInfoV3,
  transVoter,
 )
import qualified Cardano.Ledger.Conway.TxInfo as Conway
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraPlutusPurpose (..),
  PlutusScript (..),
 )
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCert)
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus (
  Datum (..),
  ExUnits,
  Language (..),
  PlutusArgs (..),
  PlutusLanguage,
  SLanguage (..),
  TxOutSource (..),
  binaryDataToData,
  decodePlutusRunnable,
  getPlutusData,
  plutusLanguage,
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transDataHash,
  transDatum,
  transEpochNo,
  transKeyHash,
  transSafeHash,
 )
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.State (StakePoolParams (..), UTxO)
import Cardano.Ledger.TxIn (TxId (TxId), TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (unless, zipWithM)
import Data.Aeson (KeyValue (..), ToJSON (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V4 as PV4

data DijkstraContextError era
  = ConwayContextError (ConwayContextError era)
  | -- | Failure translating sub-transactions for Guarding purpose at the top level
    SubTxContextError TxId (ContextError era)
  | -- | From Dijkstra onwards, attempt to use a script when there are stake ref pointers present in any outputs will result in this failure
    PointerPresentInOutput (NonEmptySet TxOutSource)
  | -- | Attempt to use PlutusV1-V3 in a sub-transaction will result in this failure
    UnsupportedScriptInSubTx Language TxId
  | -- | Attempt to use PlutusV1-V3 with non-empty direct deposits will result in this failure
    DirectDepositsNotSupported DirectDeposits
  | -- | Attempt to use PlutusV1-V3 with non-empty account balance intervals will result in this failure
    AccountBalanceIntervalsNotSupported (AccountBalanceIntervals era)
  | -- | Attempt to use PlutusV1-V3 with script hashes in guards will result in this failure
    GuardScriptHashesNotSupported (NonEmpty ScriptHash)
  | -- | Attempt to use PlutusV1-V3 with non-empty required top-level guards will result in this failure
    RequiredTopLevelGuardsNotSupported (NonEmptyMap (Credential Guard) (StrictMaybe (Data era)))
  | -- | Attempt to use PlutusV4 script with an invalid redeemer pointer will result in this failure
    ScriptHashNotFoundForPurpose (PlutusPurpose AsIx era)
  deriving (Generic)

deriving instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  , Eq (ContextError era)
  ) =>
  Eq (DijkstraContextError era)

deriving instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  , Ord (ContextError era)
  ) =>
  Ord (DijkstraContextError era)

deriving instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  , Show (ContextError era)
  ) =>
  Show (DijkstraContextError era)

instance
  ( AlonzoEraScript era
  , EraTxCert era
  , EraTxOut era
  , NFData (ContextError era)
  ) =>
  NFData (DijkstraContextError era)

instance
  ( ToJSON (TxOut era)
  , ToJSON (TxCert era)
  , ToJSON (ContextError era)
  , ToJSON (PlutusPurpose AsIx era)
  , ToJSON (PlutusPurpose AsItem era)
  , EraPParams era
  ) =>
  ToJSON (DijkstraContextError era)
  where
  toJSON = \case
    ConwayContextError x -> toJSON x
    SubTxContextError txId subTxError ->
      kindObjectValue
        "SubTxContextError"
        [ "txId" .= toJSON txId
        , "subTxError" .= toJSON subTxError
        ]
    PointerPresentInOutput x -> kindObjectValue "PointerPresentInOutput" ["txOut" .= toJSON x]
    UnsupportedScriptInSubTx lang txId ->
      kindObjectValue
        "UnsupportedScriptInSubTx"
        [ "language" .= toJSON lang
        , "txId" .= toJSON txId
        ]
    DirectDepositsNotSupported dd ->
      kindObjectValue "DirectDepositsNotSupported" ["direct_deposits" .= show dd]
    AccountBalanceIntervalsNotSupported abi ->
      kindObjectValue "AccountBalanceIntervalsNotSupported" ["account_balance_intervals" .= show abi]
    GuardScriptHashesNotSupported scriptHashes ->
      kindObjectValue "GuardScriptHashesNotSupported" ["script_hashes" .= toJSON scriptHashes]
    RequiredTopLevelGuardsNotSupported rtlg ->
      kindObjectValue "RequiredTopLevelGuardsNotSupported" ["required_top_level_guards" .= show rtlg]
    ScriptHashNotFoundForPurpose purpose ->
      kindObjectValue "ScriptHashNotFoundForPurpose" ["purpose" .= toJSON purpose]

instance
  ( EraPParams era
  , DecCBOR (TxOut era)
  , DecCBOR (TxCert era)
  , DecCBOR (ContextError era)
  , DecCBOR (PlutusPurpose AsIx era)
  , DecCBOR (PlutusPurpose AsItem era)
  ) =>
  DecCBOR (DijkstraContextError era)
  where
  decCBOR = decode $ Summands "ContextError" $ \case
    16 -> SumD ConwayContextError <! From
    17 -> SumD SubTxContextError <! From <! From
    18 -> SumD PointerPresentInOutput <! From
    19 -> SumD UnsupportedScriptInSubTx <! From <! From
    20 -> SumD DirectDepositsNotSupported <! From
    21 -> SumD AccountBalanceIntervalsNotSupported <! From
    22 -> SumD GuardScriptHashesNotSupported <! From
    23 -> SumD RequiredTopLevelGuardsNotSupported <! From
    24 -> SumD ScriptHashNotFoundForPurpose <! From
    k -> Invalid k

instance
  ( EraPParams era
  , EncCBOR (TxCert era)
  , EncCBOR (ContextError era)
  , EncCBOR (PlutusPurpose AsIx era)
  , EncCBOR (PlutusPurpose AsItem era)
  ) =>
  EncCBOR (DijkstraContextError era)
  where
  encCBOR =
    encode . \case
      ConwayContextError x -> Sum ConwayContextError 16 !> To x
      SubTxContextError txId subTxError -> Sum SubTxContextError 17 !> To txId !> To subTxError
      PointerPresentInOutput x -> Sum PointerPresentInOutput 18 !> To x
      UnsupportedScriptInSubTx lang txId ->
        Sum UnsupportedScriptInSubTx 19 !> To lang !> To txId
      DirectDepositsNotSupported dd -> Sum DirectDepositsNotSupported 20 !> To dd
      AccountBalanceIntervalsNotSupported abi -> Sum AccountBalanceIntervalsNotSupported 21 !> To abi
      GuardScriptHashesNotSupported scriptHashes ->
        Sum GuardScriptHashesNotSupported 22 !> To scriptHashes
      RequiredTopLevelGuardsNotSupported rtlg ->
        Sum RequiredTopLevelGuardsNotSupported 23 !> To rtlg
      ScriptHashNotFoundForPurpose purpose ->
        Sum ScriptHashNotFoundForPurpose 24 !> To purpose

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

  mkSupportedPlutusRunnable v = \case
    DijkstraPlutusV1 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p
    DijkstraPlutusV2 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p
    DijkstraPlutusV3 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p
    DijkstraPlutusV4 p -> SupportedPlutusRunnable $ decodePlutusRunnable v p

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

instance EraPlutusTxInfo 'PlutusV1 DijkstraEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose proxy lti = Conway.transPlutusPurposeV1V2 proxy (ltiProtVer lti)

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailUnsupportedScriptInSubTx $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      guardDijkstraFeaturesForPlutusV1toV3 tx
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

  toPlutusScriptPurpose proxy lti = Conway.transPlutusPurposeV1V2 proxy (ltiProtVer lti)

  toPlutusTxInfo proxy lti@LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailUnsupportedScriptInSubTx $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      guardDijkstraFeaturesForPlutusV1toV3 tx
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
      plutusRedeemers <- Babbage.transTxRedeemers proxy lti
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
  toPlutusTxCert _ _ = pure . transTxCertV3

  toPlutusScriptPurpose proxy lti = Conway.transPlutusPurposeV3 proxy (ltiProtVer lti)

  toPlutusTxInfo proxy lti@LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailUnsupportedScriptInSubTx $ \tx -> PlutusTxInfoResult $ do
      let
        txBody = tx ^. bodyTxL
        txInputs = txBody ^. inputsTxBodyL
        refInputs = txBody ^. referenceInputsTxBodyL
      guardDijkstraFeaturesForPlutusV1toV3 tx
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
      plutusRedeemers <- Babbage.transTxRedeemers proxy lti
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

guardDijkstraFeaturesForPlutusV1toV3 ::
  forall era.
  ( EraTx era
  , DijkstraEraTxBody era
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  Tx TopTx era ->
  Either (ContextError era) ()
guardDijkstraFeaturesForPlutusV1toV3 tx = do
  let txBody = tx ^. bodyTxL
      directDeposits = txBody ^. directDepositsTxBodyL
      accountBalanceIntervals = txBody ^. accountBalanceIntervalsTxBodyL
      requiredTopLevelGuards = txBody ^. requiredTopLevelGuardsL
      scriptHashes = [sh | ScriptHashObj sh <- toList (txBody ^. guardsTxBodyL)]
  unless (null $ unDirectDeposits directDeposits) $
    Left $
      inject $
        DirectDepositsNotSupported @era directDeposits
  unless (null $ unAccountBalanceIntervals accountBalanceIntervals) $
    Left $
      inject $
        AccountBalanceIntervalsNotSupported @era accountBalanceIntervals
  case NEMap.fromMap requiredTopLevelGuards of
    Nothing -> Right ()
    Just neRequiredTopLevelGuards ->
      Left $
        inject $
          RequiredTopLevelGuardsNotSupported @era neRequiredTopLevelGuards
  case NE.nonEmpty scriptHashes of
    Nothing -> Right ()
    Just neScriptHashes ->
      Left $
        inject $
          GuardScriptHashesNotSupported @era neScriptHashes

transFailUnsupportedScriptInSubTx ::
  forall l era.
  ( EraTx era
  , Inject (DijkstraContextError era) (ContextError era)
  , PlutusLanguage l
  ) =>
  Tx SubTx era -> PlutusTxInfoResult l era
transFailUnsupportedScriptInSubTx tx =
  PlutusTxInfoResult $
    Left $
      inject $
        UnsupportedScriptInSubTx @era (plutusLanguage (Proxy @l)) (txIdTx tx)

transTxCertV3 ::
  (ConwayEraTxCert era, TxCert era ~ DijkstraTxCert era) => TxCert era -> PV3.TxCert
transTxCertV3 = \case
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

transRedeemerPointerV4 ::
  forall era l proxy.
  ( EraTx era
  , AlonzoEraTxBody era
  , EraPlutusTxInfo l era
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  LedgerTxInfo era ->
  (PlutusPurpose AsIx era, (Data era, ExUnits)) ->
  Either (ContextError era) (PlutusScriptPurpose l, PV4.Redeemer)
transRedeemerPointerV4 proxy lti@LedgerTxInfo {ltiTx} (ptr, (d, _)) =
  case redeemerPointerInverse (ltiTx ^. bodyTxL) ptr of
    SNothing -> Left . inject $ Babbage.RedeemerPointerPointsToNothing ptr
    SJust sp -> (,Babbage.transRedeemer d) <$> toPlutusScriptPurpose proxy lti sp

instance EraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusTxCert proxy _pv cert = pure $ transTxCertV4 proxy cert

  toPlutusScriptPurpose = transPlutusPurposeV4

  toPlutusTxInfo proxy lti@LedgerTxInfo {..} =
    PlutusTxInfoResult $ do
      let
        era = Proxy @DijkstraEra
        txBody = ltiTx ^. bodyTxL
        txInputs = txBody ^. inputsTxBodyL
        refInputs = txBody ^. referenceInputsTxBodyL
      timeRange <-
        transValidityInterval era ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputsInfo <- mapM (transTxInInfoV4 ltiUTxO) (Set.toList txInputs)
      refInputsInfo <- mapM (transTxInInfoV4 ltiUTxO) (Set.toList refInputs)
      Conway.checkReferenceInputsNotDisjointFromInputs txBody
      let
        accErrors acc (ix, txOut) =
          let res = transTxOutV4 (TxOutFromOutput ix) txOut
           in case acc of
                Right l -> case res of
                  Right x -> Right $ x : l
                  Left e -> Left e
                Left (PointerPresentInOutput errs)
                  -- If the accumulator contains a PointerPresentInOutput, then
                  -- continue translating to collect all the other PointerPresentInOutput
                  -- failures
                  | Left (PointerPresentInOutput err) <- res ->
                      Left . PointerPresentInOutput $ err <> errs
                Left e -> Left e
      outputs <-
        reverse
          <$>
          -- Use foldl here to collect errors from left to right (leftmost failure
          -- takes precedence)
          foldl'
            accErrors
            (Right mempty)
            ([minBound ..] `zip` F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      plutusRedeemers <- transTxRedeemersV4 proxy lti
      let
        txInfo =
          PV4.TxInfo
            { PV4.txInfoInputs = inputsInfo
            , PV4.txInfoOutputs = outputs
            , PV4.txInfoReferenceInputs = refInputsInfo
            , PV4.txInfoMint = Conway.transMintValue (txBody ^. mintTxBodyL)
            , PV4.txInfoTxCerts = txCerts
            , PV4.txInfoValidRange = timeRange
            , PV4.txInfoRedeemers = plutusRedeemers
            , PV4.txInfoData = PV3.unsafeFromList $ Alonzo.transTxWitsDatums (ltiTx ^. witsTxL)
            , PV4.txInfoId = Conway.transTxBodyId txBody
            , PV4.txInfoVotes = Conway.transVotingProcedures (txBody ^. votingProceduresTxBodyL)
            , PV4.txInfoProposalProcedures =
                map (Conway.transProposal proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
            , PV4.txInfoCurrentTreasuryAmount =
                strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
            , PV4.txInfoTreasuryDonation = transCoinToLovelace $ txBody ^. treasuryDonationTxBodyL
            , PV4.txInfoSubTxIx = Nothing -- TODO thread the subtx index here
            , PV4.txInfoWithdrawals = transTxBodyWithdrawals txBody
            , PV4.txInfoDirectDeposits = transTxBodyDirectDeposits txBody
            , PV4.txInfoAccountBalanceIntervals =
                transAccountBalanceIntervals $ txBody ^. accountBalanceIntervalsTxBodyL
            , PV4.txInfoGuards = transTxBodyGuards txBody
            , PV4.txInfoRequiredTopLevelGuards = transTxBodyRequiredTopLevelGuards txBody
            }
      Right $ \_ -> Right txInfo

  toPlutusArgs = toPlutusV4Args

  toPlutusTxInInfo _ = transTxInInfoV4

transTxInV4 :: TxIn -> PV4.TxOutRef
transTxInV4 (TxIn txid txIx) = PV4.TxOutRef (transTxId txid) (toInteger (txIxToInt txIx))

transTxInInfoV4 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV4.TxInInfo
transTxInInfoV4 utxo txIn = do
  txOut <- first (inject . Babbage.AlonzoContextError @era) $ Alonzo.transLookupTxOut utxo txIn
  plutusTxOut <- transTxOutV4 (TxOutFromInput txIn) txOut
  Right (PV4.TxInInfo (transTxInV4 txIn) plutusTxOut)

transTxOutV4 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  TxOutSource ->
  TxOut era ->
  Either (ContextError era) PV4.TxOut
transTxOutV4 txOutSource txOut = do
  let
    val = transValue $ txOut ^. valueTxOutL
    referenceScript = Babbage.transReferenceScript $ txOut ^. referenceScriptTxOutL
    datum =
      case txOut ^. datumTxOutF of
        NoDatum -> PV2.NoOutputDatum
        DatumHash dh -> PV2.OutputDatumHash $ transDataHash dh
        Datum binaryData ->
          PV2.OutputDatum
            . PV2.Datum
            . PV2.dataToBuiltinData
            . getPlutusData
            . binaryDataToData
            $ binaryData

  addr <-
    case txOut ^. addrTxOutL of
      Addr _ pCred stakeRef ->
        PV4.Address (transCred pCred) <$> case stakeRef of
          StakeRefBase sCred -> Right . Just $ transCredToAccountId sCred
          StakeRefNull -> Right Nothing
          StakeRefPtr _ -> Left . inject . PointerPresentInOutput @era $ NES.singleton txOutSource
      AddrBootstrap _ -> Left . inject $ Babbage.ByronTxOutInContext @era txOutSource
  pure $
    PV4.TxOut
      { txOutReferenceScript = referenceScript
      , txOutDatum = datum
      , txOutValue = val
      , txOutAddress = addr
      }

-- | Translate all `Redeemers` from within a `Tx` into a Map from a `PlutusScriptPurpose`
-- to a `PV2.Redeemer`
transTxRedeemersV4 ::
  ( EraPlutusTxInfo l era
  , AlonzoEraTxBody era
  , EraTx era
  , AlonzoEraTxWits era
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  LedgerTxInfo era ->
  Either (ContextError era) (PV2.Map (PlutusScriptPurpose l) PV2.Redeemer)
transTxRedeemersV4 proxy lti@LedgerTxInfo {ltiTx} =
  PV2.unsafeFromList
    <$> mapM
      (transRedeemerPointerV4 proxy lti)
      (Map.toList $ ltiTx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)

transAccountId :: AccountId -> PV4.AccountId
transAccountId (AccountId cred) = PV4.AccountId $ transCred cred

transTxBodyWithdrawals ::
  DijkstraEraTxBody era => TxBody l era -> PV4.Map PV4.Credential PV4.Lovelace
transTxBodyWithdrawals txb = transMap transAccountAddressToCredential transCoinToLovelace withdrawals
  where
    Withdrawals withdrawals = txb ^. withdrawalsTxBodyL

transCredToAccountId :: Credential r -> PV4.AccountId
transCredToAccountId = PV4.AccountId . transCred

transTxCertV4 :: ConwayEraTxCert era => proxy 'PlutusV4 -> TxCert era -> PV4.TxCert
transTxCertV4 _proxy = \case
  RegPoolTxCert StakePoolParams {sppId, sppVrf} ->
    PV4.TxCertPoolRegister
      (transKeyHash sppId)
      (PV4.PubKeyHash (PV4.toBuiltin (hashToBytes (unVRFVerKeyHash sppVrf))))
  RetirePoolTxCert poolId retireEpochNo ->
    PV4.TxCertPoolRetire (transKeyHash poolId) (transEpochNo retireEpochNo)
  RegDepositTxCert stakeCred deposit ->
    PV4.TxCertRegAccount (transCredToAccountId stakeCred) (transCoinToLovelace deposit)
  UnRegDepositTxCert stakeCred refund ->
    PV4.TxCertUnRegAccount (transCredToAccountId stakeCred) (transCoinToLovelace refund)
  DelegTxCert stakeCred delegatee ->
    PV4.TxCertDelegAccount (transCredToAccountId stakeCred) (transDelegatee delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV4.TxCertRegAccountDeleg
      (transCredToAccountId stakeCred)
      (transDelegatee delegatee)
      (transCoinToLovelace deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV4.TxCertAuthHotCommittee (transColdCommitteeCred coldCred) (transHotCommitteeCred hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV4.TxCertResignColdCommittee (transColdCommitteeCred coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV4.TxCertRegDRep (transDRepCred drepCred) (transCoinToLovelace deposit)
  UnRegDRepTxCert drepCred refund ->
    PV4.TxCertUnRegDRep (transDRepCred drepCred) (transCoinToLovelace refund)
  UpdateDRepTxCert drepCred _anchor ->
    PV4.TxCertUpdateDRep (transDRepCred drepCred)
  _ -> error "Impossible: All TxCerts should have been accounted for"

transTxBodyRequiredTopLevelGuards ::
  DijkstraEraTxBody era => TxBody l era -> PV4.Map PV4.Credential (Maybe PV4.Datum)
transTxBodyRequiredTopLevelGuards txb = transMap transCred (fmap transDatum . strictMaybeToMaybe) requiredGuards
  where
    requiredGuards = txb ^. requiredTopLevelGuardsL

transAccountAddressToAccountId :: AccountAddress -> PV4.AccountId
transAccountAddressToAccountId (AccountAddress _ (AccountId c)) = PV4.AccountId $ transCred c

transAccountAddressToCredential :: AccountAddress -> PV4.Credential
transAccountAddressToCredential (AccountAddress _ (AccountId c)) = transCred c

transTxBodyDirectDeposits ::
  DijkstraEraTxBody era => TxBody l era -> PV4.Map PV4.Credential PV4.Lovelace
transTxBodyDirectDeposits txb = transMap transAccountAddressToCredential transCoinToLovelace deposits
  where
    DirectDeposits deposits = txb ^. directDepositsTxBodyL

-- | Translate a validity interval to PV4.POSIXTimeRange
transValidityInterval ::
  Inject (Alonzo.AlonzoContextError era) (ContextError era) =>
  Proxy era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  ValidityInterval ->
  Either (ContextError era) PV4.POSIXTimeRange
transValidityInterval era epochInfo systemStart (ValidityInterval from to) = do
  let transSlot = transSlotToPOSIXTime era epochInfo systemStart
  pFrom <- traverse transSlot from
  pTo <- traverse transSlot to
  pure $ PV4.POSIXTimeRange (strictMaybeToMaybe pFrom) (strictMaybeToMaybe pTo)

transAccountBalanceInterval :: AccountBalanceInterval era -> PV4.AccountBalanceInterval
transAccountBalanceInterval = \case
  AccountBalanceExact c -> PV4.AccountBalanceExact $ transCoinToLovelace c
  AccountBalanceLowerBound (Inclusive l) -> PV4.AccountBalanceLowerBound $ transCoinToLovelace l
  AccountBalanceUpperBound (Exclusive u) -> PV4.AccountBalanceUpperBound $ transCoinToLovelace u
  AccountBalanceBothBounds (Inclusive l) (Exclusive u) -> PV4.AccountBalanceBothBounds (transCoinToLovelace l) (transCoinToLovelace u)

transAccountBalanceIntervals :: AccountBalanceIntervals era -> PV4.AccountBalanceIntervals
transAccountBalanceIntervals (AccountBalanceIntervals balanceIntervals) =
  PV4.AccountBalanceIntervals $
    transMap transAccountAddressToAccountId transAccountBalanceInterval balanceIntervals

transTxBodyGuards :: DijkstraEraTxBody era => TxBody l era -> [PV4.Credential]
transTxBodyGuards txb = fmap transCred . F.toList $ txb ^. guardsTxBodyL

scriptPurposeToScriptInfo ::
  PV4.ScriptPurpose ->
  Maybe PV4.Datum ->
  Maybe PV4.TopTxInfo ->
  PV4.ScriptInfo
scriptPurposeToScriptInfo sp datum topInfo = case sp of
  PV4.Spending _ ref -> PV4.SpendingScript ref datum
  PV4.Minting _ sym -> PV4.MintingScript sym
  PV4.Withdrawing _ c -> PV4.WithdrawingScript $ PV4.AccountId c
  PV4.Certifying _ ix cert -> PV4.CertifyingScript ix cert
  PV4.Voting _ v -> PV4.VotingScript v
  PV4.Proposing _ ix proc -> PV4.ProposingScript ix proc
  PV4.Guarding _ ix -> PV4.GuardingScript ix topInfo

toPlutusV4Args ::
  ( AlonzoEraUTxO era
  , EraPlutusTxInfo PlutusV4 era
  ) =>
  proxy 'PlutusV4 ->
  LedgerTxInfo era ->
  PV4.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV4)
toPlutusV4Args proxy lti@LedgerTxInfo {..} txInfo plutusPurpose redeemerData = do
  scriptPurpose <- toPlutusScriptPurpose proxy lti plutusPurpose
  let
    maybeSpendingData = getSpendingDatum ltiUTxO ltiTx $ hoistPlutusPurpose toAsItem plutusPurpose
    -- TODO TopTxInfo should be set if this is a top-level transaction
    scriptInfo = scriptPurposeToScriptInfo scriptPurpose (transDatum <$> maybeSpendingData) Nothing
    sh = error "Unimplemented: ScriptHash for ScriptContext"
  pure $
    PlutusV4Args $
      PV4.ScriptContext
        { PV4.scriptContextTxInfo = txInfo
        , PV4.scriptContextRedeemer = Babbage.transRedeemer redeemerData
        , PV4.scriptContextScriptInfo = scriptInfo
        , PV4.scriptContextScriptHash = sh
        }

transTxId :: TxId -> PV4.TxId
transTxId (TxId h) = PV4.TxId $ transSafeHash h

transPlutusPurposeV4 ::
  ConwayEraPlutusTxInfo PlutusV4 era =>
  proxy 'PlutusV4 ->
  LedgerTxInfo era ->
  DijkstraPlutusPurpose AsIxItem era ->
  Either (ContextError era) (PlutusScriptPurpose PlutusV4)
transPlutusPurposeV4 proxy lti = \case
  DijkstraSpending (AsIxItem _ (TxIn txId (TxIx ix))) ->
    pure . PV4.Spending sh $ PV4.TxOutRef (transTxId txId) (toInteger ix)
  DijkstraMinting (AsIxItem _ pId) -> pure . PV4.Minting sh $ transPolicyID pId
  DijkstraCertifying (AsIxItem ix cert) ->
    PV4.Certifying sh (toInteger ix) <$> toPlutusTxCert proxy pv cert
  DijkstraWithdrawing (AsIxItem _ (AccountAddress _ (AccountId c))) ->
    pure $ PV4.Withdrawing sh (transCred c)
  DijkstraVoting (AsIxItem _ voter) -> pure $ PV4.Voting sh (transVoter voter)
  DijkstraProposing (AsIxItem ix proc) ->
    pure $ PV4.Proposing sh (toInteger ix) (transProposal proxy proc)
  DijkstraGuarding (AsIxItem ix _) -> pure $ PV4.Guarding sh (toInteger ix)
  where
    pv = ltiProtVer lti
    sh = error "Unimplemented: ScriptHash for purpose"
