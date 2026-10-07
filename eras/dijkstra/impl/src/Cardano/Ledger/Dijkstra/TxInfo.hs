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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 910
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27342
{-# OPTIONS_GHC -fno-spec-eval #-}
#endif

module Cardano.Ledger.Dijkstra.TxInfo (
  DijkstraContextError (..),
  guardDijkstraFeaturesForPlutusV1toV3,
  transFailUnsupportedScriptInSubTx,
  transTxRedeemersV4,
  transValidityInterval,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Address (AddressProtection (..), shelleyAddressView)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerLevelTxInfo (..),
  LedgerTxInfo (..),
  PlutusScriptPurpose,
  PlutusTxInfo,
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
  SupportedPlutusRunnable (..),
 )
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Scripts (toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (
  BoundedRational (..),
  Exclusive (..),
  Inclusive (..),
  Inject (..),
  ProtVer (..),
  StrictMaybe (..),
  TxIx (TxIx),
  getVersion32,
  kindObjectValue,
  strictMaybe,
  strictMaybeToMaybe,
  txIxToInt,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  Constitution (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  VotingProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Conway.TxInfo (
  ConwayContextError (..),
  ConwayEraPlutusTxInfo (..),
  transColdCommitteeCred,
  transDRepCred,
  transDelegatee,
  transHotCommitteeCred,
  transMap,
  transSlotToPOSIXTime,
  transTxInInfoV1,
  transTxInInfoV3,
  transVote,
  transVoter,
 )
import qualified Cardano.Ledger.Conway.TxInfo as Conway
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraEraScript,
  PlutusScript (..),
 )
import Cardano.Ledger.Dijkstra.TxBody (receivingScriptTargets)
import Cardano.Ledger.Dijkstra.TxCert (DijkstraTxCert)
import Cardano.Ledger.Dijkstra.UTxO ()
import Cardano.Ledger.Mary.Value (MaryValue, filterMultiAsset)
import Cardano.Ledger.Plutus (
  Datum (..),
  Language (..),
  PlutusArgs (..),
  PlutusLanguage,
  SLanguage (..),
  TxOutSource (..),
  assocMapKeys,
  binaryDataToData,
  decodePlutusRunnable,
  getPlutusData,
  plutusLanguage,
  plutusSLanguage,
  transAccountAddress,
  transCoinToLovelace,
  transCoinToValue,
  transCred,
  transDataHash,
  transDatum,
  transEpochNo,
  transKeyHash,
  transScriptHash,
  transTxIx,
 )
import Cardano.Ledger.Plutus.Data (Data)
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.State (StakePoolParams (..), UTxO (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..), txIdToHex)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (forM, unless, zipWithM, zipWithM_)
import Data.Aeson (KeyValue (..), ToJSON (..))
import Data.Foldable (Foldable (..))
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.OMap.Strict as OMap
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
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
    PointerPresentInOutput TxOutSource
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
  | -- | A protected address cannot be represented in a PlutusV1-V3 context.
    ProtectedAddressNotSupported TxOutSource
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
    ProtectedAddressNotSupported source ->
      kindObjectValue "ProtectedAddressNotSupported" ["txOut" .= toJSON source]
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
    25 -> SumD ProtectedAddressNotSupported <! From
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
      ProtectedAddressNotSupported source ->
        Sum ProtectedAddressNotSupported 25 !> To source

instance Inject (ConwayContextError era) (DijkstraContextError era) where
  inject = ConwayContextError

instance Inject (Babbage.BabbageContextError era) (DijkstraContextError era) where
  inject = ConwayContextError . Conway.BabbageContextError

instance Inject (Alonzo.AlonzoContextError era) (DijkstraContextError era) where
  inject = ConwayContextError . Conway.BabbageContextError . Babbage.AlonzoContextError

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

  toPlutusScriptPurpose proxy lti = Alonzo.transPlutusPurpose proxy (ltiProtVer lti)

  toPlutusTxInfo proxy LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailUnsupportedScriptInSubTx $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      guardDijkstraFeaturesForPlutusV1toV3 tx
      guardLegacyProtectedAddresses PlutusV1 ltiUTxO tx
      timeRange <- Conway.transValidityInterval tx ltiEpochInfo ltiSystemStart (txBody ^. vldtTxBodyL)
      inputs <- mapM (Conway.transTxInInfoV1 ltiUTxO) (Set.toList (txBody ^. inputsTxBodyL))
      mapM_ (validateV1ReferenceInput ltiUTxO) (Set.toList (txBody ^. referenceInputsTxBodyL))
      outputs <-
        zipWithM
          (Conway.transTxOutV1 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      Right
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

  toPlutusArgs = Alonzo.toPlutusV1Args

  toPlutusTxInInfo _ = transTxInInfoV1

transTxCertV1V2 ::
  ( ConwayEraTxCert era
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
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
  txCert -> Left $ inject $ Alonzo.CertificateNotSupported txCert

instance EraPlutusTxInfo 'PlutusV2 DijkstraEra where
  toPlutusTxCert _ _ = transTxCertV1V2

  toPlutusScriptPurpose proxy lti = Alonzo.transPlutusPurpose proxy (ltiProtVer lti)

  toPlutusTxInfo proxy lti@LedgerTxInfo {ltiProtVer, ltiEpochInfo, ltiSystemStart, ltiUTxO, ltiTx} =
    flip (withBothTxLevels ltiTx) transFailUnsupportedScriptInSubTx $ \tx -> PlutusTxInfoResult $ do
      let txBody = tx ^. bodyTxL
      Conway.guardConwayFeaturesForPlutusV1V2 tx
      guardDijkstraFeaturesForPlutusV1toV3 tx
      guardLegacyProtectedAddresses PlutusV2 ltiUTxO tx
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
      Right
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
      guardLegacyProtectedAddresses PlutusV3 ltiUTxO tx
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
      Right
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

-- | Inspect only addresses visible to this legacy context. V1 hides reference inputs;
-- sibling bodies are not visible to any of these contexts.
guardLegacyProtectedAddresses ::
  forall era.
  ( EraTx era
  , BabbageEraTxBody era
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  Language ->
  UTxO era ->
  Tx TopTx era ->
  Either (ContextError era) ()
guardLegacyProtectedAddresses language utxo tx = do
  let body = tx ^. bodyTxL
      referenceInputs = case language of
        PlutusV1 -> mempty
        _ -> body ^. referenceInputsTxBodyL
      check source output =
        case shelleyAddressView (output ^. addrTxOutL) of
          Just (Protected, _, _, _) -> Left . inject $ ProtectedAddressNotSupported @era source
          _ -> Right ()
      -- Missing-input errors remain owned by the language's existing translation,
      -- preserving historical error order when protection is absent.
      checkInput input = case Map.lookup input (unUTxO utxo) of
        Nothing -> Right ()
        Just output -> check (TxOutFromInput input) output
  mapM_ checkInput . Set.toList $ (body ^. inputsTxBodyL) <> referenceInputs
  zipWithM_ check (map TxOutFromOutput [minBound ..]) (F.toList (body ^. outputsTxBodyL))

-- | Preserve V1's existing validation of hidden reference inputs without translating
-- their addresses. In particular, protection is not erased into a legacy context.
validateV1ReferenceInput ::
  forall era.
  ( BabbageEraTxOut era
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) ()
validateV1ReferenceInput utxo input = do
  output <- Alonzo.transLookupTxOut utxo input
  case output ^. dataTxOutL of
    SJust _ -> Left . inject $ Babbage.InlineDatumsNotSupported @era (TxOutFromInput input)
    SNothing -> pure ()
  case output ^. addrTxOutL of
    AddrBootstrap _ -> Left . inject $ Babbage.ByronTxOutInContext @era (TxOutFromInput input)
    _ -> pure ()

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

instance EraPlutusTxInfo 'PlutusV4 DijkstraEra where
  toPlutusTxCert _proxy _pv = pure . transTxCertV4

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
      outputs <-
        zipWithM
          (transTxOutV4 . TxOutFromOutput)
          [minBound ..]
          (F.toList (txBody ^. outputsTxBodyL))
      txCerts <- Alonzo.transTxBodyCerts proxy ltiProtVer txBody
      plutusRedeemers <- transTxRedeemersV4 lti
      Right
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
          , PV4.txInfoVotes = transVotingProcedures (txBody ^. votingProceduresTxBodyL)
          , PV4.txInfoProposalProcedures =
              map (transProposal proxy) $ toList (txBody ^. proposalProceduresTxBodyL)
          , PV4.txInfoCurrentTreasuryAmount =
              strictMaybe Nothing (Just . transCoinToLovelace) $ txBody ^. currentTreasuryValueTxBodyL
          , PV4.txInfoTreasuryDonation = transCoinToLovelace $ txBody ^. treasuryDonationTxBodyL
          , PV4.txInfoSubTxIx =
              case ltiLevelTxInfo of
                LedgerTopTxInfo {} -> Nothing
                LedgerSubTxInfo txIx -> Just $ transTxIx txIx
          , PV4.txInfoWithdrawals = transWithdrawals $ txBody ^. withdrawalsTxBodyL
          , PV4.txInfoDirectDeposits = transDirectDeposits $ txBody ^. directDepositsTxBodyL
          , PV4.txInfoAccountBalanceIntervals =
              transAccountBalanceIntervals $ txBody ^. accountBalanceIntervalsTxBodyL
          , PV4.txInfoGuards = transTxBodyGuards txBody
          , PV4.txInfoRequiredTopLevelGuards = transTxBodyRequiredTopLevelGuards txBody
          }

  toPlutusArgs = toPlutusV4Args

  toPlutusTxInInfo _ = transTxInInfoV4

-- | Translate V4 redeemers, sharing the complete body-local Receiving target
-- domain across all Receiving pointers. Native and unavailable script hashes
-- retain their original output positions even though they need not have a redeemer.
-- Other purposes retain the existing pointer translation and error ordering.
transTxRedeemersV4 ::
  ( DijkstraEraScript era
  , EraPlutusTxInfo PlutusV4 era
  , EraTx era
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  LedgerTxInfo level era ->
  Either (ContextError era) (PV4.Map PV4.ScriptPurpose PV4.Redeemer)
transTxRedeemersV4 lti@LedgerTxInfo {ltiTx} =
  PV4.unsafeFromList
    <$> mapM translate (Map.toList $ ltiTx ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
  where
    targets =
      Map.fromDistinctAscList (receivingScriptTargets (ltiTx ^. bodyTxL))
    translate pair@(ptr, (datum, _)) = case ptr of
      ReceivingPurpose (AsIx ix) -> case Map.lookup ix targets of
        Nothing -> Left $ inject $ Babbage.RedeemerPointerPointsToNothing ptr
        Just _ -> do
          purpose <- toPlutusScriptPurpose SPlutusV4 lti (ReceivingPurpose (AsIxItem ix ix))
          pure (purpose, Babbage.transRedeemer datum)
      _ -> Babbage.transRedeemerPointerV2V3 SPlutusV4 lti pair

transTxInV4 :: TxIn -> PV4.TxOutRef
transTxInV4 (TxIn txid txIx) = PV4.TxOutRef (Conway.transTxId txid) (toInteger (txIxToInt txIx))

transTxInInfoV4 ::
  forall era.
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  UTxO era ->
  TxIn ->
  Either (ContextError era) PV4.TxInInfo
transTxInInfoV4 utxo txIn = do
  txOut <- Alonzo.transLookupTxOut utxo txIn
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
    val = Alonzo.transValue $ txOut ^. valueTxOutL
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
    case shelleyAddressView (txOut ^. addrTxOutL) of
      Just (protection, _, pCred, stakeRef) ->
        let addressConstructor = case protection of
              Unprotected -> PV4.Address
              Protected -> PV4.AddressProtected
         in addressConstructor (transCred pCred) <$> case stakeRef of
              StakeRefBase sCred -> Right . Just $ transCredToAccountId sCred
              StakeRefNull -> Right Nothing
              StakeRefPtr _ -> Left . inject $ PointerPresentInOutput @era txOutSource
      Nothing -> Left . inject $ Babbage.ByronTxOutInContext @era txOutSource
  pure $
    PV4.TxOut
      { txOutReferenceScript = referenceScript
      , txOutDatum = datum
      , txOutValue = val
      , txOutAddress = addr
      }

transWithdrawals :: Withdrawals -> PV4.Map PV4.Credential PV4.Lovelace
transWithdrawals (Withdrawals withdrawals) = transMap transAccountAddressToCredential transCoinToLovelace withdrawals

transDirectDeposits :: DirectDeposits -> PV4.Map PV4.Credential PV4.Lovelace
transDirectDeposits (DirectDeposits deposits) = transMap transAccountAddressToCredential transCoinToLovelace deposits

transCredToAccountId :: Credential r -> PV4.AccountId
transCredToAccountId = PV4.AccountId . transCred

transTxCertV4 :: ConwayEraTxCert era => TxCert era -> PV4.TxCert
transTxCertV4 = \case
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

-- | Translate a validity interval to PV4.POSIXTimeRange
transValidityInterval ::
  Inject (Alonzo.AlonzoContextError era) (ContextError era) =>
  proxy era ->
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
  forall proxy (l :: Language) level era.
  ( EraTx era
  , AlonzoEraTxWits era
  , DijkstraEraScript era
  , DijkstraEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo l era
  , PlutusTxInfo l ~ PV4.TxInfo
  , STxLevel level era ~ STxBothLevels level era
  , Inject (DijkstraContextError era) (ContextError era)
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  proxy l ->
  Maybe PV4.Datum ->
  LedgerTxInfo level era ->
  PV4.TxInfo ->
  PlutusPurpose AsIx era ->
  PV4.ScriptPurpose ->
  Either (ContextError era) PV4.ScriptInfo
scriptPurposeToScriptInfo proxy datum lti txInfo ixPlutusPurpose = \case
  PV4.Spending _ ref -> pure (PV4.SpendingScript ref datum)
  PV4.Minting _ currencySymbol -> pure (PV4.MintingScript currencySymbol)
  PV4.Withdrawing _ credential -> pure (PV4.WithdrawingScript $ PV4.AccountId credential)
  PV4.Certifying _ ix cert -> pure (PV4.CertifyingScript ix cert)
  PV4.Voting _ vote -> pure (PV4.VotingScript vote)
  PV4.Proposing _ ix proposal -> pure (PV4.ProposingScript ix proposal)
  PV4.Receiving _ outputIndex -> case ixPlutusPurpose of
    ReceivingPurpose (AsIx ix)
      | outputIndex == toInteger ix ->
          case StrictSeq.lookup (fromIntegral ix) (ltiTx lti ^. bodyTxL . outputsTxBodyL) of
            Nothing -> Left $ inject $ ScriptHashNotFoundForPurpose ixPlutusPurpose
            Just txOut ->
              PV4.ReceivingScript outputIndex
                <$> transTxOutV4 (TxOutFromOutput $ TxIx $ fromIntegral ix) txOut
    _ -> Left $ inject $ ScriptHashNotFoundForPurpose ixPlutusPurpose
  PV4.Guarding _ ix -> do
    guardingScriptHash <- case Map.lookup ixPlutusPurpose (ltiScriptHashesUsed lti) of
      Nothing -> Left $ inject $ ScriptHashNotFoundForPurpose ixPlutusPurpose
      Just scriptHash -> Right scriptHash
    topTxInfo <-
      withBothTxLevels
        lti
        (fmap Just . transGuardingTopTxInfo proxy txInfo guardingScriptHash)
        (\_ -> pure Nothing)
    pure (PV4.GuardingScript ix topTxInfo)

scriptHashFromScriptPurpose :: PV4.ScriptPurpose -> PV2.ScriptHash
scriptHashFromScriptPurpose = \case
  PV4.Spending sh _ -> sh
  PV4.Minting sh _ -> sh
  PV4.Withdrawing sh _ -> sh
  PV4.Certifying sh _ _ -> sh
  PV4.Voting sh _ -> sh
  PV4.Proposing sh _ _ -> sh
  PV4.Guarding sh _ -> sh
  PV4.Receiving sh _ -> sh

transGuardingTopTxInfo ::
  forall proxy (l :: Language) era.
  ( EraTx era
  , AlonzoEraTxWits era
  , DijkstraEraTxBody era
  , EraPlutusTxInfo l era
  , PlutusTxInfo l ~ PV4.TxInfo
  , Inject (DijkstraContextError era) (ContextError era)
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  ) =>
  proxy l ->
  PV4.TxInfo ->
  ScriptHash ->
  LedgerTxInfo TopTx era ->
  Either (ContextError era) PV4.TopTxInfo
transGuardingTopTxInfo proxy txInfo guardingScriptHash lti@(LedgerTxInfo {ltiTx, ltiLevelTxInfo = LedgerTopTxInfo subTxInfoResults}) = do
  let
    lookupRequiredTopLevelGuardDatum :: Tx level era -> Maybe (TxId, Data era)
    lookupRequiredTopLevelGuardDatum tx = do
      guardDatumMaybe <-
        Map.lookup (ScriptHashObj guardingScriptHash) (tx ^. bodyTxL . requiredTopLevelGuardsL)
      -- Datum is enforced to be present by `MalformedGuardDatums`, hence we can ignore
      -- here the case of it missing
      guardDatum <- strictMaybeToMaybe guardDatumMaybe
      pure (txIdTx tx, guardDatum)
  subTransactionsWithDatums <-
    forM (OMap.elems (ltiTx ^. bodyTxL . subTransactionsTxBodyL)) $ \subTx -> do
      let txId = txIdTx subTx
      subTxInfo <-
        left (inject . SubTxContextError txId) $
          case Map.lookup txId subTxInfoResults of
            Nothing ->
              -- In `Cardano.Ledger.Dijkstra.mkDijkstraStAnnSubTx` we ensure `ltiLevelTxInfo` is
              -- correctly populated for each sub-transaction
              Left $ inject $ Alonzo.ImpossibleContextError @era $ "Missing TxInfoResult for " <> txIdToHex txId
            Just txInfoResults ->
              unPlutusTxInfoResult $ lookupTxInfoResult (plutusSLanguage proxy) txInfoResults
      pure (subTxInfo, lookupRequiredTopLevelGuardDatum subTx)

  let
    subTransactions = map fst subTransactionsWithDatums

    batchGuardDatums =
      Map.fromList (mapMaybe snd subTransactionsWithDatums)
        <> maybe mempty (uncurry Map.singleton) (lookupRequiredTopLevelGuardDatum ltiTx)

    startingAccountBalanceIntervals =
      transAccountBalanceIntervals $ ltiTx ^. bodyTxL . startingAccountBalanceIntervalsTxBodyL

    foldMapBatch :: Monoid m => (forall level. Tx level era -> m) -> m
    foldMapBatch f = foldMap f subTxs <> f ltiTx

    -- We can reuse some of the translated fields, as long as the order or number of elements is
    -- guaranteed to be the same is the same operation was done on the ledger side
    batchTxInfo = subTransactions ++ [txInfo]
    subTxs = ltiTx ^. bodyTxL . subTransactionsTxBodyL
    batchWithdrawals = foldMapBatch (^. bodyTxL . withdrawalsTxBodyL)
    batchDirectDeposits = foldMapBatch (^. bodyTxL . directDepositsTxBodyL)
    batchValidityIntervals = foldMapBatch (^. bodyTxL . vldtTxBodyL)
    batchVotes = foldMapBatch (^. bodyTxL . votingProceduresTxBodyL)
    batchDatums = foldMapBatch (^. witsTxL . datsTxWitsL)
    filterBatchMintsWith f = foldMapBatch (filterMultiAsset (\_ _ -> f) . (^. bodyTxL . mintTxBodyL))
    batchMints = filterBatchMintsWith (> 0)
    batchBurns = filterBatchMintsWith (< 0)
    batchRequiredTopLevelGuards = foldMapBatch (Map.keysSet . (^. bodyTxL . requiredTopLevelGuardsL))
    batchTreasuryDonations = foldMapBatch (^. bodyTxL . treasuryDonationTxBodyL)

  batchTimeRange <-
    transValidityInterval ltiTx (ltiEpochInfo lti) (ltiSystemStart lti) batchValidityIntervals

  let
    topTxInfoSimplified =
      PV4.TopTxInfoSimplified
        { ttisIds = map PV4.txInfoId batchTxInfo
        , ttisInputs = foldMap PV4.txInfoInputs batchTxInfo
        , ttisReferenceInputs = foldMap PV4.txInfoReferenceInputs batchTxInfo
        , ttisOutputs = foldMap PV4.txInfoOutputs batchTxInfo
        , ttisMints = Conway.transMintValue batchMints
        , ttisBurns = Conway.transMintValue batchBurns
        , ttisTxCerts = foldMap PV4.txInfoTxCerts batchTxInfo
        , ttisWithdrawals = transWithdrawals batchWithdrawals
        , ttisDirectDeposits = transDirectDeposits batchDirectDeposits
        , ttisValidRange = batchTimeRange
        , ttisGuards = foldMap PV4.txInfoGuards batchTxInfo
        , ttisRequiredTopLevelGuards =
            transCred <$> Set.toList batchRequiredTopLevelGuards
        , ttisRedeemerHashes =
            -- Plutus `ScriptHash` ordering is the same as the one in Ledger, so we can just extract
            -- already translated `ScriptHash`es
            Set.toList $
              Set.fromList $
                foldMap (map scriptHashFromScriptPurpose . assocMapKeys . PV4.txInfoRedeemers) batchTxInfo
        , ttisData = PV4.unsafeFromList $ Alonzo.transDatums batchDatums
        , ttisVotes = transVotingProcedures batchVotes
        , ttisProposalProcedures = foldMap PV4.txInfoProposalProcedures batchTxInfo
        , -- For all treasury amounts, if present, they are guaranteed by the ledger rules to
          -- have the same value, so we can just pick the first one, if available.
          ttisCurrentTreasuryAmount = F.asum $ map PV4.txInfoCurrentTreasuryAmount batchTxInfo
        , ttisTreasuryDonations = transCoinToLovelace batchTreasuryDonations
        }
  pure $
    PV4.TopTxInfo
      { topTxInfoSubTransactions = subTransactions
      , topTxInfoDatums = transMap Conway.transTxId transDatum batchGuardDatums
      , topTxInfoStartingAccountBalanceIntervals = startingAccountBalanceIntervals
      , topTxInfoSimplified = topTxInfoSimplified
      }

toPlutusV4Args ::
  ( AlonzoEraUTxO era
  , AlonzoEraTxWits era
  , DijkstraEraScript era
  , DijkstraEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo PlutusV4 era
  , STxLevel level era ~ STxBothLevels level era
  , Inject (DijkstraContextError era) (ContextError era)
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (Babbage.BabbageContextError era) (ContextError era)
  ) =>
  proxy 'PlutusV4 ->
  LedgerTxInfo level era ->
  PV4.TxInfo ->
  PlutusPurpose AsIxItem era ->
  Data era ->
  Either (ContextError era) (PlutusArgs 'PlutusV4)
toPlutusV4Args proxy lti@LedgerTxInfo {..} txInfo plutusPurpose redeemerData = do
  scriptPurpose <- toPlutusScriptPurpose proxy lti plutusPurpose
  let
    spendDatum = transDatum <$> getSpendingDatum ltiUTxO ltiTx (hoistPlutusPurpose toAsItem plutusPurpose)
    ixPurpose = hoistPlutusPurpose toAsIx plutusPurpose
  scriptInfo <-
    scriptPurposeToScriptInfo proxy spendDatum lti txInfo ixPurpose scriptPurpose
  pure $
    PlutusV4Args $
      PV4.ScriptContext
        { PV4.scriptContextTxInfo = txInfo
        , PV4.scriptContextRedeemer = Babbage.transRedeemer redeemerData
        , PV4.scriptContextScriptInfo = scriptInfo
        , PV4.scriptContextScriptHash = scriptHashFromScriptPurpose scriptPurpose
        }

transPlutusPurposeV4 ::
  forall era proxy level.
  ( DijkstraEraScript era
  , ConwayEraPlutusTxInfo PlutusV4 era
  , Inject (Alonzo.AlonzoContextError era) (ContextError era)
  , Inject (DijkstraContextError era) (ContextError era)
  ) =>
  proxy 'PlutusV4 ->
  LedgerTxInfo level era ->
  PlutusPurpose AsIxItem era ->
  Either (ContextError era) (PlutusScriptPurpose PlutusV4)
transPlutusPurposeV4 proxy lti plutusPurpose = do
  let
    pv = ltiProtVer lti
    ixPurpose = hoistPlutusPurpose toAsIx plutusPurpose
  sh <-
    case Map.lookup ixPurpose (ltiScriptHashesUsed lti) of
      Nothing -> Left $ inject $ ScriptHashNotFoundForPurpose @era ixPurpose
      Just scriptHash -> Right $ transScriptHash scriptHash
  case plutusPurpose of
    SpendingPurpose (AsIxItem _ (TxIn txId (TxIx ix))) ->
      pure . PV4.Spending sh $ PV4.TxOutRef (Conway.transTxId txId) (toInteger ix)
    MintingPurpose (AsIxItem _ pId) -> pure . PV4.Minting sh $ Alonzo.transPolicyID pId
    CertifyingPurpose (AsIxItem ix cert) ->
      PV4.Certifying sh (toInteger ix) <$> toPlutusTxCert proxy pv cert
    WithdrawingPurpose (AsIxItem _ (AccountAddress _ (AccountId c))) ->
      pure $ PV4.Withdrawing sh (transCred c)
    VotingPurpose (AsIxItem _ voter) -> pure $ PV4.Voting sh (transVoter voter)
    ProposingPurpose (AsIxItem ix proc) ->
      pure $ PV4.Proposing sh (toInteger ix) (transProposal proxy proc)
    GuardingPurpose (AsIxItem ix _) -> pure $ PV4.Guarding sh (toInteger ix)
    ReceivingPurpose (AsIxItem _ outputIx) -> pure $ PV4.Receiving sh (toInteger outputIx)
    _ ->
      Left $ inject $ Alonzo.PlutusPurposeNotSupported @era $ hoistPlutusPurpose toAsItem plutusPurpose

transVotingProcedures ::
  VotingProcedures era -> PV4.Map PV4.Voter (PV4.Map PV4.GovernanceActionId PV4.Vote)
transVotingProcedures =
  transMap transVoter (transMap transGovActionId (transVote . vProcVote)) . unVotingProcedures

transProposal ::
  ConwayEraPlutusTxInfo l era =>
  proxy l ->
  ProposalProcedure era ->
  PV4.ProposalProcedure
transProposal proxy ProposalProcedure {pProcDeposit, pProcReturnAddr, pProcGovAction} =
  PV4.ProposalProcedure
    { PV4.ppDeposit = transCoinToLovelace pProcDeposit
    , PV4.ppReturnAddr = transAccountAddress pProcReturnAddr
    , PV4.ppGovernanceAction = transGovAction proxy pProcGovAction
    }

transGovActionId :: GovActionId -> PV4.GovernanceActionId
transGovActionId GovActionId {gaidTxId, gaidGovActionIx} =
  PV4.GovernanceActionId
    { PV4.gaidTxId = Conway.transTxId gaidTxId
    , PV4.gaidGovActionIx = toInteger $ unGovActionIx gaidGovActionIx
    }

transGovAction :: ConwayEraPlutusTxInfo l era => proxy l -> GovAction era -> PV4.GovernanceAction
transGovAction proxy = \case
  ParameterChange pGovActionId ppu govPolicy ->
    PV4.ParameterChange
      (transPrevGovActionId pGovActionId)
      (toPlutusChangedParameters proxy ppu)
      (transGovPolicy govPolicy)
  HardForkInitiation pGovActionId protVer ->
    PV4.HardForkInitiation
      (transPrevGovActionId pGovActionId)
      (transProtVer protVer)
  TreasuryWithdrawals withdrawals govPolicy ->
    PV4.TreasuryWithdrawals
      (transMap transAccountAddress transCoinToLovelace withdrawals)
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
      SJust (GovPurposeId gaId) -> Just (transGovActionId gaId)
      SNothing -> Nothing

transProtVer :: ProtVer -> PV4.ProtocolVersion
transProtVer (ProtVer major minor) =
  PV4.ProtocolVersion (toInteger (getVersion32 major)) (toInteger minor)

transBoundedRational :: BoundedRational r => r -> PV4.Rational
transBoundedRational = PV4.fromHaskellRatio . unboundRational
