{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxInfo (conwayTxInfo) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxInfo (
  EraPlutusContext,
  TranslationError (..),
  VersionedTxInfo (..),
  unTxCertV3,
 )
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), RdmrPtr, unRedeemers, unTxDats)
import Cardano.Ledger.Babbage.TxInfo (babbageTxInfoV1, babbageTxInfoV2)
import qualified Cardano.Ledger.Babbage.TxInfo as B
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core hiding (TranslationError)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.TxCert ()
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.TxCert
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

instance Crypto c => EraPlutusContext 'PlutusV1 (ConwayEra c) where
  -- FIXME: implement for conway era
  transTxCert = Alonzo.TxCertPlutusV1 . error "Unimplemented"

instance Crypto c => EraPlutusContext 'PlutusV2 (ConwayEra c) where
  -- FIXME: implement for conway era
  transTxCert = Alonzo.TxCertPlutusV2 . error "Unimplemented"

instance Crypto c => EraPlutusContext 'PlutusV3 (ConwayEra c) where
  transTxCert = Alonzo.TxCertPlutusV3 . conwayTransTxCert

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
    interval = tx ^. bodyTxL . vldtTxBodyL

-- | Changes from PlutusV2 TxInfo:
--
-- * `txInfoFee` no longer gets a zero ADA inserted into the field, since minting ADA is
--   not possible.
--
-- * `txInfoDCert` is renamed to `txInfoTxCerts`. Certificates are no longer just
--   about delegation, so @D@ prefix no longer make sense.
conwayTxInfoV3 ::
  forall era.
  ( EraTx era
  , AlonzoEraTxWits era
  , BabbageEraTxBody era
  , Value era ~ MaryValue (EraCrypto era)
  , EraPlutusContext 'PlutusV3 era
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
  rdmrs' <- mapM (transRedeemerPtr txBody) rdmrs
  pure . TxInfoPV3 $
    PV3.TxInfo -- TODO Add relevant CIP-1694 data to PV3.TxInfo
      { PV3.txInfoInputs = inputs
      , PV3.txInfoOutputs = outputs
      , PV3.txInfoReferenceInputs = refInputs
      , PV3.txInfoFee = Alonzo.transValue (inject @(MaryValue (EraCrypto era)) fee)
      , -- Note that this translation is different from previous Plutus versions, since we no
        -- longer add a zero ADA value to the mint field during translation:
        PV3.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
      , PV3.txInfoTxCerts = toList $ fmap (unTxCertV3 . Alonzo.transTxCert) (txBody ^. certsTxBodyL)
      , PV3.txInfoWdrl = PV3.fromList $ Map.toList (transWithdrawals (txBody ^. withdrawalsTxBodyL))
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
    witnesses = tx ^. witsTxL
    outs = txBody ^. outputsTxBodyL
    fee = txBody ^. feeTxBodyL
    datpairs = Map.toList (unTxDats $ witnesses ^. datsTxWitsL)
    rdmrs = Map.toList (unRedeemers $ witnesses ^. rdmrsTxWitsL)

-- | This is a temporary version that only translates certificates from previous eras,
-- none of them are Conway specific.
conwayTransTxCert :: ShelleyEraTxCert era => TxCert era -> PV3.TxCert
conwayTransTxCert = \case
  RegTxCert stakeCred ->
    PV3.TxCertRegStaking (Alonzo.transCred stakeCred) (error "unimplemented")
  UnRegTxCert stakeCred ->
    PV3.TxCertUnRegStaking (Alonzo.transCred stakeCred) (error "unimplemented")
  DelegStakeTxCert stakeCred _keyHash ->
    PV3.TxCertDelegStaking
      (Alonzo.transCred stakeCred)
      (error "unimplemented")
  _ -> error "Translation of Conway Certificate is not implemented yet"

-- adapted from Alonzo.TxInfo so as to drop the 'StakingCredential' constructor.
transWithdrawals :: Withdrawals c -> Map.Map PV3.Credential Integer
transWithdrawals (Withdrawals mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) =
      Map.insert (Alonzo.transCred cred) n ans

transRedeemerPtr ::
  ( MaryEraTxBody era
  , EraPlutusContext 'PlutusV3 era
  ) =>
  TxBody era ->
  (RdmrPtr, (Data era, ExUnits)) ->
  Either (TranslationError (EraCrypto era)) (PV3.ScriptPurpose, PV3.Redeemer)
transRedeemerPtr txb (ptr, (d, _)) =
  case rdptrInv txb ptr of
    SNothing -> Left (RdmrPtrPointsToNothing ptr)
    SJust sp -> Right (transScriptPurpose sp, B.transRedeemer d)

transScriptPurpose ::
  EraPlutusContext 'PlutusV3 era =>
  ScriptPurpose era ->
  PV3.ScriptPurpose
transScriptPurpose (Minting policyid) = PV3.Minting (Alonzo.transPolicyID policyid)
transScriptPurpose (Spending txin) = PV3.Spending (Alonzo.txInfoIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) =
  PV3.Rewarding (Alonzo.transCred cred)
transScriptPurpose (Certifying dcert) = PV3.Certifying $ unTxCertV3 $ Alonzo.transTxCert dcert

-- FIXME: Add support for PV3, add voting , proposing
