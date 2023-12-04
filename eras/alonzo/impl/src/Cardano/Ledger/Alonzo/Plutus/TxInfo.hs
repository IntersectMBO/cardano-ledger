{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Plutus.TxInfo (
  ContextError (..),
  TxOutSource (..),
  transDataHash,
  transKeyHash,
  transSafeHash,
  txInfoId,
  transStakeReference,
  transCred,
  transAddr,
  transTxOutAddr,
  slotToPOSIXTime,
  transVITime,
  txInfoIn',
  txInfoIn,
  txInfoOut,
  transPolicyID,
  transAssetName,
  transMultiAsset,
  transMintValue,
  transValue,
  transWithdrawals,
  transDataPair,
  transExUnits,
  transTxCert,
  transScriptPurpose,
)
where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Scripts (PlutusScript (..))
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..))
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  AlonzoEraTxOut (..),
  mintTxBodyL,
  vldtTxBodyL,
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..), unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Core as Core hiding (TranslationError)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..), zero)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Slot (EpochNo (..))
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Data.ByteString.Short as SBS (fromShort)
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (AlonzoEra c) where
  toPlutusTxCert _ = pure . transTxCert

  toPlutusScriptPurpose = transScriptPurpose

  toPlutusTxInfo proxy pp ei sysS utxo tx = do
    timeRange <- left TimeTranslationPastHorizon $ transVITime pp ei sysS interval
    -- We need to do this as a separate step
    let lookupTxOut txIn =
          case Map.lookup txIn (unUTxO utxo) of
            Nothing -> Left $ TranslationLogicMissingInput txIn
            Just txOut -> Right (txIn, txOut)
    txIns <- mapM lookupTxOut (Set.toList (txBody ^. inputsTxBodyL))
    txCerts <- mapM (toPlutusTxCert proxy) $ toList (txBody ^. certsTxBodyL)
    Right $
      PV1.TxInfo
        { PV1.txInfoInputs = mapMaybe (uncurry txInfoIn) txIns
        , PV1.txInfoOutputs = mapMaybe txInfoOut (foldr (:) [] txOuts)
        , PV1.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = transMintValue @c (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txCerts
        , PV1.txInfoWdrl = Map.toList (transWithdrawals (txBody ^. withdrawalsTxBodyL))
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories = map transKeyHash (Set.toList (txBody ^. reqSignerHashesTxBodyL))
        , PV1.txInfoData = map transDataPair dataPairs
        , PV1.txInfoId = PV1.TxId (transSafeHash (hashAnnotated txBody))
        }
    where
      txBody = tx ^. bodyTxL
      txOuts = txBody ^. outputsTxBodyL
      interval = txBody ^. vldtTxBodyL
      dataPairs = Map.toList (unTxDats $ tx ^. witsTxL . datsTxWitsL)

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusContext (AlonzoEra c) where
  data ContextError (AlonzoEra c)
    = TranslationLogicMissingInput !(TxIn c)
    | TimeTranslationPastHorizon !Text
    deriving (Eq, Show, Generic)

  mkPlutusScriptContext (AlonzoPlutusV1 p) =
    mkPlutusLanguageContext p

instance NoThunks (ContextError (AlonzoEra c))

instance Crypto c => NFData (ContextError (AlonzoEra c))

instance Crypto c => EncCBOR (ContextError (AlonzoEra c)) where
  encCBOR = \case
    TranslationLogicMissingInput txIn ->
      encode $ Sum TranslationLogicMissingInput 1 !> To txIn
    TimeTranslationPastHorizon err ->
      encode $ Sum TimeTranslationPastHorizon 7 !> To err

instance Crypto c => DecCBOR (ContextError (AlonzoEra c)) where
  decCBOR = decode $ Summands "ContextError" $ \case
    1 -> SumD TranslationLogicMissingInput <! From
    7 -> SumD TimeTranslationPastHorizon <! From
    n -> Invalid n

-- | translate a validity interval to POSIX time
transVITime ::
  EraPParams era =>
  PParams era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  ValidityInterval ->
  Either Text PV1.POSIXTimeRange
transVITime _ _ _ (ValidityInterval SNothing SNothing) = pure PV1.always
transVITime _ ei sysS (ValidityInterval (SJust i) SNothing) = do
  t <- slotToPOSIXTime ei sysS i
  pure $ PV1.from t
transVITime pp ei sysS (ValidityInterval SNothing (SJust i)) = do
  t <- slotToPOSIXTime ei sysS i
  pure $
    if HardForks.translateUpperBoundForPlutusScripts (pp ^. ppProtocolVersionL)
      then
        PV1.Interval
          (PV1.LowerBound PV1.NegInf True)
          (PV1.strictUpperBound t)
      else PV1.to t
transVITime _ ei sysS (ValidityInterval (SJust i) (SJust j)) = do
  t1 <- slotToPOSIXTime ei sysS i
  t2 <- slotToPOSIXTime ei sysS j
  pure $
    PV1.Interval
      (PV1.lowerBound t1)
      (PV1.strictUpperBound t2)

-- ========================================
-- translate TxIn and TxOut

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it and return
--   (Just translation). If does not exist in the UTxO, return Nothing.
txInfoIn ::
  (AlonzoEraTxOut era, Value era ~ MaryValue (EraCrypto era)) =>
  TxIn (EraCrypto era) ->
  TxOut era ->
  Maybe PV1.TxInInfo
txInfoIn txIn txOut = do
  let val = transValue (txOut ^. valueTxOutL)
      dataHash = case txOut ^. dataHashTxOutL of
        SNothing -> Nothing
        SJust safeHash -> Just (PV1.DatumHash (transSafeHash safeHash))
  addr <- transTxOutAddr txOut
  pure $ PV1.TxInInfo (txInfoIn' txIn) (PV1.TxOut addr val dataHash)

-- | Given a TxOut, translate it and return (Just transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Nothing
--   I.e. don't include Bootstrap Addresses in the answer.
txInfoOut ::
  (AlonzoEraTxOut era, Value era ~ MaryValue (EraCrypto era)) =>
  TxOut era ->
  Maybe PV1.TxOut
txInfoOut txOut = do
  let val = txOut ^. valueTxOutL
      dataHash = txOut ^. dataHashTxOutL
  addr <- transTxOutAddr txOut
  pure (PV1.TxOut addr (transValue val) (transDataHash <$> strictMaybeToMaybe dataHash))

-- ==================================
-- translate Values

transPolicyID :: PolicyID c -> PV1.CurrencySymbol
transPolicyID (PolicyID (ScriptHash x)) = PV1.CurrencySymbol (PV1.toBuiltin (hashToBytes x))

transAssetName :: AssetName -> PV1.TokenName
transAssetName (AssetName bs) = PV1.TokenName (PV1.toBuiltin (SBS.fromShort bs))

transMultiAsset :: MultiAsset c -> PV1.Value
transMultiAsset ma = transMultiAssetInternal ma mempty

transMultiAssetInternal :: MultiAsset c -> PV1.Value -> PV1.Value
transMultiAssetInternal (MultiAsset m) initAcc = Map.foldlWithKey' accum1 initAcc m
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          PV1.unionWith
            (+)
            ans2
            (PV1.singleton (transPolicyID sym) (transAssetName tok) quantity)

-- | Hysterical raisins:
--
-- Previously transaction body contained a mint field with MaryValue instead of a
-- MultiAsset, which has changed since then to just MultiAsset (because minting ADA
-- makes no sense). However, if we don't preserve previous translation, scripts that
-- previously succeeded will fail.
transMintValue :: MultiAsset c -> PV1.Value
transMintValue m = transMultiAssetInternal m (transCoin zero)

transValue :: MaryValue c -> PV1.Value
transValue (MaryValue c m) = transCoin c <> transMultiAsset m

-- =============================================
-- translate fields like TxCert, Withdrawals, and similar

transTxCert :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> PV1.DCert
transTxCert = \case
  RegTxCert stakeCred ->
    PV1.DCertDelegRegKey (PV1.StakingHash (transCred stakeCred))
  UnRegTxCert stakeCred ->
    PV1.DCertDelegDeRegKey (PV1.StakingHash (transCred stakeCred))
  DelegStakeTxCert stakeCred keyHash ->
    PV1.DCertDelegDelegate (PV1.StakingHash (transCred stakeCred)) (transKeyHash keyHash)
  RegPoolTxCert (PoolParams {ppId, ppVrf}) ->
    PV1.DCertPoolRegister (transKeyHash ppId) (PV1.PubKeyHash (PV1.toBuiltin (hashToBytes ppVrf)))
  RetirePoolTxCert poolId (EpochNo i) ->
    PV1.DCertPoolRetire (transKeyHash poolId) (toInteger i)
  GenesisDelegTxCert {} -> PV1.DCertGenesis
  MirTxCert {} -> PV1.DCertMir

transScriptPurpose ::
  (EraPlutusTxInfo l era, PlutusTxCert l ~ PV1.DCert) =>
  proxy l ->
  ScriptPurpose era ->
  Either (ContextError era) PV1.ScriptPurpose
transScriptPurpose proxy = \case
  Minting policyId -> pure $ PV1.Minting (transPolicyID policyId)
  Spending txIn -> pure $ PV1.Spending (txInfoIn' txIn)
  Rewarding (RewardAcnt _networkId cred) ->
    pure $ PV1.Rewarding (PV1.StakingHash (transCred cred))
  Certifying txCert -> PV1.Certifying <$> toPlutusTxCert proxy txCert
