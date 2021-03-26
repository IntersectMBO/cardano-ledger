{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.TxInfo where

-- =============================================
-- Types used in the Alonzo Era, needed to make TxInfo

import Cardano.Crypto.Hash.Class (Hash (UnsafeHash))
import Cardano.Ledger.Alonzo.Data (Data (..), getPlutusData)
-- ==============================================
-- Import Plutus stuff in the qualified Module P

import qualified Cardano.Ledger.Alonzo.FakePlutus as P
  ( Address (..),
    Context (..),
    Credential (..),
    DCert (..),
    ScriptPurpose (..),
    StakingCredential (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
  )
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( certs',
    inputs',
    inputs_fee',
    mint',
    outputs',
    reqSignerHashes',
    txfee',
    vldt',
    wdrls',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Core as Core (TxBody, TxOut, Value)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.SafeHash
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Short as SBS (fromShort, toShort)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import qualified Flat as Flat (flat)
import GHC.Records (HasField (..))
import qualified Language.PlutusCore.Evaluation.Machine.ExMemory as P (ExCPU (..), ExMemory (..))
import qualified Language.PlutusTx as P (Data (..))
import qualified Language.PlutusTx.IsData.Class as P (IsData (..))
import qualified Plutus.V1.Ledger.Ada as P (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Api as P (CostModelParameters, ExBudget (..), VerboseMode (..), evaluateScriptRestricting)
import qualified Plutus.V1.Ledger.Crypto as P (PubKeyHash (..))
import qualified Plutus.V1.Ledger.Interval as P
  ( Extended (..),
    Interval (..),
    LowerBound (..),
    UpperBound (..),
  )
import qualified Plutus.V1.Ledger.Scripts as P (Datum (..), DatumHash (..), Script (..), ValidatorHash (..))
import qualified Plutus.V1.Ledger.Slot as P (SlotRange)
import qualified Plutus.V1.Ledger.Tx as P (TxOutRef (..))
import qualified Plutus.V1.Ledger.TxId as P (TxId (..))
import qualified Plutus.V1.Ledger.Value as P (CurrencySymbol (..), TokenName (..), Value (..), singleton, unionWith)
import Shelley.Spec.Ledger.Address (Addr (..), RewardAcnt (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), Ptr (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys (KeyHash (..), hashKey)
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    PoolCert (..),
    PoolParams (..),
    TxId (..),
    TxIn (..),
    Wdrl (..),
    WitVKey (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- =========================================================
-- Translate Hashes, Credentials, Certificates etc.

transDataHash :: StrictMaybe (DataHash c) -> Maybe P.DatumHash
transDataHash (SJust safe) = Just (transDataHash' safe)
transDataHash SNothing = Nothing

transDataHash' :: (DataHash c) -> P.DatumHash
transDataHash' safe = (P.DatumHash (transSafeHash safe))

transKeyHash :: KeyHash d c -> P.PubKeyHash
transKeyHash (KeyHash (UnsafeHash h)) = P.PubKeyHash (fromShort h)

transScriptHash :: ScriptHash i -> P.ValidatorHash
transScriptHash (ScriptHash (UnsafeHash h)) = P.ValidatorHash (fromShort h)

transSafeHash :: SafeHash c i -> BS.ByteString
transSafeHash safe = case extractHash safe of UnsafeHash b -> fromShort b

transHash :: Hash h a -> BS.ByteString
transHash (UnsafeHash h) = fromShort h

transTxId :: TxId era -> P.TxId
transTxId (TxId safe) = P.TxId (transSafeHash safe)

transStakeCred :: Credential keyrole crypto -> BS.ByteString
transStakeCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) = (fromShort kh)
transStakeCred (KeyHashObj (KeyHash (UnsafeHash kh))) = (fromShort kh)

transStakeReference :: StakeReference crypto -> Maybe P.StakingCredential
transStakeReference (StakeRefBase cred) = Just (P.StakingHash (transStakeCred cred))
transStakeReference (StakeRefPtr (Ptr (SlotNo slot) i1 i2)) = Just (P.StakingPtr slot i1 i2)
transStakeReference StakeRefNull = Nothing

transCred :: Credential keyrole crypto -> P.Credential
transCred (KeyHashObj (KeyHash (UnsafeHash kh))) = P.PubKeyCredential (P.PubKeyHash (fromShort kh))
transCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) = P.ScriptCredential (P.ValidatorHash (fromShort kh))

transAddr :: Addr crypto -> Maybe P.Address
transAddr (Addr _net object stake) = Just (P.Address (transCred object) (transStakeReference stake))
transAddr (AddrBootstrap _bootaddr) = Nothing

-- ===============================
-- Translate ValidityIntervals

transVI :: ValidityInterval -> P.SlotRange
transVI (ValidityInterval SNothing SNothing) =
  P.Interval
    (P.LowerBound P.NegInf True)
    (P.UpperBound P.PosInf False)
transVI (ValidityInterval (SJust (SlotNo i)) SNothing) =
  P.Interval
    (P.LowerBound (P.Finite (fromIntegral i)) True)
    (P.UpperBound P.PosInf False)
transVI (ValidityInterval SNothing (SJust (SlotNo i))) =
  P.Interval
    (P.LowerBound P.NegInf True)
    (P.UpperBound (P.Finite (fromIntegral i)) False)
transVI (ValidityInterval (SJust (SlotNo i)) (SJust (SlotNo j))) =
  P.Interval
    (P.LowerBound (P.Finite (fromIntegral i)) True)
    (P.UpperBound (P.Finite (fromIntegral j)) False)

-- ========================================
-- translate TxIn and TxOut

transTxIn' :: CC.Crypto c => TxIn c -> P.TxOutRef
transTxIn' (TxIn txid nat) = P.TxOutRef (transTxId txid) (fromIntegral nat)

-- | Given a TxIn, look it up in the UTxO. If it exists, translate it and return
--   (Just translation). If does not exist in the UTxO, return Nothing.
transTxIn ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era),
    Core.TxOut era ~ Alonzo.TxOut era
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  Maybe (P.TxInInfo)
transTxIn (UTxO mp) txin =
  case Map.lookup txin mp of
    Nothing -> Nothing
    Just txout -> case (transAddr addr) of
      Just ad -> Just (P.TxInInfo (transTxIn' txin) (P.TxOut ad valout dhash))
      Nothing -> Nothing
      where
        valout = transValue (getField @"value" txout)
        addr = getField @"address" txout
        dhash = case getField @"datahash" txout of
          SNothing -> Nothing
          SJust (safehash) -> Just (P.DatumHash (transSafeHash safehash))

-- | Given a TxOut, translate it and return (Just transalation). It is
--   possible the address part is a Bootstrap Address, in that case return Nothing
--   I.e. don't include Bootstrap Addresses in the answer.
transTxOut ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  Alonzo.TxOut era ->
  Maybe (P.TxOut)
transTxOut (Alonzo.TxOut addr val datahash) =
  case (transAddr addr) of
    Just ad -> Just (P.TxOut ad (transValue @(Crypto era) val) (transDataHash datahash))
    Nothing -> Nothing

-- ==================================
-- translate Values

transPolicyID :: Mary.PolicyID crypto -> P.CurrencySymbol
transPolicyID (Mary.PolicyID (ScriptHash (UnsafeHash x))) = P.CurrencySymbol (fromShort x)

transAssetName :: Mary.AssetName -> P.TokenName
transAssetName (Mary.AssetName bs) = P.TokenName bs

transValue :: forall c. Mary.Value c -> P.Value
transValue (Mary.Value n mp) = Map.foldlWithKey' accum1 justada mp
  where
    accum1 ans sym mp2 = Map.foldlWithKey' accum2 ans mp2
      where
        accum2 ans2 tok quantity =
          P.unionWith
            (+)
            ans2
            (P.singleton (transPolicyID sym) (transAssetName tok) quantity)
    justada = (P.singleton P.adaSymbol P.adaToken n)

-- =============================================
-- translate fileds like DCert, Wdrl, and similar

transDCert :: DCert c -> P.DCert
transDCert (DCertDeleg (RegKey stkcred)) =
  P.DCertDelegRegKey (P.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (DeRegKey stkcred)) =
  P.DCertDelegDeRegKey (P.StakingHash (transStakeCred stkcred))
transDCert (DCertDeleg (Delegate (Delegation stkcred keyhash))) =
  P.DCertDelegDelegate
    (P.StakingHash (transStakeCred stkcred))
    (transKeyHash keyhash)
transDCert (DCertPool (RegPool pp)) =
  P.DCertPoolRegister (transKeyHash (_poolId pp)) (P.PubKeyHash (transHash (_poolVrf pp)))
transDCert (DCertPool (RetirePool keyhash (EpochNo i))) =
  P.DCertPoolRetire (transKeyHash keyhash) i
transDCert (DCertGenesis _) = P.DCertGenesis
transDCert (DCertMir _) = P.DCertMir

transWdrl :: Wdrl crypto -> Map.Map P.StakingCredential Integer
transWdrl (Wdrl mp) = Map.foldlWithKey' accum Map.empty mp
  where
    accum ans (RewardAcnt _network cred) (Coin n) = Map.insert (P.StakingHash (transStakeCred cred)) n ans

getWitVKeyHash :: (CC.Crypto crypto, Typeable kr) => WitVKey kr crypto -> P.PubKeyHash
getWitVKeyHash = P.PubKeyHash . fromShort . (\(UnsafeHash x) -> x) . (\(KeyHash x) -> x) . hashKey . (\(WitVKey x _) -> x)

transDataPair :: (DataHash c, Data era) -> (P.DatumHash, P.Datum)
transDataPair (x, y) = (transDataHash' x, P.Datum (getPlutusData y))

transCostModel :: CostModel -> P.CostModelParameters
transCostModel (CostModel mp) = Map.foldlWithKey' (\ans bytes n -> Map.insert (show bytes) n ans) Map.empty mp

transExUnits :: ExUnits -> P.ExBudget
transExUnits (ExUnits mem steps) = P.ExBudget (P.ExCPU (fromIntegral steps)) (P.ExMemory (fromIntegral mem))

-- ===================================
-- translate Script Purpose

transScriptPurpose :: CC.Crypto crypto => ScriptPurpose crypto -> P.ScriptPurpose
transScriptPurpose (Minting policyid) = P.Minting (transPolicyID policyid)
transScriptPurpose (Spending txin) = P.Spending (transTxIn' txin)
transScriptPurpose (Rewarding (RewardAcnt _network cred)) = P.Rewarding (P.StakingHash (transStakeCred cred))
transScriptPurpose (Certifying dcert) = P.Certifying (transDCert dcert)

-- ===================================

-- | Compute a Digest of the current transaction to pass to the script
--   This is the major component of the valContext function.
transTx ::
  forall era.
  ( Era era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  UTxO era ->
  Tx era ->
  P.TxInfo
transTx utxo tx =
  P.TxInfo
    { P.txInfoInputs = mapMaybe (transTxIn utxo) (Set.toList (inputs' tbody)),
      P.txInfoInputsFees = mapMaybe (transTxIn utxo) (Set.toList (inputs_fee' tbody)),
      P.txInfoOutputs = mapMaybe transTxOut (foldr (:) [] outs),
      P.txInfoFee = (transValue (inject @(Mary.Value (Crypto era)) fee)),
      P.txInfoForge = (transValue forge),
      P.txInfoDCert = (foldr (\c ans -> transDCert c : ans) [] (certs' tbody)),
      P.txInfoWdrl = transWdrl (wdrls' tbody),
      P.txInfoValidRange = (transVI interval),
      P.txInfoSignatories = map transKeyHash (Set.toList (reqSignerHashes' tbody)),
      P.txInfoData = (map transDataPair datpairs),
      P.txInfoId = (P.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody)))
    }
  where
    tbody = body' tx
    _witnesses = wits' tx
    _isval = isValidating' tx
    _auxdat = auxiliaryData' tx
    outs = outputs' tbody
    fee = txfee' tbody
    forge = mint' tbody
    interval = vldt' tbody
    datpairs = Map.toList (txdats' (wits' tx))

-- ===============================================================
-- From the specification, Figure 7 "Script Validation, cont."
-- ===============================================================

-- | valContext collects info from the Tx and the UTxO an
--   translates it into a 'Data', which the Plutus language knows how to interpret.
--   The UTxO and the PtrMap are used to 'resolve' the TxIn and the StakeRefPtr's
valContext ::
  Era era =>
  P.TxInfo ->
  ScriptPurpose (Crypto era) ->
  [Data era]
valContext txinfo sp = [Data (P.toData (P.Context txinfo (transScriptPurpose sp)))]

-- An analog to evalPlutusScript, is called runPLCScript in the Specification
-- evalPlutusScript, has slighlty different types for its parameters.

-- | Run a Plutus Script, given the script and the bounds on resources it is allocated.
evalPlutusScript :: CostModel -> ExUnits -> P.Script -> [P.Data] -> Bool
evalPlutusScript cost units (P.Script x) ds =
  case P.evaluateScriptRestricting
    P.Quiet
    (transCostModel cost)
    (transExUnits units)
    (toShort (Flat.flat x))
    ds of
    (_, Left _) -> False
    (_, Right ()) -> True
