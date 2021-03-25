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
-- import Cardano.Chain.Common.Address(Address)  -- Need to add this to the cabal file

-- ==============================================
-- Import Plutus stuff in the qualified Module P

import qualified Cardano.Ledger.Alonzo.FakePlutus as P
  ( Address (..),
    Credential (..),
    DCert (..),
    StakingCredential (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
  )
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..), Script)
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( certs',
    inputs',
    inputs_fee',
    mint',
    outputs',
    txfee',
    vldt',
    wdrls',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
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
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import qualified Flat as Flat (flat, unflat)
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
import Shelley.Spec.Ledger.TxBody (DCert (..), DelegCert (..), Delegation (..), PoolCert (..), PoolParams (..), TxId (..), TxIn (..), Wdrl (..), WitVKey (..))
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- =====================================================================

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

transAddr :: Addr crypto -> P.Address
transAddr (Addr _net object stake) = P.Address (transCred object) (transStakeReference stake)
transAddr (AddrBootstrap _bootaddr) = P.Address (P.PubKeyCredential (P.PubKeyHash undefined)) Nothing -- TODO get a hash from a Bootstrap address.

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

-- | Given a TxIn, lookit up in the UTxO. If it exists, translate it and cons the result
--   onto the answer list. If does not exist in the UTxO, just return the answer list.
consTranslatedTxIn ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era),
    Core.TxOut era ~ Alonzo.TxOut era
  ) =>
  UTxO era ->
  TxIn (Crypto era) ->
  [P.TxInInfo] ->
  [P.TxInInfo]
consTranslatedTxIn (UTxO mp) txin answer =
  case Map.lookup txin mp of
    Nothing -> answer
    Just txout -> (P.TxInInfo (transTxIn' txin) (P.TxOut (transAddr addr) valout dhash)) : answer
      where
        valout = transValue (getField @"value" txout)
        addr = getField @"address" txout
        dhash = case getField @"datahash" txout of
          SNothing -> Nothing
          SJust (safehash) -> Just (P.DatumHash (transSafeHash safehash))

transTxOut ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  Alonzo.TxOut era ->
  P.TxOut
transTxOut (Alonzo.TxOut addr val datahash) = P.TxOut (transAddr addr) (transValue @(Crypto era) val) (transDataHash datahash)

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

-- ===================================

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
    { P.txInfoInputs = foldr (consTranslatedTxIn utxo) [] (Set.toList allinputs),
      P.txInfoOutputs = (map (transTxOut) (foldr (:) [] outs)),
      P.txInfoFee = (transValue (inject @(Mary.Value (Crypto era)) fee)),
      P.txInfoForge = (transValue forge),
      P.txInfoDCert = (foldr (\c ans -> transDCert c : ans) [] (certs' tbody)),
      P.txInfoWdrl = transWdrl (wdrls' tbody),
      P.txInfoValidRange = (transVI interval),
      P.txInfoSignatories = (Set.foldl (\ans vk -> (getWitVKeyHash vk) : ans) [] vkkeyset),
      P.txInfoData = (map transDataPair datpairs),
      P.txInfoId = (P.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody)))
    }
  where
    tbody = body' tx
    _witnesses = wits' tx
    _isval = isValidating' tx
    _auxdat = auxiliaryData' tx
    allinputs = Set.union (inputs' tbody) (inputs_fee' tbody)
    outs = outputs' tbody
    fee = txfee' tbody
    forge = mint' tbody
    interval = vldt' tbody
    vkkeyset = txwitsVKey' (wits' tx)
    datpairs = Map.toList (txdats' (wits' tx))

txInfoToData :: P.TxInfo -> P.Data
txInfoToData x = P.toData x

-- | valContext collects info from the Tx and the UTxO an
--   translates it into a 'Data', which the Plutus language knows how to interpret.
--   The UTxO and the PtrMap are used to 'resolve' the TxIn and the StakeRefPtr's
valContext ::
  ( Era era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  UTxO era ->
  Tx era ->
  ScriptPurpose (Crypto era) ->
  [Data era]
valContext utxo tx _sp = [Data (txInfoToData (transTx utxo tx))]

----------------------------

transCostModel :: CostModel -> P.CostModelParameters
transCostModel (CostModel mp) = Map.foldlWithKey' (\ans bytes n -> Map.insert (show bytes) n ans) Map.empty mp

transExUnits :: ExUnits -> P.ExBudget
transExUnits (ExUnits mem steps) = P.ExBudget (P.ExCPU (fromIntegral steps)) (P.ExMemory (fromIntegral mem))
