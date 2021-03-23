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
    StakingHash (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
  )
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( inputs',
    inputs_fee',
    mint',
    outputs',
    txfee',
    vldt',
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
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Short as SBS (fromShort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import qualified Language.PlutusTx as P (Data (..))
import qualified Language.PlutusTx.IsData.Class as P (IsData (..))
import qualified Plutus.V1.Ledger.Ada as P (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Crypto as P (PubKeyHash (..))
import qualified Plutus.V1.Ledger.Interval as P
  ( Extended (..),
    Interval (..),
    LowerBound (..),
    UpperBound (..),
  )
import qualified Plutus.V1.Ledger.Scripts as P (Datum (..), DatumHash (..), ValidatorHash (..))
import qualified Plutus.V1.Ledger.Slot as P (SlotRange)
import qualified Plutus.V1.Ledger.Tx as P (TxOutRef (..))
import qualified Plutus.V1.Ledger.TxId as P (TxId (..))
import qualified Plutus.V1.Ledger.Value as P (CurrencySymbol (..), TokenName (..), Value (..), singleton, unionWith)
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj), Ptr (..), StakeReference (..))
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (..), hashKey)
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (..), WitVKey (..))
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- =========================================================

-- | The _ptrs field of the DState (State of staking pool delegations and rewards)
type PtrMap crypto = Map.Map Ptr (Credential 'Staking crypto)

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

transTxId :: TxId era -> P.TxId
transTxId (TxId safe) = P.TxId (transSafeHash safe)

-- =====================

transStakeCred :: Credential keyrole crypto -> BS.ByteString
transStakeCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) = (fromShort kh)
transStakeCred (KeyHashObj (KeyHash (UnsafeHash kh))) = (fromShort kh)

transStakeReference :: (PtrMap crypto) -> StakeReference crypto -> Maybe P.StakingHash
transStakeReference _mp (StakeRefBase cred) = Just (P.StakingHash (transStakeCred cred))
transStakeReference mp (StakeRefPtr ptr) =
  case Map.lookup ptr mp of
    Nothing -> Nothing
    Just cred -> Just (P.StakingHash (transStakeCred cred))
transStakeReference _mp StakeRefNull = Nothing

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

transTxIn' :: CC.Crypto c => TxIn c -> P.TxOutRef
transTxIn' (TxIn txid nat) = P.TxOutRef (transTxId txid) (fromIntegral nat)

transTxIn ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era),
    Core.TxOut era ~ Alonzo.TxOut era
  ) =>
  UTxO era ->
  PtrMap (Crypto era) ->
  TxIn (Crypto era) ->
  P.TxInInfo
transTxIn (UTxO mp) ptrmap txin =
  P.TxInInfo
    (transTxIn' txin)
    (transAddr ptrmap addr)
    valout
    dhash
  where
    txout = case Map.lookup txin mp of Just out -> out; Nothing -> error ("txin not in UTxO")
    valout = transValue (getField @"value" txout)
    addr = getField @"address" txout
    dhash = case getField @"datahash" txout of
      SNothing -> Nothing
      SJust (safehash) -> Just (P.DatumHash (transSafeHash safehash))

transCred :: Credential keyrole crypto -> P.Credential
transCred (KeyHashObj (KeyHash (UnsafeHash kh))) = P.PubKeyCredential (P.PubKeyHash (fromShort kh))
transCred (ScriptHashObj (ScriptHash (UnsafeHash kh))) = P.ScriptCredential (P.ValidatorHash (fromShort kh))

transAddr :: PtrMap crypto -> Addr crypto -> P.Address
transAddr ptrmap (Addr _net object stake) = P.Address (transCred object) (transStakeReference ptrmap stake)
transAddr _ptrmap (AddrBootstrap _bootaddr) = P.Address (P.PubKeyCredential (P.PubKeyHash undefined)) Nothing -- TODO get a hash from a Bootstrap address.

transTxOut ::
  forall era.
  ( Era era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  PtrMap (Crypto era) ->
  Alonzo.TxOut era ->
  P.TxOut
transTxOut ptrmap (Alonzo.TxOut addr val datahash) = P.TxOut (transAddr ptrmap addr) (transValue @(Crypto era) val) (transDataHash datahash)

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

-- ===================================

transTx ::
  forall era.
  ( Era era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  UTxO era ->
  PtrMap (Crypto era) ->
  Tx era ->
  P.TxInfo
transTx utxo ptrmap tx =
  P.TxInfo
    (map (transTxIn utxo ptrmap) (Set.toList allinputs))
    (map (transTxOut ptrmap) (foldr (:) [] outs))
    (transValue (inject @(Mary.Value (Crypto era)) fee))
    (transValue forge)
    (transVI interval)
    (Set.foldl (\ans vk -> (getWitVKeyHash vk) : ans) [] vkkeyset)
    (map transDataPair datpairs)
    (P.TxId (transSafeHash (hashAnnotated @(Crypto era) tbody)))
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

-- | valContext collects info from the Tx and the UTxO and the PtrMap and
--   translates it into a 'Data', which the Plutus language knows how to interpret.
--   The UTxO and the PtrMap are used to 'resolve' the TxIn and the StakeRefPtr's
valContext ::
  ( Era era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  UTxO era ->
  PtrMap (Crypto era) ->
  Tx era ->
  ScriptPurpose (Crypto era) ->
  [Data era]
valContext utxo ptrmap tx _sp = [Data (txInfoToData (transTx utxo ptrmap tx))]

---

getWitVKeyHash :: (CC.Crypto crypto, Typeable kr) => WitVKey kr crypto -> P.PubKeyHash
getWitVKeyHash = P.PubKeyHash . fromShort . (\(UnsafeHash x) -> x) . (\(KeyHash x) -> x) . hashKey . (\(WitVKey x _) -> x)

transDataPair :: (DataHash c, Data era) -> (P.DatumHash, P.Datum)
transDataPair (x, y) = (transDataHash' x, P.Datum (getPlutusData y))
