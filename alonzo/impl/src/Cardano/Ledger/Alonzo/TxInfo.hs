{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.TxInfo where

-- =============================================
-- Types used in the Alonzo Era, needed to make TxInfo

import Cardano.Crypto.Hash.Class (Hash (UnsafeHash))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( TxOut (..),
    inputs',
    inputs_fee',
    mint',
    outputs',
    txfee',
    vldt',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..))
import Cardano.Ledger.Core as Core (TxBody, Value)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.SafeHash
-- import Cardano.Chain.Common.Address(Address)  -- Need to add this to the cabal file
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Short as SBS (fromShort)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- ==============================================
-- Import Plutus stuff in the qualified Module P

import qualified Language.PlutusTx as P (Data (..))
import qualified Language.PlutusTx.IsData.Class as P (IsData (..))
import qualified Plutus.V1.Ledger.Ada as P (adaSymbol, adaToken)
import qualified Plutus.V1.Ledger.Address as P (Address (..))
import qualified Plutus.V1.Ledger.Contexts as P (TxInInfo (..), TxInfo (..))
import qualified Plutus.V1.Ledger.Crypto as P (PubKeyHash (..))
import qualified Plutus.V1.Ledger.Interval as P
  ( Extended (..),
    Interval (..),
    LowerBound (..),
    UpperBound (..),
  )
import qualified Plutus.V1.Ledger.Scripts as P (DatumHash (..), ValidatorHash (..))
import qualified Plutus.V1.Ledger.Slot as P (SlotRange)
import qualified Plutus.V1.Ledger.Tx as P (TxOut (..), TxOutRef (..), TxOutType (..))
import qualified Plutus.V1.Ledger.TxId as P (TxId (..))
import qualified Plutus.V1.Ledger.Value as P (CurrencySymbol (..), TokenName (..), Value (..), singleton, unionWith)
import Shelley.Spec.Ledger.Address (Addr (..), BootstrapAddress (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Shelley.Spec.Ledger.Keys (KeyHash (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (..))

-- =========================================================

transSafeHash :: SafeHash c i -> BS.ByteString
transSafeHash safe = case extractHash safe of UnsafeHash b -> fromShort b

transTxId :: TxId era -> P.TxId
transTxId (TxId safe) = P.TxId (transSafeHash safe)

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

transTxIn :: forall c. CC.Crypto c => TxIn c -> P.TxInInfo
transTxIn txin =
  P.TxInInfo
    (transTxIn' txin)
    Nothing -- NOT SURE ABOUT THIS
    (transValue (inject @(Mary.Value c) (Coin 0))) -- OR ABOUT THIS

transTxOut :: forall era. (Era era, Value era ~ Mary.Value (Crypto era)) => TxOut era -> P.TxOut
transTxOut (TxOut (Addr _network (ScriptHashObj sh) _stake) val datahash) =
  P.TxOut
    (P.ScriptAddress (transScriptHash sh))
    (transValue @(Crypto era) val)
    (P.PayToScript (transDataHash datahash))
transTxOut (TxOut (Addr _network (KeyHashObj kh) _stake) val datahash) =
  P.TxOut
    (P.PubKeyAddress (transKeyHash kh))
    (transValue @(Crypto era) val)
    (P.PayToScript (transDataHash datahash))
transTxOut (TxOut (AddrBootstrap (BootstrapAddress _ad)) val datahash) =
  P.TxOut
    (P.PubKeyAddress (P.PubKeyHash undefined)) -- TODO BootstrapAddress into ByteString
    (transValue @(Crypto era) val)
    (P.PayToScript (transDataHash datahash))

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

transKeyHash :: KeyHash d c -> P.PubKeyHash
transKeyHash (KeyHash (UnsafeHash h)) = P.PubKeyHash (fromShort h)

transScriptHash :: ScriptHash i -> P.ValidatorHash
transScriptHash (ScriptHash (UnsafeHash h)) = P.ValidatorHash (fromShort h)

transDataHash :: StrictMaybe (DataHash c) -> P.DatumHash
transDataHash (SJust safe) = P.DatumHash (transSafeHash safe)
transDataHash SNothing = P.DatumHash "\00" -- TODO THIS IS PROBABLY WRONG

transTx ::
  forall era.
  ( Era era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) =>
  Tx era ->
  P.TxInfo
transTx tx =
  P.TxInfo
    (map transTxIn (Set.toList allinputs))
    (map transTxOut (foldr (:) [] outs))
    (transValue (inject @(Mary.Value (Crypto era)) fee))
    (transValue forge)
    (transVI interval)
    [] -- MonetaryPolicyHash ???
    [] -- PubKeyHash ???
    [] -- Auxiliary Data and hashes
    (P.TxId (transSafeHash (hashAnnotated @(Crypto era) tx)))
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

txInfoToData :: P.TxInfo -> P.Data
txInfoToData x = P.toData x
