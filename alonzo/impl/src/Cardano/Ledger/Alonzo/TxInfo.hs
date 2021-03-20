{-# LANGUAGE  FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Ledger.Alonzo.TxInfo where

-- =============================================
-- Types used in the Alonzo Era, needed to make TxInfo

import qualified Data.Set as Set
import Cardano.Crypto.Hash.Class(Hash(UnsafeHash))
import Data.ByteString.Short as SBS(fromShort)
import Data.ByteString as BS(ByteString)
import qualified Cardano.Ledger.Crypto as CC(Crypto)
import Cardano.Ledger.Era(Era,Crypto)
import Cardano.Ledger.Core as Core(Value,TxBody)
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Alonzo.Tx
import Shelley.Spec.Ledger.Coin(Coin(..))
import Cardano.Ledger.Val(Val(..))
import Shelley.Spec.Ledger.TxBody(TxIn(..),TxId(..))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo(TxBody(..))
import Cardano.Ledger.Alonzo.TxBody
  ( TxOut (..),
    inputs', inputs_fee', mint', outputs', txfee', vldt',
  )
import Shelley.Spec.Ledger.Address (Addr(..),BootstrapAddress(..))
import Shelley.Spec.Ledger.Credential (Credential(ScriptHashObj,KeyHashObj))
import qualified Cardano.Ledger.Mary.Value as Mary(Value (..))
import Shelley.Spec.Ledger.Keys(KeyHash(..))
import Shelley.Spec.Ledger.Scripts(ScriptHash(..))
import Shelley.Spec.Ledger.BaseTypes(StrictMaybe(..))
-- import Cardano.Chain.Common.Address(Address)  -- Need to add this to the cabal file
import Cardano.Ledger.ShelleyMA.Timelocks(ValidityInterval(..))
import Cardano.Slotting.Slot(SlotNo(..))

-- ==============================================
-- Import Plutus stuff in the qualified Module P

import qualified Plutus.V1.Ledger.Address as P(Address(..))
import qualified Plutus.V1.Ledger.Value as P(Value(..))
import qualified Plutus.V1.Ledger.Crypto as P(PubKeyHash(..))
import qualified Plutus.V1.Ledger.Scripts as P(ValidatorHash(..),DatumHash(..))
import qualified Plutus.V1.Ledger.Contexts as P(TxInfo(..),TxInInfo(..))
import qualified Plutus.V1.Ledger.TxId as P(TxId(..))
import qualified Plutus.V1.Ledger.Tx as P(TxOut (..), TxOutRef (..), TxOutType (..))
import qualified Plutus.V1.Ledger.Slot as P(SlotRange)
import qualified Plutus.V1.Ledger.Interval as
   P(Extended(..),LowerBound(..),UpperBound(..),Interval(..))

-- =========================================================

transSafeHash :: SafeHash c i -> BS.ByteString
transSafeHash safe = case extractHash safe of { UnsafeHash b -> fromShort b }

transTxId :: TxId era -> P.TxId
transTxId (TxId safe) = P.TxId (transSafeHash safe)

transVI :: ValidityInterval -> P.SlotRange
transVI (ValidityInterval SNothing SNothing) =
    P.Interval (P.LowerBound P.NegInf True)
               (P.UpperBound P.PosInf False)
transVI (ValidityInterval (SJust (SlotNo i)) SNothing) =
    P.Interval (P.LowerBound (P.Finite (fromIntegral i)) True)
               (P.UpperBound P.PosInf False)
transVI (ValidityInterval SNothing (SJust (SlotNo i))) =
    P.Interval (P.LowerBound P.NegInf True)
               (P.UpperBound (P.Finite (fromIntegral i)) False)
transVI (ValidityInterval (SJust (SlotNo i)) (SJust (SlotNo j))) =
    P.Interval (P.LowerBound (P.Finite (fromIntegral i)) True)
               (P.UpperBound (P.Finite (fromIntegral j)) False)

transTxIn' :: CC.Crypto c => TxIn c -> P.TxOutRef
transTxIn' (TxIn txid nat) = P.TxOutRef (transTxId txid) (fromIntegral nat)

transTxIn :: forall c. CC.Crypto c => TxIn c -> P.TxInInfo
transTxIn txin = P.TxInInfo
                   (transTxIn' txin)
                   Nothing                                         -- NOT SURE ABOUT THIS
                   (transValue (inject @(Mary.Value c) (Coin 0)))  -- OR ABOUT THIS

transTxOut :: forall era. (Era era, Value era ~ Mary.Value (Crypto era)) => TxOut era -> P.TxOut
transTxOut (TxOut (Addr _network (ScriptHashObj sh) _stake) val datahash)
   = P.TxOut (P.ScriptAddress (transScriptHash sh))
             (transValue @(Crypto era) val)
             (P.PayToScript (transDataHash datahash))
transTxOut (TxOut (Addr _network (KeyHashObj kh) _stake) val datahash)
   = P.TxOut (P.PubKeyAddress (transKeyHash kh))
             (transValue  @(Crypto era) val)
             (P.PayToScript (transDataHash datahash))
transTxOut (TxOut (AddrBootstrap (BootstrapAddress _ad)) val datahash)
   = P.TxOut (P.PubKeyAddress (P.PubKeyHash  undefined)) -- TODO BootstrapAddress into ByteString
             (transValue @(Crypto era) val)
             (P.PayToScript (transDataHash datahash))

transValue :: forall c. Mary.Value c -> P.Value
transValue (Mary.Value _coin _mp) = undefined  -- TODO complete this

transKeyHash :: KeyHash d c -> P.PubKeyHash
transKeyHash (KeyHash (UnsafeHash h)) = P.PubKeyHash (fromShort h)

transScriptHash :: ScriptHash i -> P.ValidatorHash
transScriptHash (ScriptHash (UnsafeHash h)) = P.ValidatorHash (fromShort h)

transDataHash :: StrictMaybe(DataHash c) -> P.DatumHash
transDataHash (SJust safe) = P.DatumHash (transSafeHash safe)
transDataHash SNothing = P.DatumHash "\00"  -- TODO THIS IS PROBABLY WRONG

transTx :: forall era.
  ( Era era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Value era ~ Mary.Value (Crypto era)
  ) => Tx era -> P.TxInfo
transTx tx =
       P.TxInfo
         (map transTxIn (Set.toList allinputs))
         (map transTxOut (foldr (:) [] outs))
         (transValue (inject @(Mary.Value (Crypto era)) fee))
         (transValue forge)
         (transVI interval)
         [ ] -- MonetaryPolicyHash ???
         [ ] -- PubKeyHash ???
         [ ] -- Auxiliary Data and hashes
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
