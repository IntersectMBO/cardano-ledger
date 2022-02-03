{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- Eq (Some Proof)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams' (..))
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams' (..))
import qualified Cardano.Ledger.Babbage.Tx as Babbage (ValidatedTx (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (Datum (..), TxBody (..), TxOut (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys
import qualified Cardano.Ledger.Shelley.PParams as PP (PParams, PParams' (..))
import Cardano.Ledger.Shelley.Tx as Shelley (WitnessSetHKD (addrWits, bootWits, scriptWits))
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxBody (..), TxOut (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Cardano.Ledger.Val ((<+>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq (null)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Generic.Fields
import Test.Cardano.Ledger.Generic.Indexed
import Test.Cardano.Ledger.Generic.Proof

-- ===========================================================================
-- Upaters and the use of Policy to specify Merge Semantics and use of [t] as inputs.
-- When using the Updaters, one will usually consruct the fields by hand.
-- So if a Field consists of (Set t), (StrictSeq t), [t], (Maybe t), (StrictMaybe t), or (Map key t)
-- we will use a list, and convert to the appropriate type for each Field and Era.
-- Several of these: (Map key t), (Maybe t) and (StrictMaybe t) can be problematic
-- since they only have a well defined Merge semantics when (SemiGroup t) .
-- So we define specialized functions applyMap, applyMaybe and applySMaybe that raise
-- an error if a Merge semantics finds more than one copy of the elements being combined.
-- Users may choose what merge semantics they want by passing the right Policy
-- =============================================================================

-- =======================================================================
-- A Policy lets you choose to keep the old (first) or the new (override)
-- whenever we have duplicate fields

type Policy = (forall x. x -> x -> x)

first :: Policy
first x _y = x

override :: Policy
override _x y = y

class Merge t x | t -> x where
  merge :: Policy -> t -> t -> t

instance Merge (Maybe t) t where
  merge _ Nothing x = x
  merge _ x Nothing = x
  merge p x y = p x y

instance Merge (StrictMaybe t) t where
  merge _ SNothing x = x
  merge _ x SNothing = x
  merge p x y = p x y

instance Merge (Set t) t where
  merge _ x y | Set.null x = y
  merge _ x y | Set.null y = x
  merge p x y = p x y

instance Merge (Map k t) t where
  merge _ x y | Map.null x = y
  merge _ x y | Map.null y = x
  merge p x y = p x y

instance Merge (StrictSeq t) t where
  merge _ x y | Seq.null x = y
  merge _ x y | Seq.null y = x
  merge p x y = p x y

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- Updaters for Tx

updateTx :: Policy -> Proof era -> Core.Tx era -> TxField era -> Core.Tx era
updateTx p (wit@(Shelley _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (merge p d faux)
    Valid _ -> tx
updateTx p (wit@(Allegra _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (merge p d faux)
    Valid _ -> tx
updateTx p (wit@(Mary _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (merge p d faux)
    Valid _ -> tx
updateTx p wit@(Alonzo _) (Alonzo.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Alonzo.ValidatedTx fbody w iv d
    BodyI bfields -> Alonzo.ValidatedTx (newTxBody p wit bfields) w iv d
    Witnesses fwit -> Alonzo.ValidatedTx b fwit iv d
    WitnessesI wfields -> Alonzo.ValidatedTx b (newWitnesses p wit wfields) iv d
    AuxData faux -> Alonzo.ValidatedTx b w iv (merge p d faux)
    Valid iv' -> Alonzo.ValidatedTx b w (p iv iv') d
updateTx p wit@(Babbage _) (Babbage.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Babbage.ValidatedTx fbody w iv d
    BodyI bfields -> Babbage.ValidatedTx (newTxBody p wit bfields) w iv d
    Witnesses fwit -> Babbage.ValidatedTx b fwit iv d
    WitnessesI wfields -> Babbage.ValidatedTx b (newWitnesses p wit wfields) iv d
    AuxData faux -> Babbage.ValidatedTx b w iv (merge p d faux)
    Valid iv' -> Babbage.ValidatedTx b w (p iv iv') d

newTx :: Policy -> Proof era -> [TxField era] -> Core.Tx era
newTx p era = List.foldl' (updateTx p era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

updateTxBody :: Policy -> Proof era -> Core.TxBody era -> TxBodyField era -> Core.TxBody era
updateTxBody p (Shelley _) tx dt = case dt of
  (Inputs is) -> tx {Shelley._inputs = p (Shelley._inputs tx) is}
  (Collateral is) -> tx {Shelley._inputs = p (Shelley._inputs tx) is}
  (Outputs outs) -> tx {Shelley._outputs = p (Shelley._outputs tx) outs}
  (Certs cs) -> tx {Shelley._certs = p (Shelley._certs tx) cs}
  (Wdrls ws) -> tx {Shelley._wdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Shelley._wdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Shelley._txfee = (Shelley._txfee tx) <+> c}
  (Vldt (ValidityInterval (SJust n) _)) -> tx {Shelley._ttl = n}
  (Vldt (ValidityInterval SNothing _)) -> tx {Shelley._ttl = 0}
  (Slot n) -> tx {Shelley._ttl = n}
  (Update up) -> tx {Shelley._txUpdate = p (Shelley._txUpdate tx) up}
  (AdHash hs) -> tx {Shelley._mdHash = merge p (Shelley._mdHash tx) hs}
  _ -> tx
updateTxBody p (Allegra _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (merge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (merge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (merge p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (merge p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (merge p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (merge p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Mary _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (merge p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (merge p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Alonzo _) tx dt = case dt of
  (Inputs is) -> tx {Alonzo.inputs = p (Alonzo.inputs tx) is}
  (Collateral is) -> tx {Alonzo.collateral = p (Alonzo.collateral tx) is}
  (Outputs outs1) -> tx {Alonzo.outputs = p (Alonzo.outputs tx) outs1}
  (Certs cs) -> tx {Alonzo.txcerts = p (Alonzo.txcerts tx) cs}
  (Wdrls ws) -> tx {Alonzo.txwdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Alonzo.txwdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Alonzo.txfee = (Alonzo.txfee tx) <+> c}
  (Vldt vi) -> tx {Alonzo.txvldt = vi}
  (Update up) -> tx {Alonzo.txUpdates = merge p (Alonzo.txUpdates tx) up}
  (ReqSignerHashes hs) -> tx {Alonzo.reqSignerHashes = p (Alonzo.reqSignerHashes tx) hs}
  (Mint v) -> tx {Alonzo.mint = v}
  (WppHash h) -> tx {Alonzo.scriptIntegrityHash = merge p (Alonzo.scriptIntegrityHash tx) h}
  (AdHash hs) -> tx {Alonzo.adHash = merge p (Alonzo.adHash tx) hs}
  (Txnetworkid i) -> tx {Alonzo.txnetworkid = i}
  _ -> tx
updateTxBody p (Babbage _) tx dt = case dt of
  (Inputs is) -> tx {Babbage.inputs = p (Babbage.inputs tx) is}
  (Collateral is) -> tx {Babbage.collateral = p (Babbage.collateral tx) is}
  (RefInputs is) -> tx {Babbage.referenceInputs = p (Babbage.referenceInputs tx) is}
  (Outputs outs1) -> tx {Babbage.outputs = p (Babbage.outputs tx) outs1}
  (CollateralReturn outs1) -> tx {Babbage.collateralReturn = merge p (Babbage.collateralReturn tx) outs1}
  (Certs cs) -> tx {Babbage.txcerts = p (Babbage.txcerts tx) cs}
  (Wdrls ws) -> tx {Babbage.txwdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Babbage.txwdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Babbage.txfee = (Babbage.txfee tx) <+> c}
  (Vldt vi) -> tx {Babbage.txvldt = vi}
  (Update up) -> tx {Babbage.txUpdates = merge p (Babbage.txUpdates tx) up}
  (ReqSignerHashes hs) -> tx {Babbage.reqSignerHashes = p (Babbage.reqSignerHashes tx) hs}
  (Mint v) -> tx {Babbage.mint = v}
  (WppHash h) -> tx {Babbage.scriptIntegrityHash = merge p (Babbage.scriptIntegrityHash tx) h}
  (AdHash hs) -> tx {Babbage.adHash = merge p (Babbage.adHash tx) hs}
  (Txnetworkid i) -> tx {Babbage.txnetworkid = i}
  (TotalCol coin) -> tx {Babbage.totalCollateral = coin}
  (Slot _) -> tx

newTxBody :: Era era => Policy -> Proof era -> [TxBodyField era] -> Core.TxBody era
newTxBody p era = List.foldl' (updateTxBody p era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for Witnesses

updateWitnesses :: forall era. Policy -> Proof era -> Core.Witnesses era -> WitnessesField era -> Core.Witnesses era
updateWitnesses p (Shelley _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = merge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = merge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = merge p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Allegra _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = merge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = merge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = merge p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Mary _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = merge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = merge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = merge p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Alonzo _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = merge p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = merge p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = merge p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = r} -- We do not use a merging sematics on Redeemers because the Hashes would get messed up.
updateWitnesses p (Babbage _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = merge p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = merge p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = merge p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = r} -- We do not use a merging sematics on Redeemers because the Hashes would get messed up.

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> Core.Witnesses era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

updateTxOut :: Policy -> Proof era -> Core.TxOut era -> TxOutField era -> Core.TxOut era
updateTxOut _p (Shelley _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (v <+> val)
  _ -> out
updateTxOut _p (Allegra _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (v <+> val)
  _ -> out
updateTxOut _p (Mary _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (v <+> val)
  _ -> out
updateTxOut p (Alonzo _) (out@(Alonzo.TxOut a v h)) txoutd = case txoutd of
  Address addr -> Alonzo.TxOut addr v h
  Amount val -> Alonzo.TxOut a (v <+> val) h
  DHash mdh -> Alonzo.TxOut a v (merge p h mdh)
  Datum (Babbage.NoDatum) -> Alonzo.TxOut a v h
  Datum (Babbage.DatumHash dh) -> Alonzo.TxOut a v (merge p h (SJust dh))
  Datum d -> error ("Cannot use a script Datum in the Alonzo era " ++ show d)
  _ -> out
updateTxOut p (Babbage _) (out@(Babbage.TxOut a v h refscript)) txoutd = case txoutd of
  Address addr -> Babbage.TxOut addr v h refscript
  Amount val -> Babbage.TxOut a (v <+> val) h refscript
  Datum x -> Babbage.TxOut a v (p h x) refscript
  RefScript s -> Babbage.TxOut a v h (merge p refscript s)
  _ -> out

newTxOut :: Era era => Policy -> Proof era -> [TxOutField era] -> Core.TxOut era
newTxOut _ _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
newTxOut p era dts = List.foldl' (updateTxOut p era) (initialTxOut era) dts

-- =====================================================

-- | An updater specialized to the Shelley PParams (also used in Allegra and Mary)
updateShelleyPP :: PP.PParams era -> PParamsField era -> PP.PParams era
updateShelleyPP pp dpp = case dpp of
  (MinfeeA nat) -> pp {PP._minfeeA = nat}
  (MinfeeB nat) -> pp {PP._minfeeB = nat}
  (MaxBBSize nat) -> pp {PP._maxBBSize = nat}
  (MaxTxSize nat) -> pp {PP._maxTxSize = nat}
  (MaxBHSize nat) -> pp {PP._maxBHSize = nat}
  (KeyDeposit coin) -> pp {PP._keyDeposit = coin}
  (PoolDeposit coin) -> pp {PP._poolDeposit = coin}
  (EMax e) -> pp {PP._eMax = e}
  (NOpt nat) -> pp {PP._nOpt = nat}
  (A0 rat) -> pp {PP._a0 = rat}
  (Rho u) -> pp {PP._rho = u}
  (Tau u) -> pp {PP._tau = u}
  (D u) -> pp {PP._d = u}
  (ExtraEntropy nonce) -> pp {PP._extraEntropy = nonce}
  (ProtocolVersion pv) -> pp {PP._protocolVersion = pv}
  (MinPoolCost coin) -> pp {PP._minPoolCost = coin}
  _ -> pp

-- | updatePParams uses the Override policy exclusively
updatePParams :: Proof era -> Core.PParams era -> PParamsField era -> Core.PParams era
updatePParams (Shelley _) pp dpp = updateShelleyPP pp dpp
updatePParams (Allegra _) pp dpp = updateShelleyPP pp dpp
updatePParams (Mary _) pp dpp = updateShelleyPP pp dpp
updatePParams (Alonzo _) pp dpp = case dpp of
  (MinfeeA nat) -> pp {Alonzo._minfeeA = nat}
  (MinfeeB nat) -> pp {Alonzo._minfeeB = nat}
  (MaxBBSize nat) -> pp {Alonzo._maxBBSize = nat}
  (MaxTxSize nat) -> pp {Alonzo._maxTxSize = nat}
  (MaxBHSize nat) -> pp {Alonzo._maxBHSize = nat}
  (KeyDeposit coin) -> pp {Alonzo._keyDeposit = coin}
  (PoolDeposit coin) -> pp {Alonzo._poolDeposit = coin}
  (EMax e) -> pp {Alonzo._eMax = e}
  (NOpt nat) -> pp {Alonzo._nOpt = nat}
  (A0 rat) -> pp {Alonzo._a0 = rat}
  (Rho u) -> pp {Alonzo._rho = u}
  (Tau u) -> pp {Alonzo._tau = u}
  (D u) -> pp {Alonzo._d = u}
  (ExtraEntropy nonce) -> pp {Alonzo._extraEntropy = nonce}
  (ProtocolVersion pv) -> pp {Alonzo._protocolVersion = pv}
  (MinPoolCost coin) -> pp {Alonzo._minPoolCost = coin}
  Costmdls cost -> pp {Alonzo._costmdls = cost}
  MaxValSize n -> pp {Alonzo._maxValSize = n}
  MaxTxExUnits n -> pp {Alonzo._maxTxExUnits = n}
  MaxBlockExUnits n -> pp {Alonzo._maxBlockExUnits = n}
  CollateralPercentage perc -> pp {Alonzo._collateralPercentage = perc}
  _ -> pp
updatePParams (Babbage _) pp dpp = case dpp of
  (MinfeeA nat) -> pp {Babbage._minfeeA = nat}
  (MinfeeB nat) -> pp {Babbage._minfeeB = nat}
  (MaxBBSize nat) -> pp {Babbage._maxBBSize = nat}
  (MaxTxSize nat) -> pp {Babbage._maxTxSize = nat}
  (MaxBHSize nat) -> pp {Babbage._maxBHSize = nat}
  (KeyDeposit coin) -> pp {Babbage._keyDeposit = coin}
  (PoolDeposit coin) -> pp {Babbage._poolDeposit = coin}
  (EMax e) -> pp {Babbage._eMax = e}
  (NOpt nat) -> pp {Babbage._nOpt = nat}
  (A0 rat) -> pp {Babbage._a0 = rat}
  (Rho u) -> pp {Babbage._rho = u}
  (Tau u) -> pp {Babbage._tau = u}
  (ProtocolVersion pv) -> pp {Babbage._protocolVersion = pv}
  (MinPoolCost coin) -> pp {Babbage._minPoolCost = coin}
  Costmdls cost -> pp {Babbage._costmdls = cost}
  MaxValSize n -> pp {Babbage._maxValSize = n}
  MaxTxExUnits n -> pp {Babbage._maxTxExUnits = n}
  MaxBlockExUnits n -> pp {Babbage._maxBlockExUnits = n}
  CollateralPercentage perc -> pp {Babbage._collateralPercentage = perc}
  _ -> pp

newPParams :: Proof era -> [PParamsField era] -> Core.PParams era
newPParams era = List.foldl' (updatePParams era) (initialPParams era)

-- ====================================

-- | This only make sense in the Alonzo era, all other Eras return Nothing
newScriptIntegrityHash ::
  Proof era ->
  Core.PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (Alonzo.ScriptIntegrityHash (Crypto era))
newScriptIntegrityHash (Alonzo _) pp ls rds dats =
  case (hashScriptIntegrity pp (Set.fromList ls) rds dats) of
    SJust x -> SJust x
    SNothing -> SNothing
newScriptIntegrityHash _wit _pp _ls _rds _dats = SNothing

vkey :: Era era => Int -> Proof era -> VKey 'Witness (Crypto era)
vkey n _w = theVKey n
