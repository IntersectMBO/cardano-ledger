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
import Cardano.Ledger.Alonzo.Scripts (CostModels (..))
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams' (..))
import qualified Cardano.Ledger.Babbage.Tx as Babbage (ValidatedTx (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (Datum (..), TxBody (..), TxOut (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Hashes (ScriptHash)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams, PParams' (..))
import Cardano.Ledger.Shelley.Tx as Shelley (WitnessSetHKD (addrWits, bootWits, scriptWits))
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxBody (..), TxOut (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1, testingCostModelV2)
import Test.Cardano.Ledger.Generic.Fields
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
-- or combine (merge) of two values. We only use this for elements in the
-- WitnessesField data type. That is because we assemble witnesses in small
-- pieces and we combine the pieces together. Every field in WitnessSet and
-- TxWitness has clear way of being merged. We don't use Policies in the other
-- xxxField types because most of those parts cannot be safely combined.
-- (The only execeptions are Coin and Value, but they both have Monoid
-- instances, where we can easliy use (<>) instead.).

class Merge t where
  first :: t -> t -> t
  first x _ = x
  override :: t -> t -> t
  override _ y = y
  merge :: t -> t -> t

type Policy = (forall t. Merge t => t -> t -> t)

-- We need just these 4 instances to merge components of TxWitnesses

instance Ord a => Merge (Set a) where
  merge = Set.union

instance Typeable era => Merge (TxDats era) where
  merge (TxDats x) (TxDats y) = TxDats (Map.union x y)

instance Era era => Merge (Redeemers era) where
  merge (Redeemers x) (Redeemers y) = Redeemers (Map.union x y)

instance Merge (Map (ScriptHash c) v) where
  merge = Map.union

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- Updaters for Tx

updateTx :: Proof era -> Core.Tx era -> TxField era -> Core.Tx era
updateTx (wit@(Shelley _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses override wit wfields) d
    AuxData faux -> Shelley.Tx b w faux
    Valid _ -> tx
updateTx (wit@(Allegra _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses override wit wfields) d
    AuxData faux -> Shelley.Tx b w faux
    Valid _ -> tx
updateTx (wit@(Mary _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    BodyI bfields -> Shelley.Tx (newTxBody wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    WitnessesI wfields -> Shelley.Tx b (newWitnesses override wit wfields) d
    AuxData faux -> Shelley.Tx b w faux
    Valid _ -> tx
updateTx wit@(Alonzo _) (Alonzo.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Alonzo.ValidatedTx fbody w iv d
    BodyI bfields -> Alonzo.ValidatedTx (newTxBody wit bfields) w iv d
    Witnesses fwit -> Alonzo.ValidatedTx b fwit iv d
    WitnessesI wfields -> Alonzo.ValidatedTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> Alonzo.ValidatedTx b w iv faux
    Valid iv' -> Alonzo.ValidatedTx b w iv' d
updateTx wit@(Babbage _) (Babbage.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Babbage.ValidatedTx fbody w iv d
    BodyI bfields -> Babbage.ValidatedTx (newTxBody wit bfields) w iv d
    Witnesses fwit -> Babbage.ValidatedTx b fwit iv d
    WitnessesI wfields -> Babbage.ValidatedTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> Babbage.ValidatedTx b w iv faux
    Valid iv' -> Babbage.ValidatedTx b w iv' d

newTx :: Proof era -> [TxField era] -> Core.Tx era
newTx era = List.foldl' (updateTx era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

updateTxBody :: Proof era -> Core.TxBody era -> TxBodyField era -> Core.TxBody era
updateTxBody (Shelley _) tx dt = case dt of
  (Inputs is) -> tx {Shelley._inputs = is}
  (Outputs outs) -> tx {Shelley._outputs = outs}
  (Certs cs) -> tx {Shelley._certs = cs}
  (Wdrls ws) -> tx {Shelley._wdrls = ws}
  (Txfee c) -> tx {Shelley._txfee = c}
  (Vldt (ValidityInterval _ (SJust n))) -> tx {Shelley._ttl = n}
  (Vldt (ValidityInterval _ SNothing)) -> tx {Shelley._ttl = 0}
  (TTL n) -> tx {Shelley._ttl = n}
  (Update up) -> tx {Shelley._txUpdate = up}
  (AdHash hs) -> tx {Shelley._mdHash = hs}
  _ -> tx
updateTxBody (Allegra _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody is outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins outs1 certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs cs wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs ws txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls c vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt up adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups hs mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody (Mary _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody is outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins outs1 certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs cs wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs ws txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls c vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt up adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups hs mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody (Alonzo _) tx dt = case dt of
  (Inputs is) -> tx {Alonzo.inputs = is}
  (Collateral is) -> tx {Alonzo.collateral = is}
  (Outputs outs1) -> tx {Alonzo.outputs = outs1}
  (Certs cs) -> tx {Alonzo.txcerts = cs}
  (Wdrls ws) -> tx {Alonzo.txwdrls = ws}
  (Txfee c) -> tx {Alonzo.txfee = c}
  (Vldt vi) -> tx {Alonzo.txvldt = vi}
  (Update up) -> tx {Alonzo.txUpdates = up}
  (ReqSignerHashes hs) -> tx {Alonzo.reqSignerHashes = hs}
  (Mint v) -> tx {Alonzo.mint = v}
  (WppHash h) -> tx {Alonzo.scriptIntegrityHash = h}
  (AdHash hs) -> tx {Alonzo.adHash = hs}
  (Txnetworkid i) -> tx {Alonzo.txnetworkid = i}
  _ -> tx
updateTxBody (Babbage _) tx dt = case dt of
  (Inputs is) -> tx {Babbage.inputs = is}
  (Collateral is) -> tx {Babbage.collateral = is}
  (RefInputs is) -> tx {Babbage.referenceInputs = is}
  (Outputs outs1) -> tx {Babbage.outputs = outs1}
  (CollateralReturn outs1) -> tx {Babbage.collateralReturn = outs1}
  (Certs cs) -> tx {Babbage.txcerts = cs}
  (Wdrls ws) -> tx {Babbage.txwdrls = ws}
  (Txfee c) -> tx {Babbage.txfee = c}
  (Vldt vi) -> tx {Babbage.txvldt = vi}
  (Update up) -> tx {Babbage.txUpdates = up}
  (ReqSignerHashes hs) -> tx {Babbage.reqSignerHashes = hs}
  (Mint v) -> tx {Babbage.mint = v}
  (WppHash h) -> tx {Babbage.scriptIntegrityHash = h}
  (AdHash hs) -> tx {Babbage.adHash = hs}
  (Txnetworkid i) -> tx {Babbage.txnetworkid = i}
  (TotalCol coin) -> tx {Babbage.totalCollateral = coin}
  (TTL _) -> tx

newTxBody :: Era era => Proof era -> [TxBodyField era] -> Core.TxBody era
newTxBody era = List.foldl' (updateTxBody era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for Witnesses

updateWitnesses :: forall era. Policy -> Proof era -> Core.Witnesses era -> WitnessesField era -> Core.Witnesses era
updateWitnesses p (Shelley _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Allegra _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Mary _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = p (Shelley.scriptWits w) ss}
  _ -> w
updateWitnesses p (Alonzo _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}
updateWitnesses p (Babbage _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> Core.Witnesses era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

updateTxOut :: Proof era -> Core.TxOut era -> TxOutField era -> Core.TxOut era
updateTxOut (Shelley _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a val
  _ -> out
updateTxOut (Allegra _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a val
  _ -> out
updateTxOut (Mary _) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a val
  _ -> out
updateTxOut (Alonzo _) (out@(Alonzo.TxOut a v h)) txoutd = case txoutd of
  Address addr -> Alonzo.TxOut addr v h
  Amount val -> Alonzo.TxOut a val h
  DHash mdh -> Alonzo.TxOut a v mdh
  Datum d -> error ("This feature is only available from Babbage onward " ++ show d)
  _ -> out
updateTxOut (Babbage _) (Babbage.TxOut a v h refscript) txoutd = case txoutd of
  Address addr -> Babbage.TxOut addr v h refscript
  Amount val -> Babbage.TxOut a val h refscript
  DHash SNothing -> Babbage.TxOut a v Babbage.NoDatum refscript
  DHash (SJust dh) -> Babbage.TxOut a v (Babbage.DatumHash dh) refscript
  Datum d -> Babbage.TxOut a v d refscript
  RefScript s -> Babbage.TxOut a v h s

newTxOut :: Era era => Proof era -> [TxOutField era] -> Core.TxOut era
newTxOut _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
-- This is because we don't have a good story about an initial Address, so the user MUST supply one
newTxOut era dts = List.foldl' (updateTxOut era) (initialTxOut era) dts

-- =====================================================

-- | An updater specialized to the Shelley PParams (also used in Allegra and Mary)
updateShelleyPP :: Shelley.PParams era -> PParamsField era -> Shelley.PParams era
updateShelleyPP pp dpp = case dpp of
  (MinfeeA nat) -> pp {Shelley._minfeeA = nat}
  (MinfeeB nat) -> pp {Shelley._minfeeB = nat}
  (MaxBBSize nat) -> pp {Shelley._maxBBSize = nat}
  (MaxTxSize nat) -> pp {Shelley._maxTxSize = nat}
  (MaxBHSize nat) -> pp {Shelley._maxBHSize = nat}
  (KeyDeposit coin) -> pp {Shelley._keyDeposit = coin}
  (PoolDeposit coin) -> pp {Shelley._poolDeposit = coin}
  (EMax e) -> pp {Shelley._eMax = e}
  (NOpt nat) -> pp {Shelley._nOpt = nat}
  (A0 rat) -> pp {Shelley._a0 = rat}
  (Rho u) -> pp {Shelley._rho = u}
  (Tau u) -> pp {Shelley._tau = u}
  (D u) -> pp {Shelley._d = u}
  (ExtraEntropy nonce) -> pp {Shelley._extraEntropy = nonce}
  (ProtocolVersion pv) -> pp {Shelley._protocolVersion = pv}
  (MinPoolCost coin) -> pp {Shelley._minPoolCost = coin}
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
  (Costmdls cost) -> pp {Alonzo._costmdls = cost}
  (Prices n) -> pp {Alonzo._prices = n}
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
  Prices n -> pp {Babbage._prices = n}
  MaxValSize n -> pp {Babbage._maxValSize = n}
  MaxTxExUnits n -> pp {Babbage._maxTxExUnits = n}
  MaxBlockExUnits n -> pp {Babbage._maxBlockExUnits = n}
  CollateralPercentage perc -> pp {Babbage._collateralPercentage = perc}
  MaxCollateralInputs n -> pp {Babbage._maxCollateralInputs = n}
  D _ -> pp -- All these are no longer in Babbage
  ExtraEntropy _ -> pp
  AdaPerUTxOWord _ -> pp

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
newScriptIntegrityHash (Babbage _) pp ls rds dats =
  case (hashScriptIntegrity pp (Set.fromList ls) rds dats) of
    SJust x -> SJust x
    SNothing -> SNothing
newScriptIntegrityHash (Alonzo _) pp ls rds dats =
  case (hashScriptIntegrity pp (Set.fromList ls) rds dats) of
    SJust x -> SJust x
    SNothing -> SNothing
newScriptIntegrityHash _wit _pp _ls _rds _dats = SNothing

defaultCostModels :: Proof era -> PParamsField era
defaultCostModels (Shelley _) = Costmdls (CostModels mempty)
defaultCostModels (Allegra _) = Costmdls (CostModels mempty)
defaultCostModels (Mary _) = Costmdls (CostModels mempty)
defaultCostModels (Alonzo _) = Costmdls . CostModels $ Map.singleton PlutusV1 testingCostModelV1
defaultCostModels (Babbage _) =
  Costmdls . CostModels . Map.fromList $
    [ (PlutusV1, testingCostModelV1),
      (PlutusV2, testingCostModelV2)
    ]

languages :: Proof era -> [Language]
languages (Shelley _) = []
languages (Allegra _) = []
languages (Mary _) = []
languages (Alonzo _) = [PlutusV1]
languages (Babbage _) = [PlutusV1, PlutusV2]
