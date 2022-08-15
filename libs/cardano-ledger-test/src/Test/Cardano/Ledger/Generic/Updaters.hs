{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (AlonzoPParamsHKD (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import qualified Cardano.Ledger.Babbage.PParams as Babbage (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.TxBody as Babbage
  ( BabbageEraTxBody (..),
    BabbageTxOut (..),
    Datum (..),
  )
import Cardano.Ledger.Coin (Coin (Coin, unCoin))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Tx as Shelley
  ( ShelleyTx (..),
    WitnessSetHKD (addrWits, bootWits, scriptWits),
  )
import Cardano.Ledger.Shelley.TxBody as Shelley (ShelleyEraTxBody (..), ShelleyTxOut (..))
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.Val ((<×>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Lens.Micro
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

updateTx :: Proof era -> Tx era -> TxField era -> Tx era
updateTx (wit@(Shelley _)) (tx@(ShelleyTx b w d)) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx (wit@(Allegra _)) (tx@(ShelleyTx b w d)) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx (wit@(Mary _)) (tx@(ShelleyTx b w d)) dt =
  case dt of
    Body fbody -> ShelleyTx fbody w d
    BodyI bfields -> ShelleyTx (newTxBody wit bfields) w d
    TxWits fwit -> ShelleyTx b fwit d
    WitnessesI wfields -> ShelleyTx b (newWitnesses override wit wfields) d
    AuxData faux -> ShelleyTx b w faux
    Valid _ -> tx
updateTx wit@(Alonzo _) (Alonzo.AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> Alonzo.AlonzoTx fbody w iv d
    BodyI bfields -> Alonzo.AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> Alonzo.AlonzoTx b fwit iv d
    WitnessesI wfields -> Alonzo.AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> Alonzo.AlonzoTx b w iv faux
    Valid iv' -> Alonzo.AlonzoTx b w iv' d
updateTx wit@(Babbage _) (AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> AlonzoTx fbody w iv d
    BodyI bfields -> AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> AlonzoTx b fwit iv d
    WitnessesI wfields -> AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> AlonzoTx b w iv faux
    Valid iv' -> AlonzoTx b w iv' d
updateTx wit@(Conway _) (AlonzoTx b w iv d) dt =
  case dt of
    Body fbody -> AlonzoTx fbody w iv d
    BodyI bfields -> AlonzoTx (newTxBody wit bfields) w iv d
    TxWits fwit -> AlonzoTx b fwit iv d
    WitnessesI wfields -> AlonzoTx b (newWitnesses override wit wfields) iv d
    AuxData faux -> AlonzoTx b w iv faux
    Valid iv' -> AlonzoTx b w iv' d

newTx :: Proof era -> [TxField era] -> Tx era
newTx era = List.foldl' (updateTx era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

updateTxBody :: EraTxBody era => Proof era -> TxBody era -> TxBodyField era -> TxBody era
updateTxBody pf txBody dt =
  case pf of
    _ | Inputs ins <- dt -> txBody & inputsTxBodyL .~ ins
    _ | Outputs outs <- dt -> txBody & outputsTxBodyL .~ outs
    _ | Txfee fee <- dt -> txBody & feeTxBodyL .~ fee
    _ | AdHash auxDataHash <- dt -> txBody & auxDataHashTxBodyL .~ auxDataHash
    Shelley _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      TTL ttl -> txBody & ttlTxBodyL .~ ttl
      Update update -> txBody & updateTxBodyL .~ update
      _ -> txBody
    Allegra _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      _ -> txBody
    Mary _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      AdHash auxDataHash -> txBody & auxDataHashTxBodyL .~ auxDataHash
      Mint mint -> txBody & mintTxBodyL .~ mint
      _ -> txBody
    Alonzo _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      AdHash auxDataHash -> txBody & auxDataHashTxBodyL .~ auxDataHash
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      _ -> txBody
    Babbage _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      AdHash auxDataHash -> txBody & auxDataHashTxBodyL .~ auxDataHash
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      RefInputs refInputs -> txBody & referenceInputsTxBodyL .~ refInputs
      TotalCol totalCol -> txBody & totalCollateralTxBodyL .~ totalCol
      CollateralReturn collateralReturn -> txBody & collateralReturnTxBodyL .~ collateralReturn
      _ -> txBody
    Conway _ -> case dt of
      Certs certs -> txBody & certsTxBodyL .~ certs
      Wdrls wdrls -> txBody & wdrlsTxBodyL .~ wdrls
      Vldt vldt -> txBody & vldtTxBodyL .~ vldt
      Update update -> txBody & updateTxBodyL .~ update
      AdHash auxDataHash -> txBody & auxDataHashTxBodyL .~ auxDataHash
      Mint mint -> txBody & mintTxBodyL .~ mint
      Collateral collateral -> txBody & collateralInputsTxBodyL .~ collateral
      ReqSignerHashes reqSignerHashes -> txBody & reqSignerHashesTxBodyL .~ reqSignerHashes
      WppHash scriptIntegrityHash -> txBody & scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
      Txnetworkid networkId -> txBody & networkIdTxBodyL .~ networkId
      RefInputs refInputs -> txBody & referenceInputsTxBodyL .~ refInputs
      TotalCol totalCol -> txBody & totalCollateralTxBodyL .~ totalCol
      CollateralReturn collateralReturn -> txBody & collateralReturnTxBodyL .~ collateralReturn
      _ -> txBody

newTxBody :: EraTxBody era => Proof era -> [TxBodyField era] -> TxBody era
newTxBody era = List.foldl' (updateTxBody era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for TxWits

updateWitnesses :: forall era. Policy -> Proof era -> TxWits era -> WitnessesField era -> TxWits era
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
updateWitnesses p (Conway _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = p (txscripts w) ss}
  (DataWits ds) -> w {txdats = p (txdats w) ds}
  (RdmrWits r) -> w {txrdmrs = p (txrdmrs w) r}

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> TxWits era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

updateTxOut :: Proof era -> TxOut era -> TxOutField era -> TxOut era
updateTxOut (Shelley _) (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut (Allegra _) (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut (Mary _) (out@(ShelleyTxOut a v)) txoutd = case txoutd of
  Address addr -> ShelleyTxOut addr v
  Amount val -> ShelleyTxOut a val
  _ -> out
updateTxOut (Alonzo _) (out@(AlonzoTxOut a v h)) txoutd = case txoutd of
  Address addr -> AlonzoTxOut addr v h
  Amount val -> AlonzoTxOut a val h
  DHash mdh -> AlonzoTxOut a v mdh
  FDatum d -> error ("This feature is only available from Babbage onward " ++ show d)
  _ -> out
updateTxOut (Babbage _) (BabbageTxOut a v h refscript) txoutd = case txoutd of
  Address addr -> BabbageTxOut addr v h refscript
  Amount val -> BabbageTxOut a val h refscript
  DHash SNothing -> BabbageTxOut a v Babbage.NoDatum refscript
  DHash (SJust dh) -> BabbageTxOut a v (Babbage.DatumHash dh) refscript
  FDatum d -> BabbageTxOut a v d refscript
  RefScript s -> BabbageTxOut a v h s
updateTxOut (Conway _) (BabbageTxOut a v h refscript) txoutd = case txoutd of
  Address addr -> BabbageTxOut addr v h refscript
  Amount val -> BabbageTxOut a val h refscript
  DHash SNothing -> BabbageTxOut a v Babbage.NoDatum refscript
  DHash (SJust dh) -> BabbageTxOut a v (Babbage.DatumHash dh) refscript
  FDatum d -> BabbageTxOut a v d refscript
  RefScript s -> BabbageTxOut a v h s

newTxOut :: Era era => Proof era -> [TxOutField era] -> TxOut era
newTxOut _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
-- This is because we don't have a good story about an initial Address, so the user MUST supply one
newTxOut era dts = List.foldl' (updateTxOut era) (initialTxOut era) dts

-- =====================================================

-- | An updater specialized to the Shelley PParams (also used in Allegra and Mary)
updateShelleyPP :: ShelleyPParams era -> PParamsField era -> ShelleyPParams era
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
  (MinUTxOValue mu) -> pp {Shelley._minUTxOValue = mu}
  -- Not present in Shelley
  (AdaPerUTxOWord _) -> pp
  (AdaPerUTxOByte _) -> pp
  (Costmdls _) -> pp
  (Prices _) -> pp
  (MaxTxExUnits _) -> pp
  (MaxBlockExUnits _) -> pp
  (MaxValSize _) -> pp
  (MaxCollateralInputs _) -> pp
  (CollateralPercentage _) -> pp

-- | updatePParams uses the Override policy exclusively
updatePParams :: Proof era -> PParams era -> PParamsField era -> PParams era
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
  MaxCollateralInputs n -> pp {Alonzo._maxCollateralInputs = n}
  AdaPerUTxOWord n -> pp {Alonzo._coinsPerUTxOWord = n}
  AdaPerUTxOByte n -> pp {Alonzo._coinsPerUTxOWord = (8 :: Int) <×> n}
  -- Not used in Alonzo
  MinUTxOValue _ -> pp
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
  AdaPerUTxOWord n -> pp {Babbage._coinsPerUTxOByte = Coin $ (unCoin n + 7) `div` 8}
  AdaPerUTxOByte n -> pp {Babbage._coinsPerUTxOByte = n}
  -- Not used in Babbage
  D _ -> pp
  ExtraEntropy _ -> pp
  MinUTxOValue _ -> pp
updatePParams (Conway _) pp dpp = case dpp of
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
  AdaPerUTxOWord n -> pp {Babbage._coinsPerUTxOByte = Coin $ (unCoin n + 7) `div` 8}
  AdaPerUTxOByte n -> pp {Babbage._coinsPerUTxOByte = n}
  -- Not used in Conway
  D _ -> pp
  ExtraEntropy _ -> pp
  MinUTxOValue _ -> pp

newPParams :: Proof era -> [PParamsField era] -> PParams era
newPParams era = List.foldl' (updatePParams era) (initialPParams era)

-- ====================================

-- | This only make sense in the Alonzo era and forward, all other Eras return Nothing
newScriptIntegrityHash ::
  Proof era ->
  PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe (Alonzo.ScriptIntegrityHash (Crypto era))
newScriptIntegrityHash (Conway _) pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
newScriptIntegrityHash (Babbage _) pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
newScriptIntegrityHash (Alonzo _) pp ls rds dats =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls)) rds dats
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
defaultCostModels (Conway _) =
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
languages (Conway _) = [PlutusV1, PlutusV2]
