-- fromMap and toMap for Scripts
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Generic.Fields
  ( TxField (.., AuxData', Valid'),
    TxBodyField
      ( ..,
        Inputs',
        Collateral',
        RefInputs',
        Outputs',
        Certs',
        CollateralReturn',
        Update',
        ReqSignerHashes',
        WppHash',
        AdHash',
        Txnetworkid'
      ),
    WitnessesField (.., AddrWits', BootWits', ScriptWits', DataWits'),
    PParamsField (..),
    TxOutField (.., DHash', RefScript'),
    initVI,
    initWdrl,
    initValue,
    initialTxBody,
    initialWitnesses,
    initialTx,
    initialTxOut,
    initialPParams,
    valid,
    abstractTx,
    abstractTxBody,
    abstractTxOut,
    abstractWitnesses,
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash, Data (..), DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..), Prices)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (IsValid (..), ScriptIntegrityHash, ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import qualified Cardano.Ledger.Babbage.Tx as Babbage (ValidatedTx (..))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage (Datum (..), TxBody (..), TxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), NonNegativeInterval, Nonce, ProtVer (..), StrictMaybe (..), UnitInterval)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Era (Era (..), ValidateScript, hashScript)
import Cardano.Ledger.Keys (KeyHash, KeyPair (..), KeyRole (..), hashKey)
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import Cardano.Ledger.Shelley.Address.Bootstrap (BootstrapWitness (..))
import qualified Cardano.Ledger.Shelley.PParams as PP (Update)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.Shelley.Tx as Shelley (pattern WitnessSet)
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx (..), TxOut (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), Wdrl (..), WitVKey (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxBody (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (def)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq (empty, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.Proof

-- =======================================================
-- Fields are used to hold a single field of record. So the Field
-- data type (Core.T era) holds the union of all fields of (Core.T era)
-- across all eras Shelley, Allegra, Mary, Alonzo, Babbage.
-- Pattern constructors (with primed names, like C') allow users to use [a], to stand
-- for (Set a) (Maybe a) (StrictSeq a) (StrictMaybe a) (Map (hash a) a)
-- and hide the conversion details from the user. This is very convenient when
-- using Fields to construct (Core.Txx era) by hand in an era agnostic way.

data TxField era
  = Body (Core.TxBody era)
  | BodyI [TxBodyField era] -- Inlines TxBody Fields
  | Witnesses (Core.Witnesses era)
  | WitnessesI [WitnessesField era] -- Inlines Witnesess Fields
  | AuxData (StrictMaybe (Core.AuxiliaryData era))
  | Valid Alonzo.IsValid

pattern AuxData' :: [(Core.AuxiliaryData era)] -> TxField era

pattern Valid' :: Bool -> TxField era

-- =================
data TxBodyField era
  = Inputs (Set (TxIn (Crypto era)))
  | Collateral (Set (TxIn (Crypto era)))
  | RefInputs (Set (TxIn (Crypto era)))
  | Outputs (StrictSeq (Core.TxOut era))
  | CollateralReturn (StrictMaybe (Core.TxOut era))
  | TotalCol Coin
  | Certs (StrictSeq (DCert (Crypto era)))
  | Wdrls (Wdrl (Crypto era))
  | Txfee Coin
  | Vldt ValidityInterval
  | TTL SlotNo
  | Update (StrictMaybe (PP.Update era))
  | ReqSignerHashes (Set (KeyHash 'Witness (Crypto era)))
  | Mint (Core.Value era)
  | WppHash (StrictMaybe (Alonzo.ScriptIntegrityHash (Crypto era)))
  | AdHash (StrictMaybe (AuxiliaryDataHash (Crypto era)))
  | Txnetworkid (StrictMaybe Network)

pattern Inputs' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern Collateral' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern RefInputs' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern Outputs' :: [Core.TxOut era] -> TxBodyField era -- StrictSeq

pattern Certs' :: [DCert (Crypto era)] -> TxBodyField era -- StrictSeq

pattern CollateralReturn' :: [Core.TxOut era] -> TxBodyField era -- 0 or 1 element

pattern Update' :: [PP.Update era] -> TxBodyField era -- 0 or 1 element

pattern ReqSignerHashes' :: [KeyHash 'Witness (Crypto era)] -> TxBodyField era -- A set

pattern WppHash' :: [Alonzo.ScriptIntegrityHash (Crypto era)] -> TxBodyField era -- 0 or 1 element

pattern AdHash' :: [AuxiliaryDataHash (Crypto era)] -> TxBodyField era -- 0 or 1 element

pattern Txnetworkid' :: [Network] -> TxBodyField era -- 0 or 1 element

-- ====================
data WitnessesField era
  = AddrWits (Set (WitVKey 'Witness (Crypto era)))
  | BootWits (Set (BootstrapWitness (Crypto era)))
  | ScriptWits (Map (ScriptHash (Crypto era)) (Core.Script era))
  | DataWits (TxDats era)
  | RdmrWits (Redeemers era)

pattern AddrWits' :: Era era => [WitVKey 'Witness (Crypto era)] -> WitnessesField era --Set

pattern BootWits' :: Era era => [BootstrapWitness (Crypto era)] -> WitnessesField era --Set

pattern ScriptWits' :: forall era. ValidateScript era => [Core.Script era] -> WitnessesField era -- Map

pattern DataWits' :: Era era => [Data era] -> WitnessesField era -- Map

-- ================
data TxOutField era
  = Address (Addr (Crypto era))
  | Amount (Core.Value era)
  | DHash (StrictMaybe (DataHash (Crypto era)))
  | Datum (Babbage.Datum era)
  | RefScript (StrictMaybe (Core.Script era))

pattern DHash' :: [DataHash (Crypto era)] -> TxOutField era -- 0 or 1 element

pattern RefScript' :: [Core.Script era] -> TxOutField era -- 0 or 1 element

-- ==================
data PParamsField era
  = MinfeeA (Natural)
  | -- | The constant factor for the minimum fee calculation
    MinfeeB (Natural)
  | -- | Maximal block body size
    MaxBBSize (Natural)
  | -- | Maximal transaction size
    MaxTxSize (Natural)
  | -- | Maximal block header size
    MaxBHSize (Natural)
  | -- | The amount of a key registration deposit
    KeyDeposit (Coin)
  | -- | The amount of a pool registration deposit
    PoolDeposit (Coin)
  | -- | epoch bound on pool retirement
    EMax (EpochNo)
  | -- | Desired number of pools
    NOpt (Natural)
  | -- | Pool influence
    A0 (NonNegativeInterval)
  | -- | Monetary expansion
    Rho (UnitInterval)
  | -- | Treasury expansion
    Tau (UnitInterval)
  | -- | Decentralization parameter
    D (UnitInterval) -- Dropped in Babbage
  | -- | Extra entropy
    ExtraEntropy (Nonce) -- Dropped in Babbage
  | -- | Protocol version
    ProtocolVersion (ProtVer)
  | -- | Minimum Stake Pool Cost
    MinPoolCost (Coin)
  | -- | Cost in ada per byte of UTxO storage (instead of _minUTxOValue)
    AdaPerUTxOWord (Coin)
  | -- | Cost models for non-native script languages
    Costmdls ((Map Language CostModel))
  | -- | Prices of execution units (for non-native script languages)
    Prices (Prices)
  | -- | Max total script execution resources units allowed per tx
    MaxTxExUnits (ExUnits)
  | -- | Max total script execution resources units allowed per block
    MaxBlockExUnits (ExUnits)
  | -- | Max size of a Value in an output
    MaxValSize (Natural)
  | -- | The scaling percentage of the collateral relative to the fee
    CollateralPercentage (Natural)
  | -- | Maximum number of collateral inputs allowed in a transaction
    MaxCollateralInputs Natural

-- =========================================================================
-- Era parametric "empty" or initial values.

initVI :: ValidityInterval
initVI = ValidityInterval SNothing SNothing

initWdrl :: Wdrl crypto
initWdrl = Wdrl Map.empty

initValue :: Mary.Value crypto
initValue = (Mary.Value 0 Map.empty)

initialTxBody :: Era era => Proof era -> Core.TxBody era
initialTxBody (Shelley _) = Shelley.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) (SlotNo 0) SNothing SNothing
initialTxBody (Allegra _) = MA.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing (Coin 0)
initialTxBody (Mary _) = MA.TxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing initValue
initialTxBody (Alonzo _) =
  Alonzo.TxBody
    Set.empty
    Set.empty
    Seq.empty
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initValue
    SNothing
    SNothing
    SNothing
initialTxBody (Babbage _) =
  Babbage.TxBody
    Set.empty
    Set.empty
    Set.empty
    Seq.empty
    SNothing
    (Coin 0)
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initValue
    SNothing
    SNothing
    SNothing

initialWitnesses :: Era era => Proof era -> Core.Witnesses era
initialWitnesses (Shelley _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Allegra _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Mary _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Alonzo _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)
initialWitnesses (Babbage _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)

initialTx :: forall era. Proof era -> Core.Tx era
initialTx era@(Shelley _) = Shelley.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Allegra _) = Shelley.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Mary _) = Shelley.Tx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Alonzo _) =
  Alonzo.ValidatedTx
    (initialTxBody era)
    (initialWitnesses era)
    (Alonzo.IsValid True)
    SNothing
initialTx era@(Babbage _) =
  Babbage.ValidatedTx
    (initialTxBody era)
    (initialWitnesses era)
    (Alonzo.IsValid True)
    SNothing

-- | A Meaningless Addr.
initialAddr :: Era era => Proof era -> Addr (Crypto era)
initialAddr _wit = Addr Testnet pCred sCred
  where
    (KeyPair svk _ssk) = theKeyPair 0
    pCred = KeyHashObj . hashKey . vKey $ theKeyPair 1
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

initialTxOut :: Era era => Proof era -> Core.TxOut era
initialTxOut wit@(Shelley _) = Shelley.TxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Allegra _) = Shelley.TxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Mary _) = Shelley.TxOut (initialAddr wit) (inject (Coin 0))
initialTxOut wit@(Alonzo _) = Alonzo.TxOut (initialAddr wit) (inject (Coin 0)) SNothing
initialTxOut wit@(Babbage _) = Babbage.TxOut (initialAddr wit) (inject (Coin 0)) Babbage.NoDatum SNothing

initialPParams :: forall era. Proof era -> Core.PParams era
initialPParams (Shelley _) = def
initialPParams (Allegra _) = def
initialPParams (Mary _) = def
initialPParams (Alonzo _) = def
initialPParams (Babbage _) = def

-- ============================================================

abstractTx :: Proof era -> Core.Tx era -> [TxField era]
abstractTx (Babbage _) (Alonzo.ValidatedTx body wit v auxdata) =
  [Body body, Witnesses wit, Valid v, AuxData auxdata]
abstractTx (Alonzo _) (Alonzo.ValidatedTx body wit v auxdata) =
  [Body body, Witnesses wit, Valid v, AuxData auxdata]
abstractTx (Shelley _) (Shelley.Tx body wit auxdata) =
  [Body body, Witnesses wit, AuxData auxdata]
abstractTx (Mary _) (Shelley.Tx body wit auxdata) =
  [Body body, Witnesses wit, AuxData auxdata]
abstractTx (Allegra _) (Shelley.Tx body wit auxdata) =
  [Body body, Witnesses wit, AuxData auxdata]

abstractTxBody :: Proof era -> Core.TxBody era -> [TxBodyField era]
abstractTxBody (Alonzo _) (Alonzo.TxBody inp col out cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp,
    Collateral col,
    Outputs out,
    Certs cert,
    Wdrls wdrl,
    Txfee fee,
    Vldt vldt,
    Update up,
    ReqSignerHashes req,
    Mint mnt,
    WppHash sih,
    AdHash adh,
    Txnetworkid net
  ]
abstractTxBody (Babbage _) (Babbage.TxBody inp col ref out colret totcol cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp,
    Collateral col,
    RefInputs ref,
    Outputs out,
    CollateralReturn colret,
    TotalCol totcol,
    Certs cert,
    Wdrls wdrl,
    Txfee fee,
    Vldt vldt,
    Update up,
    ReqSignerHashes req,
    Mint mnt,
    WppHash sih,
    AdHash adh,
    Txnetworkid net
  ]
abstractTxBody (Shelley _) (Shelley.TxBody inp out cert wdrl fee ttlslot up adh) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, TTL ttlslot, Update up, AdHash adh]
abstractTxBody (Mary _) (MA.TxBody inp out cert wdrl fee vldt up adh mnt) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh, Mint mnt]
abstractTxBody (Allegra _) (MA.TxBody inp out cert wdrl fee vldt up adh mnt) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh, Mint mnt]

abstractWitnesses :: Proof era -> Core.Witnesses era -> [WitnessesField era]
abstractWitnesses (Shelley _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Allegra _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Mary _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Alonzo _) (TxWitness key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]
abstractWitnesses (Babbage _) (TxWitness key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]

abstractTxOut :: Era era => Proof era -> Core.TxOut era -> [TxOutField era]
abstractTxOut (Shelley _) (Shelley.TxOut addr c) = [Address addr, Amount c]
abstractTxOut (Allegra _) (Shelley.TxOut addr c) = [Address addr, Amount c]
abstractTxOut (Mary _) (Shelley.TxOut addr val) = [Address addr, Amount val]
abstractTxOut (Alonzo _) (Alonzo.TxOut addr val d) = [Address addr, Amount val, DHash d]
abstractTxOut (Babbage _) (Babbage.TxOut addr val d refscr) = [Address addr, Amount val, Datum d, RefScript refscr]

-- =================================================================
-- coercion functions for defining Primed Field constructor patterns

valid :: Alonzo.IsValid -> Bool
valid (Alonzo.IsValid b) = b

toSet :: Ord a => [a] -> Set a
toSet = Set.fromList

fromSet :: Set a -> [a]
fromSet = Set.toList

toStrictSeq :: [a] -> StrictSeq a
toStrictSeq x = Seq.fromList x

fromStrictSeq :: StrictSeq a -> [a]
fromStrictSeq s = foldr (:) [] s

toStrictMaybe :: [a] -> StrictMaybe a
toStrictMaybe [] = SNothing
toStrictMaybe [x] = SJust x
toStrictMaybe _xs = error ("toStrictMaybe applied to list with 2 or more elements")

fromStrictMaybe :: StrictMaybe a -> [a]
fromStrictMaybe SNothing = []
fromStrictMaybe (SJust x) = [x]

-- Coercing from [T era] to (Map (Hash (T era)) (T era)), for different version of T that are Hashable

toMapDat :: Era era => [Data era] -> TxDats era
toMapDat ds = TxDats (Map.fromList (map (\d -> (hashData d, d)) ds))

fromMapScript :: forall era. Map (ScriptHash (Crypto era)) (Core.Script era) -> [Core.Script era]
fromMapScript m = Map.elems m

toMapScript :: forall era. ValidateScript era => [Core.Script era] -> Map (ScriptHash (Crypto era)) (Core.Script era)
toMapScript scripts = Map.fromList (map (\s -> (hashScript @era s, s)) scripts)

-- =============================================================================
-- Patterns (with primed names, like C') allow users to use [a], to stand
-- for (Set a) (Maybe a) (StrictSeq a) (StrictMaybe a) (Map (hash a) a)
-- The pattern signatures are just underneath the data declarations

-- ========================
-- TxBody patterns

netview :: TxBodyField era -> Maybe [Network]
netview (Txnetworkid x) = Just (fromStrictMaybe x)
netview _ = Nothing

pattern Txnetworkid' x <-
  (netview -> Just x)
  where
    Txnetworkid' x = Txnetworkid (toStrictMaybe x)

adhashview :: TxBodyField era -> Maybe [AuxiliaryDataHash (Crypto era)]
adhashview (AdHash x) = Just (fromStrictMaybe x)
adhashview _ = Nothing

pattern AdHash' x <-
  (adhashview -> Just x)
  where
    AdHash' x = AdHash (toStrictMaybe x)

wppview :: TxBodyField era -> Maybe [Alonzo.ScriptIntegrityHash (Crypto era)]
wppview (WppHash x) = Just (fromStrictMaybe x)
wppview _ = Nothing

pattern WppHash' x <-
  (wppview -> Just x)
  where
    WppHash' x = WppHash (toStrictMaybe x)

signview :: TxBodyField era -> Maybe [KeyHash 'Witness (Crypto era)]
signview (ReqSignerHashes x) = Just (fromSet x)
signview _ = Nothing

pattern ReqSignerHashes' x <-
  (signview -> Just x)
  where
    ReqSignerHashes' x = ReqSignerHashes (toSet x)

updateview :: TxBodyField era -> Maybe [PP.Update era]
updateview (Update x) = Just (fromStrictMaybe x)
updateview _ = Nothing

pattern Update' x <-
  (updateview -> Just x)
  where
    Update' x = Update (toStrictMaybe x)

certsview :: TxBodyField era -> Maybe [DCert (Crypto era)]
certsview (Certs x) = Just (fromStrictSeq x)
certsview _ = Nothing

pattern Certs' x <-
  (certsview -> Just x)
  where
    Certs' x = Certs (toStrictSeq x)

colretview :: TxBodyField era -> Maybe [Core.TxOut era]
colretview (CollateralReturn x) = Just (fromStrictMaybe x)
colretview _ = Nothing

pattern CollateralReturn' x <-
  (colretview -> Just x)
  where
    CollateralReturn' x = CollateralReturn (toStrictMaybe x)

outputview :: TxBodyField era -> Maybe [Core.TxOut era]
outputview (Outputs x) = Just (fromStrictSeq x)
outputview _ = Nothing

pattern Outputs' x <-
  (outputview -> Just x)
  where
    Outputs' x = Outputs (toStrictSeq x)

inputsview :: TxBodyField era -> Maybe [TxIn (Crypto era)]
inputsview (Inputs x) = Just (fromSet x)
inputsview _ = Nothing

pattern Inputs' x <-
  (inputsview -> Just x)
  where
    Inputs' x = Inputs (toSet x)

colview :: TxBodyField era -> Maybe [TxIn (Crypto era)]
colview (Collateral x) = Just (fromSet x)
colview _ = Nothing

pattern Collateral' x <-
  (colview -> Just x)
  where
    Collateral' x = Collateral (toSet x)

refview :: TxBodyField era -> Maybe [TxIn (Crypto era)]
refview (RefInputs x) = Just (fromSet x)
refview _ = Nothing

pattern RefInputs' x <-
  (refview -> Just x)
  where
    RefInputs' x = RefInputs (toSet x)

-- =============================
-- Tx patterns

validview :: TxField era -> Maybe Bool
validview (Valid x) = Just (valid x)
validview _ = Nothing

pattern Valid' x <-
  (validview -> Just x)
  where
    Valid' x = Valid (Alonzo.IsValid x)

auxdataview :: TxField era -> Maybe [Core.AuxiliaryData era]
auxdataview (AuxData x) = Just (fromStrictMaybe x)
auxdataview _ = Nothing

pattern AuxData' x <-
  (auxdataview -> Just x)
  where
    AuxData' x = AuxData (toStrictMaybe x)

-- =======================
-- WitnessesField Patterns

datawitsview :: forall era. Era era => WitnessesField era -> Maybe [Data era]
datawitsview (DataWits (TxDats x)) = Just (Map.elems x)
datawitsview _ = Nothing

pattern DataWits' x <-
  (datawitsview -> Just x)
  where
    DataWits' x = DataWits (toMapDat x)

scriptview :: forall era. WitnessesField era -> Maybe [Core.Script era]
scriptview (ScriptWits x) = Just (fromMapScript @era x)
scriptview _ = Nothing

pattern ScriptWits' x <-
  (scriptview -> Just x)
  where
    ScriptWits' x = ScriptWits (toMapScript @era x)

addrview :: WitnessesField era -> Maybe [WitVKey 'Witness (Crypto era)]
addrview (AddrWits x) = Just (fromSet x)
addrview _ = Nothing

pattern AddrWits' x <-
  (addrview -> Just x)
  where
    AddrWits' x = AddrWits (toSet x)

bootview :: WitnessesField era -> Maybe [BootstrapWitness (Crypto era)]
bootview (BootWits x) = Just (fromSet x)
bootview _ = Nothing

pattern BootWits' x <-
  (bootview -> Just x)
  where
    BootWits' x = BootWits (toSet x)

-- ========================================
-- TxOut patterns

refscriptview :: TxOutField era -> Maybe [Core.Script era]
refscriptview (RefScript x) = Just (fromStrictMaybe x)
refscriptview _ = Nothing

pattern RefScript' x <-
  (refscriptview -> Just x)
  where
    RefScript' x = RefScript (toStrictMaybe x)

dhashview :: TxOutField era -> Maybe [DataHash (Crypto era)]
dhashview (DHash x) = Just (fromStrictMaybe x)
dhashview _ = Nothing

pattern DHash' x <-
  (dhashview -> Just x)
  where
    DHash' x = DHash (toStrictMaybe x)

-- =======================

{-

import qualified Cardano.Ledger.Babbage.PParams as Babbage (PParams' (..))
getPParamField :: Proof era -> Core.PParams era -> PParamsField era -> PParamsField era
getPParamField (Babbage _) pp field =case field of
  (MinfeeA _) -> MinfeeA (Babbage._minfeeA pp)
  (MinfeeB _)  ->  MinfeeB (Babbage._minfeeB pp)
  (MaxBBSize _)  ->  MaxBBSize (Babbage._maxBBSize pp)
  (MaxTxSize _)  ->  MaxTxSize (Babbage._maxTxSize pp)
  (MaxBHSize _)  ->  MaxBHSize (Babbage._maxBHSize pp)
  (KeyDeposit _)  ->  KeyDeposit (Babbage._keyDeposit pp)
  (PoolDeposit _)  ->  PoolDeposit (Babbage._poolDeposit pp)
  (EMax _)  ->  EMax (Babbage._eMax pp)
  (NOpt _)  ->  NOpt (Babbage._nOpt pp)
  (A0 _)  ->  A0(Babbage._a0 pp)
  (Rho _)  ->  Rho(Babbage._rho pp)
  (Tau _)  ->  Tau(Babbage._tau pp)
  (ProtocolVersion _)  -> ProtocolVersion (Babbage._protocolVersion pp)
  (MinPoolCost _)  ->  MinPoolCost (Babbage._minPoolCost pp)
  (Costmdls _) ->  Costmdls(Babbage._costmdls pp)
  (Prices _)  ->  Prices(Babbage._prices pp)
  (MaxValSize _) ->  MaxValSize(Babbage._maxValSize pp)
  (MaxTxExUnits _)  ->  MaxTxExUnits(Babbage._maxTxExUnits pp)
  (MaxBlockExUnits _)  ->  MaxBlockExUnits(Babbage._maxBlockExUnits pp)
  (CollateralPercentage _)  ->  CollateralPercentage(Babbage._collateralPercentage pp)
  (MaxCollateralInputs _)  -> MaxCollateralInputs (Babbage._maxCollateralInputs pp)
  other -> error ("Babbage does not have this field")

-}
