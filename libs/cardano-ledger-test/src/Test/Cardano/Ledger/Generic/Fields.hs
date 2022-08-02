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
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash, Data (..), hashData)
import Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..), Prices)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..), Datum (..))
import Cardano.Ledger.BaseTypes
  ( Network (..),
    NonNegativeInterval,
    Nonce,
    ProtVer (..),
    StrictMaybe (..),
    UnitInterval,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Keys (KeyHash, KeyPair (..), KeyRole (..), hashKey)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.Serialization (sizedValue)
import qualified Cardano.Ledger.Shelley.PParams as PP (Update)
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), ShelleyTxOut (..), pattern WitnessSet)
import Cardano.Ledger.Shelley.TxBody (DCert (..), ShelleyTxBody (..), Wdrl (..), WitVKey (..))
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (def)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
  = Body (TxBody era)
  | BodyI [TxBodyField era] -- Inlines TxBody Fields
  | Witnesses (Witnesses era)
  | WitnessesI [WitnessesField era] -- Inlines Witnesess Fields
  | AuxData (StrictMaybe (AuxiliaryData era))
  | Valid IsValid

pattern AuxData' :: [AuxiliaryData era] -> TxField era

pattern Valid' :: Bool -> TxField era

-- =================
data TxBodyField era
  = Inputs (Set (TxIn (Crypto era)))
  | Collateral (Set (TxIn (Crypto era)))
  | RefInputs (Set (TxIn (Crypto era)))
  | Outputs (StrictSeq (TxOut era))
  | CollateralReturn (StrictMaybe (TxOut era))
  | TotalCol (StrictMaybe Coin)
  | Certs (StrictSeq (DCert (Crypto era)))
  | Wdrls (Wdrl (Crypto era))
  | Txfee Coin
  | Vldt ValidityInterval
  | TTL SlotNo
  | Update (StrictMaybe (PP.Update era))
  | ReqSignerHashes (Set (KeyHash 'Witness (Crypto era)))
  | Mint (MultiAsset (Crypto era))
  | WppHash (StrictMaybe (ScriptIntegrityHash (Crypto era)))
  | AdHash (StrictMaybe (AuxiliaryDataHash (Crypto era)))
  | Txnetworkid (StrictMaybe Network)

pattern Inputs' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern Collateral' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern RefInputs' :: [TxIn (Crypto era)] -> TxBodyField era -- Set

pattern Outputs' :: [TxOut era] -> TxBodyField era -- StrictSeq

pattern Certs' :: [DCert (Crypto era)] -> TxBodyField era -- StrictSeq

pattern CollateralReturn' :: [TxOut era] -> TxBodyField era -- 0 or 1 element

pattern Update' :: [PP.Update era] -> TxBodyField era -- 0 or 1 element

pattern ReqSignerHashes' :: [KeyHash 'Witness (Crypto era)] -> TxBodyField era -- A set

pattern WppHash' :: [ScriptIntegrityHash (Crypto era)] -> TxBodyField era -- 0 or 1 element

pattern AdHash' :: [AuxiliaryDataHash (Crypto era)] -> TxBodyField era -- 0 or 1 element

pattern Txnetworkid' :: [Network] -> TxBodyField era -- 0 or 1 element

-- ====================
data WitnessesField era
  = AddrWits (Set (WitVKey 'Witness (Crypto era)))
  | BootWits (Set (BootstrapWitness (Crypto era)))
  | ScriptWits (Map (ScriptHash (Crypto era)) (Script era))
  | DataWits (TxDats era)
  | RdmrWits (Redeemers era)

pattern AddrWits' :: Era era => [WitVKey 'Witness (Crypto era)] -> WitnessesField era -- Set

pattern BootWits' :: Era era => [BootstrapWitness (Crypto era)] -> WitnessesField era -- Set

pattern ScriptWits' :: forall era. EraScript era => [Script era] -> WitnessesField era -- Map

pattern DataWits' :: Era era => [Data era] -> WitnessesField era -- Map

-- ================
data TxOutField era
  = Address (Addr (Crypto era))
  | Amount (Value era)
  | DHash (StrictMaybe (DataHash (Crypto era)))
  | FDatum (Datum era)
  | RefScript (StrictMaybe (Script era))

pattern DHash' :: [DataHash (Crypto era)] -> TxOutField era -- 0 or 1 element

pattern RefScript' :: [Script era] -> TxOutField era -- 0 or 1 element

-- ==================
data PParamsField era
  = MinfeeA Natural
  | -- | The constant factor for the minimum fee calculation
    MinfeeB Natural
  | -- | Maximal block body size
    MaxBBSize Natural
  | -- | Maximal transaction size
    MaxTxSize Natural
  | -- | Maximal block header size
    MaxBHSize Natural
  | -- | The amount of a key registration deposit
    KeyDeposit Coin
  | -- | The amount of a pool registration deposit
    PoolDeposit Coin
  | -- | epoch bound on pool retirement
    EMax EpochNo
  | -- | Desired number of pools
    NOpt Natural
  | -- | Pool influence
    A0 NonNegativeInterval
  | -- | Monetary expansion
    Rho UnitInterval
  | -- | Treasury expansion
    Tau UnitInterval
  | -- | Decentralization parameter
    D UnitInterval -- Dropped in Babbage
  | -- | Extra entropy
    ExtraEntropy Nonce -- Dropped in Babbage
  | -- | Protocol version
    ProtocolVersion ProtVer
  | -- | Minimum Stake Pool Cost
    MinPoolCost Coin
  | -- | Minimum Lovelace in a UTxO deprecated by AdaPerUTxOWord
    MinUTxOValue Coin
  | -- | Cost in ada per 8 bytes of UTxO storage instead of _minUTxOValue
    AdaPerUTxOWord Coin -- Dropped in Babbage
  | -- | Cost in ada per 1 byte of UTxO storage instead of _coinsPerUTxOWord
    AdaPerUTxOByte Coin -- New in Babbage
  | -- | Cost models for non-native script languages
    Costmdls CostModels
  | -- | Prices of execution units for non-native script languages
    Prices Prices
  | -- | Max total script execution resources units allowed per tx
    MaxTxExUnits ExUnits
  | -- | Max total script execution resources units allowed per block
    MaxBlockExUnits ExUnits
  | -- | Max size of a Value in an output
    MaxValSize Natural
  | -- | The scaling percentage of the collateral relative to the fee
    CollateralPercentage Natural
  | -- | Maximum number of collateral inputs allowed in a transaction
    MaxCollateralInputs Natural

-- =========================================================================
-- Era parametric "empty" or initial values.

initVI :: ValidityInterval
initVI = ValidityInterval SNothing SNothing

initWdrl :: Wdrl crypto
initWdrl = Wdrl Map.empty

initMultiAsset :: MultiAsset crypto
initMultiAsset = MultiAsset Map.empty

initValue :: MaryValue crypto
initValue = MaryValue 0 mempty

initialTxBody :: Era era => Proof era -> TxBody era
initialTxBody (Shelley _) = ShelleyTxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) (SlotNo 0) SNothing SNothing
initialTxBody (Allegra _) = MATxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing mempty
initialTxBody (Mary _) = MATxBody Set.empty Seq.empty Seq.empty initWdrl (Coin 0) initVI SNothing SNothing mempty
initialTxBody (Alonzo _) =
  AlonzoTxBody
    Set.empty
    Set.empty
    Seq.empty
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initMultiAsset
    SNothing
    SNothing
    SNothing
initialTxBody (Babbage _) =
  BabbageTxBody
    Set.empty
    Set.empty
    Set.empty
    Seq.empty
    SNothing
    SNothing
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initMultiAsset
    SNothing
    SNothing
    SNothing
initialTxBody (Conway _) =
  BabbageTxBody
    Set.empty
    Set.empty
    Set.empty
    Seq.empty
    SNothing
    SNothing
    Seq.empty
    initWdrl
    (Coin 0)
    initVI
    SNothing
    Set.empty
    initMultiAsset
    SNothing
    SNothing
    SNothing

initialWitnesses :: Era era => Proof era -> Witnesses era
initialWitnesses (Shelley _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Allegra _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Mary _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Alonzo _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)
initialWitnesses (Babbage _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)
initialWitnesses (Conway _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)

initialTx :: forall era. Proof era -> Tx era
initialTx era@(Shelley _) = ShelleyTx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Allegra _) = ShelleyTx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Mary _) = ShelleyTx (initialTxBody era) (initialWitnesses era) SNothing
initialTx era@(Alonzo _) =
  AlonzoTx
    (initialTxBody era)
    (initialWitnesses era)
    (IsValid True)
    SNothing
initialTx era@(Babbage _) =
  AlonzoTx
    (initialTxBody era)
    (initialWitnesses era)
    (IsValid True)
    SNothing
initialTx era@(Conway _) =
  AlonzoTx
    (initialTxBody era)
    (initialWitnesses era)
    (IsValid True)
    SNothing

-- | A Meaningless Addr.
initialAddr :: Era era => Proof era -> Addr (Crypto era)
initialAddr _wit = Addr Testnet pCred sCred
  where
    (KeyPair svk _ssk) = theKeyPair 0
    pCred = KeyHashObj . hashKey . vKey $ theKeyPair 1
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

initialTxOut :: Era era => Proof era -> TxOut era
initialTxOut wit@(Shelley _) = ShelleyTxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Allegra _) = ShelleyTxOut (initialAddr wit) (Coin 0)
initialTxOut wit@(Mary _) = ShelleyTxOut (initialAddr wit) (inject (Coin 0))
initialTxOut wit@(Alonzo _) = AlonzoTxOut (initialAddr wit) (inject (Coin 0)) SNothing
initialTxOut wit@(Babbage _) = BabbageTxOut (initialAddr wit) (inject (Coin 0)) NoDatum SNothing
initialTxOut wit@(Conway _) = BabbageTxOut (initialAddr wit) (inject (Coin 0)) NoDatum SNothing

initialPParams :: forall era. Proof era -> PParams era
initialPParams (Shelley _) = def
initialPParams (Allegra _) = def
initialPParams (Mary _) = def
initialPParams (Alonzo _) = def
initialPParams (Babbage _) = def
initialPParams (Conway _) = def

-- ============================================================

abstractTx :: Proof era -> Tx era -> [TxField era]
abstractTx (Conway _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, Witnesses wit, Valid v, AuxData auxdata]
abstractTx (Babbage _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, Witnesses wit, Valid v, AuxData auxdata]
abstractTx (Alonzo _) (AlonzoTx txBody wit v auxdata) =
  [Body txBody, Witnesses wit, Valid v, AuxData auxdata]
abstractTx (Shelley _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, Witnesses wit, AuxData auxdata]
abstractTx (Mary _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, Witnesses wit, AuxData auxdata]
abstractTx (Allegra _) (ShelleyTx txBody wit auxdata) =
  [Body txBody, Witnesses wit, AuxData auxdata]

abstractTxBody :: Proof era -> TxBody era -> [TxBodyField era]
abstractTxBody (Alonzo _) (AlonzoTxBody inp col out cert wdrl fee vldt up req mnt sih adh net) =
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
abstractTxBody (Conway _) (BabbageTxBody inp col ref out colret totcol cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp,
    Collateral col,
    RefInputs ref,
    Outputs (sizedValue <$> out),
    CollateralReturn (sizedValue <$> colret),
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
abstractTxBody (Babbage _) (BabbageTxBody inp col ref out colret totcol cert wdrl fee vldt up req mnt sih adh net) =
  [ Inputs inp,
    Collateral col,
    RefInputs ref,
    Outputs (sizedValue <$> out),
    CollateralReturn (sizedValue <$> colret),
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
abstractTxBody (Shelley _) (ShelleyTxBody inp out cert wdrl fee ttlslot up adh) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, TTL ttlslot, Update up, AdHash adh]
abstractTxBody (Mary _) (MATxBody inp out cert wdrl fee vldt up adh mnt) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh, Mint mnt]
abstractTxBody (Allegra _) (MATxBody inp out cert wdrl fee vldt up adh mnt) =
  [Inputs inp, Outputs out, Certs cert, Wdrls wdrl, Txfee fee, Vldt vldt, Update up, AdHash adh, Mint mnt]

abstractWitnesses :: Proof era -> Witnesses era -> [WitnessesField era]
abstractWitnesses (Shelley _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Allegra _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Mary _) (WitnessSet keys scripts boot) = [AddrWits keys, ScriptWits scripts, BootWits boot]
abstractWitnesses (Alonzo _) (TxWitness key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]
abstractWitnesses (Babbage _) (TxWitness key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]
abstractWitnesses (Conway _) (TxWitness key boot scripts dats red) =
  [AddrWits key, ScriptWits scripts, BootWits boot, DataWits dats, RdmrWits red]

abstractTxOut :: Era era => Proof era -> TxOut era -> [TxOutField era]
abstractTxOut (Shelley _) (ShelleyTxOut addr c) = [Address addr, Amount c]
abstractTxOut (Allegra _) (ShelleyTxOut addr c) = [Address addr, Amount c]
abstractTxOut (Mary _) (ShelleyTxOut addr val) = [Address addr, Amount val]
abstractTxOut (Alonzo _) (AlonzoTxOut addr val d) = [Address addr, Amount val, DHash d]
abstractTxOut (Babbage _) (BabbageTxOut addr val d refscr) =
  [Address addr, Amount val, FDatum d, RefScript refscr]
abstractTxOut (Conway _) (BabbageTxOut addr val d refscr) =
  [Address addr, Amount val, FDatum d, RefScript refscr]

-- =================================================================
-- coercion functions for defining Primed Field constructor patterns

valid :: IsValid -> Bool
valid (IsValid b) = b

toSet :: Ord a => [a] -> Set a
toSet = Set.fromList

fromSet :: Set a -> [a]
fromSet = Set.toList

toStrictSeq :: [a] -> StrictSeq a
toStrictSeq = Seq.fromList

fromStrictSeq :: StrictSeq a -> [a]
fromStrictSeq s = foldr (:) [] s

toStrictMaybe :: [a] -> StrictMaybe a
toStrictMaybe [] = SNothing
toStrictMaybe [x] = SJust x
toStrictMaybe _xs = error "toStrictMaybe applied to list with 2 or more elements"

fromStrictMaybe :: StrictMaybe a -> [a]
fromStrictMaybe SNothing = []
fromStrictMaybe (SJust x) = [x]

-- Coercing from [T era] to (Map (Hash (T era)) (T era)), for different version of T that are Hashable

toMapDat :: Era era => [Data era] -> TxDats era
toMapDat ds = TxDats (Map.fromList (map (\d -> (hashData d, d)) ds))

fromMapScript :: forall era. Map (ScriptHash (Crypto era)) (Script era) -> [Script era]
fromMapScript m = Map.elems m

toMapScript :: forall era. EraScript era => [Script era] -> Map (ScriptHash (Crypto era)) (Script era)
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

wppview :: TxBodyField era -> Maybe [ScriptIntegrityHash (Crypto era)]
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

colretview :: TxBodyField era -> Maybe [TxOut era]
colretview (CollateralReturn x) = Just (fromStrictMaybe x)
colretview _ = Nothing

pattern CollateralReturn' x <-
  (colretview -> Just x)
  where
    CollateralReturn' x = CollateralReturn (toStrictMaybe x)

outputview :: TxBodyField era -> Maybe [TxOut era]
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
    Valid' x = Valid (IsValid x)

auxdataview :: TxField era -> Maybe [AuxiliaryData era]
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

scriptview :: forall era. WitnessesField era -> Maybe [Script era]
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

refscriptview :: TxOutField era -> Maybe [Script era]
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
