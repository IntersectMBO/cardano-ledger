{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash, Data (..), DataHash, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams' (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness (Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Cardano.Ledger.BaseTypes
  ( Network (..),
    NonNegativeInterval,
    Nonce,
    StrictMaybe (..),
    UnitInterval,
  )
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Address.Bootstrap (BootstrapWitness (..))
import Cardano.Ledger.Shelley.PParams (ProtVer (..))
import qualified Cardano.Ledger.Shelley.PParams as PP (PParams, PParams' (..), Update)
import qualified Cardano.Ledger.Shelley.Scripts as Multi
import Cardano.Ledger.Shelley.Tx as Shelley (WitnessSetHKD (addrWits, bootWits, scriptWits), pattern WitnessSet)
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx (..))
import Cardano.Ledger.Shelley.TxBody (DCert (..), TxIn (..), Wdrl (..), WitVKey (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley (TxBody (..), TxOut (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA (TxBody (..))
import Cardano.Ledger.Val (inject, (<+>))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.ByteString.Char8 as BS
import Data.Default.Class (def)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq (empty, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Generic.Indexed
import Test.Cardano.Ledger.Generic.Proof

-- =============================================
-- Making era parameterized Scripts

class (Era era, ValidateScript era) => Scriptic era where
  always :: Natural -> Proof era -> (Core.Script era)
  never :: Natural -> Proof era -> (Core.Script era)
  require :: KeyHash 'Witness (Crypto era) -> Proof era -> (Core.Script era)
  allOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  anyOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  mOf :: Int -> [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)

class Scriptic era => PostShelley era where
  before :: Int -> Proof era -> Core.Script era
  after :: Int -> Proof era -> Core.Script era

class HasTokens era where
  forge :: Integer -> Core.Script era -> Core.Value era

instance CC.Crypto c => Scriptic (ShelleyEra c) where
  never _ (Shelley _) = Multi.RequireAnyOf mempty -- always False
  always _ (Shelley _) = Multi.RequireAllOf mempty -- always True
  require key (Shelley _) = Multi.RequireSignature key
  allOf xs (Shelley c) = (Multi.RequireAllOf (map ($ Shelley c) xs))
  anyOf xs (Shelley c) = (Multi.RequireAnyOf (map ($ Shelley c) xs))
  mOf n xs (Shelley c) = (Multi.RequireMOf n (map ($ Shelley c) xs))

-- Make Scripts in AllegraEra

instance CC.Crypto c => Scriptic (AllegraEra c) where
  never _ (Allegra _) = RequireAnyOf mempty -- always False
  always _ (Allegra _) = RequireAllOf mempty -- always True
  require key (Allegra _) = RequireSignature key
  allOf xs (Allegra c) = (RequireAllOf (Seq.fromList (map ($ Allegra c) xs)))
  anyOf xs (Allegra c) = (RequireAnyOf (Seq.fromList (map ($ Allegra c) xs)))
  mOf n xs (Allegra c) = (RequireMOf n (Seq.fromList (map ($ Allegra c) xs)))

instance CC.Crypto c => PostShelley (AllegraEra c) where
  before n (Allegra _) = RequireTimeStart (unique @SlotNo n)
  after n (Allegra _) = RequireTimeExpire (unique @SlotNo n)

-- Make Scripts in Mary era

instance CC.Crypto c => Scriptic (MaryEra c) where
  never _ (Mary _) = RequireAnyOf mempty -- always False
  always _ (Mary _) = RequireAllOf mempty -- always True
  require key (Mary _) = RequireSignature key
  allOf xs (Mary c) = (RequireAllOf (Seq.fromList (map ($ Mary c) xs)))
  anyOf xs (Mary c) = (RequireAnyOf (Seq.fromList (map ($ Mary c) xs)))
  mOf n xs (Mary c) = (RequireMOf n (Seq.fromList (map ($ Mary c) xs)))

instance CC.Crypto c => PostShelley (MaryEra c) where
  before n (Mary _) = RequireTimeStart (unique @SlotNo n)
  after n (Mary _) = RequireTimeExpire (unique @SlotNo n)

instance forall c. CC.Crypto c => HasTokens (MaryEra c) where
  forge n s = Mary.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Mary.PolicyID (hashScript @(MaryEra c) s)
      an = Mary.AssetName $ BS.pack "an"

instance forall c. CC.Crypto c => HasTokens (AlonzoEra c) where
  forge n s = Mary.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Mary.PolicyID (hashScript @(AlonzoEra c) s)
      an = Mary.AssetName $ BS.pack "an"

-- Make Scripts in Alonzo era

-- | Not every Alonzo Script can be used in a Timelock context.
unTime :: CC.Crypto (Crypto era) => Proof era -> (Proof era -> Script era) -> Timelock (Crypto era)
unTime wit f = case f wit of
  (TimelockScript x) -> x
  (PlutusScript "\SOH\NUL\NUL \ACK\SOH") -> (RequireAnyOf mempty)
  (PlutusScript "\SOH\NUL\NUL \STX\NUL\NUL\DC1") -> (RequireAllOf mempty)
  (PlutusScript _) -> error ("Plutus script in Timelock context")

instance CC.Crypto c => Scriptic (AlonzoEra c) where
  never n (Alonzo _) = alwaysFails n -- always False
  always n (Alonzo _) = alwaysSucceeds n -- always True
  require key (Alonzo _) = TimelockScript (RequireSignature key)
  allOf xs (Alonzo c) = TimelockScript (RequireAllOf (Seq.fromList (map (unTime (Alonzo c)) xs)))
  anyOf xs (Alonzo c) = TimelockScript (RequireAnyOf (Seq.fromList (map (unTime (Alonzo c)) xs)))
  mOf n xs (Alonzo c) = TimelockScript (RequireMOf n (Seq.fromList (map (unTime (Alonzo c)) xs)))

instance CC.Crypto c => PostShelley (AlonzoEra c) where
  before n (Alonzo _) = TimelockScript $ RequireTimeStart (unique @SlotNo n)
  after n (Alonzo _) = TimelockScript $ RequireTimeExpire (unique @SlotNo n)

-- Some examples that work in multiple Eras
matchkey :: Scriptic era => Int -> Proof era -> Core.Script era
matchkey n era = require (theKeyHash n) era

test21 :: Scriptic era => Proof era -> Core.Script era
test21 wit = allOf [always 1, matchkey 1, anyOf [matchkey 2, matchkey 3]] $ wit

test22 :: PostShelley era => Proof era -> Core.Script era
test22 wit = mOf 2 [matchkey 1, before 100, anyOf [matchkey 2, matchkey 3]] $ wit

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

initialWitnesses :: Era era => Proof era -> Core.Witnesses era
initialWitnesses (Shelley _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Allegra _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Mary _) = WitnessSet Set.empty Map.empty Set.empty
initialWitnesses (Alonzo _) = TxWitness mempty mempty mempty mempty (Redeemers mempty)

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

initialPParams :: forall era. Proof era -> Core.PParams era
initialPParams (Shelley _) = def
initialPParams (Allegra _) = def
initialPParams (Mary _) = def
initialPParams (Alonzo _) = def

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

data Policy
  = -- | Combine old and new values using Semigroup semantics (or raise an error if (Semgroup t) doesn't hold).
    Merge
  | -- | Always use the new value
    Override
  | -- | Combine old and new, but don't add any new values if they are already in old.
    NoDups

-- | filter out elements of 'xs' that are in 't'
nodups :: (Foldable t, Eq x) => t x -> [x] -> [x]
nodups t xs = filter (not . (`elem` t)) xs

class Merge t x | t -> x where
  merge :: t -> [x] -> t
  merge t xs = applyMerge Merge t xs
  applyMerge :: Policy -> t -> [x] -> t

instance (Show x, Eq x, Semigroup x) => Merge (Maybe x) x where
  applyMerge Merge Nothing [x] = Just x
  applyMerge Merge Nothing [] = Nothing
  applyMerge Merge (Just x) [y] = Just (x <> y)
  applyMerge Merge (Just x) [] = Just x
  applyMerge Override Nothing [x] = Just x
  applyMerge Override Nothing [] = Nothing
  applyMerge Override (Just _) [y] = Just y
  applyMerge Override (Just x) [] = Just x
  applyMerge NoDups Nothing [x] = Just x
  applyMerge NoDups Nothing [] = Nothing
  applyMerge NoDups (Just x) [y] = Just (if x == y then x else y)
  applyMerge NoDups (Just x) [] = Just x
  applyMerge _ _ xs =
    error ("Only null lists or lists with 1 element can be used in applyMerge for Maybe: " ++ show xs)

instance (Show x, Eq x, Semigroup x) => Merge (StrictMaybe x) x where
  applyMerge Merge SNothing [x] = SJust x
  applyMerge Merge SNothing [] = SNothing
  applyMerge Merge (SJust x) [y] = SJust (x <> y)
  applyMerge Merge (SJust x) [] = SJust x
  applyMerge Override SNothing [x] = SJust x
  applyMerge Override SNothing [] = SNothing
  applyMerge Override (SJust _) [y] = SJust y
  applyMerge Override (SJust x) [] = SJust x
  applyMerge NoDups SNothing [x] = SJust x
  applyMerge NoDups SNothing [] = SNothing
  applyMerge NoDups (SJust x) [y] = SJust (if x == y then x else y)
  applyMerge NoDups (SJust x) [] = SJust x
  applyMerge _ _ xs =
    error ("Only null lists or lists with 1 element can be used in applyMerge for Maybe: " ++ show xs)

instance Ord x => Merge (Set x) x where
  applyMerge Merge set ts = Set.union set (Set.fromList ts)
  applyMerge Override _set ts = (Set.fromList ts)
  applyMerge NoDups set ts = Set.union set (Set.fromList (nodups set ts))

instance Eq x => Merge (StrictSeq x) x where
  applyMerge Override _seqx ts = (Seq.fromList ts)
  applyMerge Merge seqx ts = seqx <> (Seq.fromList ts)
  applyMerge NoDups seqx ts = seqx <> (Seq.fromList (nodups seqx ts))

instance Eq x => Merge [x] x where
  applyMerge Merge list ts = list ++ ts
  applyMerge Override _list ts = ts
  applyMerge NoDups list ts = list ++ (nodups list ts)

instance (Show t, Semigroup t, Show key, Ord key) => Merge (Map key t) (key, t) where
  applyMerge Override _m ts = (Map.fromList ts)
  applyMerge NoDups m ts = Map.union m (Map.fromList ts)
  applyMerge Merge m ts = Map.unionWith (<>) m (Map.fromList ts)

-- | Use this when the range of the map is not a Semigroup. Note Merge cas can fail.
applyMaybe :: Show a => Policy -> Maybe a -> [a] -> Maybe a
applyMap :: (Ord key, Show t, Show key) => String -> Policy -> Map key t -> [(key, t)] -> Map key t
applyMap _ Override _m pairs = Map.fromList pairs
applyMap _ NoDups m ts = Map.union m (Map.fromList ts)
applyMap message Merge m pairs = List.foldl' accum m pairs
  where
    accum ans (k, t) = Map.insertWithKey checkdisjoint k t ans
    checkdisjoint k a b =
      error
        ( "\nWhile merging maps with supposedly disjoint domains, stored in a field: " ++ message
            ++ "\n  We found a common domain element: "
            ++ show k
            ++ "\n  With values: "
            ++ show a
            ++ "  and  "
            ++ show b
        )

-- | Use this when the Maybe type does not have a Semigroup instance
applyMaybe Merge (Just x) [y] = error ("No Semigroup in applyMaybe: " ++ show x ++ "  " ++ show y)
applyMaybe _ Nothing [x] = Just x
applyMaybe _ Nothing [] = Nothing
applyMaybe _ (Just _) [y] = Just y
applyMaybe _ (Just x) [] = Just x
applyMaybe _ _ xs =
  error ("Only null lists or lists with 1 element can be used in applyMaybe: " ++ show xs)

-- | Use this when the StrictMaybe type does not have a Semigroup instance
applySMaybe :: Show a => Policy -> StrictMaybe a -> [a] -> StrictMaybe a
applySMaybe Merge (SJust x) [y] = error ("No Semigroup in applyStrictMaybe: " ++ show x ++ "  " ++ show y)
applySMaybe _ SNothing [x] = SJust x
applySMaybe _ SNothing [] = SNothing
applySMaybe _ (SJust _) [y] = SJust y
applySMaybe _ (SJust x) [] = SJust x
applySMaybe _ _ xs =
  error ("Only null lists or lists with 1 element can be used in applyStrictMaybe: " ++ show xs)

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- Updaters for Tx

data TxField era
  = Body (Core.TxBody era)
  | Body' [TxBodyField era]
  | Witnesses (Core.Witnesses era)
  | Witnesses' [WitnessesField era]
  | AuxData [(Core.AuxiliaryData era)] -- 0 or 1 element, represents Maybe type
  | Valid Bool

updateTx :: Policy -> Proof era -> Core.Tx era -> TxField era -> Core.Tx era
updateTx p (wit@(Shelley _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    Body' bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    Witnesses' wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p (wit@(Allegra _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    Body' bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    Witnesses' wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p (wit@(Mary _)) (tx@(Shelley.Tx b w d)) dt =
  case dt of
    Body fbody -> Shelley.Tx fbody w d
    Body' bfields -> Shelley.Tx (newTxBody p wit bfields) w d
    Witnesses fwit -> Shelley.Tx b fwit d
    Witnesses' wfields -> Shelley.Tx b (newWitnesses p wit wfields) d
    AuxData faux -> Shelley.Tx b w (applySMaybe p d faux)
    Valid _ -> tx
updateTx p wit@(Alonzo _) (Alonzo.ValidatedTx b w iv d) dt =
  case dt of
    Body fbody -> Alonzo.ValidatedTx fbody w iv d
    Body' bfields -> Alonzo.ValidatedTx (newTxBody p wit bfields) w iv d
    Witnesses fwit -> Alonzo.ValidatedTx b fwit iv d
    Witnesses' wfields -> Alonzo.ValidatedTx b (newWitnesses p wit wfields) iv d
    AuxData faux -> Alonzo.ValidatedTx b w iv (applySMaybe p d faux)
    Valid iv' -> Alonzo.ValidatedTx b w (Alonzo.IsValid iv') d

newTx :: Policy -> Proof era -> [TxField era] -> Core.Tx era
newTx p era = List.foldl' (updateTx p era) (initialTx era)

--------------------------------------------------------------------
-- Updaters for TxBody

data TxBodyField era
  = Inputs [TxIn (Crypto era)]
  | Collateral [TxIn (Crypto era)]
  | Outputs [Core.TxOut era]
  | Certs [DCert (Crypto era)]
  | Wdrls (Wdrl (Crypto era))
  | Txfee Coin
  | Vldt ValidityInterval
  | Update [PP.Update era] -- 0 or 1 element, represents Maybe type
  | ReqSignerHashes [KeyHash 'Witness (Crypto era)]
  | Mint (Core.Value era)
  | WppHash [Alonzo.ScriptIntegrityHash (Crypto era)] -- 0 or 1 element, represents Maybe type
  | AdHash [AuxiliaryDataHash (Crypto era)] -- 0 or 1 element, represents Maybe type
  | Txnetworkid (StrictMaybe Network)

updateTxBody :: Policy -> Proof era -> Core.TxBody era -> TxBodyField era -> Core.TxBody era
updateTxBody p (Shelley _) tx dt = case dt of
  (Inputs is) -> tx {Shelley._inputs = applyMerge p (Shelley._inputs tx) is}
  (Collateral is) -> tx {Shelley._inputs = applyMerge p (Shelley._inputs tx) is}
  (Outputs outs) -> tx {Shelley._outputs = applyMerge p (Shelley._outputs tx) outs}
  (Certs cs) -> tx {Shelley._certs = applyMerge p (Shelley._certs tx) cs}
  (Wdrls ws) -> tx {Shelley._wdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Shelley._wdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Shelley._txfee = (Shelley._txfee tx) <+> c}
  (Vldt (ValidityInterval (SJust n) _)) -> tx {Shelley._ttl = n}
  (Vldt (ValidityInterval SNothing _)) -> tx {Shelley._ttl = 0}
  (Update up) -> tx {Shelley._txUpdate = applySMaybe p (Shelley._txUpdate tx) up}
  (AdHash hs) -> tx {Shelley._mdHash = applySMaybe p (Shelley._mdHash tx) hs}
  _ -> tx
updateTxBody p (Allegra _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (applyMerge p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (applyMerge p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (applySMaybe p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (applySMaybe p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Mary _) tx@(MA.TxBody ins outs certs wdrls txfee vldt ups adHash mint) dt = case dt of
  (Inputs is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Collateral is) -> MA.TxBody (applyMerge p (MA.inputs' tx) is) outs certs wdrls txfee vldt ups adHash mint
  (Outputs outs1) -> MA.TxBody ins (applyMerge p (MA.outputs' tx) outs1) certs wdrls txfee vldt ups adHash mint
  (Certs cs) -> MA.TxBody ins outs (applyMerge p (MA.certs' tx) cs) wdrls txfee vldt ups adHash mint
  (Wdrls ws) -> MA.TxBody ins outs certs (Wdrl (Map.unionWith (<+>) (unWdrl (MA.wdrls' tx)) (unWdrl ws))) txfee vldt ups adHash mint
  (Txfee c) -> MA.TxBody ins outs certs wdrls ((MA.txfee' tx) <+> c) vldt ups adHash mint
  (Vldt vi) -> MA.TxBody ins outs certs wdrls txfee vi ups adHash mint
  (Update up) -> MA.TxBody ins outs certs wdrls txfee vldt (applySMaybe p ups up) adHash mint
  (AdHash hs) -> MA.TxBody ins outs certs wdrls txfee vldt ups (applySMaybe p adHash hs) mint
  (Mint v) -> MA.TxBody ins outs certs wdrls txfee vldt ups adHash v
  _ -> tx
updateTxBody p (Alonzo _) tx dt = case dt of
  (Inputs is) -> tx {Alonzo.inputs = applyMerge p (Alonzo.inputs tx) is}
  (Collateral is) -> tx {Alonzo.collateral = applyMerge p (Alonzo.collateral tx) is}
  (Outputs outs1) -> tx {Alonzo.outputs = applyMerge p (Alonzo.outputs tx) outs1}
  (Certs cs) -> tx {Alonzo.txcerts = applyMerge p (Alonzo.txcerts tx) cs}
  (Wdrls ws) -> tx {Alonzo.txwdrls = Wdrl (Map.unionWith (<+>) (unWdrl (Alonzo.txwdrls tx)) (unWdrl ws))}
  (Txfee c) -> tx {Alonzo.txfee = (Alonzo.txfee tx) <+> c}
  (Vldt vi) -> tx {Alonzo.txvldt = vi}
  (Update up) -> tx {Alonzo.txUpdates = applySMaybe p (Alonzo.txUpdates tx) up}
  (ReqSignerHashes hs) -> tx {Alonzo.reqSignerHashes = applyMerge p (Alonzo.reqSignerHashes tx) hs}
  (Mint v) -> tx {Alonzo.mint = v}
  (WppHash h) -> tx {Alonzo.scriptIntegrityHash = applySMaybe p (Alonzo.scriptIntegrityHash tx) h}
  (AdHash hs) -> tx {Alonzo.adHash = applySMaybe p (Alonzo.adHash tx) hs}
  (Txnetworkid i) -> tx {Alonzo.txnetworkid = i}

newTxBody :: Era era => Policy -> Proof era -> [TxBodyField era] -> Core.TxBody era
newTxBody p era = List.foldl' (updateTxBody p era) (initialTxBody era)

--------------------------------------------------------------------
-- Updaters for Witnesses

data WitnessesField era
  = AddrWits [WitVKey 'Witness (Crypto era)]
  | BootWits [BootstrapWitness (Crypto era)]
  | ScriptWits [Core.Script era]
  | DataWits [Data era]
  | RdmrWits (Redeemers era)

hashpair ::
  forall era.
  (ValidateScript era) =>
  Proof era ->
  Core.Script era ->
  (ScriptHash (Crypto era), Core.Script (era))
hashpair _ x = (hashScript @era x, x)

updateWitnesses :: forall era. Policy -> Proof era -> Core.Witnesses era -> WitnessesField era -> Core.Witnesses era
updateWitnesses p era@(Shelley _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = applyMerge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = applyMerge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = applyMap "ScriptWits" p (Shelley.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p era@(Allegra _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = applyMerge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = applyMerge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = applyMap "ScriptWits" p (Shelley.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p era@(Mary _) w dw = case dw of
  (AddrWits ks) -> w {Shelley.addrWits = applyMerge p (Shelley.addrWits w) ks}
  (BootWits boots) -> w {Shelley.bootWits = applyMerge p (Shelley.bootWits w) boots}
  (ScriptWits ss) -> w {Shelley.scriptWits = applyMap "ScriptWits" p (Shelley.scriptWits w) (map (hashpair era) ss)}
  _ -> w
updateWitnesses p wit@(Alonzo _) w dw = case dw of
  (AddrWits ks) -> w {txwitsVKey = applyMerge p (txwitsVKey w) ks}
  (BootWits boots) -> w {txwitsBoot = applyMerge p (txwitsBoot w) boots}
  (ScriptWits ss) -> w {txscripts = applyMap "ScriptWits" p (txscripts w) (map (hashpair wit) ss)}
  (DataWits ds) ->
    w
      { txdats = TxDats $ applyMap "DataWits" p (unTxDats $ txdats w) (map (\x -> (hashData @era x, x)) ds)
      }
  (RdmrWits r) -> w {txrdmrs = r} -- We do not use a merging sematics on Redeemers because the Hashes would get messed up.

newWitnesses :: Era era => Policy -> Proof era -> [WitnessesField era] -> Core.Witnesses era
newWitnesses p era = List.foldl' (updateWitnesses p era) (initialWitnesses era)

-- =====================================================

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
    D (UnitInterval)
  | -- | Extra entropy
    ExtraEntropy (Nonce)
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

newPParams :: Proof era -> [PParamsField era] -> Core.PParams era
newPParams era = List.foldl' (updatePParams era) (initialPParams era)

--------------------------------------------------------------------
-- Updaters for TxOut

notAddress :: TxOutField era -> Bool
notAddress (Address _) = False
notAddress _ = True

applyValue :: Policy -> Proof era -> Core.Value era -> Core.Value era -> Core.Value era
applyValue Override _ _old new = new
applyValue NoDups _ _old new = new
applyValue Merge (Shelley _) old new = old <+> new
applyValue Merge (Allegra _) old new = old <+> new
applyValue Merge (Mary _) old new = old <+> new
applyValue Merge (Alonzo _) old new = old <+> new

data TxOutField era
  = Address (Addr (Crypto era))
  | Amount (Core.Value era)
  | DHash [DataHash (Crypto era)] -- 0 or 1 element, represents Maybe type

updateTxOut :: Policy -> Proof era -> Core.TxOut era -> TxOutField era -> Core.TxOut era
updateTxOut p (Shelley c) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (applyValue p (Shelley c) v val)
  _ -> out
updateTxOut p (Allegra c) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (applyValue p (Allegra c) v val)
  _ -> out
updateTxOut p (Mary c) (out@(Shelley.TxOut a v)) txoutd = case txoutd of
  Address addr -> Shelley.TxOut addr v
  Amount val -> Shelley.TxOut a (applyValue p (Mary c) v val)
  _ -> out
updateTxOut p (Alonzo c) (Alonzo.TxOut a v h) txoutd = case txoutd of
  Address addr -> Alonzo.TxOut addr v h
  Amount val -> Alonzo.TxOut a (applyValue p (Alonzo c) v val) h
  DHash mdh -> Alonzo.TxOut a v (applySMaybe Merge h mdh)

newTxOut :: Era era => Policy -> Proof era -> [TxOutField era] -> Core.TxOut era
newTxOut _ _ dts | all notAddress dts = error ("A call to newTxOut must have an (Address x) field.")
newTxOut p era dts = List.foldl' (updateTxOut p era) (initialTxOut era) dts

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

-- ====================================

-- | This only make sense in the Alonzo era, all other Eras return Nothing
newScriptIntegrityHash ::
  Proof era ->
  Core.PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  [Alonzo.ScriptIntegrityHash (Crypto era)] -- always of length 0 or 1
newScriptIntegrityHash (Alonzo _) pp ls rds dats =
  case (hashScriptIntegrity pp (Set.fromList ls) rds dats) of
    SJust x -> [x]
    SNothing -> []
newScriptIntegrityHash _wit _pp _ls _rds _dats = []

vkey :: Era era => Int -> Proof era -> VKey 'Witness (Crypto era)
vkey n _w = theVKey n
