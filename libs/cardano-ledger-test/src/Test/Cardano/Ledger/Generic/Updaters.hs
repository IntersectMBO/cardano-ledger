{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Updaters where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Plutus.Language (Language (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
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
-- pieces and we combine the pieces together. Every field in ShelleyTxWits and
-- AlonzoTxWits has clear way of being merged. We don't use Policies in the other
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

instance Era era => Merge (TxDats era) where
  merge (TxDats x) (TxDats y) = TxDats (Map.union x y)

instance AlonzoEraScript era => Merge (Redeemers era) where
  merge (Redeemers x) (Redeemers y) = Redeemers (Map.union x y)

instance Merge (Map ScriptHash v) where
  merge = Map.union

-- ====================================================================
-- Building Era parametric Records
-- ====================================================================

-- | This only make sense in the Alonzo era and forward, all other Eras return Nothing
alonzoNewScriptIntegrityHash ::
  ( AlonzoEraScript era
  , AlonzoEraPParams era
  ) =>
  PParams era ->
  [Language] ->
  Redeemers era ->
  TxDats era ->
  StrictMaybe Alonzo.ScriptIntegrityHash
alonzoNewScriptIntegrityHash pp ls =
  hashScriptIntegrity (Set.map (Alonzo.getLanguageView pp) (Set.fromList ls))

languages :: Proof era -> [Language]
languages Shelley = []
languages Allegra = []
languages Mary = []
languages Alonzo = [PlutusV1]
languages Babbage = [PlutusV1, PlutusV2]
languages Conway = [PlutusV1, PlutusV2]
