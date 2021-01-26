{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- The STS instance for UTXOW is technically an orphan.
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ExampleShelley.Rules.Utxow where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash, ValidateAuxiliaryData)
import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.ExampleShelley.Value (PolicyID, Value, policies, policyID)
import Cardano.Ledger.Shelley.Constraints (UsesAuxiliary, UsesScript, UsesTxBody, UsesTxOut, UsesValue)
import Cardano.Ledger.ExampleShelley.AuxiliaryData ()
import Cardano.Ledger.ExampleShelley.Rules.Utxo (UTXO, UtxoPredicateFailure)
import Cardano.Ledger.ExampleShelley.TxBody ()
import Cardano.Ledger.Torsor (Torsor (Delta))
import Control.SetAlgebra (eval, (◁))
import Control.State.Transition.Extended
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Delegation.Certificates (requiresVKeyWitness)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState (UTxOState)
import Shelley.Spec.Ledger.PParams (Update)
import qualified Shelley.Spec.Ledger.STS.Ledger as Shelley
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv)
import Shelley.Spec.Ledger.STS.Utxow
  ( UtxowPredicateFailure (..),
    utxoWitnessed,
  )
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Tx (Tx (_body), ValidateScript)
import Shelley.Spec.Ledger.TxBody
  ( DCert,
    EraIndependentTxBody,
    RewardAcnt (getRwdCred),
    TxIn,
    Wdrl (unWdrl),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO,
    getScriptHash,
    scriptCred,
    scriptStakeCred,
    txinsScript,
  )

-- ==========================================================

-- | We want to reuse the same rules for Mary and Allegra. This however relies
-- on being able to get a set of 'PolicyID's from the value. Since a 'Coin' has
-- no policies, we create a small class which returns a null set of 'PolicyID's
-- for 'Coin'.
--
-- This should not escape this module.
class GetPolicies a crypto where
  getPolicies :: a -> Set (PolicyID crypto)

instance GetPolicies Coin crypto where
  getPolicies = const Set.empty

instance GetPolicies (Value crypto) crypto where
  getPolicies = policies

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  ( UsesScript era,
    UsesTxOut era,
    UsesTxBody era,
    UsesAuxiliary era,
    GetPolicies (Core.Value era) (Crypto era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "mint" (Core.TxBody era) (Core.Value era)
  ) =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (Crypto era))
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . (getField @"address")) u'')
    `Set.union` Set.fromList
      ( Maybe.mapMaybe (scriptCred . getRwdCred) $
          Map.keys withdrawals
      )
    `Set.union` Set.fromList
      ( Maybe.mapMaybe
          scriptStakeCred
          (filter requiresVKeyWitness certificates)
      )
    `Set.union` (policyID `Set.map` (getPolicies $ getField @"mint" txb))
  where
    txb = _body tx
    withdrawals = unWdrl $ getField @"wdrls" txb
    u'' = eval ((txinsScript (getField @"inputs" $ _body tx) u) ◁ u)
    certificates = (toList . getField @"certs") txb

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

data UTXOW era

instance
  forall era.
  ( UsesValue era,
    UsesTxBody era,
    UsesTxOut era,
    UsesAuxiliary era,
    UsesScript era,
    ChainData (Delta (Core.Value era)),
    SerialisableData (Delta (Core.Value era)),
    ValidateScript era,
    ValidateAuxiliaryData era,
    GetPolicies (Core.Value era) (Crypto era),
    Embed (Core.EraRule "UTXO" era) (UTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ Tx era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField
      "adHash"
      (Core.TxBody era)
      ( StrictMaybe
          (AuxiliaryDataHash (Crypto era))
      ),
    HasField "mint" (Core.TxBody era) (Core.Value era),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  STS (UTXOW era)
  where
  type State (UTXOW era) = UTxOState era
  type Signal (UTXOW era) = Tx era
  type Environment (UTXOW era) = UtxoEnv era
  type BaseM (UTXOW era) = ShelleyBase
  type
    PredicateFailure (UTXOW era) =
      UtxowPredicateFailure era
  transitionRules = [utxoWitnessed scriptsNeeded]
  initialRules = []

instance
  ( Era era,
    STS (UTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ UtxoPredicateFailure era
  ) =>
  Embed (UTXO era) (UTXOW era)
  where
  wrapFailed = UtxoFailure

instance
  ( Era era,
    STS (UTXOW era),
    PredicateFailure (Core.EraRule "UTXOW" era) ~ UtxowPredicateFailure era
  ) =>
  Embed (UTXOW era) (Shelley.LEDGER era)
  where
  wrapFailed = Shelley.UtxowFailure
