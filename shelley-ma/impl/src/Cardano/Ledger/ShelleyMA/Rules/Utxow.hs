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

module Cardano.Ledger.ShelleyMA.Rules.Utxow where

import Cardano.Ledger.Compactible (Compactible (CompactForm))
import Cardano.Ledger.Constraints (UsesAuxiliary, UsesScript, UsesTxBody, UsesValue)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Mary.Value (PolicyID, Value, policies, policyID)
import Cardano.Ledger.ShelleyMA (MaryOrAllegra, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.AuxiliaryData ()
import Cardano.Ledger.ShelleyMA.Rules.Utxo ()
import Cardano.Ledger.ShelleyMA.TxBody ()
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val (DecodeMint, DecodeNonNegative)
import Control.SetAlgebra (dom, eval, (◁))
import Control.State.Transition.Extended
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Delegation.Certificates (requiresVKeyWitness)
import Shelley.Spec.Ledger.Keys (DSignable, Hash)
import Shelley.Spec.Ledger.LedgerState (UTxOState)
import Shelley.Spec.Ledger.STS.Utxo
import Shelley.Spec.Ledger.STS.Utxow
  ( UTXOW,
    UtxowPredicateFailure (..),
    initialLedgerStateUTXOW,
    utxoWitnessed,
  )
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Tx (Tx (_body))
import Shelley.Spec.Ledger.TxBody
  ( DCert,
    EraIndependentTxBody,
    RewardAcnt (getRwdCred),
    TxIn,
    TxOut (TxOut),
    Wdrl (unWdrl),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (UTxO),
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
  ( UsesValue era,
    UsesScript era,
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
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
    `Set.union` Set.fromList
      ( Maybe.mapMaybe (scriptCred . getRwdCred) $
          Map.keys withdrawals
      )
    `Set.union` Set.fromList
      ( Maybe.mapMaybe
          scriptStakeCred
          (filter requiresVKeyWitness certificates)
      )
    `Set.union` ((policyID `Set.map` (getPolicies $ getField @"mint" txb)))
  where
    txb = _body tx
    unTxOut (TxOut a _) = a
    withdrawals = unWdrl $ getField @"wdrls" txb
    UTxO u'' = eval (dom (txinsScript (getField @"inputs" txb) u) ◁ u)
    certificates = (toList . getField @"certs") txb

--------------------------------------------------------------------------------
-- UTXOW STS
--------------------------------------------------------------------------------

instance
  forall c (ma :: MaryOrAllegra).
  ( CryptoClass.Crypto c,
    UsesValue (ShelleyMAEra ma c),
    Typeable ma,
    STS (UTXO (ShelleyMAEra ma c)),
    BaseM (UTXO (ShelleyMAEra ma c)) ~ ShelleyBase,
    DecodeMint (Core.Value (ShelleyMAEra ma c)),
    DecodeNonNegative (Core.Value (ShelleyMAEra ma c)),
    GetPolicies (Core.Value (ShelleyMAEra ma c)) c,
    Eq (CompactForm (Core.Value (ShelleyMAEra ma c))),
    Core.ChainData (Core.Value (ShelleyMAEra ma c)),
    Core.ChainData (Delta (Core.Value (ShelleyMAEra ma c))),
    Core.SerialisableData (Core.Value (ShelleyMAEra ma c)),
    Core.SerialisableData (Delta (Core.Value (ShelleyMAEra ma c))),
    Core.SerialisableData (CompactForm (Core.Value (ShelleyMAEra ma c))),
    Torsor (Core.Value (ShelleyMAEra ma c)),
    DSignable c (Hash c EraIndependentTxBody)
  ) =>
  STS (UTXOW (ShelleyMAEra ma c))
  where
  type State (UTXOW (ShelleyMAEra ma c)) = UTxOState (ShelleyMAEra ma c)
  type Signal (UTXOW (ShelleyMAEra ma c)) = Tx (ShelleyMAEra ma c)
  type Environment (UTXOW (ShelleyMAEra ma c)) = UtxoEnv (ShelleyMAEra ma c)
  type BaseM (UTXOW (ShelleyMAEra ma c)) = ShelleyBase
  type
    PredicateFailure (UTXOW (ShelleyMAEra ma c)) =
      UtxowPredicateFailure (ShelleyMAEra ma c)
  transitionRules = [utxoWitnessed scriptsNeeded]
  initialRules = [initialLedgerStateUTXOW]

instance
  ( CryptoClass.Crypto c,
    STS (UTXO (ShelleyMAEra ma c)),
    BaseM (UTXO (ShelleyMAEra ma c)) ~ ShelleyBase
  ) =>
  Embed (UTXO (ShelleyMAEra ma c)) (UTXOW (ShelleyMAEra ma c))
  where
  wrapFailed = UtxoFailure
