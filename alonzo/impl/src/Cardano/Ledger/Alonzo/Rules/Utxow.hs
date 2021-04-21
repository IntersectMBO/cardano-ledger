{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Utxow where

-- import Shelley.Spec.Ledger.UTxO(UTxO(..))

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Data (DataHash)
import Cardano.Ledger.Alonzo.PParams (PParams)
import Cardano.Ledger.Alonzo.PlutusScriptApi (checkScriptData, language, scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUTXO)
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoPredicateFailure)
import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Cardano.Ledger.Alonzo.Tx
  ( ScriptPurpose,
    ValidatedTx,
    hashWitnessPPData,
    isTwoPhaseScriptAddress,
    wits',
  )
import Cardano.Ledger.Alonzo.TxBody (WitnessPPDataHash)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, SupportsSegWit (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentData)
import Cardano.Ledger.SafeHash (SafeHash)
import Control.DeepSeq (NFData (..))
import Control.Iterate.SetAlgebra (domain, eval, (⊆), (◁), (➖))
import Control.State.Transition.Extended
import Data.Coders
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..))
import Shelley.Spec.Ledger.LedgerState (UTxOState (..), unWitHashes, witsFromTxWitnesses)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.STS.Utxow
  ( ShelleyStyleWitnessNeeds,
    UtxowPredicateFailure (..),
    shelleyStyleWitness,
  )
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.Tx (TxIn (..))

-- =====================================================

-- | The Predicate failure type in the Alonzo Era. It embeds the Predicate
--   failure type of the Shelley Era, as they share some failure modes.
data AlonzoPredFail era
  = WrappedShelleyEraFailure !(UtxowPredicateFailure era)
  | UnRedeemableScripts ![(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
  | MissingNeededScriptHash (Set (ScriptHash (Crypto era)))
  | DataHashSetsDontAgree
      !(Set (DataHash (Crypto era)))
      -- ^ from the Tx
      !(Set (DataHash (Crypto era)))
      -- ^ from the UTxO restricted to the Tx inputs
  | PPViewHashesDontMatch
      !(StrictMaybe (WitnessPPDataHash (Crypto era)))
      -- ^ The PPHash in the TxBody
      !(StrictMaybe (WitnessPPDataHash (Crypto era)))
      -- ^ Computed from the current Protocol Parameters
  | MissingRequiredSigners (Set (KeyHash 'Witness (Crypto era)))
  | -- | Scripts that failed
    Phase1ScriptWitnessNotValidating
      !(Set (ScriptHash (Crypto era)))
  deriving (Generic)

deriving instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "UTXO" era)), -- The Shelley UtxowPredicateFailure needs this to Show
    Show (Core.Script era)
  ) =>
  Show (AlonzoPredFail era)

deriving instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "UTXO" era)), -- The Shelley UtxowPredicateFailure needs this to Eq
    Eq (Core.Script era)
  ) =>
  Eq (AlonzoPredFail era)

instance
  ( Era era,
    NoThunks (Core.Script era),
    NoThunks (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  NoThunks (AlonzoPredFail era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.AuxiliaryData era),
    Typeable (Core.Script era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (AlonzoPredFail era)
  where
  toCBOR x = encode (encodePredFail x)

encodePredFail ::
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  AlonzoPredFail era ->
  Encode 'Open (AlonzoPredFail era)
encodePredFail (WrappedShelleyEraFailure x) = Sum WrappedShelleyEraFailure 0 !> E toCBOR x
encodePredFail (UnRedeemableScripts x) = Sum UnRedeemableScripts 1 !> To x
encodePredFail (MissingNeededScriptHash x) = Sum MissingNeededScriptHash 2 !> To x
encodePredFail (DataHashSetsDontAgree x y) = Sum DataHashSetsDontAgree 3 !> To x !> To y
encodePredFail (PPViewHashesDontMatch x y) = Sum PPViewHashesDontMatch 4 !> To x !> To y
encodePredFail (MissingRequiredSigners x) = Sum MissingRequiredSigners 5 !> To x
encodePredFail (Phase1ScriptWitnessNotValidating x) = Sum Phase1ScriptWitnessNotValidating 6 !> To x

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (AlonzoPredFail era)
  where
  fromCBOR = decode (Summands "(AlonzoPredFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  Word ->
  Decode 'Open (AlonzoPredFail era)
decodePredFail 0 = SumD WrappedShelleyEraFailure <! D fromCBOR
decodePredFail 1 = SumD UnRedeemableScripts <! From
decodePredFail 2 = SumD MissingNeededScriptHash <! From
decodePredFail 3 = SumD DataHashSetsDontAgree <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD Phase1ScriptWitnessNotValidating <! From
decodePredFail n = Invalid n

-- =============================================

{- Defined in the Shelley Utxow rule.
type ShelleyStyleWitnessNeeds era =
  ( HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "addrWits" (Core.Tx era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    ValidateAuxiliaryData era (Crypto era),
    ValidateScript era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody)
  )
-}

-- | Constraints to make an Alonzo Utxow STS instance
--   (in addition to ShelleyStyleWitnessNeeds)
type AlonzoStyleAdditions era =
  ( HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))), -- BE SURE AND ADD THESE INSTANCES
    HasField "wppHash" (Core.TxBody era) (StrictMaybe (WitnessPPDataHash (Crypto era)))
  )

-- | A somewhat generic STS transitionRule function for the Alonzo Era.
alonzoStyleWitness ::
  forall era utxow.
  ( Era era,
    -- Fix some Core types to the Alonzo Era
    TxInBlock era ~ ValidatedTx era, -- scriptsNeeded, checkScriptData etc. are fixed at Alonzo.Tx
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (utxow era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    -- Asumptions needed since we are going to fix utxow when we use this in an STS Era
    BaseM (utxow era) ~ ShelleyBase,
    Environment (utxow era) ~ UtxoEnv era,
    State (utxow era) ~ UTxOState era,
    Signal (utxow era) ~ ValidatedTx era,
    PredicateFailure (utxow era) ~ AlonzoPredFail era,
    STS (utxow era),
    -- Supply the HasField and Validate instances for Alonzo
    ShelleyStyleWitnessNeeds era,
    AlonzoStyleAdditions era,
    -- New transaction body fields needed for Alonzo
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era)))
  ) =>
  TransitionRule (utxow era)
alonzoStyleWitness = do
  _u <- shelleyStyleWitness WrappedShelleyEraFailure
  (TRC (ue@(UtxoEnv _slot pp _stakepools _genDelegs), u', tx)) <- judgmentContext
  let txbody = getField @"body" (tx :: TxInBlock era)

  let scriptWitMap = getField @"scriptWits" tx
      failedScripts = Map.foldr accum [] scriptWitMap
        where
          accum script@(TimelockScript _) bad =
            if validateScript @era script tx
              then bad
              else (hashScript @era script) : bad
          accum (PlutusScript _) bad = bad
  null failedScripts ?! (Phase1ScriptWitnessNotValidating $ Set.fromList failedScripts)

  let utxo = _utxo u'
      sphs :: [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
      sphs = scriptsNeeded utxo tx
      unredeemed =
        -- A script is unredeemed, is we can't find the Data that it requires to execute.
        let ans = (filter (not . checkScriptData tx) sphs)
         in seq (rnf ans) ans
  null unredeemed ?! UnRedeemableScripts unredeemed

  let txScriptSet = Map.keysSet scriptWitMap
      needed = Set.fromList [script | (_purpose, script) <- sphs]
  needed == txScriptSet ?! MissingNeededScriptHash (Set.difference needed txScriptSet)

  let inputs = getField @"inputs" txbody :: (Set (TxIn (Crypto era)))
      smallUtxo = eval (inputs ◁ utxo) :: Map.Map (TxIn (Crypto era)) (Core.TxOut era)
      utxoHashes :: [SafeHash (Crypto era) EraIndependentData]
      utxoHashes =
        [ h
          | (_input, output) <- Map.toList smallUtxo,
            SJust h <- [getField @"datahash" output],
            isTwoPhaseScriptAddress @era tx (getField @"address" output)
        ]
      txHashes = domain (txdats . wits' $ tx)
      inputHashes = Set.fromList utxoHashes
  txHashes == inputHashes ?! DataHashSetsDontAgree txHashes inputHashes

  let reqSignerHashes' = getField @"reqSignerHashes" txbody
      witsKeyHashes = unWitHashes $ witsFromTxWitnesses @era tx
  eval (reqSignerHashes' ⊆ witsKeyHashes)
    ?! MissingRequiredSigners (eval $ reqSignerHashes' ➖ witsKeyHashes)

  let languages =
        [ l
          | (_hash, script) <- Map.toList scriptWitMap,
            (not . isNativeScript @era) script,
            Just l <- [language @era script]
        ]
      computedPPhash = hashWitnessPPData pp (Set.fromList languages) (txrdmrs . wits' $ tx)
      bodyPPhash = getField @"wppHash" txbody
  bodyPPhash == computedPPhash ?! PPViewHashesDontMatch bodyPPhash computedPPhash

  trans @(Core.EraRule "UTXO" era) $ TRC (ue, u', tx)

-- ====================================
-- Make the STS instance

data AlonzoUTXOW era

instance
  forall era.
  ( -- Fix some Core types to the Alonzo Era
    TxInBlock era ~ ValidatedTx era,
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    -- New transaction body fields needed for Alonzo
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    -- Supply the HasField and Validate instances for Alonzo
    ShelleyStyleWitnessNeeds era, -- supplies a subset of those needed. All the old Shelley Needs still apply.
    AlonzoStyleAdditions era
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = ValidatedTx era
  type Environment (AlonzoUTXOW era) = UtxoEnv era
  type BaseM (AlonzoUTXOW era) = ShelleyBase
  type
    PredicateFailure (AlonzoUTXOW era) =
      AlonzoPredFail era
  transitionRules = [alonzoStyleWitness]
  initialRules = []

instance
  ( Era era,
    STS (AlonzoUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ Alonzo.UtxoPredicateFailure era,
    BaseM (AlonzoUTXOW era) ~ ShelleyBase,
    PredicateFailure (AlonzoUTXOW era) ~ AlonzoPredFail era
  ) =>
  Embed (AlonzoUTXO era) (AlonzoUTXOW era)
  where
  wrapFailed = WrappedShelleyEraFailure . UtxoFailure
