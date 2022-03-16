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

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash, getRwdCred)
import Cardano.Ledger.Alonzo.Data (DataHash)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (language, scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Utxo (AlonzoUTXO)
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo (UtxoEvent, UtxoPredicateFailure)
import Cardano.Ledger.Alonzo.Rules.Utxos (ConcreteAlonzo)
import Cardano.Ledger.Alonzo.Scripts (CostModels, Script (..))
import Cardano.Ledger.Alonzo.Tx
  ( ScriptPurpose,
    ValidatedTx (..),
    hashScriptIntegrity,
    rdptr,
  )
import Cardano.Ledger.Alonzo.TxBody (ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), languages)
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr,
    TxWitness (txdats, txrdmrs),
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData)
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    quorum,
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Hashes (EraIndependentData, EraIndependentTxBody)
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( delegCWitness,
    genesisCWitness,
    poolCWitness,
    requiresVKeyWitness,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState (..),
    WitHashes (..),
    propWits,
    unWitHashes,
    witsFromTxWitnesses,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( UtxowEvent (UtxoEvent),
    UtxowPredicateFailure (..),
    validateNeededWitnesses,
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.Tx (TxIn (..), extractKeyHashWitnessSet)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (DCertDeleg, DCertGenesis, DCertPool),
    PoolCert (RegPool),
    PoolParams (..),
    Wdrl,
    unWdrl,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), txinLookup)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (domain, eval, (⊆), (➖))
import Control.State.Transition.Extended
import Data.Coders
import Data.Foldable (foldr', sequenceA_, toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class
import Validation

-- =================================================

-- | The Predicate failure type in the Alonzo Era. It embeds the Predicate
--   failure type of the Shelley Era, as they share some failure modes.
data UtxowPredicateFail era
  = WrappedShelleyEraFailure !(UtxowPredicateFailure era)
  | MissingRedeemers ![(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
  | MissingRequiredDatums
      !(Set (DataHash (Crypto era))) -- Set of missing data hashes
      !(Set (DataHash (Crypto era))) -- Set of received data hashes
  | NonOutputSupplimentaryDatums
      !(Set (DataHash (Crypto era))) -- Set of unallowed data hashes
      !(Set (DataHash (Crypto era))) -- Set of acceptable supplimental data hashes
  | PPViewHashesDontMatch
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ Computed from the current Protocol Parameters
  | MissingRequiredSigners (Set (KeyHash 'Witness (Crypto era)))
  | UnspendableUTxONoDatumHash (Set (TxIn (Crypto era)))
  | ExtraRedeemers ![RdmrPtr]
  deriving (Generic)

deriving instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "UTXO" era)), -- The Shelley UtxowPredicateFailure needs this to Show
    Show (Core.Script era)
  ) =>
  Show (UtxowPredicateFail era)

deriving instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "UTXO" era)), -- The Shelley UtxowPredicateFailure needs this to Eq
    Eq (Core.Script era)
  ) =>
  Eq (UtxowPredicateFail era)

instance
  ( Era era,
    NoThunks (Core.Script era),
    NoThunks (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  NoThunks (UtxowPredicateFail era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.AuxiliaryData era),
    Typeable (Core.Script era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (UtxowPredicateFail era)
  where
  toCBOR x = encode (encodePredFail x)

newtype AlonzoEvent era
  = WrappedShelleyEraEvent (UtxowEvent era)

encodePredFail ::
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  UtxowPredicateFail era ->
  Encode 'Open (UtxowPredicateFail era)
encodePredFail (WrappedShelleyEraFailure x) = Sum WrappedShelleyEraFailure 0 !> E toCBOR x
encodePredFail (MissingRedeemers x) = Sum MissingRedeemers 1 !> To x
encodePredFail (MissingRequiredDatums x y) = Sum MissingRequiredDatums 2 !> To x !> To y
encodePredFail (NonOutputSupplimentaryDatums x y) = Sum NonOutputSupplimentaryDatums 3 !> To x !> To y
encodePredFail (PPViewHashesDontMatch x y) = Sum PPViewHashesDontMatch 4 !> To x !> To y
encodePredFail (MissingRequiredSigners x) = Sum MissingRequiredSigners 5 !> To x
encodePredFail (UnspendableUTxONoDatumHash x) = Sum UnspendableUTxONoDatumHash 6 !> To x
encodePredFail (ExtraRedeemers x) = Sum ExtraRedeemers 7 !> To x

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  FromCBOR (UtxowPredicateFail era)
  where
  fromCBOR = decode (Summands "(UtxowPredicateFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  Word ->
  Decode 'Open (UtxowPredicateFail era)
decodePredFail 0 = SumD WrappedShelleyEraFailure <! D fromCBOR
decodePredFail 1 = SumD MissingRedeemers <! From
decodePredFail 2 = SumD MissingRequiredDatums <! From <! From
decodePredFail 3 = SumD NonOutputSupplimentaryDatums <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD UnspendableUTxONoDatumHash <! From
decodePredFail 7 = SumD ExtraRedeemers <! From
decodePredFail n = Invalid n

-- ========================================================================
-- Reusable helper functions, and reusable Tests

-- | given the "txscripts" field of the Witnesses, compute the set of languages used in a transaction
langsUsed :: forall era. (Core.Script era ~ Script era, ValidateScript era) => Map.Map (ScriptHash (Crypto era)) (Script era) -> Set Language
langsUsed hashScriptMap =
  Set.fromList
    [ l | (_hash, script) <- Map.toList hashScriptMap, (not . isNativeScript @era) script, Just l <- [language @era script]
    ]

-- =================

{-  { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} ⊆ dom(txdats txw)   -}
missingRequiredDatums ::
  forall era.
  ( HasField
      "datahash"
      (Core.TxOut era)
      (StrictMaybe (SafeHash (Crypto era) EraIndependentData)),
    ValidateScript era,
    Core.Script era ~ Script era,
    ExtendedUTxO era
  ) =>
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  UTxO era ->
  ValidatedTx era ->
  Core.TxBody era ->
  Test (UtxowPredicateFail era)
missingRequiredDatums scriptwits utxo tx txbody = do
  let (inputHashes, txinsNoDhash) = inputDataHashes scriptwits tx utxo
      txHashes = domain (unTxDats . txdats . wits $ tx)
      unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      outputDatumHashes =
        Set.fromList
          [ dh | out <- toList (getField @"outputs" txbody), SJust dh <- [getField @"datahash" out]
          ]
      supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
      (okSupplimentalDHs, notOkSupplimentalDHs) =
        Set.partition (`Set.member` outputDatumHashes) supplimentalDatumHashes
  sequenceA_
    [ failureUnless
        (Set.null txinsNoDhash)
        (UnspendableUTxONoDatumHash txinsNoDhash),
      failureUnless
        (Set.null unmatchedDatumHashes)
        (MissingRequiredDatums unmatchedDatumHashes txHashes),
      failureUnless
        (Set.null notOkSupplimentalDHs)
        (NonOutputSupplimentaryDatums notOkSupplimentalDHs okSupplimentalDHs)
    ]

-- ==================
{-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
hasExactSetOfRedeemers ::
  forall era.
  ( Era era,
    ValidateScript era,
    ExtendedUTxO era,
    Core.Script era ~ Script era,
    Core.Tx era ~ ValidatedTx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  UTxO era ->
  Core.Tx era ->
  Core.TxBody era ->
  Test (UtxowPredicateFail era)
hasExactSetOfRedeemers utxo tx txbody = do
  let redeemersNeeded =
        [ (rp, (sp, sh))
          | (sp, sh) <- Alonzo.scriptsNeeded utxo tx,
            SJust rp <- [rdptr @era txbody sp],
            Just script <- [Map.lookup sh (txscripts utxo tx)],
            (not . isNativeScript @era) script
        ]
      (extraRdmrs, missingRdmrs) =
        extSymmetricDifference
          (Map.keys $ unRedeemers $ txrdmrs $ wits tx)
          id
          redeemersNeeded
          fst
  sequenceA_
    [ failureUnless (null extraRdmrs) (ExtraRedeemers extraRdmrs),
      failureUnless (null missingRdmrs) (MissingRedeemers (map snd missingRdmrs))
    ]

-- ======================
requiredSignersAreWitnessed ::
  forall era.
  ( HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era)))
  ) =>
  Core.TxBody era ->
  WitHashes (Crypto era) ->
  Test (UtxowPredicateFail era)
requiredSignersAreWitnessed txbody witsKeyHashes = do
  let reqSignerHashes' = getField @"reqSignerHashes" txbody
  failureUnless
    (eval (reqSignerHashes' ⊆ unWitHashes witsKeyHashes))
    (MissingRequiredSigners (eval $ reqSignerHashes' ➖ unWitHashes witsKeyHashes))

-- =======================
{-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
ppViewHashesMatch ::
  forall era.
  ( ValidateScript era,
    ExtendedUTxO era,
    Core.Script era ~ Script era,
    Core.Tx era ~ ValidatedTx era,
    HasField "scriptIntegrityHash" (Core.TxBody era) (StrictMaybe (ScriptIntegrityHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) CostModels
  ) =>
  Core.Tx era ->
  Core.TxBody era ->
  Core.PParams era ->
  UTxO era ->
  Test (UtxowPredicateFail era)
ppViewHashesMatch tx txbody pp utxo = do
  let langs = languages @era tx utxo
      computedPPhash = hashScriptIntegrity pp langs (txrdmrs . wits $ tx) (txdats . wits $ tx)
      bodyPPhash = getField @"scriptIntegrityHash" txbody
  failureUnless
    (bodyPPhash == computedPPhash)
    (PPViewHashesDontMatch bodyPPhash computedPPhash)

-- ==============================================================
-- Here we define the transtion function, using reusable tests.
-- The tests are very generic and resusabe, but the transition
-- function is very specific to the Alonzo Era.

-- | A very specialized transitionRule function for the Alonzo Era.
alonzoStyleWitness ::
  forall era.
  ( ValidateScript era,
    ValidateAuxiliaryData era (Crypto era),
    ExtendedUTxO era,
    -- Fix some Core types to the Alonzo Era
    ConcreteAlonzo era,
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era
  ) =>
  TransitionRule (AlonzoUTXOW era)
alonzoStyleWitness = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      txbody = getField @"body" (tx :: Core.Tx era)
      witsKeyHashes = witsFromTxWitnesses @era tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  runTestOnSignal $ Shelley.validateFailedScripts tx

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx))
      sReceived = Map.keysSet (getField @"scriptWits" tx)
  runTest $ Shelley.validateMissingScripts pp sNeeded sReceived

  {- inputHashes := { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} -}
  {-  inputHashes ⊆ dom(txdats txw)  -}
  runTest $ missingRequiredDatums (getField @"scriptWits" tx) utxo tx txbody

  {- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h) ∈ txouts tx -}
  -- This is incorporated into missingRequiredDatums, see the
  -- (failure . UnspendableUTxONoDatumHash) path.

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers utxo tx txbody

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsVKeyNeeded genDelegs utxo tx witsKeyHashes

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded must include the reqSignerHashes in the union   -}
  {- reqSignerHashes txbody ⊆ witsKeyHashes -}
  runTestOnSignal $ requiredSignersAreWitnessed txbody witsKeyHashes

  -- check genesis keys signatures for instantaneous rewards certificates
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  coreNodeQuorum <- liftSTS $ asks quorum
  runTest $
    Shelley.validateMIRInsufficientGenesisSigs genDelegs coreNodeQuorum witsKeyHashes tx

  -- check metadata hash
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  runTestOnSignal $
    Shelley.validateMetadata pp tx

  {- languages txw ⊆ dom(costmdls tx)  -}
  -- This check is checked when building the TxInfo using collectTwoPhaseScriptInputs, if it fails
  -- It raises 'NoCostModel' a construcotr of the predicate failure 'CollectError'. This check
  -- which appears in the spec, seems broken since costmdls is a projection of PPrams, not Tx

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  runTest $ ppViewHashesMatch tx txbody pp utxo

  trans @(Core.EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

-- ================================

-- | Collect the set of hashes of keys that needs to sign a given transaction.
--  This set consists of the txin owners, certificate authors, and withdrawal
--  reward accounts.
--
--  Compared to pre-Alonzo eras, we additionally gather the certificates
--  required to authenticate collateral witnesses.
witsVKeyNeeded ::
  forall era tx.
  ( Era era,
    HasField "body" tx (Core.TxBody era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  UTxO era ->
  tx ->
  GenDelegs (Crypto era) ->
  WitHashes (Crypto era)
witsVKeyNeeded utxo' tx genDelegs =
  WitHashes $
    certAuthors
      `Set.union` inputAuthors
      `Set.union` owners
      `Set.union` wdrlAuthors
      `Set.union` updateKeys
  where
    txbody = getField @"body" tx
    inputAuthors :: Set (KeyHash 'Witness (Crypto era))
    inputAuthors =
      foldr'
        accum
        Set.empty
        ( getField @"inputs" txbody
            `Set.union` getField @"collateral" txbody
        )
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just out ->
              case getTxOutAddr out of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWdrl (getField @"wdrls" txbody))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr' accum Set.empty (getField @"certs" txbody)
      where
        accum (DCertPool (RegPool pool)) ans =
          Set.union
            (Set.map asWitness (_poolOwners pool))
            ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered outby requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (Crypto era))
    certAuthors = foldr' accum Set.empty (getField @"certs" txbody)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (Crypto era))
    updateKeys =
      asWitness
        `Set.map` propWits
          ( strictMaybeToMaybe $
              getField @"update" txbody
          )
          genDelegs

extSymmetricDifference :: (Ord k) => [a] -> (a -> k) -> [b] -> (b -> k) -> ([a], [b])
extSymmetricDifference as fa bs fb = (extraA, extraB)
  where
    intersection = Set.fromList (map fa as) `Set.intersection` Set.fromList (map fb bs)
    extraA = filter (\x -> not $ fa x `Set.member` intersection) as
    extraB = filter (\x -> not $ fb x `Set.member` intersection) bs

-- ====================================
-- Make the STS instance

data AlonzoUTXOW era

instance
  forall era.
  ( ValidateScript era,
    ValidateAuxiliaryData era (Crypto era),
    ExtendedUTxO era,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Fix some Core types to the Alonzo Era
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    ConcreteAlonzo era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = ValidatedTx era
  type Environment (AlonzoUTXOW era) = UtxoEnv era
  type BaseM (AlonzoUTXOW era) = ShelleyBase
  type PredicateFailure (AlonzoUTXOW era) = UtxowPredicateFail era
  type Event (AlonzoUTXOW era) = AlonzoEvent era
  transitionRules = [alonzoStyleWitness]
  initialRules = []

instance
  ( Era era,
    STS (AlonzoUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ Alonzo.UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ Alonzo.UtxoEvent era,
    BaseM (AlonzoUTXOW era) ~ ShelleyBase,
    PredicateFailure (AlonzoUTXOW era) ~ UtxowPredicateFail era,
    Event (AlonzoUTXOW era) ~ AlonzoEvent era
  ) =>
  Embed (AlonzoUTXO era) (AlonzoUTXOW era)
  where
  wrapFailed = WrappedShelleyEraFailure . UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

-- ==========================================================
-- inject instances

instance Inject (UtxowPredicateFail era) (UtxowPredicateFail era) where
  inject = id

instance Inject (UtxowPredicateFailure era) (UtxowPredicateFail era) where
  inject = WrappedShelleyEraFailure
