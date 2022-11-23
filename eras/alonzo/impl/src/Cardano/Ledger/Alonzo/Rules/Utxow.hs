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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Utxow
  ( AlonzoUTXOW,
    AlonzoUtxowEvent (WrappedShelleyEraEvent),
    AlonzoUtxowPredFailure (..),
    hasExactSetOfRedeemers,
    missingRequiredDatums,
    ppViewHashesMatch,
    requiredSignersAreWitnessed,
    witsVKeyNeeded,
  )
where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash, getRwdCred)
import Cardano.Ledger.Alonzo.Era (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.PParams (getLanguageView)
import Cardano.Ledger.Alonzo.Rules.Utxo
  ( AlonzoUTXO,
    AlonzoUtxoEvent,
    AlonzoUtxoPredFailure,
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), CostModels)
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoEraTx,
    ScriptPurpose,
    hashScriptIntegrity,
    rdptr,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( AlonzoEraTxBody (..),
    ScriptIntegrityHash,
    ShelleyEraTxBody (..),
  )
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), languages)
import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoEraTxWits (..),
    RdmrPtr,
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..), getInputDataHashesTxBody)
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    quorum,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), Test, runTest, runTestOnSignal)
import Cardano.Ledger.Shelley.Delegation.Certificates
  ( delegCWitness,
    genesisCWitness,
    poolCWitness,
    requiresVKeyWitness,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState (..),
    propWits,
    witsFromTxWitnesses,
  )
import Cardano.Ledger.Shelley.Rules
  ( ShelleyUtxowEvent (UtxoEvent),
    ShelleyUtxowPredFailure (..),
    UtxoEnv (..),
    validateNeededWitnesses,
  )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Tx (TxIn (..), extractKeyHashWitnessSet)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (DCertDeleg, DCertGenesis, DCertPool),
    PoolCert (RegPool),
    PoolParams (..),
    unWdrl,
  )
import Cardano.Ledger.Shelley.UTxO (ShelleyScriptsNeeded (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), txinLookup)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (domain, eval, (⊆), (➖))
import Control.State.Transition.Extended
import Data.Foldable (foldr', sequenceA_)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import Lens.Micro
import NoThunks.Class
import Validation

-- =================================================

-- | The Predicate failure type in the Alonzo Era. It embeds the Predicate
--   failure type of the Shelley Era, as they share some failure modes.
data AlonzoUtxowPredFailure era
  = ShelleyInAlonzoUtxowPredFailure !(ShelleyUtxowPredFailure era)
  | -- | List of scripts for which no redeemers were supplied
    MissingRedeemers
      ![(ScriptPurpose (EraCrypto era), ScriptHash (EraCrypto era))]
  | MissingRequiredDatums
      !(Set (DataHash (EraCrypto era)))
      -- ^ Set of missing data hashes
      !(Set (DataHash (EraCrypto era)))
      -- ^ Set of received data hashes
  | NonOutputSupplimentaryDatums
      !(Set (DataHash (EraCrypto era)))
      -- ^ Set of unallowed data hashes
      !(Set (DataHash (EraCrypto era)))
      -- ^ Set of acceptable supplimental data hashes
  | PPViewHashesDontMatch
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
      -- ^ The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
      -- ^ Computed from the current Protocol Parameters
  | -- | Set of witnesses which were needed and not supplied
    MissingRequiredSigners
      (Set (KeyHash 'Witness (EraCrypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (EraCrypto era)))
  | -- | List of redeemers not needed
    ExtraRedeemers
      ![RdmrPtr]
  deriving (Generic)

deriving instance
  ( Era era,
    Show (PredicateFailure (EraRule "UTXO" era)), -- The ShelleyUtxowPredFailure needs this to Show
    Show (Script era)
  ) =>
  Show (AlonzoUtxowPredFailure era)

deriving instance
  ( Era era,
    Eq (PredicateFailure (EraRule "UTXO" era)), -- The ShelleyUtxowPredFailure needs this to Eq
    Eq (Script era)
  ) =>
  Eq (AlonzoUtxowPredFailure era)

instance
  ( Era era,
    NoThunks (Script era),
    NoThunks (PredicateFailure (EraRule "UTXO" era))
  ) =>
  NoThunks (AlonzoUtxowPredFailure era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (TxAuxData era),
    ToCBOR (Script era)
  ) =>
  ToCBOR (AlonzoUtxowPredFailure era)
  where
  toCBOR x = encode (encodePredFail x)

newtype AlonzoUtxowEvent era
  = WrappedShelleyEraEvent (ShelleyUtxowEvent era)

encodePredFail ::
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (Script era),
    Typeable (TxAuxData era)
  ) =>
  AlonzoUtxowPredFailure era ->
  Encode 'Open (AlonzoUtxowPredFailure era)
encodePredFail (ShelleyInAlonzoUtxowPredFailure x) = Sum ShelleyInAlonzoUtxowPredFailure 0 !> E toCBOR x
encodePredFail (MissingRedeemers x) = Sum MissingRedeemers 1 !> To x
encodePredFail (MissingRequiredDatums x y) = Sum MissingRequiredDatums 2 !> To x !> To y
encodePredFail (NonOutputSupplimentaryDatums x y) = Sum NonOutputSupplimentaryDatums 3 !> To x !> To y
encodePredFail (PPViewHashesDontMatch x y) = Sum PPViewHashesDontMatch 4 !> To x !> To y
encodePredFail (MissingRequiredSigners x) = Sum MissingRequiredSigners 5 !> To x
encodePredFail (UnspendableUTxONoDatumHash x) = Sum UnspendableUTxONoDatumHash 6 !> To x
encodePredFail (ExtraRedeemers x) = Sum ExtraRedeemers 7 !> To x

instance
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "UTXO" era)),
    Typeable (Script era),
    Typeable (TxAuxData era)
  ) =>
  FromCBOR (AlonzoUtxowPredFailure era)
  where
  fromCBOR = decode (Summands "(UtxowPredicateFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Script era),
    Typeable (TxAuxData era)
  ) =>
  Word ->
  Decode 'Open (AlonzoUtxowPredFailure era)
decodePredFail 0 = SumD ShelleyInAlonzoUtxowPredFailure <! D fromCBOR
decodePredFail 1 = SumD MissingRedeemers <! From
decodePredFail 2 = SumD MissingRequiredDatums <! From <! From
decodePredFail 3 = SumD NonOutputSupplimentaryDatums <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD UnspendableUTxONoDatumHash <! From
decodePredFail 7 = SumD ExtraRedeemers <! From
decodePredFail n = Invalid n

-- =================

{- { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} ⊆ dom(txdats txw)   -}
{- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h, ) ∈ txouts tx ∪ utxo (refInputs tx) } -}
missingRequiredDatums ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era
  ) =>
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  UTxO era ->
  Tx era ->
  Test (AlonzoUtxowPredFailure era)
missingRequiredDatums scriptWits utxo tx = do
  let txBody = tx ^. bodyTxL
      (inputHashes, txInsNoDataHash) = getInputDataHashesTxBody utxo txBody scriptWits
      txHashes = domain (unTxDats $ tx ^. witsTxL . datsTxWitsL)
      unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      allowedSupplimentalDataHashes = getAllowedSupplimentalDataHashes txBody utxo
      supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
      (okSupplimentalDHs, notOkSupplimentalDHs) =
        Set.partition (`Set.member` allowedSupplimentalDataHashes) supplimentalDatumHashes
  sequenceA_
    [ failureUnless
        (Set.null txInsNoDataHash)
        (UnspendableUTxONoDatumHash txInsNoDataHash),
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
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era
  ) =>
  UTxO era ->
  Tx era ->
  AlonzoScriptsNeeded era ->
  Test (AlonzoUtxowPredFailure era)
hasExactSetOfRedeemers utxo tx (AlonzoScriptsNeeded scriptsNeeded) = do
  let txBody = tx ^. bodyTxL
      redeemersNeeded =
        [ (rp, (sp, sh))
          | (sp, sh) <- scriptsNeeded,
            SJust rp <- [rdptr @era txBody sp],
            Just script <- [Map.lookup sh (txscripts utxo tx)],
            (not . isNativeScript) script
        ]
      (extraRdmrs, missingRdmrs) =
        extSymmetricDifference
          (Map.keys $ unRedeemers $ tx ^. witsTxL . rdmrsTxWitsL)
          id
          redeemersNeeded
          fst
  sequenceA_
    [ failureUnless (null extraRdmrs) (ExtraRedeemers extraRdmrs),
      failureUnless (null missingRdmrs) (MissingRedeemers (map snd missingRdmrs))
    ]

-- ======================
requiredSignersAreWitnessed ::
  AlonzoEraTxBody era =>
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  Test (AlonzoUtxowPredFailure era)
requiredSignersAreWitnessed txBody witsKeyHashes = do
  let reqSignerHashes' = txBody ^. reqSignerHashesTxBodyL
  failureUnless
    (eval (reqSignerHashes' ⊆ witsKeyHashes))
    (MissingRequiredSigners (eval $ reqSignerHashes' ➖ witsKeyHashes))

-- =======================
{-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
ppViewHashesMatch ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era,
    HasField "_costmdls" (PParams era) CostModels
  ) =>
  Tx era ->
  PParams era ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era)) ->
  Test (AlonzoUtxowPredFailure era)
ppViewHashesMatch tx pp utxo sNeeded = do
  let langs = languages @era tx utxo sNeeded
      langViews = Set.map (getLanguageView pp) langs
      txWits = tx ^. witsTxL
      computedPPhash = hashScriptIntegrity langViews (txWits ^. rdmrsTxWitsL) (txWits ^. datsTxWitsL)
      bodyPPhash = tx ^. bodyTxL . scriptIntegrityHashTxBodyL
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
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    Script era ~ AlonzoScript era,
    HasField "_costmdls" (PParams era) CostModels,
    Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody),
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ Tx era
  ) =>
  TransitionRule (AlonzoUTXOW era)
alonzoStyleWitness = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = utxosUtxo u
      txBody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses @era tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  runTestOnSignal $ Shelley.validateFailedScripts tx

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let scriptsNeeded = getScriptsNeeded utxo txBody
      scriptsHashesNeeded = getScriptsHashesNeeded scriptsNeeded
      shelleyScriptsNeeded = ShelleyScriptsNeeded scriptsHashesNeeded
      scriptsReceived = Map.keysSet (tx ^. witsTxL . scriptTxWitsL)
  runTest $ Shelley.validateMissingScripts pp shelleyScriptsNeeded scriptsReceived

  {- inputHashes := { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} -}
  {-  inputHashes ⊆ dom(txdats txw)  -}
  runTest $ missingRequiredDatums (tx ^. witsTxL . scriptTxWitsL) utxo tx

  {- dom(txdats txw) ⊆ inputHashes ∪ {h | ( , , h) ∈ txouts tx -}
  -- This is incorporated into missingRequiredDatums, see the
  -- (failure . UnspendableUTxONoDatumHash) path.

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h ↦ s ∈ txscripts txw, s ∈ Scriptph2}     -}
  runTest $ hasExactSetOfRedeemers utxo tx scriptsNeeded

  -- check VKey witnesses
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txBodyHash ⟧_σ                -}
  runTestOnSignal $ Shelley.validateVerifiedWits tx

  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  runTest $ validateNeededWitnesses witsVKeyNeeded genDelegs utxo tx witsKeyHashes

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded must include the reqSignerHashes in the union   -}
  {- reqSignerHashes txBody ⊆ witsKeyHashes -}
  runTestOnSignal $ requiredSignersAreWitnessed txBody witsKeyHashes

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
  runTest $ ppViewHashesMatch tx pp utxo scriptsHashesNeeded

  trans @(EraRule "UTXO" era) $
    TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)

-- ================================

-- | Collect the set of hashes of keys that needs to sign a given transaction.
--  This set consists of the txin owners, certificate authors, and withdrawal
--  reward accounts.
--
--  Compared to pre-Alonzo eras, we additionally gather the certificates
--  required to authenticate collateral witnesses.
witsVKeyNeeded ::
  forall era.
  (EraTx era, AlonzoEraTxBody era) =>
  UTxO era ->
  Tx era ->
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
witsVKeyNeeded utxo' tx genDelegs =
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
    `Set.union` updateKeys
  where
    txBody = tx ^. bodyTxL
    inputAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    inputAuthors =
      foldr'
        accum
        Set.empty
        ((txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL))
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just txOut ->
              case txOut ^. addrTxOutL of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWdrl (txBody ^. wdrlsTxBodyL))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (EraCrypto era))
    owners = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum (DCertPool (RegPool pool)) ans =
          Set.union
            (Set.map asWitness (ppOwners pool))
            ans
        accum _cert ans = ans
    cwitness (DCertDeleg dc) = extractKeyHashWitnessSet [delegCWitness dc]
    cwitness (DCertPool pc) = extractKeyHashWitnessSet [poolCWitness pc]
    cwitness (DCertGenesis gc) = Set.singleton (asWitness $ genesisCWitness gc)
    cwitness c = error $ show c ++ " does not have a witness"
    -- key reg requires no witness but this is already filtered out by requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    certAuthors = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (EraCrypto era))
    updateKeys =
      asWitness
        `Set.map` propWits
          (strictMaybeToMaybe $ txBody ^. updateTxBodyL)
          genDelegs

extSymmetricDifference :: (Ord k) => [a] -> (a -> k) -> [b] -> (b -> k) -> ([a], [b])
extSymmetricDifference as fa bs fb = (extraA, extraB)
  where
    intersection = Set.fromList (map fa as) `Set.intersection` Set.fromList (map fb bs)
    extraA = filter (\x -> not $ fa x `Set.member` intersection) as
    extraB = filter (\x -> not $ fb x `Set.member` intersection) bs

-- ====================================
-- Make the STS instance

instance
  forall era.
  ( AlonzoEraTx era,
    EraTxAuxData era,
    ExtendedUTxO era,
    EraUTxO era,
    ScriptsNeeded era ~ AlonzoScriptsNeeded era,
    Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody),
    Script era ~ AlonzoScript era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (EraRule "UTXO" era) ~ UtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ Tx era
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = Tx era
  type Environment (AlonzoUTXOW era) = UtxoEnv era
  type BaseM (AlonzoUTXOW era) = ShelleyBase
  type PredicateFailure (AlonzoUTXOW era) = AlonzoUtxowPredFailure era
  type Event (AlonzoUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [alonzoStyleWitness]
  initialRules = []

instance
  ( Era era,
    STS (AlonzoUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ AlonzoUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era,
    BaseM (AlonzoUTXOW era) ~ ShelleyBase,
    PredicateFailure (AlonzoUTXOW era) ~ AlonzoUtxowPredFailure era,
    Event (AlonzoUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (AlonzoUTXO era) (AlonzoUTXOW era)
  where
  wrapFailed = ShelleyInAlonzoUtxowPredFailure . UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

-- ==========================================================
-- inject instances

instance Inject (AlonzoUtxowPredFailure era) (AlonzoUtxowPredFailure era) where
  inject = id

instance Inject (ShelleyUtxowPredFailure era) (AlonzoUtxowPredFailure era) where
  inject = ShelleyInAlonzoUtxowPredFailure
