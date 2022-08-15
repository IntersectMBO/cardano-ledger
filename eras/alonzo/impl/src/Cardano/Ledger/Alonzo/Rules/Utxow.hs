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

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash, getRwdCred)
import Cardano.Ledger.Alonzo.Era (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.PParams (getLanguageView)
import Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo (scriptsNeeded)
import Cardano.Ledger.Alonzo.Rules.Utxo
  ( AlonzoUTXO,
    AlonzoUtxoEvent,
    AlonzoUtxoPredFailure,
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), CostModels)
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoTx (..),
    ScriptPurpose,
    hashScriptIntegrity,
    rdptr,
  )
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), ScriptIntegrityHash, ShelleyEraTxBody (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), languages)
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr,
    TxWitness (txdats, txrdmrs),
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.BaseTypes
  ( ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    quorum,
    strictMaybeToMaybe,
  )
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
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( ShelleyUtxowEvent (UtxoEvent),
    ShelleyUtxowPredFailure (..),
    validateNeededWitnesses,
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.Tx (TxIn (..), extractKeyHashWitnessSet)
import Cardano.Ledger.Shelley.TxBody
  ( DCert (DCertDeleg, DCertGenesis, DCertPool),
    PoolCert (RegPool),
    PoolParams (..),
    unWdrl,
  )
import Cardano.Ledger.Shelley.UTxO (UTxO (..), txinLookup)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (domain, eval, (⊆), (➖))
import Control.State.Transition.Extended
import Data.Coders
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
      ![(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
  | MissingRequiredDatums
      !(Set (DataHash (Crypto era)))
      -- ^ Set of missing data hashes
      !(Set (DataHash (Crypto era)))
      -- ^ Set of received data hashes
  | NonOutputSupplimentaryDatums
      !(Set (DataHash (Crypto era)))
      -- ^ Set of unallowed data hashes
      !(Set (DataHash (Crypto era)))
      -- ^ Set of acceptable supplimental data hashes
  | PPViewHashesDontMatch
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ The PPHash in the TxBody
      !(StrictMaybe (ScriptIntegrityHash (Crypto era)))
      -- ^ Computed from the current Protocol Parameters
  | -- | Set of witnesses which were needed and not supplied
    MissingRequiredSigners
      (Set (KeyHash 'Witness (Crypto era)))
  | -- | Set of transaction inputs that are TwoPhase scripts, and should have a DataHash but don't
    UnspendableUTxONoDatumHash
      (Set (TxIn (Crypto era)))
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
    Typeable (AuxiliaryData era),
    Typeable (Script era),
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
    Typeable (AuxiliaryData era)
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
    Typeable (AuxiliaryData era)
  ) =>
  FromCBOR (AlonzoUtxowPredFailure era)
  where
  fromCBOR = decode (Summands "(UtxowPredicateFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Script era),
    Typeable (AuxiliaryData era)
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
  ( EraTx era,
    Script era ~ AlonzoScript era,
    ExtendedUTxO era
  ) =>
  Map.Map (ScriptHash (Crypto era)) (Script era) ->
  UTxO era ->
  AlonzoTx era ->
  TxBody era ->
  Test (AlonzoUtxowPredFailure era)
missingRequiredDatums scriptwits utxo tx txbody = do
  let (inputHashes, txinsNoDhash) = inputDataHashes scriptwits tx utxo
      txHashes = domain (unTxDats . txdats . wits $ tx)
      unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      allowedSupplimentalDataHashes = getAllowedSupplimentalDataHashes txbody utxo
      supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
      (okSupplimentalDHs, notOkSupplimentalDHs) =
        Set.partition (`Set.member` allowedSupplimentalDataHashes) supplimentalDatumHashes
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
  ( EraTx era,
    ShelleyEraTxBody era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era,
    Tx era ~ AlonzoTx era
  ) =>
  UTxO era ->
  Tx era ->
  TxBody era ->
  Test (AlonzoUtxowPredFailure era)
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
  ( AlonzoEraTxBody era
  ) =>
  TxBody era ->
  Set (KeyHash 'Witness (Crypto era)) ->
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
  ( AlonzoEraTxBody era,
    ExtendedUTxO era,
    Script era ~ AlonzoScript era,
    Tx era ~ AlonzoTx era,
    HasField "_costmdls" (PParams era) CostModels
  ) =>
  Tx era ->
  TxBody era ->
  PParams era ->
  UTxO era ->
  Set (ScriptHash (Crypto era)) ->
  Test (AlonzoUtxowPredFailure era)
ppViewHashesMatch tx txBody pp utxo sNeeded = do
  -- FIXME: No need to supply txBody as a separate argument: txBody = tx ^. bodyTxL
  let langs = languages @era tx utxo sNeeded
      langViews = Set.map (getLanguageView pp) langs
      computedPPhash = hashScriptIntegrity langViews (txrdmrs . wits $ tx) (txdats . wits $ tx)
      bodyPPhash = txBody ^. scriptIntegrityHashTxBodyL
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
  ( EraTx era,
    AlonzoEraTxBody era,
    ExtendedUTxO era,
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    TxWits era ~ TxWitness era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (EraRule "UTXO" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ AlonzoTx era
  ) =>
  TransitionRule (AlonzoUTXOW era)
alonzoStyleWitness = do
  (TRC (UtxoEnv slot pp stakepools genDelegs, u, tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u
      txbody = tx ^. bodyTxL
      witsKeyHashes = witsFromTxWitnesses @era tx

  -- check scripts
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  runTestOnSignal $ Shelley.validateFailedScripts tx

  {-  { h | (_,h) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  let sNeeded = Set.fromList (map snd (Alonzo.scriptsNeeded utxo tx))
      sReceived = Map.keysSet (tx ^. witsTxL . scriptWitsL)
  runTest $ Shelley.validateMissingScripts pp sNeeded sReceived

  {- inputHashes := { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isTwoPhaseScriptAddress tx a} -}
  {-  inputHashes ⊆ dom(txdats txw)  -}
  runTest $ missingRequiredDatums (tx ^. witsTxL . scriptWitsL) utxo tx txbody

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
  runTest $ ppViewHashesMatch tx txbody pp utxo sNeeded

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
  GenDelegs (Crypto era) ->
  Set (KeyHash 'Witness (Crypto era))
witsVKeyNeeded utxo' tx genDelegs =
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
    `Set.union` updateKeys
  where
    txBody = tx ^. bodyTxL
    inputAuthors :: Set (KeyHash 'Witness (Crypto era))
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

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWdrl (txBody ^. wdrlsTxBodyL))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr' accum Set.empty (txBody ^. certsTxBodyL)
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
    -- key reg requires no witness but this is already filtered out by requiresVKeyWitness
    -- before the call to `cwitness`, so this error should never be reached.

    certAuthors :: Set (KeyHash 'Witness (Crypto era))
    certAuthors = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert ans | requiresVKeyWitness cert = Set.union (cwitness cert) ans
        accum _cert ans = ans
    updateKeys :: Set (KeyHash 'Witness (Crypto era))
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
  ( EraTx era,
    AlonzoEraTxBody era,
    EraAuxiliaryData era,
    ExtendedUTxO era,
    Signable (DSIGN (Crypto era)) (Hash (HASH (Crypto era)) EraIndependentTxBody),
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    TxWits era ~ TxWitness era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (AlonzoUTXOW era),
    Environment (EraRule "UTXO" era) ~ ShelleyUtxoEnv era,
    State (EraRule "UTXO" era) ~ UTxOState era,
    Signal (EraRule "UTXO" era) ~ AlonzoTx era
  ) =>
  STS (AlonzoUTXOW era)
  where
  type State (AlonzoUTXOW era) = UTxOState era
  type Signal (AlonzoUTXOW era) = AlonzoTx era
  type Environment (AlonzoUTXOW era) = ShelleyUtxoEnv era
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
