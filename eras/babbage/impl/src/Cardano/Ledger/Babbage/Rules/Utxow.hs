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

module Cardano.Ledger.Babbage.Rules.Utxow where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash, getRwdCred)
import Cardano.Ledger.Babbage.Data (DataHash)
import Cardano.Ledger.Babbage.Language (Language (..))
import Cardano.Ledger.Babbage.PParams (PParams)
import Cardano.Ledger.Babbage.PlutusScriptApi (language, scriptsNeeded)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUTXO)
import qualified Cardano.Ledger.Babbage.Rules.Utxo as Babbage (UtxoEvent, UtxoPredicateFailure)
import Cardano.Ledger.Babbage.Scripts (Script (..))
import Cardano.Ledger.Babbage.Tx
  ( ScriptPurpose,
    ValidatedTx (..),
    hashScriptIntegrity,
    isTwoPhaseScriptAddress,
    rdptr,
  )
import Cardano.Ledger.Babbage.TxBody (ScriptIntegrityHash)
import Cardano.Ledger.Babbage.TxWitness
  ( RdmrPtr,
    TxWitness (..),
    unRedeemers,
    unTxDats,
  )
import Cardano.Ledger.BaseTypes
  ( ShelleyBase,
    StrictMaybe (..),
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Rules.ValidationMode ((?!#))
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
  ( ShelleyStyleWitnessNeeds,
    UtxowEvent (UtxoEvent),
    UtxowPredicateFailure (..),
    shelleyStyleWitness,
  )
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
import Control.SetAlgebra (domain, eval, (⊆), (◁), (➖))
import Control.State.Transition.Extended
import Data.Coders
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class

-- =================================================

-- | The Predicate failure type in the Babbage Era. It embeds the Predicate
--   failure type of the Shelley Era, as they share some failure modes.
data BabbagePredFail era
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
  Show (BabbagePredFail era)

deriving instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "UTXO" era)), -- The Shelley UtxowPredicateFailure needs this to Eq
    Eq (Core.Script era)
  ) =>
  Eq (BabbagePredFail era)

instance
  ( Era era,
    NoThunks (Core.Script era),
    NoThunks (PredicateFailure (Core.EraRule "UTXO" era))
  ) =>
  NoThunks (BabbagePredFail era)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.AuxiliaryData era),
    Typeable (Core.Script era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (BabbagePredFail era)
  where
  toCBOR x = encode (encodePredFail x)

newtype BabbageEvent era
  = WrappedShelleyEraEvent (UtxowEvent era)

encodePredFail ::
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "UTXO" era)),
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  BabbagePredFail era ->
  Encode 'Open (BabbagePredFail era)
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
  FromCBOR (BabbagePredFail era)
  where
  fromCBOR = decode (Summands "(BabbagePredFail" decodePredFail)

decodePredFail ::
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "UTXO" era)), -- TODO, we should be able to get rid of this constraint
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era)
  ) =>
  Word ->
  Decode 'Open (BabbagePredFail era)
decodePredFail 0 = SumD WrappedShelleyEraFailure <! D fromCBOR
decodePredFail 1 = SumD MissingRedeemers <! From
decodePredFail 2 = SumD MissingRequiredDatums <! From <! From
decodePredFail 3 = SumD NonOutputSupplimentaryDatums <! From <! From
decodePredFail 4 = SumD PPViewHashesDontMatch <! From <! From
decodePredFail 5 = SumD MissingRequiredSigners <! From
decodePredFail 6 = SumD UnspendableUTxONoDatumHash <! From
decodePredFail 7 = SumD ExtraRedeemers <! From
decodePredFail n = Invalid n

-- =============================================

-- | given the "txscripts" field of the Witnesses, compute the set of languages used in a transaction
langsUsed :: forall era. (Core.Script era ~ Script era, ValidateScript era) => Map.Map (ScriptHash (Crypto era)) (Script era) -> Set Language
langsUsed hashScriptMap =
  Set.fromList
    [ l | (_hash, script) <- Map.toList hashScriptMap, (not . isNativeScript @era) script, Just l <- [language @era script]
    ]

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

-- | Constraints to make an Babbage Utxow STS instance
--   (in addition to ShelleyStyleWitnessNeeds)
type BabbageStyleAdditions era =
  ( HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))), -- BE SURE AND ADD THESE INSTANCES
    HasField "scriptIntegrityHash" (Core.TxBody era) (StrictMaybe (ScriptIntegrityHash (Crypto era)))
  )

-- | A somewhat generic STS transitionRule function for the Babbage Era.
alonzoStyleWitness ::
  forall era utxow.
  ( Era era,
    -- Fix some Core types to the Babbage Era
    Core.Tx era ~ ValidatedTx era, -- scriptsNeeded, checkScriptData etc. are fixed at Babbage.Tx
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
    PredicateFailure (utxow era) ~ BabbagePredFail era,
    STS (utxow era),
    -- Supply the HasField and Validate instances for Babbage
    ShelleyStyleWitnessNeeds era,
    BabbageStyleAdditions era,
    -- New transaction body fields needed for Babbage
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (utxow era)
alonzoStyleWitness = do
  (TRC (UtxoEnv _slot pp _stakepools _genDelegs, u', tx)) <- judgmentContext

  {-  (utxo,_,_,_ ) := utxoSt  -}
  {-  txb := txbody tx  -}
  {-  txw := txwits tx  -}
  {-  witsKeyHashes := { hashKey vk | vk ∈ dom(txwitsVKey txw) }  -}
  let utxo = _utxo u'
      txbody = getField @"body" (tx :: Core.Tx era)
      witsKeyHashes = unWitHashes $ witsFromTxWitnesses @era tx

  {-  { h | (_ → (a,_,h)) ∈ txins tx ◁ utxo, isNonNativeScriptAddress tx a} = dom(txdats txw)   -}
  let inputs = getField @"inputs" txbody :: (Set (TxIn (Crypto era)))
      smallUtxo = eval (inputs ◁ utxo) :: Map.Map (TxIn (Crypto era)) (Core.TxOut era)
      twoPhaseOuts =
        [ output
          | (_input, output) <- Map.toList smallUtxo,
            isTwoPhaseScriptAddress @era tx (getField @"address" output)
        ]
      utxoHashes' = mapM (getField @"datahash") twoPhaseOuts
  case utxoHashes' of
    SNothing ->
      -- In the spec, the Nothing value can end up on the left hand side
      -- of the equality check, but we must explicitly rule it out.
      failBecause . UnspendableUTxONoDatumHash . Set.fromList $
        [ input
          | (input, output) <- Map.toList smallUtxo,
            SNothing <- [getField @"datahash" output],
            isTwoPhaseScriptAddress @era tx (getField @"address" output)
        ]
    SJust utxoHashes -> do
      let txHashes = domain (unTxDats . txdats . wits $ tx)
          inputHashes = Set.fromList utxoHashes
          unmatchedDatumHashes = eval (inputHashes ➖ txHashes)
      Set.null unmatchedDatumHashes ?! MissingRequiredDatums unmatchedDatumHashes txHashes

      -- Check that all supplimental datums contained in the witness set appear in the outputs.
      let outputDatumHashes =
            Set.fromList
              [ dh
                | out <- toList $ getField @"outputs" txbody,
                  SJust dh <- [getField @"datahash" out]
              ]
          supplimentalDatumHashes = eval (txHashes ➖ inputHashes)
          (okSupplimentalDHs, notOkSupplimentalDHs) =
            Set.partition (`Set.member` outputDatumHashes) supplimentalDatumHashes
      Set.null notOkSupplimentalDHs
        ?! NonOutputSupplimentaryDatums notOkSupplimentalDHs okSupplimentalDHs

  {-  dom (txrdmrs tx) = { rdptr txb sp | (sp, h) ∈ scriptsNeeded utxo tx,
                           h \mapsto s ∈ txscripts txw, s ∈ Scriptph2}     -}
  let redeemersNeeded =
        [ (rp, (sp, sh))
          | (sp, sh) <- scriptsNeeded utxo tx,
            SJust rp <- [rdptr @era txbody sp],
            Just script <- [Map.lookup sh (getField @"scriptWits" tx)],
            (not . isNativeScript @era) script
        ]
      (extraRdmrs, missingRdmrs) =
        extSymmetricDifference
          (Map.keys $ unRedeemers $ txrdmrs $ wits tx)
          id
          redeemersNeeded
          fst
  null extraRdmrs ?! ExtraRedeemers extraRdmrs
  null missingRdmrs ?! MissingRedeemers (map snd missingRdmrs)

  {-  THIS DOES NOT APPPEAR IN THE SPEC as a separate check, but
      witsVKeyNeeded includes the reqSignerHashes in the union   -}
  let reqSignerHashes' = getField @"reqSignerHashes" txbody
  eval (reqSignerHashes' ⊆ witsKeyHashes)
    ?!# MissingRequiredSigners (eval $ reqSignerHashes' ➖ witsKeyHashes)

  {-  scriptIntegrityHash txb = hashScriptIntegrity pp (languages txw) (txrdmrs txw)  -}
  let languages =
        [ l
          | (_hash, script) <- Map.toList (getField @"scriptWits" tx),
            (not . isNativeScript @era) script,
            Just l <- [language @era script]
        ]
      computedPPhash = hashScriptIntegrity pp (Set.fromList languages) (txrdmrs . wits $ tx) (txdats . wits $ tx)
      bodyPPhash = getField @"scriptIntegrityHash" txbody
  bodyPPhash == computedPPhash ?! PPViewHashesDontMatch bodyPPhash computedPPhash

  {- The shelleyStyleWitness calls the UTXO rule which applies all these rules -}
  {-  ∀ s ∈ range(txscripts txw) ∩ Scriptnative), runNativeScript s tx   -}
  {-  { s | (_,s) ∈ scriptsNeeded utxo tx} = dom(txscripts txw)          -}
  {-  ∀ (vk ↦ σ) ∈ (txwitsVKey txw), V_vk⟦ txbodyHash ⟧_σ                -}
  {-  witsVKeyNeeded utxo tx genDelegs ⊆ witsKeyHashes                   -}
  {-  genSig := { hashKey gkey | gkey ∈ dom(genDelegs)} ∩ witsKeyHashes  -}
  {-  { c ∈ txcerts txb ∩ DCert_mir} ≠ ∅  ⇒ (|genSig| ≥ Quorum) ∧ (d pp > 0)  -}
  {-   adh := txADhash txb;  ad := auxiliaryData tx                      -}
  {-  ((adh = ◇) ∧ (ad= ◇)) ∨ (adh = hashAD ad)                          -}
  shelleyStyleWitness witsVKeyNeeded WrappedShelleyEraFailure

-- | Collect the set of hashes of keys that needs to sign a given transaction.
--  This set consists of the txin owners, certificate authors, and withdrawal
--  reward accounts.
--
--  Compared to pre-Babbage eras, we additionally gather the certificates
--  required to authenticate collateral witnesses.
witsVKeyNeeded ::
  forall era tx.
  ( Era era,
    HasField "body" tx (Core.TxBody era),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "address" (Core.TxOut era) (Addr (Crypto era))
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
      foldr
        accum
        Set.empty
        ( getField @"inputs" txbody
            `Set.union` getField @"collateral" txbody
        )
      where
        accum txin ans =
          case txinLookup txin utxo' of
            Just out ->
              case getField @"address" out of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness (Crypto era))
    wdrlAuthors = Map.foldrWithKey accum Set.empty (unWdrl (getField @"wdrls" txbody))
      where
        accum key _ ans = Set.union (extractKeyHashWitnessSet [getRwdCred key]) ans
    owners :: Set (KeyHash 'Witness (Crypto era))
    owners = foldr accum Set.empty (getField @"certs" txbody)
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
    certAuthors = foldr accum Set.empty (getField @"certs" txbody)
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

data BabbageUTXOW era

instance
  forall era.
  ( -- Fix some Core types to the Babbage Era
    Core.Tx era ~ ValidatedTx era,
    Core.PParams era ~ PParams era,
    Core.Script era ~ Script era,
    -- Allow UTXOW to call UTXO
    Embed (Core.EraRule "UTXO" era) (BabbageUTXOW era),
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    -- New transaction body fields needed for Babbage
    HasField "reqSignerHashes" (Core.TxBody era) (Set (KeyHash 'Witness (Crypto era))),
    HasField "collateral" (Core.TxBody era) (Set (TxIn (Crypto era))),
    -- Supply the HasField and Validate instances for Babbage
    ShelleyStyleWitnessNeeds era, -- supplies a subset of those needed. All the old Shelley Needs still apply.
    Show (Core.TxOut era),
    BabbageStyleAdditions era
  ) =>
  STS (BabbageUTXOW era)
  where
  type State (BabbageUTXOW era) = UTxOState era
  type Signal (BabbageUTXOW era) = ValidatedTx era
  type Environment (BabbageUTXOW era) = UtxoEnv era
  type BaseM (BabbageUTXOW era) = ShelleyBase
  type PredicateFailure (BabbageUTXOW era) = BabbagePredFail era
  type Event (BabbageUTXOW era) = BabbageEvent era
  transitionRules = [alonzoStyleWitness]
  initialRules = []

instance
  ( Era era,
    STS (BabbageUTXO era),
    PredicateFailure (Core.EraRule "UTXO" era) ~ Babbage.UtxoPredicateFailure era,
    Event (Core.EraRule "UTXO" era) ~ Babbage.UtxoEvent era,
    BaseM (BabbageUTXOW era) ~ ShelleyBase,
    PredicateFailure (BabbageUTXOW era) ~ BabbagePredFail era,
    Event (BabbageUTXOW era) ~ BabbageEvent era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = WrappedShelleyEraFailure . UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent
