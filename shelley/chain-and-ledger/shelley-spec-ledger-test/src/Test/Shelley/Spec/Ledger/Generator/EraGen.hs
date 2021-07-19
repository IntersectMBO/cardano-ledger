{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Infrastructure for generating STS Traces over any Era
module Test.Shelley.Spec.Ledger.Generator.EraGen
  ( genUtxo0,
    genesisId,
    EraGen (..),
    MinLEDGER_STS,
    MinCHAIN_STS,
    MinUTXO_STS,
    MinGenTxBody,
    MinGenTxout (..),
    Label (..),
    Sets (..),
    someKeyPairs,
    allScripts,
  )
where

import Cardano.Binary (Annotator, FromCBOR, ToCBOR (toCBOR))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (toAddr)
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash, ValidateAuxiliaryData (..))
import Cardano.Ledger.BaseTypes (Network (..), ShelleyBase, StrictMaybe, UnitInterval)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto, HASH)
import Cardano.Ledger.Era (Crypto, Era, TxInBlock, ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Pretty (PrettyA (..))
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.Constraints (UsesPParams (..))
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Slotting.Slot (SlotNo)
import Control.State.Transition.Extended (STS (..))
import Data.Coerce (coerce)
import Data.Default.Class (Default)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Natural (Natural)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.API
  ( Addr (Addr),
    Block (..),
    Credential (ScriptHashObj),
    DPState,
    KeyPairs,
    LedgerEnv,
    LedgerState,
    LedgersEnv,
    StakeReference (StakeRefBase),
  )
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.PParams (ProtVer, Update)
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainState)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv)
import Shelley.Spec.Ledger.Tx (TxId (TxId))
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, Wdrl, WitVKey)
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.QuickCheck (Gen, choose, shuffle)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    ScriptInfo,
    TwoPhaseInfo (..),
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (ScriptClass, baseScripts, combinedScripts, keyPairs)
import Test.Shelley.Spec.Ledger.Utils (Split (..))

{------------------------------------------------------------------------------
 An EraGen instance makes it possible to run the Shelley property tests. The idea
 is to generate (not fully) random Transactions, i.e. with enough coherency to be
 a real transaction, but which are strung together into Traces. In each step of the trace
 one of these (not fully) random transactions is applied. The idea is that some property should
 hold on any trace. Because we want these tests to work in any Era there are two things
 to consider:
 1) A Transaction generator needs to be parametric over all Eras.
 2) Since the Internals of the STS rules differ from Era to Era, the STS instances
    must also adapt to many Eras.

 To account for the "not fully" random nature of tranactions we use the type GenEnv which
 holds enough information to build "not fully" random transactions that are still coherent.

 For Transactions, we account for these differences by using the type families found in
 Cardano.Ledger.Core and other modules, and by a set of Era specific generators encoded
 in the EraGen class. Generally there is some "method" in the class for each type family.

 For traces we use the "class HasTrace (CHAIN era) (GenEnv era)"

 The following constraints encode the minimal properties needed to build a chain for
 any Era. It should be an invariant that these properties hold for all Eras (Shelley, Allegra, Mary, Alonzo ...)
 If we introduce a new Era, where they do not hold, we must adjust these things, so they do.
 1) Add a new type family
 2) Add new methods to EraGen
 3) Change the minimal constraints, so that they now hold for all Eras
 4) Change the generators to use the new methods.

 -----------------------------------------------------------------------------}

-- | Minimal requirements on the LEDGER and LEDGERS instances
type MinLEDGER_STS era =
  ( Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    BaseM (Core.EraRule "LEDGER" era) ~ ShelleyBase,
    Signal (Core.EraRule "LEDGER" era) ~ TxInBlock era,
    State (Core.EraRule "LEDGER" era) ~ (UTxOState era, DPState (Crypto era)),
    Environment (Core.EraRule "LEDGER" era) ~ LedgerEnv era,
    BaseM (Core.EraRule "LEDGERS" era) ~ ShelleyBase,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (TxInBlock era),
    STS (Core.EraRule "LEDGER" era)
  )

-- | Minimal requirements on the CHAIN instances
type MinCHAIN_STS era =
  ( STS (CHAIN era),
    BaseM (CHAIN era) ~ ShelleyBase,
    Environment (CHAIN era) ~ (),
    State (CHAIN era) ~ ChainState era,
    Signal (CHAIN era) ~ Block era
  )

-- | Minimal requirements on the UTxO instances
type MinUTXO_STS era =
  ( STS (Core.EraRule "UTXOW" era),
    BaseM (Core.EraRule "UTXOW" era) ~ ShelleyBase,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    Signal (Core.EraRule "UTXOW" era) ~ TxInBlock era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    Signal (Core.EraRule "UTXO" era) ~ TxInBlock era
  )

-- | Minimal requirements on Core.PParams to generate random stuff
type MinGenPParams era =
  ( UsesPParams era,
    Default (Core.PParams era),
    HasField "_minPoolCost" (Core.PParams era) Coin,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_minfeeA" (Core.PParams era) Natural,
    HasField "_minUTxOValue" (Core.PParams era) Coin,
    HasField "_minfeeB" (Core.PParams era) Natural
  )

type MinGenWitnesses era =
  ( ToCBOR (Core.Witnesses era),
    Eq (Core.Witnesses era),
    Monoid (Core.Witnesses era)
  )

type MinGenAuxData era =
  ( ValidateAuxiliaryData era (Crypto era),
    ToCBOR (Core.AuxiliaryData era), -- Needs to be serialized
    Eq (Core.AuxiliaryData era),
    Show (Core.AuxiliaryData era),
    FromCBOR (Annotator (Core.AuxiliaryData era)) -- arises because some pattern Constructors deserialize
  )

type MinGenTxBody era =
  ( Eq (Core.TxBody era),
    ToCBOR (Core.TxBody era),
    NoThunks (Core.TxBody era),
    Show (Core.TxBody era),
    FromCBOR (Annotator (Core.TxBody era)) -- arises because some pattern Constructors deserialize
  )

class Show (Core.TxOut era) => MinGenTxout era where
  calcEraMinUTxO :: Core.TxOut era -> Core.PParams era -> Coin
  addValToTxOut :: Core.Value era -> Core.TxOut era -> Core.TxOut era
  genEraTxOut :: GenEnv era -> Gen (Core.Value era) -> [Addr (Crypto era)] -> Gen [Core.TxOut era]

-- ======================================================================================
-- The EraGen class. Generally one method for each type family in Cardano.Ledger.Core
-- ======================================================================================

class
  ( Era era,
    Split (Core.Value era),
    ScriptClass era,
    MinGenPParams era,
    MinGenWitnesses era,
    MinGenAuxData era,
    MinGenTxBody era,
    MinGenTxout era,
    PrettyA (Core.Tx era),
    PrettyA (Core.TxBody era),
    PrettyA (Core.Witnesses era),
    PrettyA (Core.Value era)
  ) =>
  EraGen era
  where
  -- | Generate a genesis value for the Era
  genGenesisValue :: GenEnv era -> Gen (Core.Value era)

  -- | A list of two-phase scripts that can be chosen when building a transaction
  genEraTwoPhaseScripts :: [TwoPhaseInfo era]
  genEraTwoPhaseScripts = []

  -- | Given some pre-generated data, generate an era-specific TxBody,
  -- and a list of additional scripts for eras that sometimes require
  -- additional script witnessing.
  genEraTxBody ::
    GenEnv era ->
    UTxO era ->
    Core.PParams era ->
    SlotNo ->
    Set (TxIn (Crypto era)) ->
    StrictSeq (Core.TxOut era) ->
    StrictSeq (DCert (Crypto era)) ->
    Wdrl (Crypto era) ->
    Coin ->
    StrictMaybe (Update era) ->
    StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
    Gen (Core.TxBody era, [Core.Script era])

  -- | Generate era-specific auxiliary data
  genEraAuxiliaryData :: Constants -> Gen (StrictMaybe (Core.AuxiliaryData era))

  -- | Update an era-specific TxBody
  updateEraTxBody ::
    UTxO era ->
    Core.PParams era ->
    Core.Witnesses era ->
    Core.TxBody era ->
    Coin ->
    -- | This overrides the existing TxFee
    Set (TxIn (Crypto era)) ->
    -- | This is to be Unioned with the existing TxIn
    (Core.TxOut era) ->
    -- | This is to be Appended to the end of the existing TxOut
    Core.TxBody era

  -- |  Union the TxIn with the existing TxIn in the TxBody
  addInputs :: Core.TxBody era -> Set (TxIn (Crypto era)) -> Core.TxBody era
  addInputs txb _ins = txb

  genEraPParamsDelta :: Constants -> Core.PParams era -> Gen (Core.PParamsDelta era)

  genEraPParams :: Constants -> Gen (Core.PParams era)

  -- Its is VERY IMPORTANT that the decentralisation parameter "_d" be non-zero and less than 1.
  -- The system will deadlock if d==0 and there are no registered stake pools.
  -- use Test.Shelley.Spec.Ledger.Generator.Update(genDecentralisationParam) in your instance.

  genEraWitnesses ::
    (UTxO era, Core.TxBody era, ScriptInfo era) ->
    (Set (WitVKey 'Witness (Crypto era))) ->
    Map (ScriptHash (Crypto era)) (Core.Script era) ->
    Core.Witnesses era

  -- When choosing new recipeients from the UTxO, choose only those whose Outputs meet this predicate.
  genEraGoodTxOut :: Core.TxOut era -> Bool
  genEraGoodTxOut _ = True -- The default implementation marks every TxOut as good.

  unsafeApplyTx :: Core.Tx era -> TxInBlock era

  -- | compute the delta cost of an additional script on  per Era basis.
  genEraScriptCost :: Core.PParams era -> Core.Script era -> Coin
  genEraScriptCost _pp _script = Coin 0

  -- | A final opportunity to tweak things when the generator is done. Possible uses
  --   1) Add tracing when debugging on a per Era basis
  genEraDone :: Core.PParams era -> (Core.Tx era) -> Gen (Core.Tx era)
  genEraDone _pp x = pure x

  -- | A final opportunity to tweak things at the block level. Possible uses
  --   2) Run a test that might decide to 'discard' the test, because we got unlucky, and a rare unfixible condition has occurred.
  genEraTweakBlock :: Core.PParams era -> Seq (TxInBlock era) -> Gen (Seq (TxInBlock era))
  genEraTweakBlock _pp seqTx = pure seqTx

{------------------------------------------------------------------------------
  Generators shared across eras
 -----------------------------------------------------------------------------}

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: CC.Crypto crypto => Constants -> Int -> Int -> Gen (KeyPairs crypto)
someKeyPairs c lower upper =
  take
    <$> choose (lower, upper)
    <*> shuffle (keyPairs c)

genUtxo0 :: forall era. EraGen era => GenEnv era -> Gen (UTxO era)
genUtxo0 ge@(GenEnv _ _ c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts}) = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts @era c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    (genEraTxOut @era ge)
      (genGenesisValue @era ge)
      (fmap (toAddr Testnet) genesisKeys ++ fmap (scriptsToAddr' Testnet) genesisScripts)
  return (genesisCoins genesisId outs)
  where
    scriptsToAddr' :: Network -> (Core.Script era, Core.Script era) -> Addr (Crypto era)
    scriptsToAddr' n (payScript, stakeScript) =
      Addr n (scriptToCred' payScript) (StakeRefBase $ scriptToCred' stakeScript)

    scriptToCred' :: Core.Script era -> Credential kr (Crypto era)
    scriptToCred' = ScriptHashObj . hashScript @era

-- | We share this dummy TxId as genesis transaction id across eras
genesisId ::
  Hash.HashAlgorithm (CC.HASH crypto) =>
  TxId crypto
genesisId = TxId (unsafeMakeSafeHash (mkDummyHash 0))
  where
    mkDummyHash :: forall h a. Hash.HashAlgorithm h => Int -> Hash.Hash h a
    mkDummyHash = coerce . Hash.hashWithSerialiser @h toCBOR

-- ==========================================================

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts` (i.e compound scripts) AND
-- some simple scripts (NOT compound. ie either signature or Plutus scripts).
someScripts ::
  forall era.
  EraGen era =>
  Constants ->
  Int ->
  Int ->
  Gen [(Core.Script era, Core.Script era)]
someScripts c lower upper = take <$> choose (lower, upper) <*> shuffle (allScripts @era c)

allScripts :: forall era. EraGen era => Constants -> [(Core.Script era, Core.Script era)]
allScripts c = (zipWith combine genEraTwoPhaseScripts (baseScripts @era c) ++ combinedScripts @era c)
  where
    -- make pairs of scripts (payment,staking) where the payment part is a PlutusScript
    combine :: TwoPhaseInfo era -> (Core.Script era, Core.Script era) -> (Core.Script era, Core.Script era)
    combine info (_, stake) = (getScript info, stake)

-- =========================================================

data Label t where
  Body' :: Label (Core.TxBody era)
  Wits' :: Label (Core.Witnesses era)

class Sets (x :: Label t) y where
  set :: Label t -> y -> y
