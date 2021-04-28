{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
   MinGenTxout(..),
   Label(..),
   Sets(..),
 ) where

import Cardano.Binary (ToCBOR (toCBOR),FromCBOR,Annotator)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (HASH)
import Cardano.Ledger.Era (Crypto, ValidateScript (..),TxInBlock)
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.Constraints (UsesPParams(..))
import Shelley.Spec.Ledger.PParams(Update)
import Cardano.Slotting.Slot (SlotNo)
import Data.Coerce (coerce)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Default.Class(Default)
import Shelley.Spec.Ledger.API
  ( Addr (Addr),
    Credential (ScriptHashObj),
    StakeReference (StakeRefBase),
    Block(..),
  )
import Shelley.Spec.Ledger.Address (toAddr)
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe,ShelleyBase)
import Shelley.Spec.Ledger.Tx (TxId (TxId))
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, Wdrl, WitVKey)
import Shelley.Spec.Ledger.UTxO (UTxO)
import Shelley.Spec.Ledger.Keys(KeyRole(Witness))
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core
  ( GenEnv (..),
    genesisCoins,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (someKeyPairs)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (ScriptClass, someScripts)
import Test.Shelley.Spec.Ledger.Utils (Split (..))
import Data.Sequence (Seq)
import Shelley.Spec.Ledger.API
  ( DPState,
    LedgerEnv,
    LedgerState,
    LedgersEnv,
  )
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.STS.Chain(CHAIN,ChainState)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv)
import Control.State.Transition.Extended(STS(..))

import GHC.Records(HasField(..))
import Shelley.Spec.Ledger.BaseTypes(UnitInterval)
import Shelley.Spec.Ledger.PParams(ProtVer)
import Shelley.Spec.Ledger.Slot (EpochNo)
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Cardano.Ledger.Era (Era)
import GHC.Natural(Natural)
import Cardano.Ledger.AuxiliaryData(ValidateAuxiliaryData(..))
import NoThunks.Class(NoThunks)
import Data.Map(Map)


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
    FromCBOR(Annotator (Core.AuxiliaryData era)) -- arises because some pattern Constructors deserialize
  )

type MinGenTxBody era =
  ( Eq (Core.TxBody era),
    ToCBOR (Core.TxBody era),
    NoThunks (Core.TxBody era),
    Show (Core.TxBody era),
    FromCBOR(Annotator (Core.TxBody era)) -- arises because some pattern Constructors deserialize
  )

class MinGenTxout era where
  calcEraMinUTxO :: Core.TxOut era -> Core.PParams era -> Coin
  addValToTxOut :: Core.Value era -> Core.TxOut era -> Core.TxOut era
  genEraTxOut :: Gen (Core.Value era) -> [Addr (Crypto era)] -> Gen [Core.TxOut era]

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
    MinGenTxout era
  ) =>
  EraGen era
  where
  -- | Generate a genesis value for the Era
  genGenesisValue :: GenEnv era -> Gen (Core.Value era)

  -- | Given some pre-generated data, generate an era-specific TxBody,
  -- and a list of additional scripts for eras that sometimes require
  -- additional script witnessing.
  genEraTxBody ::
    GenEnv era ->
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
    Core.TxBody era ->
    Coin ->
    Set (TxIn (Crypto era)) ->
    StrictSeq (Core.TxOut era) ->
    Core.TxBody era

  genEraPParamsDelta :: Constants -> Core.PParams era -> Gen (Core.PParamsDelta era)

  genEraPParams :: Constants -> Gen (Core.PParams era)
   -- Its is VERY IMPORTANT that the decentralisation parameter "_d" be non-zero and less than 1.
   -- The system will deadlock if d==0 and there are no registered stake pools.
   -- use Test.Shelley.Spec.Ledger.Generator.Update(genDecentralisationParam) in your instance.

  genEraWitnesses ::
     (Set (WitVKey 'Witness (Crypto era))) ->
     Map (ScriptHash (Crypto era)) (Core.Script era) ->
     Core.Witnesses era

  unsafeApplyTx :: Core.Tx era -> TxInBlock era



{------------------------------------------------------------------------------
  Generators shared across eras
 -----------------------------------------------------------------------------}

genUtxo0 :: forall era. EraGen era => GenEnv era -> Gen (UTxO era)
genUtxo0 ge@(GenEnv _ c@Constants {minGenesisUTxOouts, maxGenesisUTxOouts}) = do
  genesisKeys <- someKeyPairs c minGenesisUTxOouts maxGenesisUTxOouts
  genesisScripts <- someScripts @era c minGenesisUTxOouts maxGenesisUTxOouts
  outs <-
    (genEraTxOut @era)
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


-- =========================================================


data Label t where
  Body' :: Label (Core.TxBody era)
  Wits' :: Label (Core.Witnesses era)

class Sets (x :: Label t) y where
  set :: Label t -> y -> y