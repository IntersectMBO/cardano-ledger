{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | = Overview
--
-- the `Test.Cardano.Ledger.Model` namespace exists to enable QuickCheck style
-- property based testing, without having the obstacles that come from directly
-- stating those properties in terms of the types in the primary implementation.
-- The problems are, in no particular order:
--
--  - the ledger and transaction types are large records with many degrees of
--    freedom; most combinations are invalid.  the spec mostly defines which
--    ledgers are valid in terms of which states are reachable from initial
--    conditions via valid transactions, and which transactions are valid in terms
--    of the ledger transition rules.
--  - The use of cryptographic hashing in generated values interfere with
--    shrinking; any modification of a transaction demands a corresponding change
--    to the ledger state.
--  - testing era specific logic requires considerable effort to adapt to new eras,
--    whereas era agnostic API's are not able to interact with features that
--    differ between eras under test.
--
--
-- These issues are addressed in the Ledger Model by way of separating the
-- generation of test inputs and checking those inputs for required properties into
-- different types; with the generation step abstracting out many of the issues
-- that obstruct its use with quickcheck.  A summary of those differences is:
--
--  - A separate hierarchy of types for most ledger concepts which mirror the
--    types in the "real" implementation.
--  - conversion from model types to real types is mediated by the classes and
--    functions in the `Test.Cardano.Ledger.Model.Elaborators` namespace.
--    conversion that can use era agnostic API's are in the form of default
--    methods on the corresponding classes, and the parts that require era
--    specific types are left to the per-era instances of those classes.
--  - A type parameter is used as a "flag" to enable or select which features
--    apply to the era being modeled in a single heirarchy of types for all eras.
--    in contrast with the "real" implementation uses a type parameter for the
--    era; with type families determining which types appear in the heirarchy of
--    types that are used in that implementation.  The model parameter represents
--    only which features are present/absent, rather than specifying a particular
--    era.
--  - no cryptography, the model instead uses surrogate keys of one form or
--    another in place of hashes and signatures.
--
-- An overview of how a property is checked in this model:
--
-- * "Test.Cardano.Ledger.Model.FeatureSet" : Which features are available in an
--    era are encoded in the era type used in the model.
-- * "Test.Cardano.Ledger.Model.Rules" : The spec is reimplemented in terms of
--    the model, to streamline other steps.
-- * "Test.Cardano.Ledger.Model.API" : this module, which provides a reasonable
--    environment to run the model implementation
-- * "Test.Cardano.Ledger.Model.Generators" : the collection of quickcheck
--    generators.
-- * "Test.Cardano.Ledger.Model.Elaborators" : generated models are converted to
--      real transactions and ledger states.
-- * "Test.Cardano.Ledger.Model.Properties" : elaborated results are checked,
--    both regular properties of the elaborated types, and that the behaviour
--    between models and elaborated ledgerstates agree.
--
--
--
-- = Adding a new era to the ledger model.
--
-- There are two "steps" to adding a new era, providing a new instance to
-- `Test.Cardano.Ledger.Model.Elaborators.ElaborateEraModel` for the new era, and
-- then extending the model to support features that are new in the new era.
--
-- The former is a fairly mechanical process of giving the implementation of the
-- era specific types, whereas the latter depends on what's being changed in the
-- new era.
--
-- In case new data types become available, or the cardinality changes between
-- eras, it will probably be needed to extend the `FeatureSet` type to capture that
-- change.  Feature support GADTs can then be used in places in the model data
-- types where the presence or absence depends on the feature in question.  This
-- should also have a downstream effect on the ElaborateEraModel instances; and
-- that class will need to be adjusted to account for the new data.
--
-- If instead a change of behavior is to be expressed, in a way that's visible in
-- the ledgerstate itself (as with the protocol version), then model versions of
-- the conditional behavior should be recreated in
-- "Test.Cardano.Ledger.Model.Rules", and will likely need to be mirrored in
-- "Test.Cardano.Ledger.Model.Generators".
module Test.Cardano.Ledger.Model.API
  ( -- | = Top level model types
    ModelBlock (..),
    ModelEpoch (..),
    ModelGenesis (..),
    -- | = User facing model API
    applyModelEpoch,
    applyModelBlock,
    applyModelDCert,
    applyModelTx,
    applyModelTick,
    applyModelBlocksMade,
    -- | = Model emulation data types.
    ModelLedger (..),
    HasModelLedger (..),
    mkModelLedger,
    HasModelM,
    ModelM (..),
    execModelM,
    execModelMWithProv,
    modelM,
    -- | = miscelaneous lenses.
    getModelLedger_epoch,
    getModelLedger_rewards,
    getModelLedger_utxos,
    modelBlock_slot,
    modelEpoch_blocksMade,
    modelGenesis_pp,
    modelGenesis_utxos,
    modelLedger_nes,
    modelLedger_slotOffset,
    modelMWithProv,
    modelMWithProvT,
    modelEpoch_blocks,
    modelBlock_txSeq,
  )
where

import Cardano.Ledger.BaseTypes
  ( Globals (..),
    epochInfo,
  )
import Cardano.Ledger.Coin
  ( Coin,
    word64ToCoin,
  )
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo.API
  ( epochInfoFirst,
  )
import Cardano.Slotting.Slot
  ( EpochNo (..),
    SlotNo (..),
  )
import Control.DeepSeq (NFData)
import Control.Lens
  ( Lens',
    foldOf,
    forOf_,
    ifor_,
    lens,
    set,
    to,
    use,
    uses,
    view,
    (%=),
    (.=),
    (<<.=),
    (<>=),
    _2,
  )
import qualified Control.Monad.RWS.CPS as RWS
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Writer.Class (MonadWriter)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Model.Acnt (ModelAcntF (..))
import Test.Cardano.Ledger.Model.BaseTypes
  ( HasGlobals,
    ModelBlocksMade,
    PreservedAda (..),
    getGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet,
    KnownRequiredFeatures,
    KnownScriptFeature,
    RequiredFeatures (..),
    ScriptFeature,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( ModelDPState (..),
    ModelDState (..),
    ModelEpochState (..),
    ModelGenesisDelegation,
    ModelLState (..),
    ModelNewEpochState (..),
    ModelPState (..),
    ModelSnapshot (..),
    ModelSnapshots (..),
    ModelUTxOState (..),
    modelDPState_dstate,
    modelDState_rewards,
    modelEpochState_ls,
    modelLState_dpstate,
    modelLState_utxoSt,
    modelNewEpochState_bCur,
    modelNewEpochState_el,
    modelNewEpochState_es,
    modelUTxOState_utxo,
  )
import Test.Cardano.Ledger.Model.PParams
  ( ModelPParams,
    getModelPParams,
  )
import Test.Cardano.Ledger.Model.Prov
  ( ModelProvenanceState,
    MonadModelProvenance,
    SomeMonadTrans (..),
  )
import Test.Cardano.Ledger.Model.Rules
  ( ModelDPSEnv (..),
    ModelLEnv (..),
    ModelRule (..),
    applyRule,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress,
    ModelCredential,
  )
import Test.Cardano.Ledger.Model.Tx
  ( HasModelDCert (..),
    HasModelTx (..),
    ModelDCert,
    ModelTx,
    modelTx,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelUTxOId,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap,
    emptyUTxOMap,
    mkModelUTxOMap,
  )

-- | Model version of Block; which from the view of the model, is all of the
-- transactions for a particular slot.  block metadata is elsewhere.
data ModelBlock era = ModelBlock
  { -- | block position in the epoch.  this is relative to the start of the
    -- epoch, not an absolute slot number
    _modelBlock_slot :: SlotNo,
    _modelBlock_txSeq :: [ModelTx era]
  }
  deriving (Show, Generic, Eq)

instance NFData (ModelBlock era)

instance HasModelDCert era (ModelBlock era) where
  modelDCerts = modelBlock_txSeq . traverse . modelDCerts
  {-# INLINE modelDCerts #-}

instance HasModelTx era (ModelBlock era) where
  modelTxs = modelBlock_txSeq . traverse
  {-# INLINE modelTxs #-}

modelBlock_slot :: Lens' (ModelBlock era) SlotNo
modelBlock_slot = lens _modelBlock_slot (\s b -> s {_modelBlock_slot = b})
{-# INLINE modelBlock_slot #-}

modelBlock_txSeq :: Lens' (ModelBlock era) [ModelTx era]
modelBlock_txSeq = lens _modelBlock_txSeq (\s b -> s {_modelBlock_txSeq = b})
{-# INLINE modelBlock_txSeq #-}

instance RequiredFeatures ModelBlock where
  filterFeatures tag (ModelBlock slotNo txns) =
    ModelBlock slotNo
      <$> traverse (filterFeatures tag) txns

instance RequiredFeatures ModelEpoch where
  filterFeatures tag (ModelEpoch blocks x) =
    ModelEpoch
      <$> traverse (filterFeatures tag) blocks
      <*> pure x

-- TODO: explicit Epoch.
data ModelEpoch era = ModelEpoch
  { _modelEpoch_blocks :: [ModelBlock era],
    _modelEpoch_blocksMade :: ModelBlocksMade
  }
  deriving (Show, Generic, Eq)

instance NFData (ModelEpoch era)

instance HasModelDCert era (ModelEpoch era) where
  modelDCerts = modelEpoch_blocks . traverse . modelDCerts
  {-# INLINE modelDCerts #-}

instance HasModelTx era (ModelEpoch era) where
  modelTxs = modelEpoch_blocks . traverse . modelTxs
  {-# INLINE modelTxs #-}

modelEpoch_blocks :: Lens' (ModelEpoch era) [ModelBlock era]
modelEpoch_blocks = lens _modelEpoch_blocks (\s b -> s {_modelEpoch_blocks = b})
{-# INLINE modelEpoch_blocks #-}

modelEpoch_blocksMade :: Lens' (ModelEpoch era) ModelBlocksMade
modelEpoch_blocksMade = lens _modelEpoch_blocksMade (\s b -> s {_modelEpoch_blocksMade = b})
{-# INLINE modelEpoch_blocksMade #-}

-- Approximates Model versions of other ledger API's

class HasModelLedger era a | a -> era where
  modelLedger :: Lens' a (ModelLedger era)

instance HasModelLedger era (ModelLedger era) where
  modelLedger = id

data ModelLedger era = ModelLedger
  { -- | offset from first slot in epoch
    _modelLedger_slotOffset :: !SlotNo,
    _modelLedger_nes :: !(ModelNewEpochState era)
  }
  deriving (Eq, Show, Generic)

instance NFData (ModelLedger era)

applyModelTick :: HasModelM era st r m => SlotNo -> m ()
applyModelTick mslot = do
  globals <- asks getGlobals
  let ei = epochInfo globals
  Identity firstSlot <- epochInfoFirst ei <$> use (modelLedger . to getModelLedger_epoch)
  let slot = firstSlot + mslot

  modelLedger . modelLedger_slotOffset %= max mslot

  nes <- use $ modelLedger . modelLedger_nes
  (nes', _errs') <-
    applyRule
      (Proxy @'ModelRule_TICK)
      (Const slot)
      globals
      Proxy
      nes
  -- TODO: unless (null $ errs') sulk
  modelLedger . modelLedger_nes .= nes'

applyModelBlock :: HasModelM era st r m => ModelBlock era -> m ()
applyModelBlock (ModelBlock mslot txs) = do
  applyModelTick mslot
  ifor_ txs (applyModelTx mslot)

-- TODO: take _mtxValidity into account
applyModelTx :: HasModelM era st r m => SlotNo -> Int -> ModelTx era -> m ()
applyModelTx slot txIx tx = do
  globals <- asks getGlobals

  es <- use $ modelLedger . modelLedger_nes . modelNewEpochState_es
  (ls', _errs) <-
    applyRule
      (Proxy @'ModelRule_LEDGER)
      tx
      globals
      (ModelLEnv slot txIx $ getModelPParams es)
      (_modelEpochState_ls es)
  -- TODO: unless (null errs) sulk
  let es' = set modelEpochState_ls ls' es
  modelLedger . modelLedger_nes . modelNewEpochState_es .= es'

applyModelBlocksMade :: forall era m st r. HasModelM era st r m => ModelBlocksMade -> m ()
applyModelBlocksMade blocksMade = do
  epochNo <- use $ modelLedger . to getModelLedger_epoch

  -- we "emulate" blocks made
  modelLedger . modelLedger_nes . modelNewEpochState_bCur <>= blocksMade

  currentSlotOffset <- modelLedger . modelLedger_slotOffset <<.= 0

  globals <- asks getGlobals
  let ei = epochInfo globals
      prevEpoch = epochNo
      epoch = succ epochNo
      firstOfOld = runIdentity $ epochInfoFirst ei prevEpoch
      firstOfNew = runIdentity $ epochInfoFirst ei epoch

      -- make sure we do a reward update every epoch, even if the model hasn't
      -- any slots.
      neededSlot = SlotNo (randomnessStabilisationWindow globals) + firstOfOld
      currentSlot = firstOfOld + currentSlotOffset

  nes <- use $ modelLedger . modelLedger_nes

  (nes', errs') <-
    if currentSlot > neededSlot
      then pure (nes, mempty)
      else
        applyRule
          (Proxy @'ModelRule_TICK)
          (Const neededSlot + 1)
          globals
          Proxy
          nes
  (nes'', errs'') <-
    applyRule
      (Proxy @'ModelRule_TICK)
      (Const firstOfNew)
      globals
      Proxy
      nes'
  let errs = errs' <> errs''

  if null errs
    then modelLedger . modelLedger_nes .= nes''
    else error $ show errs

-- | Run a single 'ModelEpoch'.
--
-- This API runs "as if" every tick in the epoch occurs, even if the ModelEpoch
-- has no blocks in the slots where important events occur.
applyModelEpoch :: HasModelM era st r m => ModelEpoch era -> m ()
applyModelEpoch epoch = do
  forOf_ (modelEpoch_blocks . traverse) epoch applyModelBlock
  applyModelBlocksMade (_modelEpoch_blocksMade epoch)

type ModelLedgerError :: FeatureSet -> Type
type ModelLedgerError era = ()

newtype ModelM era m a = ModelM {unModelM :: RWS.RWST Globals (ModelLedgerError era) (ModelLedger era) m a}
  deriving newtype (Functor, Applicative, Monad)

runModelM ::
  MonadModelProvenance era provM =>
  ModelM era provM a ->
  Globals ->
  ModelLedger era ->
  provM (Either (ModelLedgerError era) a, ModelLedger era)
runModelM (ModelM k) r s = do
  (a, s', w) <- RWS.runRWST k r s
  pure $
    if (w == mempty)
      then (Right a, s')
      else (Left w, s)

deriving newtype instance Monad m => MonadReader Globals (ModelM era m)

deriving newtype instance Monad m => MonadWriter (ModelLedgerError era) (ModelM era m)

deriving newtype instance Monad m => MonadState (ModelLedger era) (ModelM era m)

deriving via
  (SomeMonadTrans (RWS.RWST Globals (ModelLedgerError era) (ModelLedger era)) m)
  instance
    MonadModelProvenance era m => MonadModelProvenance era (ModelM era m)

execModelM ::
  forall era.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m ()) ->
  Globals ->
  ModelLedger era ->
  ModelLedger era
execModelM k r s = snd $ modelM k r s

modelM ::
  forall era a.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m a) ->
  Globals ->
  ModelLedger era ->
  (a, ModelLedger era)
modelM k r s =
  let k' :: ModelM era (Tagged era) a
      k' = k
      Tagged (x, s') = runModelM k' r s
   in (either (error . (<> "modelM:") . show) id x, s')

execModelMWithProv ::
  forall era.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m ()) ->
  Globals ->
  (ModelProvenanceState era, ModelLedger era) ->
  (ModelProvenanceState era, ModelLedger era)
execModelMWithProv k r s = snd $ modelMWithProv k r s

modelMWithProv ::
  forall era a.
  KnownRequiredFeatures era =>
  (forall m. HasModelM era (ModelLedger era) Globals m => m a) ->
  Globals ->
  (ModelProvenanceState era, ModelLedger era) ->
  (a, (ModelProvenanceState era, ModelLedger era))
modelMWithProv k r (p, s) =
  let k' :: ModelM era (State.State (ModelProvenanceState era)) a
      k' = k
      ((x, s'), p') = State.runState (runModelM k' r s) p
   in (either (error . (<> "modelM:") . show) id x, (p', s'))

modelMWithProvT ::
  forall era a m.
  (KnownRequiredFeatures era, Monad m) =>
  (ModelM era (State.StateT (ModelProvenanceState era) m) a) ->
  Globals ->
  (ModelProvenanceState era, ModelLedger era) ->
  m (a, (ModelProvenanceState era, ModelLedger era))
modelMWithProvT k r (p, s) = do
  ((x, s'), p') <- State.runStateT (runModelM k r s) p
  pure (either (error . (<> "modelM:") . show) id x, (p', s'))

-- | Just enough context to evaluate a model transaction.
type HasModelM era st r m =
  ( MonadReader r m,
    HasGlobals r,
    MonadState st m,
    HasModelLedger era st,
    KnownRequiredFeatures era,
    MonadModelProvenance era m
  )

mkModelLedger ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Globals ->
  ModelGenesis era ->
  ModelLedger era
mkModelLedger globals (ModelGenesis pp genDelegs utxos) =
  ModelLedger
    { _modelLedger_nes = nes,
      _modelLedger_slotOffset = 0
    }
  where
    utxos' = mkModelUTxOMap utxos
    nes =
      ModelNewEpochState
        { _modelNewEpochState_el = 0,
          _modelNewEpochState_bPrev = mempty,
          _modelNewEpochState_bCur = mempty,
          _modelNewEpochState_es =
            ModelEpochState
              { _modelEpochState_acnt =
                  ModelAcnt
                    { _modelAcnt_treasury = Val.zero,
                      _modelAcnt_reserves = reserves
                    },
                _modelEpochState_ss =
                  ModelSnapshots
                    { _modelSnapshots_pstake =
                        pure
                          ModelSnapshot
                            { _modelSnapshot_stake = Map.empty,
                              _modelSnapshot_delegations = Map.empty,
                              _modelSnapshot_pools = Map.empty,
                              _modelSnapshot_utxos = emptyUTxOMap,
                              _modelSnapshot_rewards = Map.empty
                            },
                      _modelSnapshots_feeSS = mempty
                    },
                _modelEpochState_ls =
                  ModelLState
                    { _modelLState_utxoSt =
                        ModelUTxOState
                          { _modelUTxOState_utxo = utxos',
                            _modelUTxOState_deposited = mempty,
                            _modelUTxOState_fees = mempty
                          },
                      _modelLState_dpstate =
                        ModelDPState
                          { _modelDPState_dstate =
                              ModelDState
                                { _modelDState_rewards = Map.empty,
                                  _modelDState_delegations = Map.empty,
                                  _modelDState_fGenDelegs = Map.empty,
                                  _modelDState_genDelegs = genDelegs,
                                  _modelDState_iRwd = mempty
                                },
                            _modelDPState_pstate =
                              ModelPState
                                { _modelPState_poolParams = Map.empty,
                                  _modelPState_fPoolParams = Map.empty,
                                  _modelPState_retiring = Map.empty
                                }
                          }
                    },
                _modelEpochState_prevPp = pp,
                _modelEpochState_pp = pp
              },
          _modelNewEpochState_ru = Nothing
        }

    reserves =
      word64ToCoin (maxLovelaceSupply globals)
        ~~ foldOf (traverse . _2) utxos

data ModelGenesis era = ModelGenesis
  { _modelGenesis_pp :: !(ModelPParams era),
    _modelGenesis_genDelegs :: !ModelGenesisDelegation,
    _modelGenesis_utxos :: !(Map.Map ModelUTxOId (ModelAddress (ScriptFeature era), Coin))
  }
  deriving (Show, Generic)

modelGenesis_pp :: Lens' (ModelGenesis era) (ModelPParams era)
modelGenesis_pp = lens _modelGenesis_pp $ \s b -> s {_modelGenesis_pp = b}

modelGenesis_utxos :: Lens' (ModelGenesis era) (Map.Map ModelUTxOId (ModelAddress (ScriptFeature era), Coin))
modelGenesis_utxos = lens _modelGenesis_utxos $ \s b -> s {_modelGenesis_utxos = b}

instance NFData (ModelGenesis era)

instance PreservedAda (ModelLedger era) where
  totalPreservedAda = totalPreservedAda . _modelLedger_nes

applyModelDCert ::
  HasModelM era st r m =>
  ModelDCert era ->
  m ()
applyModelDCert dCert = do
  globals <- asks getGlobals

  pp <- uses (modelLedger . modelLedger_nes) getModelPParams
  st <- use $ modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate
  (st', _errs) <-
    applyRule
      (Proxy @'ModelRule_DELPL)
      dCert
      globals
      ModelDPSEnv
        { _modelDPSEnv_slot = 999,
          _modelDPSEnv_txIx = 999,
          _modelDPSEnv_pp = pp,
          _modelDPSEnv_tx = modelTx
        }
      st
  -- TODO: unless (null errs) sulk
  modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate
    .= st'

-- a couple of optics are defined for convenience of the generators, but as
-- Getters instead of lenses because they should not be used to modify the state
-- (instead, go through applyRule/applyModelTx or similar)
getModelLedger_utxos :: ModelLedger era -> ModelUTxOMap era
getModelLedger_utxos = view $ modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo
{-# INLINE getModelLedger_utxos #-}

getModelLedger_epoch :: ModelLedger era -> EpochNo
getModelLedger_epoch = view $ modelLedger_nes . modelNewEpochState_el
{-# INLINE getModelLedger_epoch #-}

modelLedger_slotOffset :: Lens' (ModelLedger era) SlotNo
modelLedger_slotOffset a2fb s = (\b -> s {_modelLedger_slotOffset = b}) <$> a2fb (_modelLedger_slotOffset s)
{-# INLINE modelLedger_slotOffset #-}

getModelLedger_rewards :: ModelLedger era -> Map.Map (ModelCredential 'Staking (ScriptFeature era)) Coin
getModelLedger_rewards = view $ modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_dpstate . modelDPState_dstate . modelDState_rewards
{-# INLINE getModelLedger_rewards #-}

modelLedger_nes :: Lens' (ModelLedger era) (ModelNewEpochState era)
modelLedger_nes a2fb s = (\b -> s {_modelLedger_nes = b}) <$> a2fb (_modelLedger_nes s)
{-# INLINE modelLedger_nes #-}
