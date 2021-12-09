{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | = Model ledger quickcheck generators.
--
-- The main idea here is to generate a bunch of model transactions applied to a
-- model initial (genesis) state, in hopes of encountering as many important
-- corner cases as possible.
--
-- This module mainly contains only the
-- environment used in the generators.  see also:
--
--  * "Test.Cardano.Ledger.Model.Generators.Chain" for the generators themselves.
--  * "Test.Cardano.Ledger.Model.Generators.Shrinking" for tools to shrink
--    generated models.
--  * "Test.Cardano.Ledger.Model.Fixup" for adjusting models to "be correct"
--    after minor manipulations that may have renderded them invalid for minor
--    reasons.
module Test.Cardano.Ledger.Model.Generators where

import Cardano.Ledger.BaseTypes
  ( Globals,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Slotting.Slot (EpochNo)
import Control.Lens
  ( Lens',
    use,
    (<<+=),
    pattern (:>),
  )
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Supply
  ( MonadSupply (..),
  )
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.HKD
  ( FFoldable (..),
    FFunctor (..),
    FRepeat (..),
    FTraversable (..),
    FZip (..),
    ffmapDefault,
    ffoldMapDefault,
    gfrepeat,
    gftraverse,
    gfzipWith,
  )
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import QuickCheck.GenT
  ( Gen,
    MonadGen,
    choose,
    frequency,
    getSize,
  )
import Test.Cardano.Ledger.Model.API
  ( HasModelLedger (modelLedger),
    ModelLedger,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( HasGlobals,
    getGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( IfSupportsPlutus,
    KnownRequiredFeatures,
  )
import Test.Cardano.Ledger.Model.Prov
  ( MonadModelProvenance (..),
  )

-- | some parameters to fine tune the behavior of model generation.
data ModelGeneratorParamsF f = ModelGeneratorParams
  { _modelGeneratorParams_epochs :: f EpochNo,
    _modelGeneratorParams_genesesAcct :: f Coin,
    _modelGeneratorParams_numGenesesAcct :: f Int,
    _modelGeneratorParams_txnsPerSlot :: f Int,
    _modelGeneratorParams_numSlotsUsed :: f Int, -- TODO: make this a fraction of slots used.
    _modelGeneratorParams_numTxInputs :: f Int,
    _modelGeneratorParams_numDCerts :: f Int,
    _modelGeneratorParams_numWdrls :: f Int
  }
  deriving (Generic)

instance FFunctor ModelGeneratorParamsF where ffmap = ffmapDefault

instance FZip ModelGeneratorParamsF where fzipWith = gfzipWith

instance FRepeat ModelGeneratorParamsF where frepeat = gfrepeat

instance FFoldable ModelGeneratorParamsF where ffoldMap = ffoldMapDefault

instance FTraversable ModelGeneratorParamsF where ftraverse = gftraverse

type ModelGeneratorParams = ModelGeneratorParamsF Gen

defaultModelGeneratorParams :: ModelGeneratorParams
defaultModelGeneratorParams =
  ModelGeneratorParams
    { _modelGeneratorParams_epochs = choose (20, 30),
      _modelGeneratorParams_genesesAcct = Coin <$> choose (100_000 * minOutput, 45e12),
      _modelGeneratorParams_numGenesesAcct = choose (1, 20),
      _modelGeneratorParams_txnsPerSlot = choose (1, 20),
      _modelGeneratorParams_numSlotsUsed = choose (0, 100),
      _modelGeneratorParams_numTxInputs = frequency [(10, pure 1), (1, choose (1, 8))],
      _modelGeneratorParams_numDCerts = frequency [(10, pure 0), (1, choose (1, 5))],
      _modelGeneratorParams_numWdrls = choose (1, 5)
    }

data ModelGeneratorContext = ModelGeneratorContext
  { _modelGeneratorContext_globals :: !Globals,
    _modelGeneratorContext_modelGeneratorParams :: !ModelGeneratorParams
  }

modelGeneratorContext_globals :: Lens' ModelGeneratorContext Globals
modelGeneratorContext_globals a2fb s = (\b -> s {_modelGeneratorContext_globals = b}) <$> a2fb (_modelGeneratorContext_globals s)
{-# INLINE modelGeneratorContext_globals #-}

modelGeneratorContext_modelGeneratorParams :: Lens' ModelGeneratorContext ModelGeneratorParams
modelGeneratorContext_modelGeneratorParams a2fb s = (\b -> s {_modelGeneratorContext_modelGeneratorParams = b}) <$> a2fb (_modelGeneratorContext_modelGeneratorParams s)
{-# INLINE modelGeneratorContext_modelGeneratorParams #-}

instance HasGlobals ModelGeneratorContext where
  getGlobals = _modelGeneratorContext_globals

type HasGenModelM st era m =
  ( MonadReader ModelGeneratorContext m,
    State.MonadState st m,
    HasModelLedger era st,
    MonadGen m,
    KnownRequiredFeatures era,
    MonadSupply Integer m,
    Show st,
    MonadModelProvenance era m
  )

type AllowScripts sf = IfSupportsPlutus () Bool sf

minOutput :: Integer
minOutput = 500_000

newtype GenModelM era a = GenModelM {unGenModelM :: State.StateT (Faucet (ModelLedger era)) (ReaderT ModelGeneratorContext Gen) a}
  deriving (Functor, Applicative, Monad, MonadGen)

deriving newtype instance MonadReader ModelGeneratorContext (GenModelM era)

deriving newtype instance State.MonadState (Faucet (ModelLedger era)) (GenModelM era)

deriving newtype instance MonadSupply Integer (GenModelM era)

instance MonadModelProvenance era (GenModelM era) where
  setProvenance _ = pure ()
  setSlot _ _ = pure ()
  clearProvenance = pure ()
  delegProvenance _ = pure ()
  rewardOperatorProvenance _ _ = pure ()
  rewardMemberProvenance _ _ = pure ()
  wdrlProvenance _ = pure ()
  mirProvenance _ = pure ()

runGenModelM ::
  KnownRequiredFeatures era =>
  ModelGeneratorContext ->
  Faucet (ModelLedger era) ->
  (forall st m. HasGenModelM st era m => m a) ->
  Gen a
runGenModelM r s k = runReaderT (State.evalStateT (unGenModelM k) s) r

class ChooseElems a where
  chooseElems :: MonadGen m => Int -> a -> m (a, a)

mkSeqChooseElems ::
  MonadGen m =>
  (s -> Int) ->
  s ->
  (s -> a -> s) ->
  (Int -> s -> a) ->
  (Int -> s -> s) ->
  (Int -> s -> s) ->
  (s -> s -> s) ->
  Int ->
  s ->
  m (s, s)
mkSeqChooseElems seqSize emptySeq seqAppend seqElemAt seqTake seqDrop seqConcat =
  let go n (xs, ys) =
        let sz = seqSize ys
         in if n <= 0 || sz <= 0
              then pure (xs, ys)
              else do
                i <- choose (0, sz - 1)
                go (n - 1) (seqAppend xs (seqElemAt i ys), seqTake i ys `seqConcat` seqDrop (i + 1) ys)
   in \n0 xs0 -> go n0 (emptySeq, xs0)
{-# INLINE mkSeqChooseElems #-}

instance ChooseElems (Seq a) where
  chooseElems = mkSeqChooseElems Seq.length mempty (:>) (flip Seq.index) Seq.take Seq.drop (<>)

instance ChooseElems (StrictSeq.StrictSeq a) where
  chooseElems = mkSeqChooseElems StrictSeq.length mempty (StrictSeq.:|>) (\i -> maybe (error "ChooseElems @StrictSeq") id . StrictSeq.lookup i) StrictSeq.take StrictSeq.drop (<>)

instance Ord k => ChooseElems (Map k a) where
  chooseElems = mkSeqChooseElems Map.size Map.empty (\xs (k, x) -> Map.insert k x xs) Map.elemAt Map.take Map.drop Map.union

instance Ord a => ChooseElems (Set a) where
  chooseElems = mkSeqChooseElems Set.size Set.empty (flip Set.insert) Set.elemAt Set.take Set.drop Set.union

data Faucet a = Faucet
  { _faucet_supply :: !Integer,
    _faucet_state :: !a
  }
  deriving (Show)

faucet_supply :: Lens' (Faucet a) Integer
faucet_supply a2fb s = (\b -> s {_faucet_supply = b}) <$> a2fb (_faucet_supply s)
{-# INLINE faucet_supply #-}

faucet_state :: Lens' (Faucet a) a
faucet_state a2fb s = (\b -> s {_faucet_state = b}) <$> a2fb (_faucet_state s)
{-# INLINE faucet_state #-}

instance HasModelLedger era a => HasModelLedger era (Faucet a) where
  modelLedger = faucet_state . modelLedger

instance {-# OVERLAPPING #-} (Monad m) => MonadSupply Integer (State.StateT (Faucet s) m) where
  supply = faucet_supply <<+= 1
  peek = use faucet_supply
  exhausted = pure False

genPartition :: [a] -> Gen ([a], [a])
genPartition xs = do
  size <- getSize
  let bias = frequency [(max 0 (100 - size), pure False), (max 0 size, pure True)]
  partitionEithers <$> traverse (\x -> bool (Left x) (Right x) <$> bias) xs
