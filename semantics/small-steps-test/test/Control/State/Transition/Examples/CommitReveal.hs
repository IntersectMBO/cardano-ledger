{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.State.Transition.Examples.CommitReveal where

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash (Hash, HashAlgorithm, hashWithSerialiser)
import Cardano.Crypto.Hash.Short (ShortHash)
import Control.State.Transition
  ( Environment,
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    transitionRules,
    (?!),
  )
import qualified Control.State.Transition.Trace as Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import Data.Kind (Type)
import Data.List.Unique (allUnique)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC
import Prelude hiding (id)

-- | Commit-reveal transition system, where data hashes are committed and then
-- revealed.
--
-- The first type parameter denotes the type of hash algorithm used.
--
-- The second parameter denotes the structure used to associate a commit to its
-- hash. See 'CRSt'
--
-- The third parameter is the data that will be associated to the 'Commit'
-- signal.
data CR hashAlgo (hashToDataMap :: Type -> Type -> Type) commitData

-- | Commit-reveal transition system state.
data CRSt hashAlgo hashToDataMap commitData = CRSt
  { -- | Part of the state used to associate data to the hash that was committed.
    --
    -- This is used only by the generators, so 'hashToDataMap' will be
    -- instantiated to a 'Map' in the generators, for testing purposes; and it
    -- will be instantiated to 'NoOpMap' in an eventual implementation.
    --
    -- Here 'hashToData' is an example of a phantom variable, which wouldn't be
    -- present in the formal specification, but it is needed in the executable
    -- spec to be able to generate traces.
    hashToData :: !(hashToDataMap (Hash hashAlgo Data) commitData),
    committedHashes :: !(Set (Hash hashAlgo Data))
  }

deriving instance
  (Eq (hashToDataMap (Hash hashAlgo Data) commitData)) =>
  Eq (CRSt hashAlgo hashToDataMap commitData)

deriving instance
  (Show (hashToDataMap (Hash hashAlgo Data) commitData)) =>
  Show (CRSt hashAlgo hashToDataMap commitData)

class MapLike m k v where
  insert :: k -> v -> m k v -> m k v

  delete :: k -> m k v -> m k v

data NoOpMap a b = NoOpMap

-- | This is the 'MapLike' instance one would use if the executable spec would
-- be used in an implementation (where no generators are needed).
instance MapLike NoOpMap a b where
  insert _ _ _ = NoOpMap

  delete _ _ = NoOpMap

instance Ord k => MapLike Map k v where
  insert = Map.insert

  delete = Map.delete

data CRSignal hashAlgo commitData
  = Commit (Hash hashAlgo Data) commitData
  | Reveal Data
  deriving (Ord, Eq, Show)

isCommit :: CRSignal hashAlgo commitData -> Bool
isCommit Commit {} = True
isCommit _ = False

newtype Data = Data {getData :: (Id, Int)}
  deriving (Eq, Show, ToCBOR, Ord, QC.Arbitrary)

newtype Id = Id {getId :: Int}
  deriving (Eq, Show, ToCBOR, Ord, QC.Arbitrary)

data CRPredicateFailure hashAlgo (hashToDataMap :: Type -> Type -> Type) commitData
  = InvalidReveal Data
  | AlreadyComitted (Hash hashAlgo Data)
  deriving (Eq, Show)

instance
  ( HashAlgorithm hashAlgo,
    Typeable hashToDataMap,
    Typeable commitData,
    MapLike hashToDataMap (Hash hashAlgo Data) commitData,
    Monoid (hashToDataMap (Hash hashAlgo Data) commitData)
  ) =>
  STS (CR hashAlgo hashToDataMap commitData)
  where
  type Environment (CR hashAlgo hashToDataMap commitData) = ()

  type
    State (CR hashAlgo hashToDataMap commitData) =
      CRSt hashAlgo hashToDataMap commitData

  type
    Signal (CR hashAlgo hashToDataMap commitData) =
      CRSignal hashAlgo commitData

  type
    PredicateFailure (CR hashAlgo hashToDataMap commitData) =
      CRPredicateFailure hashAlgo hashToDataMap commitData

  initialRules =
    [ pure
        $! CRSt
          { hashToData = mempty,
            committedHashes = Set.empty
          }
    ]

  transitionRules =
    [ do
        TRC ((), CRSt {hashToData, committedHashes}, crSignal) <- judgmentContext
        case crSignal of
          Commit dataHash commitData -> do
            dataHash `Set.notMember` committedHashes ?! AlreadyComitted dataHash
            pure
              $! CRSt
                { hashToData = insert dataHash commitData hashToData,
                  committedHashes = Set.insert dataHash committedHashes
                }
          Reveal someData -> do
            hashWithSerialiser toCBOR someData `Set.member` committedHashes
              ?! InvalidReveal someData
            pure
              $! CRSt
                { hashToData =
                    delete
                      (hashWithSerialiser toCBOR someData)
                      hashToData,
                  committedHashes =
                    Set.delete
                      (hashWithSerialiser toCBOR someData)
                      committedHashes
                }
    ]

instance
  HashAlgorithm hashAlgo =>
  STS.Gen.HasTrace (CR hashAlgo Map Data) ()
  where
  envGen :: () -> QC.Gen ()
  envGen _ = pure ()

  sigGen ::
    () ->
    () ->
    CRSt hashAlgo Map Data ->
    QC.Gen (CRSignal hashAlgo Data)
  sigGen () () CRSt {hashToData, committedHashes} =
    if Set.null committedHashes
      then genCommit
      else QC.oneof [genCommit, genReveal]
    where
      genCommit = do
        id <- Id <$> QC.arbitrary
        n <- QC.choose (-2, 2)
        let newData = Data (id, n)
        pure $! Commit (hashWithSerialiser toCBOR newData) newData
      genReveal = do
        hashToReveal <- QC.elements $ Set.toList committedHashes
        let dataToReveal = hashToData Map.! hashToReveal
        pure $! Reveal dataToReveal

  shrinkSignal (Commit _ someData) =
    recalculateCommit <$> QC.shrink someData
    where
      recalculateCommit shrunkData =
        Commit
          (hashWithSerialiser toCBOR shrunkData)
          shrunkData
  shrinkSignal (Reveal someData) = Reveal <$> QC.shrink someData

-- | Check that unique data is generated. This is supposed to fail, since
-- there's nothing in the STS that prevents two commits of the same data. The
-- resulting minimal counterexample should be a trace of the form:
--
-- > commit (hash d0) -> reveal d0 -> commit (hash d0)
--
-- where it shouldn't be possible to shrink @d0@ any further.
prop_qc_UniqueData :: QC.Property
prop_qc_UniqueData =
  STS.Gen.forAllTrace @(CR ShortHash Map Data) @()
    ()
    100
    ()
    (noDuplicatedData . Trace.traceSignals Trace.OldestFirst)
  where
    noDuplicatedData :: [CRSignal ShortHash Data] -> Bool
    noDuplicatedData = allUnique . filter isCommit

-- | Check that only valid signals are generated.
--
-- This property should fail since the generators don't check that the generated
-- commits are unique. The resulting minimal counterexample should be a trace of
-- the form:
--
-- > commit (hash d0) -> commit (hash d0)
--
-- where it shouldn't be possible to shrink @d0@ any further.
prop_qc_OnlyValidSignals :: QC.Property
prop_qc_OnlyValidSignals =
  QC.withMaxSuccess 5000 $ -- We need to test a large
  -- number of times to make sure
  -- we get a collision in the
  -- generated data
    STS.Gen.onlyValidSignalsAreGenerated @(CR ShortHash Map Data) @() () 150 ()
