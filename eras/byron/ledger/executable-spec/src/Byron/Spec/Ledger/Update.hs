{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- This is for the Hashable Set instance
{-# OPTIONS_GHC -Wno-orphans #-}

module Byron.Spec.Ledger.Update
  ( module Byron.Spec.Ledger.Update,
    PredicateFailure (),
  )
where

import Byron.Spec.Ledger.Core
  ( BlockCount (..),
    HasHash,
    Owner (Owner),
    Relation (..),
    Slot,
    SlotCount (..),
    VKey (VKey),
    VKeyGenesis (VKeyGenesis),
    dom,
    hash,
    (*.),
    (-.),
    (∈),
    (∉),
    (⋪),
    (▷),
    (▷<=),
    (▷>=),
    (◁),
    (⨃),
  )
import qualified Byron.Spec.Ledger.Core as Core
import qualified Byron.Spec.Ledger.Core.Generators as CoreGen
import Byron.Spec.Ledger.Core.Omniscient (skey)
import qualified Byron.Spec.Ledger.GlobalParams as GP
import Byron.Spec.Ledger.Util (mkGoblinGens)
import Control.Arrow (second, (&&&))
import Control.Monad (mzero)
import Control.State.Transition
import Control.State.Transition.Generator
  ( HasTrace,
    SignalGenerator,
    envGen,
    sigGen,
    tinkerWithSigGen,
  )
import Data.AbstractSize (HasTypeReps)
import Data.Bimap (Bimap, empty, lookupR)
import qualified Data.Bimap as Bimap
import Data.Char (isAscii)
import Data.Coerce (coerce)
import Data.Data (Data, Typeable)
import Data.Foldable (foldl', toList)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Ix (inRange)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.Internal (Field1 (..), Field2 (..), Field3 (..))
import Lens.Micro.TH (makeLenses)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural
import Test.Goblin
  ( AddShrinks (..),
    GeneOps,
    Goblin (..),
    GoblinData,
    SeedGoblin (..),
    mkEmptyGoblin,
    saveInBagOfTricks,
    tinkerRummagedOrConjureOrSave,
    transcribeGenesAsInt,
    (<$$>),
  )
import Test.Goblin.TH (deriveAddShrinks, deriveGoblin, deriveSeedGoblin)
import Prelude

newtype FactorA = FactorA Int
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, NoThunks)
  deriving anyclass (HasTypeReps)

newtype FactorB = FactorB Int
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, NoThunks)
  deriving anyclass (HasTypeReps)

newtype UpAdptThd = UpAdptThd Double
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, Num, Real, Fractional, RealFrac, NoThunks)
  deriving anyclass (HasTypeReps)

newtype BkSgnCntT = BkSgnCntT Double
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, Num, Fractional, NoThunks)
  deriving anyclass (HasTypeReps)

-- | Protocol parameters.
data PParams = PParams -- TODO: this should be a module of @byron-spec-ledger@.
  { -- | Maximum (abstract) block size in words
    _maxBkSz :: !Natural,
    -- | Maximum (abstract) block header size in words
    _maxHdrSz :: !Natural,
    -- | Maximum (abstract) transaction size in words
    _maxTxSz :: !Natural,
    -- | Maximum (abstract) update proposal size in words
    _maxPropSz :: !Natural,
    -- | Fraction [0, 1] of the blocks that can be signed by any given key in a
    -- window of lenght '_bkSgnCntW'. This value will be typically between 1/5
    -- and 1/4
    _bkSgnCntT :: !BkSgnCntT,
    -- | Number of slots in an epoch.
    -- TODO: this should be removed since the number of slots per epoch should remain constant.
    _bkSlotsPerEpoch :: !Core.SlotCount,
    -- | Update proposal TTL in slots
    _upTtl :: !Core.SlotCount,
    -- | Script version
    _scriptVersion :: !Natural,
    -- | Update adoption threshold: a proportion of block issuers that have to
    -- endorse a given version to become candidate for adoption
    _upAdptThd :: !UpAdptThd,
    -- | Minimum fees per transaction
    _factorA :: !FactorA, -- TODO: these should have type 'Word64', like in `cardano-ledger`.

    -- | Additional fees per transaction size
    _factorB :: !FactorB
  }
  deriving (Eq, Generic, Ord, Show, Hashable, Data, Typeable, NoThunks)

makeLenses ''PParams

instance HasTypeReps PParams

newtype UpId = UpId Int
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, NoThunks)
  deriving anyclass (HasTypeReps)

-- | Protocol version
data ProtVer = ProtVer
  { _pvMaj :: Natural,
    _pvMin :: Natural,
    _pvAlt :: Natural
  }
  deriving (Eq, Generic, Ord, Show, Hashable, Data, Typeable, NoThunks)

makeLenses ''ProtVer

instance HasTypeReps ProtVer

newtype ApName = ApName String
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable, NoThunks)

instance HasTypeReps ApName

-- | Application version
newtype ApVer = ApVer Natural
  deriving stock (Generic, Show, Data, Typeable)
  deriving newtype (Eq, Ord, Num, Hashable, NoThunks)

instance HasTypeReps ApVer

data SwVer = SwVer
  { _svName :: ApName,
    _svVer :: ApVer
  }
  deriving (Eq, Generic, Show, Hashable, Data, Typeable, NoThunks)

makeLenses ''SwVer

instance HasTypeReps SwVer

-- | Part of the update proposal which must be signed
type UpSD =
  ( ProtVer,
    PParams,
    SwVer,
    Set STag,
    Metadata
  )

-- | System tag, this represents a target operating system for the update (e.g.
-- @linux@, @win64@, or @mac32@).
type STag = String

-- | For now we do not have any requirements on metadata.
data Metadata = Metadata
  deriving (Eq, Ord, Show, Generic, Hashable, Data, Typeable, NoThunks)

-- | Update proposal
data UProp = UProp
  { _upId :: UpId,
    _upIssuer :: Core.VKey,
    _upParams :: PParams,
    _upPV :: ProtVer,
    _upSwVer :: SwVer,
    _upSig :: Core.Sig UpSD,
    -- | System tags involved in the update proposal.
    _upSTags :: Set STag,
    -- | Metadata required for performing software updates.
    _upMdt :: Metadata
  }
  deriving (Eq, Generic, Show, Hashable, Data, Typeable, NoThunks)

-- We need the Hashable instance before making lenses.
instance Hashable a => Hashable (Set a) where
  hashWithSalt = H.hashUsing Set.toList

makeLenses ''UProp

upSigData :: Lens' UProp UpSD
upSigData =
  lens
    (\up -> (up ^. upPV, up ^. upParams, up ^. upSwVer, up ^. upSTags, up ^. upMdt))
    ( \up (pv, pps, sv, stags, mdt) ->
        up
          & upParams .~ pps
          & upPV .~ pv
          & upSwVer .~ sv
          & upSTags .~ stags
          & upMdt .~ mdt
    )

getUpSigData :: UProp -> UpSD
getUpSigData = view upSigData

mkUProp ::
  UpId ->
  Core.VKey ->
  ProtVer ->
  PParams ->
  SwVer ->
  Set STag ->
  Metadata ->
  UProp
mkUProp aUpId issuer pv pps sv stags mdt = uprop
  where
    uprop =
      UProp
        { _upId = aUpId,
          _upIssuer = issuer,
          _upParams = pps,
          _upPV = pv,
          _upSwVer = sv,
          _upSig = Core.sign (skey issuer) (uprop ^. upSigData),
          _upSTags = stags,
          _upMdt = mdt
        }

instance HasTypeReps (ProtVer, PParams, SwVer, Set STag, Metadata)

instance HasTypeReps Metadata

instance HasTypeReps UProp

-- | Test if a pair is present in a map.
inMap :: (Ord key, Eq v) => key -> v -> Map key v -> Bool
inMap key v m = case Map.lookup key m of
  Just x | x == v -> True
  _ -> False

-- | Invert a map
--
--  Examples:
--
--  >>> import qualified Data.Map.Strict as Map
--  >>> import Byron.Spec.Ledger.Update (invertMap)
--  >>> invertMap (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3), ('d', 1)])
--  fromList [(1,fromList "ad"),(2,fromList "b"),(3,fromList "c")]
invertMap ::
  (Ord k, Ord v) =>
  Map k v ->
  Map v (Set k)
invertMap =
  Map.fromListWith (Set.union)
    . fmap (fmap Set.singleton . swap)
    . Map.toList

-- | Invert a map which we assert to be a bijection.
--   If this map is not a bijection, the behaviour is not guaranteed.
--
--   Examples:
--
--   >>> import qualified Data.Map.Strict as Map
--   >>> invertBijection (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3)])
--   fromList [(1,'a'),(2,'b'),(3,'c')]
invertBijection ::
  Ord v =>
  Map k v ->
  Map v k
invertBijection =
  Map.fromListWith const
    . fmap swap
    . Map.toList

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

infix 1 ==>

-- | Check whether a protocol version can follow the current protocol version.
pvCanFollow ::
  -- | Next protocol version
  ProtVer ->
  -- | Previous protocol version
  ProtVer ->
  Bool
pvCanFollow (ProtVer mjn mn an) (ProtVer mjp mip ap) =
  (mjp, mip, ap) < (mjn, mn, an)
    && (inRange (0, 1) (mjn - mjp))
    && ((mjp == mjn) ==> (mip + 1 == mn))
    && ((mjp + 1 == mjn) ==> (mn == 0))

-- | Check whether an update proposal marks a valid update
checkUpdateConstraints ::
  PParams ->
  UProp ->
  [UpdateConstraintViolation]
checkUpdateConstraints pps prop =
  catMaybes
    [ (prop ^. upParams . maxBkSz <=? 2 * pps ^. maxBkSz)
        `orError` BlockSizeTooLarge,
      (prop ^. upParams . maxTxSz + 1 <=? prop ^. upParams . maxBkSz)
        `orError` TransactionSizeTooLarge,
      (pps ^. scriptVersion <=? prop ^. upParams . scriptVersion)
        `orError` ScriptVersionTooSmall,
      (prop ^. upParams . scriptVersion <=? pps ^. scriptVersion + 1)
        `orError` ScriptVersionTooLarge
    ]

(<=?) :: Ord a => a -> a -> Maybe (a, Threshold a)
x <=? y = if x <= y then Nothing else Just (x, Threshold y)

infix 4 <=?

orError :: Maybe (a, b) -> (a -> b -> e) -> Maybe e
orError mab ferr = uncurry ferr <$> mab

canUpdate :: PParams -> UProp -> Rule UPPVV ctx ()
canUpdate pps prop = violations == [] ?! CannotUpdatePv violations
  where
    violations = checkUpdateConstraints pps prop

-- | Violations on the constraints of the allowed values for new protocol
-- parameters.
data UpdateConstraintViolation
  = BlockSizeTooLarge Natural (Threshold Natural)
  | TransactionSizeTooLarge Natural (Threshold Natural)
  | ScriptVersionTooLarge Natural (Threshold Natural)
  | ScriptVersionTooSmall Natural (Threshold Natural)
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NoThunks)

svCanFollow ::
  Map ApName (ApVer, Core.Slot, Metadata) ->
  (ApName, ApVer) ->
  Bool
svCanFollow avs (an, av) =
  ( case Map.lookup an avs of
      Nothing -> True
      Just (x, _, _) -> av == x + 1
  )
    && (an `Set.notMember` dom avs ==> (av == ApVer 0 || av == ApVer 1))
  where

------------------------------------------------------------------------
-- Update proposals
------------------------------------------------------------------------

-- | Update Proposal Software Version Validation
data UPSVV deriving (Generic, Data, Typeable)

-- | These `PredicateFailure`s are all "throwable". The disjunction of the
--   rules' preconditions is not `True` - the `PredicateFailure`s represent
--   `False` cases.
data UpsvvPredicateFailure
  = AlreadyProposedSv
  | CannotFollowSv
  | InvalidApplicationName
  | InvalidSystemTags
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPSVV where
  type Environment UPSVV = Map ApName (ApVer, Core.Slot, Metadata)
  type State UPSVV = Map UpId (ApName, ApVer, Metadata)
  type Signal UPSVV = UProp
  type PredicateFailure UPSVV = UpsvvPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC (avs, raus, up) <- judgmentContext
        let SwVer an av = up ^. upSwVer
        apNameValid an ?! InvalidApplicationName
        svCanFollow avs (an, av) ?! CannotFollowSv
        an `notElem` fmap fst' (Map.elems raus) ?! AlreadyProposedSv
        all sTagValid (up ^. upSTags) ?! InvalidSystemTags
        return $! raus ⨃ [(up ^. upId, (an, av, up ^. upMdt))]
    ]
    where
      fst' (x, _, _) = x

      apNameValid (ApName n) = all isAscii n && length n <= 12

      sTagValid tag = all isAscii tag && length tag <= 10

data UPPVV deriving (Generic, Data, Typeable)

-- | These `PredicateFailure`s are all "throwable". The disjunction of the
--   rules' preconditions is not `True` - the `PredicateFailure`s represent
--   `False` cases.
data UppvvPredicateFailure
  = CannotFollowPv
  | CannotUpdatePv [UpdateConstraintViolation]
  | AlreadyProposedPv
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPPVV where
  type
    Environment UPPVV =
      ( ProtVer,
        PParams
      )
  type State UPPVV = Map UpId (ProtVer, PParams)
  type Signal UPPVV = UProp
  type PredicateFailure UPPVV = UppvvPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC ((pv, pps), rpus, up) <- judgmentContext
        let pid = up ^. upId
            nv = up ^. upPV
            ppsn = up ^. upParams
        pvCanFollow nv pv ?! CannotFollowPv
        canUpdate pps up
        nv `notElem` (fst <$> Map.elems rpus) ?! AlreadyProposedPv
        return $! rpus ⨃ [(pid, (nv, ppsn))]
    ]

-- | Update proposal validity
data UPV deriving (Generic, Data, Typeable)

-- | These `PredicateFailure`s are all throwable.
data UpvPredicateFailure
  = UPPVVFailure (PredicateFailure UPPVV)
  | UPSVVFailure (PredicateFailure UPSVV)
  | AVChangedInPVUpdate ApName ApVer (Maybe (ApVer, Slot, Metadata))
  | ParamsChangedInSVUpdate
  | PVChangedInSVUpdate
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPV where
  type
    Environment UPV =
      ( ProtVer,
        PParams,
        Map ApName (ApVer, Core.Slot, Metadata)
      )

  type
    State UPV =
      ( Map UpId (ProtVer, PParams),
        Map UpId (ApName, ApVer, Metadata)
      )

  type Signal UPV = UProp
  type PredicateFailure UPV = UpvPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (pv, pps, avs),
            (rpus, raus),
            up
            ) <-
          judgmentContext
        rpus' <- trans @UPPVV $ TRC ((pv, pps), rpus, up)
        let SwVer an av = up ^. upSwVer
        inMap an av (swVer <$> avs) ?! AVChangedInPVUpdate an av (Map.lookup an avs)
        pure $! (rpus', raus),
      do
        TRC
          ( (pv, pps, avs),
            (rpus, raus),
            up
            ) <-
          judgmentContext
        pv == up ^. upPV ?! PVChangedInSVUpdate
        up ^. upParams == pps ?! ParamsChangedInSVUpdate
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        pure $! (rpus, raus'),
      do
        TRC
          ( (pv, pps, avs),
            (rpus, raus),
            up
            ) <-
          judgmentContext
        rpus' <- trans @UPPVV $ TRC ((pv, pps), rpus, up)
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        pure $! (rpus', raus')
    ]
    where
      swVer (x, _, _) = x

instance Embed UPPVV UPV where
  wrapFailed = UPPVVFailure

instance Embed UPSVV UPV where
  wrapFailed = UPSVVFailure

data UPREG deriving (Generic, Data, Typeable)

-- | These `PredicateFailure`s are all throwable.
data UpregPredicateFailure
  = UPVFailure (PredicateFailure UPV)
  | NotGenesisDelegate
  | DoesNotVerify
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPREG where
  type
    Environment UPREG =
      ( ProtVer,
        PParams,
        Map ApName (ApVer, Core.Slot, Metadata),
        Bimap Core.VKeyGenesis Core.VKey
      )
  type
    State UPREG =
      ( Map UpId (ProtVer, PParams),
        Map UpId (ApName, ApVer, Metadata)
      )
  type Signal UPREG = UProp
  type PredicateFailure UPREG = UpregPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (pv, pps, avs, dms),
            (rpus, raus),
            up
            ) <-
          judgmentContext
        (rpus', raus') <- trans @UPV $ TRC ((pv, pps, avs), (rpus, raus), up)
        let vk = up ^. upIssuer
        dms ▷ Set.singleton vk /= empty ?! NotGenesisDelegate
        Core.verify vk (up ^. upSigData) (up ^. upSig) ?! DoesNotVerify
        return $! (rpus', raus')
    ]

instance Embed UPV UPREG where
  wrapFailed = UPVFailure

------------------------------------------------------------------------
-- Update voting
------------------------------------------------------------------------

data Vote = Vote
  { _vCaster :: Core.VKey,
    _vPropId :: UpId,
    _vSig :: Core.Sig UpId
  }
  deriving (Eq, Generic, Show, Hashable, Data, Typeable, NoThunks)

makeLenses ''Vote

instance HasTypeReps Vote

mkVote :: Core.VKey -> UpId -> Vote
mkVote caster proposalId =
  Vote
    { _vCaster = caster,
      _vPropId = proposalId,
      _vSig = Core.sign (skey caster) proposalId
    }

instance HasHash (Maybe Byron.Spec.Ledger.Update.UProp, [Byron.Spec.Ledger.Update.Vote]) where
  hash = Core.Hash . Just . H.hash

data ADDVOTE deriving (Generic, Data, Typeable)

-- | These `PredicateFailure`s are all throwable.
data AddvotePredicateFailure
  = AVSigDoesNotVerify
  | NoUpdateProposal UpId
  | VoteByNonGenesisDelegate VKey
  | RepeatVoteByGenesisDelegate VKey
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS ADDVOTE where
  type
    Environment ADDVOTE =
      ( Set UpId,
        Bimap Core.VKeyGenesis Core.VKey
      )
  type State ADDVOTE = Set (UpId, Core.VKeyGenesis)
  type Signal ADDVOTE = Vote
  type PredicateFailure ADDVOTE = AddvotePredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (rups, dms),
            vts,
            vote
            ) <-
          judgmentContext
        let pid = vote ^. vPropId
            vk = vote ^. vCaster
            vtsPid =
              case lookupR vk dms of
                Just vks -> Set.singleton (pid, vks)
                Nothing -> Set.empty
        vtsPid /= Set.empty ?! VoteByNonGenesisDelegate vk
        not (vtsPid `Set.isSubsetOf` vts) ?! RepeatVoteByGenesisDelegate vk
        Set.member pid rups ?! NoUpdateProposal pid
        Core.verify vk pid (vote ^. vSig) ?! AVSigDoesNotVerify
        return $! vts <> vtsPid
    ]

data UPVOTE deriving (Generic, Data, Typeable)

-- | The 3 non-embedded `PredicateFailure`s here are all structural. The
-- disjuntion of the preconditions is `True` - one rule either fires or the
-- other does.
data UpvotePredicateFailure
  = ADDVOTEFailure (PredicateFailure ADDVOTE)
  | S_HigherThanThdAndNotAlreadyConfirmed
  | S_CfmThdNotReached
  | S_AlreadyConfirmed
  deriving (Eq, Show, Data, Generic, Typeable, NoThunks)

instance STS UPVOTE where
  type
    Environment UPVOTE =
      ( Core.Slot,
        Word8,
        Set UpId,
        Bimap Core.VKeyGenesis Core.VKey
      )
  type
    State UPVOTE =
      ( Map UpId Core.Slot,
        Set (UpId, Core.VKeyGenesis)
      )
  type Signal UPVOTE = Vote
  type PredicateFailure UPVOTE = UpvotePredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (_, t, rups, dms),
            (cps, vts),
            vote
            ) <-
          judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        size ([pid] ◁ vts') < t || pid ∈ dom cps ?! S_HigherThanThdAndNotAlreadyConfirmed
        pure
          $! ( cps,
               vts'
             ),
      do
        TRC
          ( (sn, t, rups, dms),
            (cps, vts),
            vote
            ) <-
          judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        t <= size ([pid] ◁ vts') ?! S_CfmThdNotReached
        pid ∉ dom cps ?! S_AlreadyConfirmed
        pure
          $! ( cps ⨃ [(pid, sn)],
               vts'
             )
    ]

instance Embed ADDVOTE UPVOTE where
  wrapFailed = ADDVOTEFailure

------------------------------------------------------------------------
-- Update voting
------------------------------------------------------------------------

data FADS deriving (Generic, Data, Typeable)

data FadsPredicateFailure
  deriving (Eq, Show, Data, Typeable, Generic)

instance STS FADS where
  type Environment FADS = ()
  type State FADS = [(Core.Slot, (ProtVer, PParams))]
  type Signal FADS = (Core.Slot, (ProtVer, PParams))
  type PredicateFailure FADS = FadsPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (),
            fads,
            (sn, (bv, ppsc))
            ) <-
          judgmentContext
        return $ case fads of
          ((_, (pvc, _)) : _) ->
            if pvc < bv
              then (sn, (bv, ppsc)) : fads
              else fads
          _ -> (sn, (bv, ppsc)) : fads
    ]

data UPEND deriving (Generic, Data, Typeable)

-- | Find the key that corresponds to the value satisfying the given predicate.
-- In case zero or more than one key is found this function returns Nothing.
findKey :: (v -> Bool) -> Map k v -> Maybe (k, v)
findKey p m =
  case Map.toList (Map.filter p m) of
    [(k, v)] -> Just (k, v)
    _ -> Nothing

-- | `S_TryNextRule` is a structural `PredicateFailure`, used to fail from
-- one transition rule to the other. The other `PredicateFailure`s are all
-- throwable.
data UpendPredicateFailure
  = ProtVerUnknown ProtVer
  | S_TryNextRule
  | CanAdopt ProtVer
  | CannotAdopt ProtVer
  | NotADelegate VKey
  | UnconfirmedProposal UpId
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPEND where
  type
    Environment UPEND =
      ( Core.Slot, -- Current slot number
        Natural, -- Adoption threshold
        Bimap VKeyGenesis VKey, -- Delegation map
        Map UpId Core.Slot, -- Confirmed proposals
        Map UpId (ProtVer, PParams), -- Registered update proposals
        BlockCount -- Chain stability parameter. This
        -- is deemed to be a global
        -- constant that we temporarily put
        -- there.
      )
  type
    State UPEND =
      ( [(Core.Slot, (ProtVer, PParams))],
        Set (ProtVer, Core.VKeyGenesis)
      )
  type Signal UPEND = (ProtVer, Core.VKey)
  type PredicateFailure UPEND = UpendPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (sn, _t, _dms, cps, rpus, k),
            (fads, bvs),
            (bv, _vk)
            ) <-
          judgmentContext
        case findKey ((== bv) . fst) rpus of
          Just (pid, _) -> do
            -- If we found the proposal id that corresponds to 'bv' then we
            -- have to check that it isn't confirmed for this rule to succeed.
            pid ∉ dom (cps ▷<= sn -. 2 *. k) ?! S_TryNextRule
            pure $! (fads, bvs)
          Nothing ->
            -- If we didn't find the proposal id that corresponds to 'bv' then
            -- this rule succeeds.
            --
            -- Note that the difference w.r.t. the case above is that this case
            -- will succeed, whereas the case above can cause a predicate
            -- failure if the condition of the '!?' operator is not met. Since
            -- even on failure we _need_ to return a state, the case above also
            -- returns the state unchanged in this case.
            pure $! (fads, bvs),
      do
        TRC
          ( (sn, t, dms, cps, rpus, k),
            (fads, bvs),
            (bv, vk)
            ) <-
          judgmentContext
        case lookupR vk dms of
          Nothing -> do
            False ?! S_TryNextRule
            pure $! (fads, bvs)
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            size ([bv] ◁ bvs') < t ?! CanAdopt bv
            case findKey ((== bv) . fst) rpus of
              Just (pid, _) -> do
                pid ∈ dom (cps ▷<= sn -. 2 *. k) ?! S_TryNextRule
                pure $! (fads, bvs')
              Nothing -> do
                False ?! S_TryNextRule
                pure $! (fads, bvs'),
      do
        TRC
          ( (sn, t, dms, cps, rpus, k),
            (fads, bvs),
            (bv, vk)
            ) <-
          judgmentContext
        case lookupR vk dms of
          Nothing -> do
            False ?! NotADelegate vk
            pure $! (fads, bvs)
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            t <= size ([bv] ◁ bvs') ?! CannotAdopt bv
            case findKey ((== bv) . fst) rpus of
              Just (pid, (_, ppsc)) -> do
                pid ∈ dom (cps ▷<= sn -. 2 *. k) ?! UnconfirmedProposal pid
                fads' <- trans @FADS $ TRC ((), fads, (sn, (bv, ppsc)))
                pure $! (fads', bvs')
              Nothing -> do
                False ?! ProtVerUnknown bv
                pure $! (fads, bvs')
    ]

instance Embed FADS UPEND where
  wrapFailed = error "No possible failures in FADS"

------------------------------------------------------------------------
-- Update interface
------------------------------------------------------------------------

-- | The update interface environment is shared amongst various rules, so
--   we define it as an alias here.
type UPIEnv =
  ( Core.Slot,
    Bimap Core.VKeyGenesis Core.VKey,
    BlockCount, -- This is a global constant in the formal
    -- specification, which we put in this environment so
    -- that we can test with different values of it.
    Word8 -- Number of genesis keys, @ngk@. Also a global constant in the
    -- formal specification which is placed here so that we can test
    -- with different values.
  )

delegationMap :: UPIEnv -> Bimap Core.VKeyGenesis Core.VKey
delegationMap (_, dms, _, _) = dms

-- | The update interface state is shared amongst various rules, so we define it
-- as an alias here.
type UPIState =
  ( (ProtVer, PParams), -- (pv, pps)
    [(Core.Slot, (ProtVer, PParams))], -- fads
    Map ApName (ApVer, Core.Slot, Metadata), -- avs
    Map UpId (ProtVer, PParams), -- rpus
    Map UpId (ApName, ApVer, Metadata), -- raus
    Map UpId Core.Slot, -- cps
    Set (UpId, Core.VKeyGenesis), -- vts
    Set (ProtVer, Core.VKeyGenesis), -- bvs
    Map UpId Core.Slot -- pws
  )

emptyUPIState :: UPIState
emptyUPIState =
  ( ( ProtVer 0 0 0,
      initialPParams
    ),
    [],
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Set.empty,
    Set.empty,
    Map.empty
  )

initialPParams :: PParams
initialPParams =
  PParams -- TODO: choose more sensible default values
    { _maxBkSz = 10000, -- max sizes chosen as non-zero to allow progress
      _maxHdrSz = 1000,
      _maxTxSz = 500,
      _maxPropSz = 10,
      _bkSgnCntT = 0.22, -- As defined in the spec.
      _bkSlotsPerEpoch = 10, -- TODO: we need to remove this, since this should
      -- be a constant. Also the name slots-per-epoch is
      -- wrong.
      _upTtl = 10, -- The proposal time to live needs to be related to @k@ (or the number
      -- of slots in an epoch). We pick an arbitrary value here.
      _scriptVersion = 0,
      _upAdptThd = 0.6, -- Value currently used in mainet

      -- To determine the factors @A@ and @B@ used in the calculation of the
      -- transaction fees we need to know the constant @C@ that we use to bound
      -- the size of a transaction.
      --
      -- We have that for all transactions @tx@:
      --
      -- > size (elaborate tx) <= C * abstractSize tx
      --
      -- where @elaborate@ elaborates an abstract transaction into a concrete
      -- one.
      --
      -- We have that the (concrete) minimum fee is calculated as follows:
      --
      -- > minFee tx = A_C + B_C * C
      --
      -- where @A_C@ and @B_C@ are the concrete constants that correspond to
      -- abstract constants @A@ and @B@.
      --
      -- We need to guarantee that the abstract minimum fee we use for
      -- transactions is no less than the concrete minimum fee, since otherwise
      -- we run the risk that in the elaboration we end up paying a fee to low.
      --
      -- Now consider the minimum fee for an elaborated transaction:
      --
      -- > A_C + B_C * (size (elaborate tx))
      -- > <= { size (elaborate tx) <= C * abstractSize tx }
      -- > A_C + B_C * C * abstractSize tx
      --
      -- Which means that we should set:
      --
      -- > _factorA = A_C
      -- > _factorB = B_C * C
      --
      -- For now we choose small numbers here so that we do not need a high UTxO
      -- balance when generating the initial UTxO (see @module
      -- Byron.Spec.Ledger.UTxO.Generators@).
      _factorA = FactorA 1, -- In mainet this value is set to 155381000000000 (A_C in the derivation above)
      _factorB = FactorB (10 * fromIntegral GP.c) -- In mainet this value is set to 43946000000
    }

protocolVersion :: UPIState -> ProtVer
protocolVersion ((pv, _), _, _, _, _, _, _, _, _) = pv

protocolParameters :: UPIState -> PParams
protocolParameters ((_, pps), _, _, _, _, _, _, _, _) = pps

applicationVersions :: UPIState -> Map ApName (ApVer, Core.Slot, Metadata)
applicationVersions ((_, _), _, avs, _, _, _, _, _, _) = avs

confirmedProposals :: UPIState -> Map UpId Core.Slot
confirmedProposals ((_, _), _, _, _, _, cps, _, _, _) = cps

futureAdoptions :: UPIState -> [(Core.Slot, (ProtVer, PParams))]
futureAdoptions ((_, _), fads, _, _, _, _, _, _, _) = fads

endorsements :: UPIState -> Set (ProtVer, Core.VKeyGenesis)
endorsements ((_, _), _, _, _, _, _, _, bvs, _) = bvs

registeredProtocolUpdateProposals :: UPIState -> Map UpId (ProtVer, PParams)
registeredProtocolUpdateProposals ((_, _), _, _, rpus, _, _, _, _, _) = rpus

data UPIREG deriving (Generic, Data, Typeable)

data UpiregPredicateFailure
  = UPREGFailure (PredicateFailure UPREG)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPIREG where
  type Environment UPIREG = UPIEnv
  type State UPIREG = UPIState
  type Signal UPIREG = UProp
  type PredicateFailure UPIREG = UpiregPredicateFailure

  initialRules = [return $! emptyUPIState]

  transitionRules =
    [ do
        TRC
          ( (sn, dms, _k, _ngk),
            ( (pv, pps),
              fads,
              avs,
              rpus,
              raus,
              cps,
              vts,
              bvs,
              pws
              ),
            up
            ) <-
          judgmentContext
        (rpus', raus') <- trans @UPREG $ TRC ((pv, pps, avs, dms), (rpus, raus), up)
        let pws' = pws ⨃ [(up ^. upId, sn)]
        pure
          $! ( (pv, pps),
               fads,
               avs,
               rpus',
               raus',
               cps,
               vts,
               bvs,
               pws'
             )
    ]

instance Embed UPREG UPIREG where
  wrapFailed = UPREGFailure

instance HasTrace UPIREG where
  envGen _ = upiEnvGen

  sigGen (_slot, dms, _k, _ngk) ((pv, pps), _fads, avs, rpus, raus, _cps, _vts, _bvs, pws) =
    do
      (vk, pv', pps', sv') <-
        (,,,) <$> issuerGen
          <*> pvGen
          <*> pparamsGen
          <*> swVerGen

      Gen.frequency
        [ -- Do not change the protocol version. We generate a lower fraction of
          -- these kind of update proposals since we want to have more test cases
          -- in which the protocol parameters change. Software only updates do
          -- not offer as many possible variations as protocol parameter updates
          -- do.
          (10, generateUpdateProposalWith vk pps pv sv'),
          -- Do not change the software version (unless there are no software
          -- versions in @avs@).
          ( 45,
            do
              -- Pick a current software version (if available)
              let makeSoftwareVersion (apName, (apVersion, _, _)) = SwVer apName apVersion
                  avsList = Map.toList avs
              currentSoftwareVersion <-
                if null avsList
                  then pure $! sv'
                  else makeSoftwareVersion <$> Gen.element avsList

              generateUpdateProposalWith vk pps' pv' currentSoftwareVersion
          ),
          -- Change protocol and software version.
          (45, generateUpdateProposalWith vk pps' pv' sv')
        ]
    where
      idGen :: Gen UpId
      idGen = do
        -- Chose an increment for the maximum version seen in the update
        -- proposal IDs.
        inc <- Gen.integral (Range.constant 1 10)
        case Set.toDescList $ dom pws of
          [] -> UpId <$> Gen.element [0 .. inc]
          (UpId maxId : _) -> pure $ UpId (maxId + inc)

      -- As issuer we chose a current delegate. The delegation map must not be
      -- empty for this signal generator to succeed.
      issuerGen :: Gen Core.VKey
      issuerGen =
        if null delegates
          then error "There are no delegates to issue an update proposal."
          else Gen.element delegates
        where
          delegates = Set.toList (range dms)

      pparamsGen :: Gen PParams
      pparamsGen = ppsUpdateFrom pps

      pvGen :: Gen ProtVer
      pvGen =
        nextAltVersion
          <$> Gen.element
            [ (_pvMaj pv + 1, 0),
              (_pvMaj pv, _pvMin pv + 1)
            ]
        where
          -- Get the next alternate version, alt, so that @(maj, min, alt)@
          -- is not part of the registered protocol-update proposals
          -- (@rpus@).
          nextAltVersion :: (Natural, Natural) -> ProtVer
          nextAltVersion (maj, mn) =
            dom (range rpus)
              & Set.filter protocolVersionEqualsMajMin
              & Set.map _pvAlt
              & Set.toDescList
              & nextVersion
            where
              protocolVersionEqualsMajMin :: ProtVer -> Bool
              protocolVersionEqualsMajMin pv' =
                _pvMaj pv' == maj && _pvMin pv' == mn

              nextVersion :: [Natural] -> ProtVer
              nextVersion [] = ProtVer maj mn 0
              nextVersion (x : _) = ProtVer maj mn (1 + x)

      -- Generate a software version update.
      swVerGen :: Gen SwVer
      swVerGen =
        if null possibleNextVersions
          then genNewApp
          else Gen.choice [genANextVersion, genNewApp]
        where
          possibleNextVersions :: [(ApName, ApVer)]
          possibleNextVersions = Set.toList $ nextVersions \\ registeredNextVersions
            where
              nextVersions :: Set (ApName, ApVer)
              nextVersions = Set.fromList $ zip currentAppNames nextAppVersions
                where
                  (currentAppNames, currentAppVersions) = unzip $ Map.toList avs
                  nextAppVersions :: [ApVer]
                  nextAppVersions = (+ 1) . fst3 <$> currentAppVersions
              registeredNextVersions :: Set (ApName, ApVer)
              registeredNextVersions = Set.map (fst3 &&& snd3) (range raus)

          -- Generate the next version for an existing application
          genANextVersion :: Gen SwVer
          genANextVersion = uncurry SwVer <$> Gen.element possibleNextVersions

          fst3 (x, _, _) = x
          snd3 (_, y, _) = y

          -- Generate a new application
          genNewApp :: Gen SwVer
          genNewApp =
            (`SwVer` 1) . ApName
              <$> Gen.filter
                ((`notElem` usedNames) . ApName)
                (Gen.list (Range.constant 0 12) Gen.ascii)
            where
              usedNames =
                Set.map fst3 (range raus)
                  `union` dom avs

      generateUpdateProposalWith ::
        VKey ->
        PParams ->
        ProtVer ->
        SwVer ->
        Gen UProp
      generateUpdateProposalWith vk pps' pv' sv' =
        mkUProp
          <$> idGen
          <*> pure vk
          <*> pure pv'
          <*> pure pps'
          <*> pure sv'
          <*> stTagsGen
          <*> mdtGen

      stTagsGen :: Gen (Set STag)
      stTagsGen =
        -- TODO: We need to benchmark this against @Gen.set@. This seems to be
        -- slightly faster.
        Set.fromList
          <$> Gen.list (Range.linear 0 5) (Gen.list (Range.constant 0 10) Gen.ascii)

      mdtGen :: Gen Metadata
      mdtGen = pure Metadata

upiEnvGen :: Gen UPIEnv
upiEnvGen = do
  ngk <- Gen.integral (Range.linear 1 14)
  (,,,)
    <$> CoreGen.slotGen 0 10 -- Current slot
    <*> dmapGen ngk -- Delegation map
    <*> (BlockCount <$> Gen.word64 (Range.constant 0 100)) -- Chain stability parameter (k)
    <*> pure ngk

-- Generate an initial delegation map, using a constant number of genesis
-- keys, which is determined in this generator.
dmapGen :: Word8 -> Gen (Bimap Core.VKeyGenesis Core.VKey)
dmapGen ngk = Bimap.fromList . uncurry zip <$> vkgVkPairsGen
  where
    vkgVkPairsGen :: Gen ([Core.VKeyGenesis], [Core.VKey])
    vkgVkPairsGen = (vkgs,) <$> Gen.filter (not . null) (Gen.subsequence vks)
      where
        vkgs = VKeyGenesis . VKey . Owner . fromIntegral <$> [0 .. ngk - 1]
        -- As delegation targets we choose twice the number of genesis keys.
        -- Note that the genesis keys can delegate to themselves in the
        -- generated delegation map.
        vks = VKey . Owner . fromIntegral <$> [0 .. 2 * (ngk - 1)]

-- | Generate a protocol parameter update from a given set of current
-- protocol-parameters, ensuring the consistency of the new protocol parameters
-- w.r.t. the current ones, according to the @canUpdate@ predicate in the
-- formal specification.
--
-- TODO: we can move this into a Generator's module, but first we need to
-- disentangle the dependencies. Moving this to @Byron.Spec.Ledger.Update.Generators@ will
-- cause a circular dependency. I think the rules need to be moved into their
-- own modules.
ppsUpdateFrom :: PParams -> Gen PParams
ppsUpdateFrom pps = do
  -- NOTE: we only generate small changes in the parameters to avoid leaving the
  -- protocol parameters in a state that won't allow to produce any valid blocks
  -- anymore (for instance if the maximum block size drops to a very small
  -- value).

  -- Determine the change in the block size: a decrement or an increment that
  -- is no more than twice the current block maximum size.
  --
  -- We don't expect the maximum block size to change often, so we generate
  -- more values around the current block size (@_maxBkSz@).
  newMaxBkSize <-
    Gen.integral
      ( Range.linearFrom
          _maxBkSz
          (_maxBkSz -? 100) -- Decrement value was determined ad-hoc
          (2 * _maxBkSz)
      )
      `increasingProbabilityAt` (_maxBkSz -? 100, 2 * _maxBkSz)

  -- Similarly, we don't expect the transaction size to be changed often, so we
  -- also generate more values around the current maximum transaction size.
  let minTxSzBound = _maxTxSz `min` newMaxBkSize -? 1
  newMaxTxSize <-
    Gen.integral
      ( Range.exponential
          (minTxSzBound -? 10) -- Decrement value determined ad-hoc
          (newMaxBkSize -? 1)
      )

  PParams
    <$> pure newMaxBkSize
    <*> nextMaxHdrSzGen
    <*> pure newMaxTxSize
    <*> nextMaxPropSz
    <*> nextBkSgnCntT
    <*> pure _bkSlotsPerEpoch -- This parameter should be removed from 'PParams'
    <*> nextUpTtl
    <*> nextScriptVersion
    <*> nextUpAdptThd
    <*> nextFactorA
    <*> nextFactorB
  where
    PParams
      { _maxBkSz,
        _maxHdrSz,
        _maxTxSz,
        _maxPropSz,
        _bkSgnCntT,
        _bkSlotsPerEpoch,
        _upTtl,
        _scriptVersion,
        _upAdptThd,
        _factorA,
        _factorB
      } = pps

    FactorA fA = _factorA
    FactorB fB = _factorB
    BkSgnCntT bsct = _bkSgnCntT
    UpAdptThd uat = _upAdptThd

    nextMaxHdrSzGen :: Gen Natural
    nextMaxHdrSzGen =
      Gen.integral
        ( Range.exponentialFrom
            _maxHdrSz
            (_maxHdrSz -? 10)
            (2 * _maxHdrSz)
        )

    nextMaxPropSz :: Gen Natural
    nextMaxPropSz =
      Gen.integral
        ( Range.exponentialFrom
            _maxPropSz
            (_maxPropSz -? 1)
            (2 * _maxPropSz)
        )

    nextBkSgnCntT :: Gen BkSgnCntT
    nextBkSgnCntT =
      BkSgnCntT
        <$> Gen.double
          ( Range.exponentialFloatFrom
              bsct
              (bsct - 0.01)
              (bsct + 0.01)
          )

    nextUpTtl :: Gen SlotCount
    nextUpTtl =
      SlotCount
        <$>
        -- TODO: here we need to decide what is right the minimum value for the
        -- update-proposal TTL, and maybe adapt the rules to check the value of
        -- this parameter cannot change to anything below this value.
        --
        -- For now we choose an arbitrary constant.
        Gen.integral (Range.exponentialFrom currUpTtl minTtl (2 * currUpTtl))
        `increasingProbabilityAt` (minTtl, 2 * currUpTtl)
      where
        SlotCount currUpTtl = _upTtl
        minTtl = 2

    -- The new script version can be increased at most 1 unit
    nextScriptVersion :: Gen Natural
    nextScriptVersion = Gen.element [_scriptVersion, _scriptVersion + 1]

    nextUpAdptThd :: Gen UpAdptThd
    nextUpAdptThd =
      UpAdptThd
        <$> Gen.double (Range.exponentialFloatFrom uat 0 1)
        `increasingProbabilityAt` (0, 1)

    nextFactorA :: Gen FactorA
    nextFactorA =
      FactorA
        <$>
        -- TODO: we choose arbitrary numbers here for now.
        Gen.integral (Range.exponentialFrom fA 0 10)
        `increasingProbabilityAt` (0, 10)

    -- The next value of the factor B shouldn't drop below 'GP.c' since when
    -- elaborating this factor we divide it by 'GP.c' (see 'initialPParams').
    nextFactorB :: Gen FactorB
    nextFactorB =
      FactorB
        <$> Gen.integral (Range.exponentialFrom fB minFactorB maxFactorB)
        `increasingProbabilityAt` (minFactorB, maxFactorB)
      where
        minFactorB = 5 * fromIntegral GP.c
        maxFactorB = 15 * fromIntegral GP.c

    (-?) :: Natural -> Natural -> Natural
    n -? m = if n < m then 0 else n - m

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt ::
  Gen a ->
  (a, a) ->
  Gen a
increasingProbabilityAt gen (lower, upper) =
  Gen.frequency
    [ (5, pure lower),
      (90, gen),
      (5, pure upper)
    ]

-- | Generate a random update proposal id, by picking a large number so that the
-- probability of having an update proposal with that id is nearly zero.
randomUpId :: Gen UpId
randomUpId = UpId <$> Gen.integral (Range.constant 10000 10100)

-- | Update the signature of the update proposal.
reSign :: UProp -> UProp
reSign uprop =
  uprop
    & upSig .~ Core.sign (skey (uprop ^. upIssuer)) (uprop ^. upSigData)

data UPIVOTE deriving (Generic, Data, Typeable)

data UpivotePredicateFailure
  = UPVOTEFailure (PredicateFailure UPVOTE)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPIVOTE where
  type Environment UPIVOTE = UPIEnv
  type State UPIVOTE = UPIState
  type Signal UPIVOTE = Vote
  type PredicateFailure UPIVOTE = UpivotePredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC
          ( (sn, dms, _k, ngk),
            ( (pv, pps),
              fads,
              avs,
              rpus,
              raus,
              cps,
              vts,
              bvs,
              pws
              ),
            v
            ) <-
          judgmentContext
        let q = pps ^. upAdptThd
        (cps', vts') <-
          trans @UPVOTE $
            TRC
              ( ( sn,
                  floor $ q * fromIntegral ngk,
                  dom pws,
                  dms
                ),
                ( cps,
                  vts
                ),
                v
              )
        pure
          $! ( (pv, pps),
               fads,
               avs,
               rpus,
               raus,
               cps',
               vts',
               bvs,
               pws
             )
    ]

instance Embed UPVOTE UPIVOTE where
  wrapFailed = UPVOTEFailure

data APPLYVOTES deriving (Generic, Data, Typeable)

data ApplyVotesPredicateFailure
  = UpivoteFailure (PredicateFailure UPIVOTE)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS APPLYVOTES where
  type Environment APPLYVOTES = UPIEnv
  type State APPLYVOTES = UPIState
  type Signal APPLYVOTES = [Vote]
  type PredicateFailure APPLYVOTES = ApplyVotesPredicateFailure

  initialRules = [return $! emptyUPIState]

  transitionRules =
    [ do
        TRC (env, us, sig) <- judgmentContext
        case (sig :: [Vote]) of
          [] -> return us
          (x : xs) -> do
            us' <- trans @UPIVOTE $ TRC (env, us, x)
            us'' <- trans @APPLYVOTES $ TRC (env, us', xs)
            return us''
    ]

instance Embed UPIVOTE APPLYVOTES where
  wrapFailed = UpivoteFailure

data UPIVOTES deriving (Generic, Data, Typeable)

data UpivotesPredicateFailure
  = ApplyVotesFailure (PredicateFailure APPLYVOTES)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPIVOTES where
  type Environment UPIVOTES = UPIEnv
  type State UPIVOTES = UPIState
  type Signal UPIVOTES = [Vote]
  type PredicateFailure UPIVOTES = UpivotesPredicateFailure

  initialRules = [return $! emptyUPIState]

  transitionRules =
    [ do
        TRC (env, us, xs) <- judgmentContext
        us' <- trans @APPLYVOTES $ TRC (env, us, xs)
        -- Check which proposals are confirmed and stable, and update the
        -- application versions map.
        let (sn, _dms, _k, _ngk) = env
            ( (pv, pps),
              fads,
              avs,
              rpus,
              raus,
              cps,
              vts,
              bvs,
              pws
              ) = us'
            -- Ideally we could bump application versions for those proposals that
            -- are stable, i.e. in:
            --
            -- > dom (cps ▷<= sn -. 2 *. k)
            --
            -- However in the legacy code, application versions are adopted as
            -- soon as they are confirmed. The mainnet chain already contains such
            -- proposal, so we cannot improve this.
            cfmRaus = (dom cps) ◁ raus
            avsNew =
              [ (an, (av, sn, m))
                | (an, av, m) <- toList cfmRaus
              ]
        pure
          $! ( (pv, pps),
               fads,
               avs ⨃ avsNew,
               rpus,
               (dom cps) ⋪ raus,
               cps,
               vts,
               bvs,
               pws
             )
    ]

instance Embed APPLYVOTES UPIVOTES where
  wrapFailed = ApplyVotesFailure

instance HasTrace UPIVOTES where
  envGen _ = upiEnvGen

  sigGen (_slot, dms, _k, _ngk) ((_pv, _pps), _fads, _avs, rpus, _raus, _cps, vts, _bvs, _pws) =
    (mkVoteForDelegate <$>) . concatMap replicateFst
      <$> genVotesOnMostVotedProposals completedVotes
    where
      -- Votes needed for confirmation, per proposal ID.
      completedVotes :: [(UpId, [Core.VKeyGenesis])]
      completedVotes =
        completeVotes
          (dom dms)
          (groupVotesPerProposalId vts)
          & fmap Set.toList
          & Map.toList

      -- Make a vote for a delegate of a genesis key.
      mkVoteForDelegate ::
        (UpId, Core.VKeyGenesis) ->
        Vote
      mkVoteForDelegate (proposalId, vkg) =
        Vote vk proposalId (Core.sign (skey vk) proposalId)
        where
          vk = fromMaybe err $ Bimap.lookup vkg dms
            where
              err =
                error $
                  "Byron.Spec.Ledger.Update.mkVoteForDelegate: "
                    ++ "the genesis key was not found in the delegation map, "
                    ++ "but it should be since we used `dms` to get the keys"
                    ++ "that can vote (and so they should have a pre-image in `dms`)."

      -- Group the votes issuing proposal id, taking into account the
      -- proposals with no votes.
      groupVotesPerProposalId ::
        Set (UpId, Core.VKeyGenesis) ->
        Map UpId (Set Core.VKeyGenesis)
      groupVotesPerProposalId =
        foldl' addVote proposalIdsWithNoVotes
        where
          proposalIdsWithNoVotes :: Map UpId (Set Core.VKeyGenesis)
          proposalIdsWithNoVotes = Map.fromList $ (,Set.empty) <$> Set.toList (dom rpus)

          addVote ::
            Map UpId (Set Core.VKeyGenesis) ->
            (UpId, Core.VKeyGenesis) ->
            Map UpId (Set Core.VKeyGenesis)
          addVote m (proposalId, genesisKey) =
            case Map.lookup proposalId m of
              Nothing ->
                Map.insert proposalId (Set.singleton genesisKey) m
              Just votesForProposalId ->
                Map.insert proposalId (Set.insert genesisKey votesForProposalId) m

      -- Add the missing votes w.r.t. a set of votes cast so far and genesis
      -- keys that can vote.
      completeVotes ::
        -- Genesis keys that can vote
        Set Core.VKeyGenesis ->
        -- Votes for the registered update proposals
        Map UpId (Set Core.VKeyGenesis) ->
        Map UpId (Set Core.VKeyGenesis)
      completeVotes genesisKeys votes =
        (genesisKeys \\) <$> votes

      -- Given a sequence of update proposals ID's and the genesis keys that need
      -- to vote for confirmation, generate votes on the most voted proposals.
      --
      -- A proposal is said to be most voted if it is associated to the
      -- minimal number of votes needed for confirmation.
      --
      -- This basically takes the top @n@ most voted proposals (for some arbitrary
      -- @n@), say @[(p_0, vs_0), ..., (p_n-1, vs_(n-1))]@ and generates votes of the
      -- form, @(p_i, vs_i_j)@, where @vs_i_j@ is an arbitrary subsequence of @vs_i@.
      genVotesOnMostVotedProposals ::
        [(UpId, [Core.VKeyGenesis])] ->
        Gen [(UpId, [Core.VKeyGenesis])]
      genVotesOnMostVotedProposals votesNeeded = do
        -- Determine on how many proposals we will vote
        numberOfProposals <- Gen.int (Range.constant 0 (length votesNeeded))
        let votes :: [(UpId, [Core.VKeyGenesis])]
            votes = take numberOfProposals $ sortOn (length . snd) votesNeeded
        zip (fst <$> votes) <$> traverse Gen.subsequence (snd <$> votes)

      replicateFst ::
        (a, [b]) ->
        [(a, b)]
      replicateFst (a, bs) = zip (repeat a) bs

data UPIEND deriving (Generic, Data, Typeable)

data UpiendPredicateFailure
  = UPENDFailure (PredicateFailure UPEND)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPIEND where
  type Environment UPIEND = UPIEnv
  type State UPIEND = UPIState
  type Signal UPIEND = (ProtVer, Core.VKey)
  type PredicateFailure UPIEND = UpiendPredicateFailure

  initialRules = [return $! emptyUPIState]

  transitionRules =
    [ do
        TRC
          ( (sn, dms, k, ngk),
            ( (pv, pps),
              fads,
              avs,
              rpus,
              raus,
              cps,
              vts,
              bvs,
              pws
              ),
            (bv, vk)
            ) <-
          judgmentContext
        let t = floor $ pps ^. upAdptThd * fromIntegral ngk
        (fads', bvs') <- trans @UPEND $ TRC ((sn, t, dms, cps, rpus, k), (fads, bvs), (bv, vk))
        let u = pps ^. upTtl
            pidskeep = dom (pws ▷>= sn -. u) `union` dom cps
            vskeep = dom (range rpus')
            rpus' = pidskeep ◁ rpus
        return
          ( (pv, pps),
            fads',
            avs,
            rpus',
            pidskeep ◁ raus,
            cps,
            pidskeep ◁ vts,
            vskeep ◁ bvs',
            pidskeep ◁ pws
          )
    ]

instance Embed UPEND UPIEND where
  wrapFailed = UPENDFailure

-- | Given a list of protocol versions and keys endorsing those versions,
-- generate a protocol-version endorsement, or 'Nothing' if the list of
-- endorsements is empty. The version to be endorsed will be selected from those
-- versions that have the most endorsements.
pickHighlyEndorsedProtocolVersion ::
  -- | Current set of endorsements
  [(ProtVer, Set Core.VKeyGenesis)] ->
  Gen (Maybe ProtVer)
pickHighlyEndorsedProtocolVersion endorsementsList =
  if null mostEndorsedProposals
    then pure Nothing
    else Just <$> Gen.element mostEndorsedProposals
  where
    -- Take the top 5 most voted proposals, and endorse them. The constant 5 is determined arbitrarily
    -- here.

    mostEndorsedProposals :: [ProtVer]
    mostEndorsedProposals =
      sortOn (Down . second length) endorsementsList
        & take 5
        & fmap fst

data PVBUMP deriving (Generic, Data, Typeable)

-- PVBUMP has no predicate failures
data PvbumpPredicateFailure = NoPVBUMPFailure
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS PVBUMP where
  type
    Environment PVBUMP =
      ( Core.Slot,
        [(Core.Slot, (ProtVer, PParams))],
        BlockCount -- Chain stability parameter; this is a global
        -- constant in the formal specification, which we put
        -- in this environment so that we can test with
        -- different values of it.
      )
  type
    State PVBUMP =
      (ProtVer, PParams)

  type Signal PVBUMP = ()
  type PredicateFailure PVBUMP = PvbumpPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC ((s_n, fads, k), (pv, pps), ()) <- judgmentContext
        case s_n -. 4 *. k <=◁ fads of
          [] ->
            pure $! (pv, pps)
          (_s, (pv_c, pps_c)) : _xs ->
            pure $! (pv_c, pps_c)
    ]

data UPIEC deriving (Generic, Data, Typeable)

data UpiecPredicateFailure
  = PVBUMPFailure (PredicateFailure PVBUMP)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS UPIEC where
  type
    Environment UPIEC =
      ( Core.Epoch,
        BlockCount -- Chain stability parameter; this is a global
        -- constant in the formal specification, which we put
        -- in this environment so that we can test with
        -- different values of it.
      )
  type State UPIEC = UPIState
  type Signal UPIEC = ()
  type PredicateFailure UPIEC = UpiecPredicateFailure

  initialRules = []
  transitionRules =
    [ do
        TRC ((e_n, k), us, ()) <- judgmentContext
        let (pv, pps) = us ^. _1 :: (ProtVer, PParams)
            fads = us ^. _2 :: [(Core.Slot, (ProtVer, PParams))]
        (pv', pps') <-
          trans @PVBUMP $
            TRC ((GP.epochFirstSlot k e_n, fads, k), (pv, pps), ())
        return
          $! if pv == pv'
            then us
            else
              ( (pv', pps') :: (ProtVer, PParams),
                [] :: [(Core.Slot, (ProtVer, PParams))],
                us ^. _3 :: Map ApName (ApVer, Core.Slot, Metadata),
                Map.empty :: Map UpId (ProtVer, PParams),
                -- Note that we delete the registered application proposals from the
                -- state on epoch change, since adopting these depends on the @cps@
                -- and @pws@ sets, which are deleted as well. So it doesn't seem
                -- sensible to keep @raus@ around.
                Map.empty :: Map UpId (ApName, ApVer, Metadata),
                Map.empty :: Map UpId Core.Slot,
                Set.empty :: Set (UpId, Core.VKeyGenesis),
                Set.empty :: Set (ProtVer, Core.VKeyGenesis),
                Map.empty :: Map UpId Core.Slot
              )
    ]

instance Embed PVBUMP UPIEC where
  wrapFailed = PVBUMPFailure

-- | Generate an optional update-proposal and a list of votes, given an update
-- environment and state.
--
-- The update proposal and votes need to be generated at the same time, since
-- this allow us to generate update votes for update proposals issued in the
-- same block as the votes.
updateProposalAndVotesGen ::
  UPIEnv ->
  UPIState ->
  Gen (Maybe UProp, [Vote])
updateProposalAndVotesGen upienv upistate = do
  let rpus = registeredProtocolUpdateProposals upistate
  if Set.null (dom rpus)
    then generateUpdateProposalAndVotes
    else
      Gen.frequency
        [ (5, generateOnlyVotes),
          (1, generateUpdateProposalAndVotes)
        ]
  where
    generateOnlyVotes = (Nothing,) <$> sigGen @UPIVOTES upienv upistate
    generateUpdateProposalAndVotes = do
      updateProposal <- sigGen @UPIREG upienv upistate
      -- We want to have the possibility of generating votes for the proposal we
      -- registered.
      case applySTS @UPIREG (TRC (upienv, upistate, updateProposal)) of
        Left _ ->
          (Just updateProposal,)
            <$> sigGen @UPIVOTES upienv upistate
        Right upistateAfterRegistration ->
          (Just updateProposal,)
            <$> sigGen @UPIVOTES upienv upistateAfterRegistration

-- | Generate an endorsement given an update environment and state.
protocolVersionEndorsementGen ::
  UPIEnv ->
  UPIState ->
  Gen ProtVer
protocolVersionEndorsementGen upienv upistate =
  fromMaybe (protocolVersion upistate)
    <$> pickHighlyEndorsedProtocolVersion endorsementsList
  where
    -- Generate a list of protocol version endorsements. For this we look at the
    -- current endorsements, and confirmed and stable proposals.
    --
    -- If there are no endorsements, then the confirmed and stable proposals
    -- provide fresh protocol versions that can be endorsed.
    endorsementsList :: [(ProtVer, Set Core.VKeyGenesis)]
    endorsementsList =
      endorsementsMap `Map.union` emptyEndorsements
        & Map.toList
      where
        emptyEndorsements :: Map ProtVer (Set Core.VKeyGenesis)
        emptyEndorsements =
          zip stableAndConfirmedVersions (repeat Set.empty)
            & Map.fromList
          where
            stableAndConfirmedVersions ::
              [ProtVer]
            stableAndConfirmedVersions =
              stableAndConfirmedProposalIDs ◁ rpus
                & Map.elems
                & fmap fst
              where
                stableAndConfirmedProposalIDs =
                  dom (confirmedProposals upistate ▷<= sn -. 4 *. k)
                  where
                    (sn, _, k, _) = upienv

                rpus = registeredProtocolUpdateProposals upistate

        endorsementsMap :: Map ProtVer (Set Core.VKeyGenesis)
        endorsementsMap =
          Set.toList (endorsements upistate)
            & fmap (second Set.singleton)
            & Map.fromListWith Set.union

--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

deriveGoblin ''ApVer
deriveGoblin ''ApName
deriveGoblin ''Metadata
deriveGoblin ''ProtVer
deriveGoblin ''PParams
deriveGoblin ''SwVer
deriveGoblin ''UpId
deriveGoblin ''UProp
deriveGoblin ''Vote

instance GeneOps g => Goblin g FactorA where
  tinker gen =
    tinkerRummagedOrConjureOrSave
      ( (\x -> FactorA (x `mod` fromIntegral GP.lovelaceCap))
          <$$> tinker ((\(FactorA x) -> x) <$> gen)
      )
  conjure =
    saveInBagOfTricks
      =<< ( FactorA . (`mod` fromIntegral GP.lovelaceCap)
              <$> conjure
          )

instance GeneOps g => Goblin g FactorB where
  tinker gen =
    tinkerRummagedOrConjureOrSave
      ( (\x -> FactorB (x `mod` fromIntegral GP.lovelaceCap))
          <$$> tinker ((\(FactorB x) -> x) <$> gen)
      )
  conjure =
    saveInBagOfTricks
      =<< ( FactorB . (`mod` fromIntegral GP.lovelaceCap)
              <$> conjure
          )

instance GeneOps g => Goblin g BkSgnCntT where
  tinker _ =
    pure <$> conjure
  conjure =
    saveInBagOfTricks =<< do
      i <- transcribeGenesAsInt 100
      pure (BkSgnCntT (fromIntegral i / 100))

instance GeneOps g => Goblin g UpAdptThd where
  tinker _ =
    pure <$> conjure
  conjure =
    saveInBagOfTricks =<< do
      i <- transcribeGenesAsInt 100
      pure (UpAdptThd (fromIntegral i / 100))

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''ApName
deriveAddShrinks ''ApVer
deriveAddShrinks ''BkSgnCntT
deriveAddShrinks ''FactorA
deriveAddShrinks ''FactorB
deriveAddShrinks ''Metadata
deriveAddShrinks ''PParams
deriveAddShrinks ''ProtVer
deriveAddShrinks ''SwVer
deriveAddShrinks ''UpAdptThd
deriveAddShrinks ''UpId
deriveAddShrinks ''UProp
deriveAddShrinks ''Vote

--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''ApName
deriveSeedGoblin ''ApVer
deriveSeedGoblin ''BkSgnCntT
deriveSeedGoblin ''FactorA
deriveSeedGoblin ''FactorB
deriveSeedGoblin ''SwVer
deriveSeedGoblin ''PParams
deriveSeedGoblin ''ProtVer
deriveSeedGoblin ''Metadata
deriveSeedGoblin ''UpAdptThd
deriveSeedGoblin ''UpId

--------------------------------------------------------------------------------
-- GoblinData & goblin-tinkered SignalGenerators
--------------------------------------------------------------------------------

mkGoblinGens
  "UPIREG"
  [ "UPREGFailure_DoesNotVerify",
    "UPREGFailure_NotGenesisDelegate",
    "UPREGFailure_UPVFailure_AVChangedInPVUpdate",
    "UPREGFailure_UPVFailure_PVChangedInSVUpdate",
    "UPREGFailure_UPVFailure_ParamsChangedInSVUpdate",
    "UPREGFailure_UPVFailure_UPPVVFailure_CannotFollowPv",
    "UPREGFailure_UPVFailure_UPPVVFailure_CannotUpdatePv",
    "UPREGFailure_UPVFailure_UPSVVFailure_AlreadyProposedSv",
    "UPREGFailure_UPVFailure_UPSVVFailure_CannotFollowSv",
    "UPREGFailure_UPVFailure_UPSVVFailure_InvalidApplicationName",
    "UPREGFailure_UPVFailure_UPSVVFailure_InvalidSystemTags"
  ]

mkGoblinGens
  "UPIVOTES"
  [ "ApplyVotesFailure_UpivoteFailure_UPVOTEFailure_ADDVOTEFailure_AVSigDoesNotVerify",
    "ApplyVotesFailure_UpivoteFailure_UPVOTEFailure_ADDVOTEFailure_NoUpdateProposal"
  ]

--------------------------------------------------------------------------------
-- Tampering functions
--
-- These must be dropped at the end of the file because they reference
-- TH-expanded definitions.
--------------------------------------------------------------------------------

-- | Tamper with the update proposal in such a way that the following
-- predicate failures are triggered with equal probability:
--
-- - UPREGFailure
--   - UPVFailure
--     - UPVFailure
--       - UPPVVFailure
--         - CannotFollowPv
--         - CannotUpdatePv
--         - AlreadyProposedPv
--       - UPSVVFailure
--         - AlreadyProposedSv
--         - CannotFollowSv
--         - InvalidApplicationName
--         - InvalidSystemTags
--       - AVChangedInPVUpdate
--       - ParamsChangedInSVUpdate
--       - PVChangedInSVUpdate
--   - NotGenesisDelegate
--   - DoesNotVerify
tamperWithUpdateProposal :: UPIEnv -> UPIState -> UProp -> Gen UProp
tamperWithUpdateProposal env st uprop = do
  -- The frequencies above were determined ad-hoc to get an even coverage in the
  -- resulting predicate failures.
  let failureGenerators =
        [ (1, invalidProtocolVersion),
          (1, invalidParametersUpdate),
          (5, duplicatedProtocolVersion),
          (5, duplicatedSoftwareVersion),
          (1, invalidSoftwareVersion),
          (1, invalidApplicationName),
          (1, invalidSystemTag),
          (1, invalidIssuer)
        ]
          ++ (map (\sg -> (1, sg env st)) goblinGensUPIREG)
  tamperedUprop <- Gen.frequency failureGenerators
  -- We need to re-sign the update proposal since we changed the contents of
  -- 'uprop', however in 10/n of the cases we want to trigger a 'DoesNotVerify'
  -- error (where 'n' is the total number of predicate failures, 'n = length
  -- failureGenerators + 1'). Thus, in 1-/n of the cases we simply return the
  -- tampered proposal without re-signing it, which will cause the
  -- 'DoesNotVerify' failure.
  Gen.frequency
    [ (length failureGenerators, pure $! reSign tamperedUprop),
      -- Using 10 in the frequency below will give you us around 15%
      -- of proposals with an invalid hash.
      (10, pure $! tamperedUprop)
    ]
  where
    ((_pv, _pps), _fads, _avs, rpus, raus, _cps, _vts, _bvs, _pws) = st

    invalidProtocolVersion :: Gen UProp
    invalidProtocolVersion =
      (\mj mn alt -> uprop {_upPV = ProtVer mj mn alt})
        <$> Gen.integral (Range.constant 0 100)
        <*> Gen.integral (Range.constant 0 100)
        <*> Gen.integral (Range.constant 0 100)

    invalidParametersUpdate :: Gen UProp
    invalidParametersUpdate =
      Gen.element
        [ uprop & upParams . maxBkSz .~ uprop ^. upParams . maxBkSz * 3,
          uprop & upParams . maxTxSz .~ uprop ^. upParams . maxBkSz * 2,
          uprop & upParams . scriptVersion .~ uprop ^. upParams . scriptVersion + 2
        ]

    duplicatedProtocolVersion :: Gen UProp
    duplicatedProtocolVersion =
      let registeredVersions = fst <$> Map.elems rpus
       in if null registeredVersions
            then mzero
            else do
              duplicatedVersion <- Gen.element registeredVersions
              pure $! uprop & upPV .~ duplicatedVersion

    duplicatedSoftwareVersion :: Gen UProp
    duplicatedSoftwareVersion =
      let registeredVersions = fmap fstSnd (Map.elems raus)
       in if null registeredVersions
            then mzero
            else do
              (an, av) <- Gen.element registeredVersions
              pure $! uprop & upSwVer .~ SwVer {_svName = an, _svVer = av}
      where
        fstSnd :: (a, b, c) -> (a, b)
        fstSnd (x, y, _) = (x, y)

    invalidSoftwareVersion :: Gen UProp
    invalidSoftwareVersion =
      pure $! over (upSwVer . svVer) (+ 42) uprop

    invalidApplicationName :: Gen UProp
    invalidApplicationName = do
      randomName <- ApName <$> Gen.string (Range.linear 10 20) Gen.unicode
      pure $! uprop & upSwVer . svName .~ randomName

    invalidSystemTag :: Gen UProp
    invalidSystemTag = do
      randomTag <- Gen.string (Range.linear 10 20) Gen.unicode
      pure $! over upSTags (Set.insert randomTag) uprop

    invalidIssuer :: Gen UProp
    invalidIssuer =
      -- We use a large (constant) increment here, so that we have a bigger chance to get a
      -- non-genesis delegate.
      pure $! over upIssuer (VKey . Owner . (100 +) . coerce) uprop

-- | Tamper with some of the votes provided as parameter in such a way that the following
-- predicate failures are triggered with equal probability:
--
-- - AVSigDoesNotVerify
-- - NoUpdateProposal
tamperWithVotes :: UPIEnv -> UPIState -> [Vote] -> Gen [Vote]
tamperWithVotes env st vs =
  Gen.frequency
    [ (1, go vs),
      (1, Gen.choice (map (\sg -> sg env st) goblinGensUPIVOTES))
    ]
  where
    go [] = do
      -- If there are no votes, then we generate a random one.
      vote <- mkVote <$> CoreGen.vkGen <*> randomUpId
      (: []) <$> tamperWithVote vote
    go [vote] =
      -- If we have only one vote we duplicate it and try again, raising the
      -- probabilities that one of the votes in the list will be tampered with.
      go [vote, vote]
    go votes =
      traverse tamperWithVote votes

tamperWithVote :: Vote -> Gen Vote
tamperWithVote vote =
  Gen.choice
    [ -- Change the vote by some random proposal id. There might be a chance
      -- that the proposal id exists though, but this should be minimal if
      -- we generate only small valid proposal id's.
      mkVote (vote ^. vCaster)
        . UpId
        <$> Gen.integral (Range.constant 10000 10100),
      do
        vk <- CoreGen.vkGen
        -- Replace the signature by the signature of some random key.
        pure $! vote & vSig .~ Core.sign (skey vk) (vote ^. vPropId),
      pure $! vote
    ]

--------------------------------------------------------------------------------
-- FieldX instances for a 9-tuple
--------------------------------------------------------------------------------

instance Field1 (a, b, c, d, e, f, g, h, i) (a', b, c, d, e, f, g, h, i) a a' where
  _1 k ~(a, b, c, d, e, f, g, h, i) = (\a' -> (a', b, c, d, e, f, g, h, i)) <$> k a
  {-# INLINE _1 #-}

instance Field2 (a, b, c, d, e, f, g, h, i) (a, b', c, d, e, f, g, h, i) b b' where
  _2 k ~(a, b, c, d, e, f, g, h, i) = (\b' -> (a, b', c, d, e, f, g, h, i)) <$> k b
  {-# INLINE _2 #-}

instance Field3 (a, b, c, d, e, f, g, h, i) (a, b, c', d, e, f, g, h, i) c c' where
  _3 k ~(a, b, c, d, e, f, g, h, i) = (\c' -> (a, b, c', d, e, f, g, h, i)) <$> k c
  {-# INLINE _3 #-}
