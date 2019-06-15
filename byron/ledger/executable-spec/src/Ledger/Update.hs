{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- This is for the Hashable Set instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Update
  (module Ledger.Update)
where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (mzero)
import Data.Bimap (Bimap, empty, lookupR)
import qualified Data.Bimap as Bimap
import Data.Char (isAscii)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Ix (inRange)
import Data.List (notElem)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set, union, (\\))
import qualified Data.Set as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural

import Data.AbstractSize (HasTypeReps)
import Control.State.Transition
import Control.State.Transition.Generator (HasTrace, initEnvGen, sigGen)

import Ledger.Core
  ( BlockCount(..)
  , HasHash
  , KeyPair(KeyPair)
  , Owner(Owner)
  , Relation(..)
  , Slot(..)
  , SlotCount(..)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , (*.)
  , (-.)
  , (∈)
  , (∉)
  , (⋪)
  , (▷)
  , (▷<=)
  , (▷>=)
  , (◁)
  , (⨃)
  , dom
  , hash
  , keyPair
  , minusSlotMaybe
  , sKey
  , skey
  , unSlot
  )
import qualified Ledger.Core as Core
import qualified Ledger.Core.Generators as CoreGen
import qualified Ledger.GlobalParams as GP

import Prelude hiding (min)

-- | Protocol parameters.
--
data PParams = PParams -- TODO: this should be a module of @cs-ledger@.
  { _maxBkSz  :: !Natural
  -- ^ Maximum (abstract) block size in words
  , _maxHdrSz :: !Natural
  -- ^ Maximum (abstract) block header size in words
  , _maxTxSz :: !Natural
  -- ^ Maximum (abstract) transaction size in words
  , _maxPropSz :: !Natural
  -- ^ Maximum (abstract) update proposal size in words
  , _bkSgnCntT :: Double
  -- ^ Fraction [0, 1] of the blocks that can be signed by any given key in a
  -- window of lenght '_bkSgnCntW'. This value will be typically between 1/5
  -- and 1/4
  , _bkSlotsPerEpoch :: Core.SlotCount -- TODO: this should be removed since
                                       -- the number of slots per epoch should
                                       -- remain constant. ^ Number of slots in
                                       -- an epoch
  , _upTtl :: !Core.SlotCount
  -- ^ Update proposal TTL in slots
  , _scriptVersion :: !Natural
  -- ^ Script version
  , _cfmThd :: !Int -- TODO: this should be a double
  -- ^ Update proposal confirmation threshold (number of votes)
  , _upAdptThd :: !Double
  -- ^ Update adoption threshold: a proportion of block issuers that have to
  -- endorse a given version to become candidate for adoption
  , _stableAfter :: !Core.BlockCount
  -- ^ Chain stability parameter
  , _factorA :: !Int -- TODO: this should be a double
  -- ^ Minimum fees per transaction
  , _factorB :: !Int
  -- ^ Additional fees per transaction size
  } deriving (Eq, Generic, Ord, Show, Hashable)

makeLenses ''PParams

instance HasTypeReps PParams

newtype UpId = UpId Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Hashable)
  deriving anyclass (HasTypeReps)

-- | Protocol version
data ProtVer = ProtVer
  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Natural
  } deriving (Eq, Generic, Ord, Show, Hashable)

makeLenses ''ProtVer

instance HasTypeReps ProtVer

newtype ApName = ApName String
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Hashable)

instance HasTypeReps ApName

-- | Application version
newtype ApVer = ApVer Natural
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num, Hashable)

instance HasTypeReps ApVer

data SwVer = SwVer
  { _svName :: ApName
  , _svVer :: ApVer
  } deriving (Eq, Generic, Show, Hashable)

makeLenses ''SwVer

instance HasTypeReps SwVer

-- | Part of the update proposal which must be signed
type UpSD =
  ( ProtVer
  , PParams
  , SwVer
  )

-- | System tag, this represents a target operating system for the update (e.g.
-- @linux@, @win64@, or @mac32@).
type STag = String

-- | For now we do not have any requirements on metadata.
data Metadata = Metadata
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Update proposal
data UProp = UProp
  { _upId :: UpId
  , _upIssuer :: Core.VKey
  , _upParams :: PParams
  , _upPV :: ProtVer
  , _upSwVer :: SwVer
  , _upSig :: Core.Sig UpSD
  , _upSTags :: Set STag
  -- ^ System tags involved in the update proposal.
  , _upMdt :: Metadata
  -- ^ Metadata required for performing software updates.
  } deriving (Eq, Generic, Show, Hashable)

instance Hashable a => Hashable (Set a) where
  hashWithSalt = H.hashUsing Set.toList


makeLenses ''UProp

instance HasTypeReps (ProtVer, PParams, SwVer)
instance HasTypeReps Metadata
instance HasTypeReps UProp
instance HasTypeReps (Maybe UProp)

upSigData :: Lens' UProp UpSD
upSigData = lens
  (\up -> (up ^. upPV, up ^. upParams, up ^. upSwVer))
  (\up (pv, pps, sv) -> up
    & upParams .~ pps
    & upPV .~ pv
    & upSwVer .~ sv
  )

-- | Test if a pair is present in a map.
inMap :: (Ord key, Eq v) => key -> v -> Map key v -> Bool
inMap key v m = case Map.lookup key m of
  Just x | x == v -> True
  _ -> False

-- | Invert a map
--
--   Examples:
--
--   >>> invertMap (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3), ('d', 1)])
--   fromList [(1,fromList "ad"),(2,fromList "b"),(3,fromList "c")]
invertMap
  :: (Ord k, Ord v)
  => Map k v
  -> Map v (Set k)
invertMap
  = Map.fromListWith (Set.union)
  . fmap (fmap Set.singleton . swap)
  . Map.toList

-- | Invert a map which we assert to be a bijection.
--   If this map is not a bijection, the behaviour is not guaranteed.
--
--   Examples:
--
--   >>> invertBijection (Map.fromList [('a', 1 :: Int), ('b', 2), ('c', 3)])
--   fromList [(1,'a'),(2,'b'),(3,'c')]
invertBijection
  :: Ord v
  => Map k v
  -> Map v k
invertBijection
  = Map.fromListWith const
  . fmap swap
  . Map.toList


(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b
infix 1 ==>

pvCanFollow
  :: ProtVer
  -> ProtVer
  -> Bool
pvCanFollow (ProtVer mjn min an) (ProtVer mjp mip ap)
  = (mjp, mip, ap) < (mjn, min, an)
  && (inRange (0,1) (mjn - mjp))
  && ((mjp == mjn) ==> (mip +1 == min))
  && ((mjp +1 == mjn) ==> (min == 0))

-- | Check whether an update proposal marks a valid update
--
--   TODO At the moment we don't check size in here - should we?
canUpdate
  :: PParams
  -> UProp
  -> Bool
canUpdate pps prop =
  (prop ^. upParams . maxBkSz <= 2 * pps ^. maxBkSz)
  && (prop ^. upParams . maxBkSz > prop ^. upParams . maxTxSz)
  && (inRange (0,1) $ prop ^. upParams . scriptVersion - pps ^. scriptVersion)

svCanFollow
  :: Map ApName (ApVer, Core.Slot, Metadata)
  -> (ApName, ApVer)
  -> Bool
svCanFollow avs (an,av) =
    (case Map.lookup an avs of
      Nothing -> True
      Just (x, _, _) -> av == x + 1
    ) && (an `Set.notMember` dom avs ==> av == ApVer 0)
  where

------------------------------------------------------------------------
-- Update proposals
------------------------------------------------------------------------

-- | Update Proposal Software Version Validation
data UPSVV

instance STS UPSVV where
  type Environment UPSVV = Map ApName (ApVer, Core.Slot, Metadata)
  type State UPSVV = Map UpId (ApName, ApVer, Metadata)
  type Signal UPSVV = UProp

  data PredicateFailure UPSVV
    = AlreadyProposedSv
    | CannotFollowSv
    | InvalidApplicationName
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (avs, raus, up) <- judgmentContext
        let SwVer an av = up ^. upSwVer
        apNameValid an ?! InvalidApplicationName
        svCanFollow avs (an,av) ?! CannotFollowSv
        (an, av) `notElem` fmap fstSnd (Map.elems raus) ?! AlreadyProposedSv
        return $! raus ⨃ [(up ^. upId, (an, av, up ^. upMdt))]
    ]
    where
      apNameValid (ApName n) = all isAscii n && length n <= 12
      fstSnd (x, y, _) = (x, y)


data UPPVV

instance STS UPPVV where
  type Environment UPPVV =
    ( ProtVer
    , PParams
    )
  type State UPPVV = Map UpId (ProtVer, PParams)
  type Signal UPPVV = UProp

  data PredicateFailure UPPVV
    = CannotFollowPv
    | CannotUpdatePv
    | AlreadyProposedPv
    | InvalidSystemTags
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ((pv, pps), rpus, up) <- judgmentContext
        let pid = up ^. upId
            nv = up ^. upPV
            ppsn = up ^. upParams
        pvCanFollow nv pv ?! CannotFollowPv
        canUpdate pps up ?! CannotUpdatePv
        nv `notElem` (fst <$> Map.elems rpus) ?! AlreadyProposedPv
        all sTagValid (up ^. upSTags) ?! InvalidSystemTags
        return $! rpus ⨃ [(pid, (nv, ppsn))]
    ]
    where
      sTagValid tag = all isAscii tag && length tag <= 10


-- | Update proposal validity
data UPV

instance STS UPV where
  type Environment UPV =
    ( ProtVer
    , PParams
    , Map ApName (ApVer, Core.Slot, Metadata)
    )

  type State UPV =
    ( Map UpId (ProtVer, PParams)
    , Map UpId (ApName, ApVer, Metadata)
    )

  type Signal UPV = UProp

  data PredicateFailure UPV
    = UPPVVFailure (PredicateFailure UPPVV)
    | UPSVVFailure (PredicateFailure UPSVV)
    | AVChangedInPVUpdate ApName ApVer
    | ParamsChangedInSVUpdate
    | PVChangedInSVUpdate
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (pv, pps, avs)
            , (rpus, raus)
            , up
            ) <- judgmentContext
        rpus' <- trans @UPPVV $ TRC ((pv, pps), rpus, up)
        let SwVer an av = up ^. upSwVer
        inMap an av (swVer <$> avs) ?! AVChangedInPVUpdate an av
        return $! (rpus', raus)
    , do
        TRC ( (pv, pps, avs)
            , (rpus, raus)
            , up
            ) <- judgmentContext
        pv == up ^. upPV ?! PVChangedInSVUpdate
        up ^. upParams == pps ?! ParamsChangedInSVUpdate
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        return $! (rpus, raus')
    , do
        TRC ( (pv, pps, avs)
            , (rpus, raus)
            , up
            ) <- judgmentContext
        rpus' <- trans @UPPVV $ TRC ((pv, pps), rpus, up)
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        return $! (rpus', raus')
    ]
    where
      swVer (x, _, _) = x

instance Embed UPPVV UPV where
  wrapFailed = UPPVVFailure

instance Embed UPSVV UPV where
  wrapFailed = UPSVVFailure

data UPREG

instance STS UPREG where
  type Environment UPREG =
    ( ProtVer
    , PParams
    , Map ApName (ApVer, Core.Slot, Metadata)
    , Bimap Core.VKeyGenesis Core.VKey
    )
  type State UPREG =
    ( Map UpId (ProtVer, PParams)
    , Map UpId (ApName, ApVer, Metadata)
    )
  type Signal UPREG = UProp

  data PredicateFailure UPREG
    = UPVFailure (PredicateFailure UPV)
    | NotGenesisDelegate
    | DoesNotVerify
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (pv, pps, avs, dms)
            , (rpus, raus)
            , up
            ) <- judgmentContext
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
  { _vCaster :: Core.VKey
  , _vPropId :: UpId
  , _vSig :: Core.Sig UpId
  } deriving (Eq, Generic, Show, Hashable)

makeLenses ''Vote

instance HasTypeReps Vote

instance HasHash (Maybe Ledger.Update.UProp, [Ledger.Update.Vote]) where
  hash = Core.Hash . H.hash


data ADDVOTE

instance STS ADDVOTE where
  type Environment ADDVOTE =
    ( Set UpId
    , Bimap Core.VKeyGenesis Core.VKey
    )
  type State ADDVOTE = Set (UpId, Core.VKeyGenesis)
  type Signal ADDVOTE = Vote

  data PredicateFailure ADDVOTE
    = AVSigDoesNotVerify
    | NoUpdateProposal UpId
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (rups, dms)
            , vts
            , vote
            ) <- judgmentContext
        let pid = vote ^. vPropId
            vk = vote ^. vCaster
            vtsPid =
              case lookupR vk dms of
                Just vks -> Set.singleton (pid, vks)
                Nothing  -> Set.empty
        Set.member pid rups ?! NoUpdateProposal pid
        Core.verify vk pid (vote ^. vSig) ?! AVSigDoesNotVerify
        return $! vts <> vtsPid
    ]

data UPVOTE

instance STS UPVOTE where
  type Environment UPVOTE =
    ( Core.Slot
    , PParams
    , Set UpId
    , Bimap Core.VKeyGenesis Core.VKey
    )
  type State UPVOTE =
    ( Map UpId Core.Slot
    , Set (UpId, Core.VKeyGenesis)
    )
  type Signal UPVOTE = Vote
  data PredicateFailure UPVOTE
    = ADDVOTEFailure (PredicateFailure ADDVOTE)
    | HigherThanThdAndNotAlreadyConfirmed
    | CfmThdNotReached
    | AlreadyConfirmed
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (_, pps, rups, dms)
            , (cps, vts)
            , vote
            ) <- judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        size (Set.singleton pid ◁ vts') < (pps ^. cfmThd) || pid `Set.member` (dom cps) ?! HigherThanThdAndNotAlreadyConfirmed
        return (cps, vts')
    , do
        TRC ( (sn, pps, rups, dms)
            , (cps, vts)
            , vote
            ) <- judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        (pps ^. cfmThd) <= size (Set.singleton pid ◁ vts') ?! CfmThdNotReached
        pid `Set.notMember` dom cps ?! AlreadyConfirmed
        return (cps ⨃ [(pid, sn)], vts')

    ]

instance Embed ADDVOTE UPVOTE where
  wrapFailed = ADDVOTEFailure

------------------------------------------------------------------------
-- Update voting
------------------------------------------------------------------------

data FADS

instance STS FADS where
  type Environment FADS = ()
  type State FADS = [(Core.Slot, (ProtVer, PParams))]
  type Signal FADS = (Core.Slot, (ProtVer, PParams))
  data PredicateFailure FADS
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( ()
            , fads
            , (sn, (bv, ppsc))
            ) <- judgmentContext
        return $ case fads of
          ((_, (pvc, _)) : _ ) -> if pvc < bv
            then (sn, (bv, ppsc)) : fads
            else fads
          _ -> (sn, (bv, ppsc)) : fads
    ]

data UPEND

-- | Find the key that corresponds to the value satisfying the given predicate.
-- In case zero or more than one key is found this function returns Nothing.
findKey :: (v -> Bool) -> Map k v -> Maybe (k, v)
findKey p m =
  case Map.toList (Map.filter p m) of
    [(k, v)] -> Just (k, v)
    _        -> Nothing

instance STS UPEND where
  type Environment UPEND =
    ( Core.Slot                    -- Current slot number
    , Natural                      -- Adoption threshold
    , Bimap VKeyGenesis VKey       -- Delegation map
    , Map UpId Core.Slot           -- Confirmed proposals
    , Map UpId (ProtVer, PParams)  -- Registered update proposals
    , BlockCount                   -- Chain stability parameter. This
                                   -- is deemed to be a global
                                   -- constant that we temporarily put
                                   -- there.
    )
  type State UPEND =
    ( [(Core.Slot, (ProtVer, PParams))]
    , Set (ProtVer, Core.VKeyGenesis)
    )
  type Signal UPEND = (ProtVer, Core.VKey)

  data PredicateFailure UPEND
    = ProtVerUnknown ProtVer
    | TryNextRule
    | CanAdopt ProtVer
    | CannotAdopt ProtVer
    | NotADelegate VKey
    | UnconfirmedProposal UpId
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [
      do
        TRC ( (sn, _t, _dms, cps, rpus, k)
            , (fads, bvs)
            , (bv, _vk)
            ) <- judgmentContext
        case findKey ((== bv) . fst) rpus of
          Just (pid, _) -> do
            -- If we found the proposal id that corresponds to 'bv' then we
            -- have to check that it isn't confirmed for this rule to succeed.
            pid ∉ dom (cps ▷<= sn  -. 2 *. k) ?! TryNextRule
            return $! (fads, bvs)
          Nothing ->
            -- If we didn't find the proposal id that corresponds to 'bv' then
            -- this rule succeeds.
            --
            -- Note that the difference w.r.t. the case above is that this case
            -- will succeed, whereas the case above can cause a predicate
            -- failure if the condition of the '!?' operator is not met. Since
            -- even on failure we _need_ to return a state, the case above also
            -- returns the state unchanged in this case.
            return $! (fads, bvs)

    , do
        TRC ( (sn, t, dms, cps, rpus, k)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        case lookupR vk dms of
          Nothing  -> do
            False ?! TryNextRule
            return $! (fads, bvs)
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            size ([bv] ◁ bvs) < t ?! CanAdopt bv
            case findKey ((== bv) . fst) rpus of
              Just (pid, _) -> do
                pid ∈ dom (cps ▷<= sn -. 2 *. k) ?! TryNextRule
                return $! (fads, bvs')
              Nothing -> do
                True ?! TryNextRule
                return $! (fads, bvs')

    , do
        TRC ( (sn, t, dms, cps, rpus, k)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        case lookupR vk dms of
          Nothing  -> do
            False ?! NotADelegate vk
            return $! (fads, bvs)
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            t <= size ([bv] ◁ bvs) ?! CannotAdopt bv
            case findKey ((== bv) . fst) rpus of
              Just (pid, (_, ppsc)) -> do
                pid ∈ dom (cps  ▷<= sn -. 2 *. k) ?! UnconfirmedProposal pid
                fads' <- trans @FADS $ TRC ((), fads, (sn, (bv, ppsc)))
                return $! (fads', bvs')
              Nothing -> do
                True ?! ProtVerUnknown bv
                return $! (fads, bvs')

    ]

instance Embed FADS UPEND where
  wrapFailed = error "No possible failures in FADS"

------------------------------------------------------------------------
-- Update interface
------------------------------------------------------------------------

-- | The update interface environment is shared amongst various rules, so
--   we define it as an alias here.
type UPIEnv =
  ( Core.Slot
  , Bimap Core.VKeyGenesis Core.VKey
  , BlockCount -- this is a global constant in the formal
               -- specification, which we put in this environment so
               -- that we can test with different values of it.
  )

-- | The update interface state is shared amongst various rules, so we define it
-- as an alias here.
type UPIState =
  ( (ProtVer, PParams)
  , [(Core.Slot, (ProtVer, PParams))]
  , Map ApName (ApVer, Core.Slot, Metadata)
  , Map UpId (ProtVer, PParams)
  , Map UpId (ApName, ApVer, Metadata)
  , Map UpId Core.Slot
  , Set (UpId, Core.VKeyGenesis)
  , Set (ProtVer, Core.VKeyGenesis)
  , Map UpId Core.Slot
  )

emptyUPIState :: UPIState
emptyUPIState =
  (( ProtVer 0 0 0
   , PParams                    -- TODO: choose more sensible default values
     { _maxBkSz = 100         -- max sizes chosen as non-zero to allow progress
     , _maxHdrSz = 10
     , _maxTxSz = 50
     , _maxPropSz = 10
     , _bkSgnCntT = 0.22    -- As defined in the spec.
     , _bkSlotsPerEpoch = 10 -- TODO: we need to remove this
     , _upTtl = 10
     , _scriptVersion = 0
     , _cfmThd = 4 -- TODO: this should be a double
     , _upAdptThd = 0.6 -- Value currently used in mainet
     , _stableAfter = 5 -- TODO: the k stability parameter needs to be removed
                        -- from here as well!

     -- To calculate the factors @A@ and @B@ we need to know the constant @C@
     -- that we use to bound the size of a transaction.
     --
     -- We have that for all transactions @tx@
     --
     -- > size (elaborate tx) <= C * abstractSize tx
     --
     -- where @elaborate@ elaborates an abstract transaction into a concrete
     -- one.
     --
     -- Then we would expect that the concrete fee is also bounded by the
     -- concrete fee, this is:
     --
     -- > A_C + B_C * size (elaborate tx) <= C * (A + B * abstractSize tx)
     --
     -- where @A_C@ and @B_C@ are the concrete factors that correspond to @A@
     -- and @B@. Now consider:
     --
     -- > C * (A + B * abstractSize tx)
     -- > =
     -- > C * A + B * C * abstractSize tx
     -- > >=
     -- > C * A + B * size (elaborate tx)
     -- = { Choosing A_C = C * A, B_C = B}
     -- > A_C + B_C * size (elaborate tx)
     --
     -- Which means that given C, we should set:
     --
     -- > _factorA = A_C / C
     -- > _factorB = B_C
     --
     -- TODO: But if the derivation above is correct the value of B would be
     -- quite large (if we take the used value in mainet, i.e.
     -- 155381000000000).
     --
     -- For now we choose arbitrary numbers here.
     , _factorA = 1 -- In mainet this value is set to 43946000000 (A_C in the derivation above)
     , _factorB = 2 -- In mainet this value is set to 155381000000000
     }
   )
  , []
  , Map.empty
  , Map.empty
  , Map.empty
  , Map.empty
  , Set.empty
  , Set.empty
  , Map.empty)

protocolVersion :: UPIState -> ProtVer
protocolVersion ((pv, _), _, _, _, _, _, _, _, _) = pv

avs :: UPIState -> Map ApName (ApVer, Core.Slot, Metadata)
avs ((_, _), _, avs, _, _, _, _, _, _) = avs

data UPIREG

instance STS UPIREG where
  type Environment UPIREG = UPIEnv
  type State UPIREG = UPIState
  type Signal UPIREG = UProp
  data PredicateFailure UPIREG
    = UPREGFailure (PredicateFailure UPREG)
    deriving (Eq, Show)

  initialRules = [ return $! emptyUPIState ]

  transitionRules =
    [ do
        TRC ( (sn, dms, _k)
            , ( (pv, pps)
              , fads
              , avs
              , rpus
              , raus
              , cps
              , vts
              , bvs
              , pws)
            , up) <- judgmentContext
        (rpus', raus') <- trans @UPREG $ TRC ((pv, pps, avs, dms), (rpus, raus), up)
        let pws' = pws ⨃ [(up ^. upId, sn)]
        pure $! ( (pv, pps)
                , fads
                , avs
                , rpus'
                , raus'
                , cps
                , vts
                , bvs
                , pws'
                )

    ]

instance Embed UPREG UPIREG where
  wrapFailed = UPREGFailure

instance HasTrace UPIREG where

  initEnvGen = (,,)
             <$> CoreGen.slotGen 0 10 -- current slot
             <*> dmapGen              -- delegation map
             <*> blockCountGen        -- k
    where
      dmapGen = Bimap.fromList . uncurry zip <$> vkgVkPairsGen
      vkgVkPairsGen = do
        n <- Gen.integral (Range.linear 1 14) -- number of genesis keys
        let
          vkgs = VKeyGenesis . VKey . Owner <$> [0 .. n - 1]
          -- As delegation targets we choose twice the number of genesis keys.
          -- Note that the genesis keys can delegate to themselves in the
          -- generated delegation map.
          vks = VKey . Owner <$> [0 .. 2 * (n - 1)]
        (vkgs,) <$> Gen.filter (not . null) (Gen.subsequence vks)

      -- TODO: use the generator in Core once PR 570 is merged.
      blockCountGen = BlockCount <$> Gen.word64 (Range.linear 0 100)

  sigGen (slot, dms, k) ((pv, pps), fads, avs, rpus, raus, cps, vts, bvs, pws)
    = do
    (vk, pv', pps', sv') <- (,,,) <$> issuerGen
                                  <*> pvGen
                                  <*> pparamsGen
                                  <*> swVerGen

    Gen.choice
      [ -- Do not change the protocol version
        UProp
        <$> idGen
        <*> pure vk
        <*> pure pps
        <*> pure pv
        <*> pure sv'
        <*> pure (Core.sign (skey vk) (pv, pps, sv'))
        <*> stTagsGen
        <*> mdtGen
      , -- Do not change the software version (unless there are no software versions in @avs@)
        do
          -- Pick a current software version (if available)
          let makeSoftwareVersion (apName, (apVersion, _, _)) = SwVer apName apVersion
              avsList = Map.toList avs
          swCurr <- if null avsList
                    then pure $! sv'
                    else makeSoftwareVersion <$> Gen.element avsList
          UProp
            <$> idGen
            <*> pure vk
            <*> pure pps'
            <*> pure pv'
            <*> pure swCurr
            <*> pure (Core.sign (skey vk) (pv, pps, swCurr))
            <*> stTagsGen
            <*> mdtGen
      , -- Change protocol and software version
        UProp
        <$> idGen
        <*> pure vk
        <*> pure pps'
        <*> pure pv'
        <*> pure sv'
        <*> pure (Core.sign (skey vk) (pv', pps', sv'))
        <*> stTagsGen
        <*> mdtGen
      ]

    where
      idGen :: Gen UpId
      idGen = do
        -- Chose an increment for the maximum version seen in the update
        -- proposal id's.
        inc <- Gen.integral (Range.constant 0 10)
        case Set.toDescList $ dom rpus of
          [] -> UpId <$> Gen.element [0 .. inc]
          (UpId maxId:_) -> pure $ UpId (maxId + inc)

      -- As issuer we chose a current delegate. The delegation map must not be
      -- empty for this signal generator to succeed.
      issuerGen :: Gen Core.VKey
      issuerGen = if null delegates
                  then error "There are no delegates to issue an update proposal."
                  else Gen.element delegates

        where
          delegates = Set.toList (range dms)

      pparamsGen :: Gen PParams
      pparamsGen = ppsUpdateFrom pps
        -- TODO: remember to generate update proposals where the maximum
        -- proposal size falls below the minimum abstract size that a proposal
        -- can have (this case shouldn't be generated often, but we want to
        -- cover it)

      pvGen :: Gen ProtVer
      pvGen = do
        -- We want to generate a large number of alternative versions so that
        -- we have a low probability of collisions (remember that we're trying
        -- to generate only valid signals)
        inc <- Gen.integral (Range.constant 0 10000)
        Gen.element [ ProtVer (_pvMaj + 1) 0            (_pvAlt + inc)
                    , ProtVer _pvMaj       (_pvMin + 1) (_pvAlt + inc)
                    ]
        where ProtVer {_pvMaj, _pvMin, _pvAlt} = pv

      swVerGen :: Gen SwVer
      swVerGen =
        -- First, we generate the possible version increment for the existing
        -- software.
        if null possibleNextVersions
        then genNewApp
        else Gen.choice [genANextVersion, genNewApp]
        where
          -- Generate the next version for an existing application
          genANextVersion :: Gen SwVer
          genANextVersion = uncurry SwVer <$> Gen.element possibleNextVersions
          possibleNextVersions = Set.toList $ nextVersions \\ registeredNextVersions
          nextVersions = Set.fromList $ zip appNames appNextVersions
          appNames = Set.toList (dom avs)
          appNextVersions = (+1) . fst3 <$> Set.toList (range avs)
          registeredNextVersions = Set.map (fst3 &&& snd3) (range raus)

          fst3 (x, _, _) = x
          snd3 (_, y, _) = y

          -- Generate a new application
          genNewApp :: Gen SwVer
          genNewApp
            = ((`SwVer` 0) . ApName)
            <$> Gen.filter ((`notElem` appNames) . ApName)
                           (Gen.list (Range.constant 0 12) Gen.ascii)

      stTagsGen :: Gen (Set STag)
      stTagsGen =
        -- TODO: We need to benchmark this against @Gen.set@. This seems to be
        -- slightly faster.
        Set.fromList <$>
          Gen.list (Range.linear 0 10) (Gen.list (Range.constant 0 10) Gen.ascii)

      mdtGen :: Gen Metadata
      mdtGen = pure Metadata


-- | Generate a protocol parameter update from a given set of current
-- protocol-parameters, ensuring the consistency of the new protocol parameters
-- w.r.t. the current ones, according to the @canUpdate@ predicate in the
-- formal specification.
--
-- TODO: we can move this into a Generator's module, but first we need to
-- disentangle the dependencies. Moving this to Ledger.Update.Generators will
-- cause a circular dependency. I think the rules need to be moved into their
-- own modules.
ppsUpdateFrom :: PParams -> Gen PParams
ppsUpdateFrom pps = do
  -- Determine the change in the block size: a decrement or an increment that
  -- is no more than twice the current block maximum size.
  --
  -- We don't expect the maximum block size to change often, so we generate
  -- more values around the current block size (@_maxBkSz@).
  newMaxBkSize <- Gen.integral (Range.exponentialFrom _maxBkSz 1 (2 * _maxBkSz))
                  `increasingProbabilityAt`
                  (1, 2 * _maxBkSz)

  -- Similarly, we don't expect the transaction size to be changed often, so we
  -- also generate more values around the current maximum transaction size.
  newMaxTxSize <- Gen.integral (Range.exponentialFrom _maxTxSz 0 (newMaxBkSize - 1))
                  `increasingProbabilityAt`
                  (0, newMaxBkSize - 1)

  PParams
    <$> pure newMaxBkSize
    <*> nextMaxHdrSzGen
    <*> pure newMaxTxSize
    <*> nextMaxPropSz
    <*> nextBkSgnCntT
    <*> pure _bkSlotsPerEpoch -- This parameter should be removed from 'PParams'
    <*> nextUpTtl
    <*> nextScriptVersion
    <*> nextCfmThd
    <*> nextUpAdptThd
    <*> pure _stableAfter     -- This parameter should be removed from 'PParams'
    <*> nextFactorA
    <*> nextFactorB

  where
    PParams{ _maxBkSz
           , _maxHdrSz
           , _maxTxSz
           , _maxPropSz
           , _bkSgnCntT
           , _bkSlotsPerEpoch
           , _upTtl
           , _scriptVersion
           , _cfmThd
           , _upAdptThd
           , _stableAfter
           , _factorA
           , _factorB
           } = pps

    nextMaxHdrSzGen :: Gen Natural
    nextMaxHdrSzGen =
      Gen.integral (Range.exponentialFrom _maxHdrSz 0 (2 * _maxHdrSz))
      `increasingProbabilityAt` (0, (2 * _maxHdrSz))

    nextMaxPropSz :: Gen Natural
    nextMaxPropSz =
      Gen.integral (Range.exponentialFrom _maxPropSz 0 (2 * _maxPropSz))
      `increasingProbabilityAt` (0, 2 * _maxPropSz)

    nextBkSgnCntT :: Gen Double
    nextBkSgnCntT =
      Gen.double (Range.exponentialFloatFrom _bkSgnCntT 0 1)
      `increasingProbabilityAt` (0, 1)

    nextUpTtl :: Gen SlotCount
    nextUpTtl = SlotCount <$>
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

    nextCfmThd :: Gen Int
    nextCfmThd = Gen.integral (Range.exponentialFrom 0 _cfmThd (_cfmThd + 1))
      `increasingProbabilityAt`
      (0, _cfmThd + 1)

    nextUpAdptThd :: Gen Double
    nextUpAdptThd =
      Gen.double (Range.exponentialFloatFrom _upAdptThd 0 1)
      `increasingProbabilityAt` (0, 1)

    nextFactorA :: Gen Int
    nextFactorA =
      -- TODO: we choose arbitrary numbers here for now.
      Gen.integral (Range.exponentialFrom _factorA 0 10)
      `increasingProbabilityAt` (0, 10)

    nextFactorB :: Gen Int
    nextFactorB =
      -- TODO: we choose arbitrary numbers here for now.
      Gen.integral (Range.exponentialFrom _factorB 0 10)
      `increasingProbabilityAt` (0, 10)

-- | Generate values the given distribution in 90% of the cases, and values at
-- the bounds of the range in 10% of the cases.
--
-- This can be used to generate enough extreme values. The exponential and
-- linear distributions provided by @hedgehog@ will generate a small percentage
-- of these (0-1%).
increasingProbabilityAt
  :: Gen a
  -> (a, a)
  -> Gen a
increasingProbabilityAt gen (lower, upper)
  = Gen.frequency [ (5, pure lower)
                  , (90, gen)
                  , (5, pure upper)
                  ]

data UPIVOTE

instance STS UPIVOTE where
  type Environment UPIVOTE = UPIEnv
  type State UPIVOTE = UPIState
  type Signal UPIVOTE = Vote
  data PredicateFailure UPIVOTE
    = UPVOTEFailure (PredicateFailure UPVOTE)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (sn, dms, k)
            , ( (pv, pps)
              , fads
              , avs
              , rpus
              , raus
              , cps
              , vts
              , bvs
              , pws)
            , v) <- judgmentContext
        (cps', vts') <- trans @UPVOTE $ TRC ((sn, pps, dom pws, dms), (cps, vts), v)
        let
          stblCps  = Map.keys $ Map.filter stable cps'
          stable s = unSlot s <= unSlot sn - 2 * unBlockCount k
          avsnew   = [ (an, (av, sn, m))
                     | pid <- stblCps
                     , (an, av, m) <- maybeToList $ Map.lookup pid raus
                     ]
        return ( (pv, pps)
                , fads
                , avs ⨃ avsnew
                , rpus
                , Set.fromList stblCps ⋪ raus
                , cps'
                , vts'
                , bvs
                , pws
                )

    ]

instance Embed UPVOTE UPIVOTE where
  wrapFailed = UPVOTEFailure


data UPIVOTES

instance STS UPIVOTES where
  type Environment UPIVOTES = UPIEnv
  type State UPIVOTES = UPIState
  type Signal UPIVOTES = [Vote]

  data PredicateFailure UPIVOTES
    = UpivoteFailure (PredicateFailure UPIVOTE)
    deriving (Eq, Show)

  initialRules = [ return $! emptyUPIState ]

  transitionRules =
    [ do
        TRC (env, us, sig) <- judgmentContext
        case (sig :: [Vote]) of
          []     -> return us
          (x:xs) -> do
            us'  <- trans @UPIVOTES $ TRC (env, us, xs)
            us'' <- trans @UPIVOTE  $ TRC (env, us', x)
            return us''
    ]

instance Embed UPIVOTE UPIVOTES where
  wrapFailed = UpivoteFailure


data UPIEND

instance STS UPIEND where
  type Environment UPIEND = UPIEnv
  type State UPIEND = UPIState
  type Signal UPIEND = (ProtVer, Core.VKey)
  data PredicateFailure UPIEND
    = UPENDFailure (PredicateFailure UPEND)
    deriving (Eq, Show)

  initialRules = [ return $! emptyUPIState ]

  transitionRules =
    [ do
        TRC ( (sn, dms, k)
            , ( (pv, pps)
              , fads
              , avs
              , rpus
              , raus
              , cps
              , vts
              , bvs
              , pws)
            , (bv,vk)) <- judgmentContext
        let
          t = floor $ pps ^. upAdptThd * fromIntegral GP.ngk
        (fads', bvs') <- trans @UPEND $ TRC ((sn, t, dms, cps, rpus, k), (fads, bvs), (bv,vk))
        let
          u        = pps ^. upTtl
          pidskeep = dom (pws ▷>= sn -. u) `union` dom cps
          vskeep   = dom (range rpus')
          rpus'    = pidskeep ◁ rpus
        return ( (pv, pps)
                , fads'
                , avs
                , rpus'
                , pidskeep ◁ raus
                , cps
                , pidskeep ◁ vts
                , vskeep ◁ bvs'
                , pidskeep ◁ pws
                )

    ]

instance Embed UPEND UPIEND where
  wrapFailed = UPENDFailure

data PVBUMP

instance STS PVBUMP where
  type Environment PVBUMP =
    ( Core.Slot
    , [(Core.Slot, (ProtVer, PParams))]
    , BlockCount -- Chain stability parameter; this is a global
                 -- constant in the formal specification, which we put
                 -- in this environment so that we can test with
                 -- different values of it.
    )
  type State PVBUMP =
    (ProtVer, PParams)

  type Signal PVBUMP = ()
  data PredicateFailure PVBUMP
    = NewEpoch
    | OldEpoch
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ((s_n, fads, k), (pv, pps), ()) <- judgmentContext
        let
          mFirstStableSlot = minusSlotMaybe s_n (SlotCount . (2 *) . unBlockCount $ k)
          r = case mFirstStableSlot of
                Nothing -> []
                Just s  -> filter ((<= s) . fst) fads
        if r == []
          then return $! (pv, pps)
          else do
            let (_, (pv_c, pps_c)) = last r
            return $! (pv_c, pps_c)
    ]

data UPIEC

instance STS UPIEC where
  type Environment UPIEC =
    ( Core.Slot
    , BlockCount -- Chain stability parameter; this is a global
                 -- constant in the formal specification, which we put
                 -- in this environment so that we can test with
                 -- different values of it.
    )
  type State UPIEC = UPIState
  type Signal UPIEC = ()
  data PredicateFailure UPIEC
    = PVBUMPFailure (PredicateFailure PVBUMP)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ((s_n, k), us, ()) <- judgmentContext
        let
          (pv, pps) = us ^. _1 :: (ProtVer, PParams)
          fads      = us ^. _2 :: [(Core.Slot, (ProtVer, PParams))]
        (pv', pps') <- trans @PVBUMP $ TRC ((s_n, fads, k), (pv, pps), ())
        return $! if pv == pv'
          then us
          else
            ( (pv', pps') :: (ProtVer, PParams)
            , []          :: [(Core.Slot, (ProtVer, PParams))]
            , us ^. _3    :: Map ApName (ApVer, Core.Slot, Metadata)
            , Map.empty   :: Map UpId (ProtVer, PParams)
            , us ^. _5    :: Map UpId (ApName, ApVer, Metadata)
            , Map.empty   :: Map UpId Core.Slot
            , Set.empty   :: Set (UpId, Core.VKeyGenesis)
            , Set.empty   :: Set (ProtVer, Core.VKeyGenesis)
            , Map.empty   :: Map UpId Core.Slot
            )
    ]

instance Embed PVBUMP UPIEC where
  wrapFailed = PVBUMPFailure
