{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Update where

import Control.Lens
import Data.Ix (inRange)
import Data.List (foldl', partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Numeric.Natural

import Control.State.Transition

import Ledger.Core (Relation(..), (⨃), (▹), (⋪), (◃))
import qualified Ledger.Core as Core
import Ledger.Delegation (liveAfter)

import Prelude hiding (min)

-- | Protocol parameters.
--
data PParams = PParams -- TODO: this should be a module of @cs-ledger@.
  { _maxBkSz  :: Natural
  -- ^ Maximum (abstract) block size in words.
  , _maxHdrSz :: Natural
  -- ^ Maximum (abstract) block header size in words.
  , _maxTxSz :: Natural
  -- ^ Maximum (abstract) transaction size in words.
  , _maxPropSz :: Natural
  -- ^ Maximum (abstract) update proposal size in words.
  , _bkSgnCntT :: Double
  -- ^ Fraction [0, 1] of the blocks that can be signed by any given key in a
  -- window of lenght '_bkSgnCntW'. This value will be typically between 1/5
  -- and 1/4.
  , _bkSlotsPerEpoch :: Core.SlotCount
  -- ^ Number of slots in an epoch
  , _upTtl :: Core.SlotCount
  -- ^ Update proposal TTL in slots
  , _scriptVersion :: Natural
  -- ^ Script version
  , _cfmThd :: Int
  -- ^ Update proposal confirmation threshold (number of votes)
  , _upAdptThd :: Int
  -- ^ Update adoption threshold (number of block issuers)
  , _stableAfter :: Core.BlockCount
  -- ^ Chain stability parameter
  } deriving (Eq, Ord, Show)

makeLenses ''PParams

newtype UpId = UpId Int
  deriving (Eq, Ord, Show)

-- | Protocol version
data ProtVer = ProtVer
  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Natural
  } deriving (Eq, Ord, Show)

makeLenses ''ProtVer

newtype ApName = ApName String
  deriving (Eq, Ord, Show)

-- | Application version
newtype ApVer = ApVer Natural
  deriving (Eq, Ord, Num, Show)

data SwVer = SwVer
  { _svName :: ApName
  , _svVer :: ApVer
  } deriving (Eq, Show, Generic)

makeLenses ''SwVer

-- | Part of the update proposal which must be signed
type UpSD =
  ( ProtVer
  , PParams
  , SwVer
  )

-- | Update proposal
data UProp = UProp
  { _upId :: UpId
  , _upIssuer :: Core.VKey
  , _upParams :: PParams
  , _upPV :: ProtVer
  , _upSwVer :: SwVer
  , _upSig :: Core.Sig UpSD
  }

makeLenses ''UProp

upSigData :: Lens' UProp UpSD
upSigData = lens
  (\up -> (up ^. upPV, up ^. upParams, up ^. upSwVer))
  (\up (pv, pps, sv) -> up
    & upParams .~ pps
    & upPV .~ pv
    & upSwVer .~ sv
  )

-- | Test if a pair is present in a map.
inMap :: (Ord k, Eq v) => k -> v -> Map k v -> Bool
inMap k v m = case Map.lookup k m of
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
  :: Map ApName (ApVer, Core.Slot)
  -> (ApName, ApVer)
  -> Bool
svCanFollow avs (an,av) =
    (case Map.lookup an avs of
      Nothing -> True
      Just (x,_) -> av == x + 1
    ) && (an `Set.notMember` dom avs ==> av == ApVer 0)
  where

------------------------------------------------------------------------
-- Update proposals
------------------------------------------------------------------------

-- | Update Proposal Software Version Validation
data UPSVV

instance STS UPSVV where
  type Environment UPSVV = Map ApName (ApVer, Core.Slot)
  type State UPSVV = Map UpId (ApName, ApVer)
  type Signal UPSVV = UProp

  data PredicateFailure UPSVV
    = AlreadyProposedSv
    | CannotFollowSv
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (avs, raus, up) <- judgmentContext
        let SwVer an av = up ^. upSwVer
        svCanFollow avs (an,av) ?! CannotFollowSv
        (an, av) `notElem` Map.elems raus ?! AlreadyProposedSv
        return $! raus ⨃ [(up ^. upId, (an, av))]
    ]

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
        return $! rpus ⨃ Map.singleton pid (nv, ppsn)
    ]


-- | Update proposal validity
data UPV

instance STS UPV where
  type Environment UPV =
    ( ProtVer
    , PParams
    , Map ApName (ApVer, Core.Slot)
    )

  type State UPV =
    ( Map UpId (ProtVer, PParams)
    , Map UpId (ApName, ApVer)
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
        inMap an av (fst <$> avs) ?! AVChangedInPVUpdate an av
        return (rpus', raus)
    , do
        TRC ( (pv, pps, avs)
            , (rpus, raus)
            , up
            ) <- judgmentContext
        pv == up ^. upPV ?! PVChangedInSVUpdate
        up ^. upParams == pps ?! ParamsChangedInSVUpdate
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        return (rpus, raus')
    , do
        TRC ( (pv, pps, avs)
            , (rpus, raus)
            , up
            ) <- judgmentContext
        rpus' <- trans @UPPVV $ TRC ((pv, pps), rpus, up)
        raus' <- trans @UPSVV $ TRC (avs, raus, up)
        return (rpus', raus')
    ]

instance Embed UPPVV UPV where
  wrapFailed = UPPVVFailure

instance Embed UPSVV UPV where
  wrapFailed = UPSVVFailure

data UPREG

instance STS UPREG where
  type Environment UPREG =
    ( ProtVer
    , PParams
    , Map ApName (ApVer, Core.Slot)
    , Map Core.VKeyGenesis Core.VKey
    )
  type State UPREG =
    ( Map UpId (ProtVer, PParams)
    , Map UpId (ApName, ApVer)
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
        dms ▹ Set.singleton vk /= Map.empty ?! NotGenesisDelegate
        Core.verify vk (up ^. upSigData) (up ^. upSig) ?! DoesNotVerify
        return (rpus', raus')


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
  }

makeLenses ''Vote

data ADDVOTE

instance STS ADDVOTE where
  type Environment ADDVOTE =
    ( Set UpId
    , Map Core.VKeyGenesis Core.VKey
    )
  type State ADDVOTE = Core.PairSet UpId Core.VKeyGenesis
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
            vtsPid = Set.fromList
              [(pid, vks) | vks <- Set.toList . Map.findWithDefault Set.empty vk $ invertMap dms ]
        Set.member pid rups ?! NoUpdateProposal pid
        Core.verify vk pid (vote ^. vSig) ?! AVSigDoesNotVerify
        return $! Core.PairSet $ Set.union (Core.unPairSet vts) vtsPid
    ]

data UPVOTE

instance STS UPVOTE where
  type Environment UPVOTE =
    ( Core.Slot
    , PParams
    , Set UpId
    , Map Core.VKeyGenesis Core.VKey
    )
  type State UPVOTE =
    ( Map UpId Core.Slot
    , Core.PairSet UpId Core.VKeyGenesis
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
        Core.psSize (Set.singleton pid ◃ vts') < (pps ^. cfmThd) || pid `Set.member` (dom cps) ?! HigherThanThdAndNotAlreadyConfirmed
        return (cps, vts')
    , do
        TRC ( (sn, pps, rups, dms)
            , (cps, vts)
            , vote
            ) <- judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        (pps ^. cfmThd) <= Core.psSize (Set.singleton pid ◃ vts') ?! CfmThdNotReached
        pid `Set.notMember` dom cps ?! AlreadyConfirmed
        return (cps ⨃ Map.singleton pid sn, vts')

    ]

instance Embed ADDVOTE UPVOTE where
  wrapFailed = ADDVOTEFailure

------------------------------------------------------------------------
-- Update voting
------------------------------------------------------------------------

canAdopt :: PParams -> Core.PairSet ProtVer Core.VKey -> ProtVer -> Bool
canAdopt pps bvs bv = (pps ^. upAdptThd) <= Core.psSize (Set.singleton bv ◃ bvs)

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

instance STS UPEND where
  type Environment UPEND =
    ( Core.BlockCount
    , Core.Slot
    , (ProtVer, PParams)
    , Map UpId Core.Slot
    , Map UpId (ProtVer, PParams)
    )
  type State UPEND =
    ( [(Core.Slot, (ProtVer, PParams))]
    , Core.PairSet ProtVer Core.VKey
    )
  type Signal UPEND = (ProtVer, Core.VKey)

  data PredicateFailure UPEND
    = ProtVerUnknown
    | ProtVerTooRecent
    | NotAFailure
    | CanAdopt
    | CannotAdopt
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (k, sn, _, cps, rpus)
            , (fads, bvs)
            , (bv, _vk)
            ) <- judgmentContext
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid ->
            pid `Map.notMember` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! NotAFailure
          Nothing -> True ?! NotAFailure
        return (fads, bvs)
    , do
        TRC ( (k, sn, (_pv, pps), cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        let bvs' = bvs ∪ singleton bv vk
        (not $ canAdopt pps bvs' bv) ?! CanAdopt
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid ->
            pid `Map.member` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! ProtVerTooRecent
          Nothing -> True ?! ProtVerUnknown
        return (fads, bvs')
    , do
        TRC ( (k, sn, (_pv, pps), cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        let bvs' = bvs ∪ singleton bv vk
        canAdopt pps bvs' bv ?! CannotAdopt
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid -> do
            -- This is safe by virtue of the fact we've just inverted the map and found this there
            let (_, ppsc) = fromJust $ Map.lookup pid rpus
            pid `Map.member` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! ProtVerTooRecent
            fads' <- trans @FADS $ TRC ((), fads, (sn, (bv, ppsc)))
            return (fads', bvs')
          Nothing -> do
            True ?! ProtVerUnknown
            return (fads, bvs)

    ]

instance Embed FADS UPEND where
  wrapFailed = error "No possible failures in FADS"

------------------------------------------------------------------------
-- Update interface
------------------------------------------------------------------------

-- | The update interface environment is shared amongst various rules, so
--   we define it as an alias here.
type UPIEnv =
  ( Core.Epoch
  , Core.Slot
  , Map Core.VKeyGenesis Core.VKey
  )

-- | The update interface state is shared amongst various rules, so we define it
-- as an alias here.
type UPIState =
  ( Core.Epoch
  , (ProtVer, PParams)
  , [(Core.Slot, (ProtVer, PParams))]
  , Map ApName (ApVer, Core.Slot)
  , Map UpId (ProtVer, PParams)
  , Map UpId (ApName, ApVer)
  , Map UpId Core.Slot
  , Core.PairSet UpId Core.VKeyGenesis
  , Core.PairSet ProtVer Core.VKey
  , Map UpId Core.Slot
  )

data UPIREG

instance STS UPIREG where
  type Environment UPIREG = UPIEnv
  type State UPIREG = UPIState
  type Signal UPIREG = UProp
  data PredicateFailure UPIREG
    = UPREGFailure (PredicateFailure UPREG)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (_ec, sn, dms)
            , ( ep
              , (pv, pps)
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
        return ( ep
                , (pv, pps)
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
        TRC ( (_ec, sn, dms)
            , ( ep
              , (pv, pps)
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
        let avsnew = Map.fromList [(an, (av, sn)) | pid <- Map.keys cps'
                                                  , (an, av) <- maybeToList $ Map.lookup pid raus]
        return ( ep
                , (pv, pps)
                , fads
                , avs ⨃ avsnew
                , rpus
                , dom cps ⋪ raus
                , cps'
                , vts'
                , bvs
                , pws
                )

    ]

instance Embed UPVOTE UPIVOTE where
  wrapFailed = UPVOTEFailure

data UPIEND

instance STS UPIEND where
  type Environment UPIEND = UPIEnv
  type State UPIEND = UPIState
  type Signal UPIEND = (ProtVer, Core.VKey)
  data PredicateFailure UPIEND
    = UPENDFailure (PredicateFailure UPEND)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (_ec, sn, _dms)
            , ( ep
              , (pv, pps)
              , fads
              , avs
              , rpus
              , raus
              , cps
              , vts
              , bvs
              , pws)
            , (bv,vk)) <- judgmentContext
        let k = pps ^. stableAfter
            u = pps ^. upTtl
        (fads', bvs') <- trans @UPEND $ TRC ((k, sn, (pv, pps), cps, rpus), (fads, bvs), (bv,vk))
        let pidskeep = dom (Map.filter (>= (sn `Core.minusSlot` u)) pws) `Set.union` dom cps
            rpus' = pidskeep ◃ rpus
            vskeep = Set.fromList . fmap fst $ Map.elems rpus'
        return ( ep
                , (pv, pps)
                , fads'
                , avs
                , rpus'
                , pidskeep ◃ raus
                , cps
                , pidskeep ◃ vts
                , vskeep ◃ bvs'
                , pidskeep ◃ pws
                )

    ]

instance Embed UPEND UPIEND where
  wrapFailed = UPENDFailure

data PVBUMP

instance STS PVBUMP where
  type Environment PVBUMP =
    ( Core.BlockCount
    , Core.Slot
    )
  type State PVBUMP =
    ( Core.Epoch
    , (ProtVer, PParams)
    , [(Core.Slot, (ProtVer, PParams))]
    )
  type Signal PVBUMP = Core.Epoch
  data PredicateFailure PVBUMP
    = NewEpoch
    | OldEpoch
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (_, (ep, (pv, pps), fads), en) <- judgmentContext
        en <= ep ?! NewEpoch
        return (ep, (pv, pps), fads)
    , do
        TRC ((k, sn), (ep, (pv, pps), fads), en) <- judgmentContext
        ep < en ?! OldEpoch
        return $ case (partition (\(s, _) -> s <= sn `Core.minusSlot` liveAfter k) fads) of
          ([], _) -> (ep, (pv, pps), fads)
          ((_, (pvc, ppsc)):_, rest) -> (en, (pvc, ppsc), rest)
    ]

data UPIEC

instance STS UPIEC where
  type Environment UPIEC = UPIEnv
  type State UPIEC = UPIState
  type Signal UPIEC = Core.Epoch
  data PredicateFailure UPIEC
    = PVBUMPFailure (PredicateFailure PVBUMP)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC ( (_ec, sn, _dms)
            , ( ep
              , (pv, pps)
              , fads
              , avs
              , rpus
              , raus
              , cps
              , vts
              , bvs
              , pws)
            , en) <- judgmentContext
        let k = pps ^. stableAfter
        (e', (pv', pps'), fads') <- trans @PVBUMP $ TRC ((k, sn), (ep, (pv, pps), fads), en)
        let pidskeep = Set.fromList [ pid | pid <- Set.toList . foldl' Set.union Set.empty
                                     . Map.elems . Map.filterWithKey (\pvi _ -> pv' < pvi)
                                     $ invertMap (fst <$> rpus)]
        return ( e'
                , (pv', pps')
                , fads'
                , avs
                , pidskeep ◃ rpus
                , raus
                , pidskeep ◃ cps
                , pidskeep ◃ vts
                , bvs
                , pidskeep ◃ pws
                )

    ]

instance Embed PVBUMP UPIEC where
  wrapFailed = PVBUMPFailure
