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
import Control.State.Transition
import Data.Ix (inRange)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Numeric.Natural
import Ledger.Core ((⨃), (▹))
import qualified Ledger.Core as Core

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
  , _dLiveness :: Core.SlotCount
  -- ^ Delegation liveness parameter: number of slots it takes a delegation
  -- certificate to take effect.
  , _bkSgnCntW :: Int
  -- ^ Size of the moving window to count signatures.
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
  } deriving (Eq, Show)

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
    ) && (an `Set.notMember` Map.keysSet avs ==> av == ApVer 0)
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
            vtsPid = Set.fromList
              [(pid, vks) | vks <- Set.toList . Map.findWithDefault Set.empty vk $ invertMap dms ]
        Set.member pid rups ?! NoUpdateProposal pid
        Core.verify vk pid (vote ^. vSig) ?! AVSigDoesNotVerify
        return $! Set.union vts vtsPid
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
    , Set (UpId, Core.VKeyGenesis)
    )
  type Signal UPVOTE = Vote
  data PredicateFailure UPVOTE
    = ADDVOTEFailure (PredicateFailure ADDVOTE)
    | LowerThanThdAndNotAlreadyConfirmed
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
        Set.size (Set.filter ((== pid) . fst) vts') < (pps ^. cfmThd) || pid `Set.member` (Map.keysSet cps) ?! LowerThanThdAndNotAlreadyConfirmed
        return (cps, vts')
    , do
        TRC ( (sn, pps, rups, dms)
            , (cps, vts)
            , vote
            ) <- judgmentContext
        vts' <- trans @ADDVOTE $ TRC ((rups, dms), vts, vote)
        let pid = vote ^. vPropId
        (pps ^. cfmThd) <= Set.size (Set.filter ((== pid) . fst) vts') ?! CfmThdNotReached
        pid `Set.notMember` Map.keysSet cps ?! AlreadyConfirmed
        return (cps ⨃ Map.singleton pid sn, vts')

    ]

instance Embed ADDVOTE UPVOTE where
  wrapFailed = ADDVOTEFailure

------------------------------------------------------------------------
-- Update voting
------------------------------------------------------------------------

canAdopt :: PParams -> Set (ProtVer, Core.VKey) -> ProtVer -> Bool
canAdopt pps bvs bv = (pps ^. upAdptThd) <= Set.size (Set.filter ((== bv) . fst) bvs)

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
    ( Core.SlotCount
    , Core.Slot
    , (ProtVer, PParams)
    , Map UpId Core.Slot
    , Map UpId (ProtVer, PParams)
    )
  type State UPEND =
    ( [(Core.Slot, (ProtVer, PParams))]
    , Set (ProtVer, Core.VKey)
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
          Just pid -> do
            pid `Map.notMember` (Map.filter (<= sn `Core.minusSlot` (2*k)) cps) ?! NotAFailure
          Nothing -> True ?! NotAFailure
        return (fads, bvs)
    , do
        TRC ( (k, sn, (_pv, pps), cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        let bvs' = bvs `Set.union` Set.singleton (bv, vk)
        (not $ canAdopt pps bvs' bv) ?! CanAdopt
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid ->
            pid `Map.member` (Map.filter (<= sn `Core.minusSlot` (2*k)) cps) ?! ProtVerTooRecent
          Nothing -> True ?! ProtVerUnknown
        return (fads, bvs')
    , do
        TRC ( (k, sn, (_pv, pps), cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        let bvs' = bvs `Set.union` Set.singleton (bv, vk)
        canAdopt pps bvs' bv ?! CannotAdopt
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid -> do
            -- This is safe by virtue of the fact we've just inverted the map and found this there
            let (_, ppsc) = fromJust $ Map.lookup pid rpus
            pid `Map.member` (Map.filter (<= sn `Core.minusSlot` (2*k)) cps) ?! ProtVerTooRecent
            fads' <- trans @FADS $ TRC ((), fads, (sn, (bv, ppsc)))
            return (fads', bvs')
          Nothing -> do
            True ?! ProtVerUnknown
            return (fads, bvs)

    ]

instance Embed FADS UPEND where
  wrapFailed = error "No possible failures in FADS"
