{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Update
  (module Ledger.Update)
where

import Control.Lens
import qualified Crypto.Hash as Crypto
import Data.AbstractSize (HasTypeReps)
import Data.Bimap (Bimap, empty, lookupR)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAscii)
import Data.Ix (inRange)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Numeric.Natural

import Control.State.Transition

import Ledger.Core (Relation(..), (⋪), (▹), (◃), (⨃), unSlot, HasHash, hash, PairSet(..), VKeyGenesis, VKey, BlockCount(..))
import qualified Ledger.Core as Core
import Ledger.Delegation (liveAfter, k)
import qualified Ledger.GlobalParams as GP

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
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''PParams

instance HasTypeReps PParams

newtype UpId = UpId Int
  deriving (Eq, Generic, Ord, Show)

instance HasTypeReps UpId

-- | Protocol version
data ProtVer = ProtVer
  { _pvMaj :: Natural
  , _pvMin :: Natural
  , _pvAlt :: Natural
  } deriving (Eq, Generic, Ord, Show)

makeLenses ''ProtVer

instance HasTypeReps ProtVer

newtype ApName = ApName String
  deriving (Eq, Generic, Ord, Show)

instance HasTypeReps ApName

-- | Application version
newtype ApVer = ApVer Natural
  deriving (Eq, Generic, Ord, Num, Show)

instance HasTypeReps ApVer

data SwVer = SwVer
  { _svName :: ApName
  , _svVer :: ApVer
  } deriving (Eq, Generic, Show)

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
data Metadata = Metadata deriving (Eq, Ord, Show, Generic)

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
  } deriving (Eq, Generic, Show)

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
        return $! rpus ⨃ Map.singleton pid (nv, ppsn)
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
        dms ▹ Set.singleton vk /= empty ?! NotGenesisDelegate
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
  } deriving (Eq, Generic, Show)

makeLenses ''Vote

instance HasTypeReps Vote

instance BA.ByteArrayAccess (Maybe Ledger.Update.UProp, [Ledger.Update.Vote]) where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack . show

instance HasHash (Maybe Ledger.Update.UProp, [Ledger.Update.Vote]) where
  hash = Crypto.hash


data ADDVOTE

instance STS ADDVOTE where
  type Environment ADDVOTE =
    ( Set UpId
    , Bimap Core.VKeyGenesis Core.VKey
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
            vtsPid = Core.PairSet $
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
    ( Core.Slot
    , Bimap VKeyGenesis VKey
    , Map UpId Core.Slot
    , Map UpId (ProtVer, PParams)
    )
  type State UPEND =
    ( [(Core.Slot, (ProtVer, PParams))]
    , Core.PairSet ProtVer Core.VKeyGenesis
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
    [
      do
        TRC ( (sn, _dms, cps, rpus)
            , (fads, bvs)
            , (bv, _vk)
            ) <- judgmentContext
        case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
          Just pid -> do
            pid `Map.notMember` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! NotAFailure
          Nothing -> True ?! NotAFailure
        return $! (fads, bvs)

    , do
        TRC ( (sn, dms, cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        case lookupR vk dms of
          Nothing  -> do
            False ?! CannotAdopt -- md 2019-05-03: not sure what to
                                 -- return/error on here
            return $! (fads, bvs) -- a silly thing needed to get types line up
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            -- (not $ canAdopt pps bvs' bv) ?! CanAdopt
            Core.psSize (Set.singleton bv ◃ bvs) < (fromIntegral . unBlockCount) t ?! CanAdopt
            case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
              Just pid -> do
                pid `Map.member` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! ProtVerTooRecent
                return $! (fads, bvs')
              Nothing -> do
                True ?! ProtVerUnknown
                return $! (fads, bvs') -- a silly thing needed to get types line up

    , do
        TRC ( (sn, dms, cps, rpus)
            , (fads, bvs)
            , (bv, vk)
            ) <- judgmentContext
        let vksM = lookupR vk dms
        case vksM of
          Nothing  -> do
            False ?! CannotAdopt -- md 2019-05-03: not sure what to
                                 -- return/error on here
            return $! (fads, bvs) -- a silly thing needed to get types line up
          Just vks -> do
            let bvs' = bvs ∪ singleton bv vks
            -- canAdopt pps bvs' bv ?! CannotAdopt
            (fromIntegral . unBlockCount) t <= Core.psSize (Set.singleton bv ◃ bvs) ?! CannotAdopt
            case (Map.lookup bv (invertBijection $ fst <$> rpus)) of
              Just pid -> do
                pid `Map.member` (Map.filter (<= sn `Core.minusSlot` liveAfter k) cps) ?! ProtVerTooRecent
                -- This is safe by virtue of the fact we've just inverted the map and found this there
                let (_, ppsc) = fromJust $ Map.lookup pid rpus
                fads' <- trans @FADS $ TRC ((), fads, (sn, (bv, ppsc)))
                return $! (fads', bvs')
              Nothing -> do
                True ?! ProtVerUnknown
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
  , Core.PairSet UpId Core.VKeyGenesis
  , Core.PairSet ProtVer Core.VKeyGenesis
  , Map UpId Core.Slot
  )

emptyUPIState :: UPIState
emptyUPIState =
  ((ProtVer 0 0 0, PParams 0 0 0 0 0.0 0 0 0 0 0 0)
  , []
  , Map.empty
  , Map.empty
  , Map.empty
  , Map.empty
  , Core.PairSet Set.empty
  , Core.PairSet Set.empty
  , Map.empty)

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
        TRC ( (sn, dms)
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
        return ( (pv, pps)
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
        TRC ( (sn, dms)
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
          stable s = unSlot s <= unSlot sn - 2 * unBlockCount GP.k
          avsnew   =
            Map.fromList [ (an, (av, sn, m))
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

  initialRules = []
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

  initialRules = []
  transitionRules =
    [ do
        TRC ( (sn, dms)
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
        let u = pps ^. upTtl
        (fads', bvs') <- trans @UPEND $ TRC ((sn, dms, cps, rpus), (fads, bvs), (bv,vk))
        let pidskeep = dom (Map.filter (>= (sn `Core.minusSlot` u)) pws) `Set.union` dom cps
            rpus' = pidskeep ◃ rpus
            vskeep = Set.fromList . fmap fst $ Map.elems rpus'
        return ( (pv, pps)
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
    ( Core.Slot
    , [(Core.Slot, (ProtVer, PParams))]
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
        TRC ((s_n, fads), (pv, pps), ()) <- judgmentContext
        let r = filter ((<= s_n) . fst) fads
        if r == []
          then return $! (pv, pps)
          else do
            let (_, (pv_c, pps_c)) = last r
            return $! (pv_c, pps_c)
    ]

data UPIEC

instance STS UPIEC where
  type Environment UPIEC = Core.Slot
  type State UPIEC = UPIState
  type Signal UPIEC = ()
  data PredicateFailure UPIEC
    = PVBUMPFailure (PredicateFailure PVBUMP)
    deriving (Eq, Show)

  initialRules = []
  transitionRules =
    [ do
        TRC (s_n, us, ()) <- judgmentContext
        let
          (pv, pps) = us ^. _1 :: (ProtVer, PParams)
          fads      = us ^. _2 :: [(Core.Slot, (ProtVer, PParams))]
        (pv', pps') <- trans @PVBUMP $ TRC ((s_n, fads), (pv, pps), ())
        return $! if pv == pv'
          then us
          else
            ( (pv', pps')       :: (ProtVer, PParams)
            , []                :: [(Core.Slot, (ProtVer, PParams))]
            , us ^. _3          :: Map ApName (ApVer, Core.Slot, Metadata)
            , Map.empty         :: Map UpId (ProtVer, PParams)
            , us ^. _5          :: Map UpId (ApName, ApVer, Metadata)
            , Map.empty         :: Map UpId Core.Slot
            , PairSet Set.empty :: Core.PairSet UpId Core.VKeyGenesis
            , PairSet Set.empty :: Core.PairSet ProtVer Core.VKeyGenesis
            , Map.empty         :: Map UpId Core.Slot
            )
    ]

instance Embed PVBUMP UPIEC where
  wrapFailed = PVBUMPFailure

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The number of blocks needed for endorsement
t :: BlockCount
t = undefined
