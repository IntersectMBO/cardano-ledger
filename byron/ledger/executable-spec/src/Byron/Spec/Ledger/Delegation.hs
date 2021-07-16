{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Ledger.Delegation
  ( -- * Delegation scheduling
    SDELEG,
    SDELEGS,
    DSState (DSState),
    _dSStateScheduledDelegations,
    _dSStateKeyEpochDelegations,
    DCert (DCert),
    delegator,
    delegate,
    depoch,
    dwho,
    mkDCert,
    signature,

    -- * Delegation activation
    ADELEG,
    ADELEGS,
    DSEnv
      ( DSEnv,
        _dSEnvAllowedDelegators,
        _dSEnvEpoch,
        _dSEnvSlot,
        _dSEnvK
      ),
    allowedDelegators,
    DState
      ( DState,
        _dStateDelegationMap,
        _dStateLastDelegation
      ),

    -- * Delegation interface
    DELEG,
    DIEnv,
    DIState (DIState),
    _dIStateDelegationMap,
    _dIStateLastDelegation,
    _dIStateScheduledDelegations,
    _dIStateKeyEpochDelegations,
    liveAfter,
    EpochDiff (..),

    -- * State lens fields
    slot,
    epoch,
    delegationMap,

    -- * State lens type classes
    HasScheduledDelegations,
    scheduledDelegations,
    dmsL,

    -- * Generators
    dcertGen,
    dcertsGen,
    initialEnvFromGenesisKeys,
    randomDCertGen,
    goblinGensDELEG,

    -- * Functions on delegation state
    delegatorOf,

    -- * Support Functions for delegation properties
    delegatorDelegate,
    emptyDelegationPayloadRatio,
    thisEpochDelegationsRatio,
    nextEpochDelegationsRatio,
    selfDelegationsRatio,
    multipleDelegationsRatio,
    maxDelegationsTo,
    changedDelegationsRatio,
    maxChangedDelegations,
    repeatedDelegationsRatio,
    maxRepeatedDelegations,
    maxCertsPerBlock,

    -- * Predicate failures
    AdelegPredicateFailure (..),
    AdelegsPredicateFailure (..),
    SdelegPredicateFailure (..),
    SdelegsPredicateFailure (..),
    MsdelegPredicateFailure (..),
    DelegPredicateFailure (..),
    tamperedDcerts,
  )
where

import Byron.Spec.Ledger.Core
  ( BlockCount,
    Epoch (Epoch),
    HasHash,
    Hash (Hash),
    Owner (Owner),
    Sig,
    Slot (Slot),
    SlotCount (SlotCount),
    VKey (VKey),
    VKeyGenesis (VKeyGenesis),
    addSlot,
    hash,
    mkVkGenesisSet,
    owner,
    range,
    unBlockCount,
    unVKeyGenesis,
    verify,
    (∈),
    (∉),
    (⨃),
  )
import Byron.Spec.Ledger.Core.Generators (epochGen, slotGen)
import qualified Byron.Spec.Ledger.Core.Generators as CoreGen
import Byron.Spec.Ledger.Core.Omniscient (signWithGenesisKey)
import Byron.Spec.Ledger.Util (mkGoblinGens)
import Control.Arrow ((&&&))
import Control.State.Transition
  ( Embed,
    Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    trans,
    transitionRules,
    wrapFailed,
    (?!),
  )
import Control.State.Transition.Generator
  ( HasTrace,
    SignalGenerator,
    envGen,
    genTrace,
    sigGen,
    tinkerWithSigGen,
  )
import Control.State.Transition.Trace (TraceOrder (OldestFirst), traceSignals)
import Data.AbstractSize
import Data.Bimap (Bimap, (!>))
import qualified Data.Bimap as Bimap
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Data.List as List
import Data.List.Unique (count)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro (Lens', lens, to, (%~), (&), (.~), (<>~), (^.), _1)
import Lens.Micro.TH (makeFields)
import NoThunks.Class (NoThunks (..), allNoThunks, noThunksInKeysAndValues)
import Test.Goblin
  ( AddShrinks (..),
    Goblin (..),
    GoblinData,
    SeedGoblin (..),
    mkEmptyGoblin,
  )
import Test.Goblin.TH (deriveAddShrinks, deriveGoblin, deriveSeedGoblin)

--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A delegation certificate.
data DCert = DCert
  { -- | Key that delegates
    delegator :: VKeyGenesis,
    -- | Key that the delegator is delegating to
    delegate :: VKey,
    -- | Certificate epoch
    depoch :: Epoch,
    -- | Witness for the delegation certificate
    signature :: Sig (VKey, Epoch)
  }
  deriving (Show, Eq, Ord, Generic, Hashable, Data, Typeable, NoThunks)

instance HasTypeReps DCert

instance HasHash [DCert] where
  hash = Hash . Just . H.hash

mkDCert ::
  VKeyGenesis ->
  Sig (VKey, Epoch) ->
  VKey ->
  Epoch ->
  DCert
mkDCert vkg s vkd e =
  DCert
    { delegator = vkg,
      delegate = vkd,
      depoch = e,
      signature = s
    }

-- | Who is delegating to whom.
dwho :: DCert -> (VKeyGenesis, VKey)
dwho = delegator &&& delegate

-- | Part of the delegation certificate that is signed by the delegator.
dbody :: DCert -> (VKey, Epoch)
dbody = delegate &&& depoch

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { -- | Set of allowed delegators
    _dSEnvAllowedDelegators :: Set VKeyGenesis,
    -- | Current epoch
    _dSEnvEpoch :: Epoch,
    -- | Current slot
    _dSEnvSlot :: Slot,
    -- | Chain stability parameter
    _dSEnvK :: BlockCount
  }
  deriving (Show, Eq, Generic, NoThunks)

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))],
    _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  }
  deriving (Show, Eq, Generic, NoThunks)

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dStateDelegationMap :: Bimap VKeyGenesis VKey,
    -- | When was the last time each genesis key delegated.
    _dStateLastDelegation :: Map VKeyGenesis Slot
  }
  deriving (Eq, Show, Generic)

instance NoThunks DState where
  wNoThunks ctxt (DState dmap lastDeleg) =
    allNoThunks
      [ noThunksInKeysAndValues ctxt $ Bimap.toList dmap,
        noThunksInKeysAndValues ctxt $ Map.toList lastDeleg
      ]

makeFields ''DState

delegatorOf :: Bimap VKeyGenesis VKey -> VKey -> Maybe VKeyGenesis
delegatorOf = flip Bimap.lookupR

-- | Interface environment is the same as scheduling environment.
type DIEnv = DSEnv

data DIState = DIState
  { _dIStateDelegationMap :: Bimap VKeyGenesis VKey,
    _dIStateLastDelegation :: Map VKeyGenesis Slot,
    _dIStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))],
    _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  }
  deriving (Show, Eq, Generic)

makeFields ''DIState

instance NoThunks DIState where
  wNoThunks ctxt (DIState dmap lastDeleg sds sked) =
    allNoThunks
      [ noThunksInKeysAndValues ctxt $ Bimap.toList dmap,
        noThunksInKeysAndValues ctxt $ Map.toList lastDeleg,
        wNoThunks ctxt sds,
        wNoThunks ctxt sked
      ]

dmsL ::
  HasDelegationMap a (Bimap VKeyGenesis VKey) =>
  Lens' a (Bimap VKeyGenesis VKey)
dmsL = delegationMap

dIStateDSState :: Lens' DIState DSState
dIStateDSState =
  lens
    (\dis -> DSState (dis ^. scheduledDelegations) (dis ^. keyEpochDelegations))
    ( \dis dss ->
        dis
          & scheduledDelegations
          .~ dss
          ^. scheduledDelegations
          & keyEpochDelegations
          .~ dss
          ^. keyEpochDelegations
    )

dIStateDState :: Lens' DIState DState
dIStateDState =
  lens
    (\dis -> DState (dis ^. delegationMap) (dis ^. lastDelegation))
    ( \dis dss ->
        dis
          & delegationMap
          .~ dss
          ^. delegationMap
          & lastDelegation
          .~ dss
          ^. lastDelegation
    )

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------

-- | Delegation scheduling rules
data SDELEG deriving (Data, Typeable)

data EpochDiff = EpochDiff {currentEpoch :: Epoch, certEpoch :: Epoch}
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

-- | These `PredicateFailure`s are all "throwable". The disjunction of the
--   rules' preconditions is not `True` - the `PredicateFailure`s represent
--   `False` cases.
data SdelegPredicateFailure
  = IsNotGenesisKey
  | EpochInThePast EpochDiff
  | EpochPastNextEpoch EpochDiff
  | HasAlreadyDelegated
  | IsAlreadyScheduled
  | DoesNotVerify
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS SDELEG where
  type State SDELEG = DSState
  type Signal SDELEG = DCert
  type Environment SDELEG = DSEnv
  type PredicateFailure SDELEG = SdelegPredicateFailure

  initialRules =
    [ return
        DSState
          { _dSStateScheduledDelegations = [],
            _dSStateKeyEpochDelegations = Set.empty
          }
    ]
  transitionRules =
    [ do
        TRC (env, st, cert) <- judgmentContext
        verify
          (unVKeyGenesis $ delegator cert)
          (dbody cert)
          (signature cert)
          ?! DoesNotVerify
        notAlreadyDelegated st cert ?! HasAlreadyDelegated
        let d = liveAfter (env ^. k)
        notAlreadyScheduled d env st cert ?! IsAlreadyScheduled
        Set.member (cert ^. to dwho . _1) (env ^. allowedDelegators) ?! IsNotGenesisKey
        env ^. epoch <= cert ^. to depoch
          ?! EpochInThePast
            EpochDiff
              { currentEpoch = env ^. epoch,
                certEpoch = cert ^. to depoch
              }
        cert ^. to depoch <= env ^. epoch + 1
          ?! EpochPastNextEpoch
            EpochDiff
              { currentEpoch = env ^. epoch,
                certEpoch = cert ^. to depoch
              }
        return $
          st
            & scheduledDelegations
              <>~ [ ( (env ^. slot) `addSlot` d,
                      cert ^. to dwho
                    )
                  ]
            & keyEpochDelegations %~ Set.insert (epochDelegator cert)
    ]
    where
      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> Bool
      notAlreadyDelegated st cert =
        Set.notMember (epochDelegator cert) (st ^. keyEpochDelegations)

      epochDelegator :: DCert -> (Epoch, VKeyGenesis)
      epochDelegator cert = (cert ^. to depoch, cert ^. to dwho . _1)

      -- Check whether there is a scheduled delegation from this key
      notAlreadyScheduled :: SlotCount -> DSEnv -> DSState -> DCert -> Bool
      notAlreadyScheduled d env st cert =
        List.notElem
          ((env ^. slot) `addSlot` d, cert ^. to dwho . _1)
          (st ^. scheduledDelegations . to (fmap $ fmap fst))

-- | Compute after which slot the delegation certificate will become live,
-- using the chain stability parameter.
liveAfter :: BlockCount -> SlotCount
liveAfter bc = SlotCount $ 2 * unBlockCount bc

-- | None of these `PredicateFailure`s are actually "throwable". The
--   disjuction of the rules' preconditions is `True`, which means that one of
--   them will pass. The `PredicateFailure` just act as switches to direct
--   control flow to the successful one.
data AdelegPredicateFailure
  = S_BeforeExistingDelegation
  | S_NoLastDelegation
  | S_AfterExistingDelegation
  | S_AlreadyADelegateOf VKey VKeyGenesis
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

-- | Delegation rules
data ADELEG deriving (Data, Typeable)

instance STS ADELEG where
  type State ADELEG = DState
  type Signal ADELEG = (Slot, (VKeyGenesis, VKey))
  type Environment ADELEG = Set VKeyGenesis
  type PredicateFailure ADELEG = AdelegPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        return
          DState
            { _dStateDelegationMap =
                Bimap.fromList $
                  map (\vkg@(VKeyGenesis key) -> (vkg, key)) (Set.toList env),
              _dStateLastDelegation = Map.fromSet (const (Slot 0)) env
            }
    ]

  transitionRules =
    [ do
        TRC
          ( _env,
            DState
              { _dStateDelegationMap = dms,
                _dStateLastDelegation = dws
              },
            (s, (vks, vkd))
            ) <-
          judgmentContext
        vkd ∉ range dms ?! S_AlreadyADelegateOf vkd (dms !> vkd)
        case Map.lookup vks dws of
          Nothing -> pure () -- If vks hasn't delegated, then we proceed and
          -- update the @ADELEG@ state.
          Just sp -> sp < s ?! S_BeforeExistingDelegation
        return
          $! DState
            { _dStateDelegationMap = dms ⨃ [(vks, vkd)],
              _dStateLastDelegation = dws ⨃ [(vks, s)]
            },
      do
        TRC
          ( _env,
            st@DState
              { _dStateDelegationMap = dms,
                _dStateLastDelegation = dws
              },
            (s, (vks, vkd))
            ) <-
          judgmentContext
        if vkd ∈ range dms
          then return st
          else do
            case Map.lookup vks dws of
              Just sp -> sp >= s ?! S_AfterExistingDelegation
              Nothing ->
                error $
                  "This can't happen since otherwise "
                    ++ "the previous rule would have been triggered."
            return st
    ]

-- | Delegation scheduling sequencing
data SDELEGS deriving (Data, Typeable)

data SdelegsPredicateFailure
  = SDelegFailure (PredicateFailure SDELEG)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS SDELEGS where
  type State SDELEGS = DSState
  type Signal SDELEGS = [DCert]
  type Environment SDELEGS = DSEnv
  type PredicateFailure SDELEGS = SdelegsPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @SDELEG $ IRC env
    ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x : xs) -> do
            dss' <- trans @SDELEG $ TRC (env, st, x)
            dss'' <- trans @SDELEGS $ TRC (env, dss', xs)
            return dss''
    ]

instance Embed SDELEG SDELEGS where
  wrapFailed = SDelegFailure

-- | Delegation rules sequencing
data ADELEGS deriving (Data, Typeable)

data AdelegsPredicateFailure
  = ADelegFailure (PredicateFailure ADELEG)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS ADELEGS where
  type State ADELEGS = DState
  type Signal ADELEGS = [(Slot, (VKeyGenesis, VKey))]
  type Environment ADELEGS = Set VKeyGenesis

  type
    PredicateFailure ADELEGS =
      AdelegsPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        trans @ADELEG $ IRC env
    ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x : xs) -> do
            ds' <- trans @ADELEG $ TRC (env, st, x)
            ds'' <- trans @ADELEGS $ TRC (env, ds', xs)
            return ds''
    ]

instance Embed ADELEG ADELEGS where
  wrapFailed = ADelegFailure

-- | Delegation interface
data DELEG deriving (Data, Typeable)

data DelegPredicateFailure
  = SDelegSFailure (PredicateFailure SDELEGS)
  | ADelegSFailure (PredicateFailure ADELEGS)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS DELEG where
  type State DELEG = DIState
  type Signal DELEG = [DCert]
  type Environment DELEG = DIEnv

  type PredicateFailure DELEG = DelegPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        initADelegsState <- trans @ADELEGS $ IRC (env ^. allowedDelegators)
        initSDelegsState <- trans @SDELEGS $ IRC env
        pure
          $! DIState
            { _dIStateDelegationMap = initADelegsState ^. delegationMap,
              _dIStateLastDelegation = initADelegsState ^. lastDelegation,
              _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations,
              _dIStateKeyEpochDelegations = initSDelegsState ^. keyEpochDelegations
            }
    ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        sds <- trans @SDELEGS $ TRC (env, st ^. dIStateDSState, sig)
        let slots = filter ((<= (env ^. slot)) . fst) $ sds ^. scheduledDelegations
        as <- trans @ADELEGS $ TRC (env ^. allowedDelegators, st ^. dIStateDState, slots)
        return $
          DIState
            (as ^. delegationMap)
            (as ^. lastDelegation)
            ( filter (((env ^. slot) `addSlot` 1 <=) . fst) $
                sds ^. scheduledDelegations
            )
            ( Set.filter ((env ^. epoch <=) . fst) $
                sds ^. keyEpochDelegations
            )
    ]

instance Embed SDELEGS DELEG where
  wrapFailed = SDelegSFailure

instance Embed ADELEGS DELEG where
  wrapFailed = ADelegSFailure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

dcertGen :: DSEnv -> Set (Epoch, VKeyGenesis) -> Gen (Maybe DCert)
dcertGen env eks =
  let allowed :: [VKeyGenesis]
      allowed = Set.toList (_dSEnvAllowedDelegators env)
      -- We can generate delegation certificates using the allowed delegators,
      -- and we can delegate for the current or next epoch only.
      preCandidates :: Set (Epoch, VKeyGenesis)
      preCandidates =
        Set.fromList $
          zip (repeat $ _dSEnvEpoch env) allowed
            ++ zip (repeat $ _dSEnvEpoch env + 1) allowed
      -- We obtain the candidates by removing the ones that already delegated in
      -- this or next epoch.
      candidates :: [(Epoch, VKeyGenesis)]
      candidates = Set.toList $ preCandidates \\ eks
      -- Next, we choose to whom these keys delegate.
      target :: [VKey]
      -- NOTE: we might want to make this configurable for now we chose an upper
      -- bound equal to two times the number of genesis keys to increase the
      -- chance of having two genesis keys delegating to the same key.
      target = VKey . Owner <$> [0 .. (2 * fromIntegral (length allowed))]

      mkDCert' ((e, vkg), vk) = mkDCert vkg (signWithGenesisKey vkg (vk, e)) vk e
   in if null candidates
        then return Nothing
        else Just <$> Gen.element (mkDCert' <$> zip candidates target)

dcertsGen :: DSEnv -> DIState -> Gen [DCert]
dcertsGen env st =
  -- This generator can result in an empty list of delegation certificates if
  -- no delegation certificates can be produced, according to the delegation
  -- rules, given the initial state and environment.
  catMaybes . traceSignals OldestFirst <$> genTrace @MSDELEG () n env subSt (sigGen @MSDELEG)
  where
    n = env ^. allowedDelegators . to length . to fromIntegral
    subSt =
      DSState
        { _dSStateScheduledDelegations = _dIStateScheduledDelegations st,
          _dSStateKeyEpochDelegations = _dIStateKeyEpochDelegations st
        }

-- | Generate a random delegation certificate, which has a high probability of failing since
-- we do not consider the current delegation state. So for instance, we could generate a
-- delegation certificate for a genesis key that already delegated in this epoch.
randomDCertGen :: Environment DELEG -> Gen DCert
randomDCertGen env = do
  (vkg, vk, e) <- (,,) <$> vkgGen' <*> vkGen' <*> epochGen'
  pure $! mkDCert vkg (signWithGenesisKey vkg (vk, e)) vk e
  where
    vkgGen' = Gen.element $ Set.toList allowed
    allowed = _dSEnvAllowedDelegators env
    vkGen' = Gen.element $ VKey . Owner <$> [0 .. (2 * fromIntegral (length allowed))]
    epochGen' =
      Epoch
        . fromIntegral -- We don't care about underflow. We want to generate large epochs anyway.
        . (fromIntegral n +)
        <$> Gen.integral (Range.constant (-2 :: Int) 2)
      where
        Epoch n = _dSEnvEpoch env

-- | Dummy transition system needed for generating sequences of delegation certificates.
data MSDELEG deriving (Data, Typeable)

data MsdelegPredicateFailure = SDELEGFailure (PredicateFailure SDELEG)
  deriving (Eq, Show, Data, Typeable, Generic, NoThunks)

instance STS MSDELEG where
  type Environment MSDELEG = DSEnv
  type State MSDELEG = DSState
  type Signal MSDELEG = Maybe DCert
  type PredicateFailure MSDELEG = MsdelegPredicateFailure

  initialRules = []

  transitionRules =
    [ do
        TRC (env, st, msig) <- judgmentContext
        case msig of
          Nothing -> pure st
          Just sig -> trans @SDELEG $ TRC (env, st, sig)
    ]

instance Embed SDELEG MSDELEG where
  wrapFailed = SDELEGFailure

instance HasTrace MSDELEG where
  envGen = delegEnvGen

  sigGen env st = dcertGen env (_dSStateKeyEpochDelegations st)

instance HasTrace DELEG where
  envGen = delegEnvGen

  sigGen = dcertsGen

delegEnvGen :: Word64 -> Gen DSEnv
delegEnvGen chainLength = do
  ngk <- Gen.integral (Range.linear 1 14)
  initialEnvFromGenesisKeys ngk chainLength

-- | Generate an initial 'DELEG' environment from the given number of genesis
-- keys.
initialEnvFromGenesisKeys ::
  -- | Number of genesis keys.
  Word8 ->
  -- | Chain length
  Word64 ->
  Gen DSEnv
initialEnvFromGenesisKeys ngk chainLength =
  DSEnv
    -- We need at least one delegator in the environment to be able to generate
    -- delegation certificates.
    --
    -- The use of a linear generator and its bound is rather arbitrary. The
    -- sizes passed to the `Gen` monad would be around 100~300, so we rather
    -- arbitrarily decided that this size times 100 is a reasonable upper
    -- bounds for epochs.
    --
    -- A similar remark applies to the ranges chosen for slot and slot count
    -- generators.
    <$> pure (mkVkGenesisSet ngk)
    <*> epochGen 0 10
    <*> slotGen 0 100
    <*> CoreGen.k chainLength (chainLength `div` 10)

--------------------------------------------------------------------------------
-- Shared support functions for Delegation Properties
--------------------------------------------------------------------------------

delegatorDelegate :: DCert -> (VKeyGenesis, VKey)
delegatorDelegate = delegator &&& delegate

ratioInt :: Int -> Int -> Double
ratioInt x y =
  fromIntegral x / fromIntegral y

-- | Transforms the list and returns the ratio of lengths of
-- the transformed and original lists.
lenRatio :: ([a] -> [b]) -> [a] -> Double
lenRatio f xs =
  ratioInt
    (length (f xs))
    (length xs)

-- | True if the tuple count (snd item) is >= 2
multiple :: (a, Int) -> Bool
multiple = (2 <=) . snd

-- | Count the number of delegates/delegators in the given certificates
delegateCounter ::
  Ord a =>
  ((VKeyGenesis, VKey) -> a) ->
  [DCert] ->
  [(a, Int)]
delegateCounter pick certs =
  fmap delegatorDelegate certs
    -- Remove duplicated elements, since we're not
    -- interested in repeated delegations
    & List.nub
    -- Select the (unique) delegate/delegators
    & fmap pick
    -- If we have more than one occurrence, then there were
    -- multiple delegations from/to that delegate
    & count

-- | Count the number of times each delegate was delegated to
delegateCounts :: [DCert] -> [(VKey, Int)]
delegateCounts = delegateCounter snd

-- | Count the number of times each delegator changed their delegation
delegatorCounts :: [DCert] -> [(VKeyGenesis, Int)]
delegatorCounts = delegateCounter fst

-- | Ratio of certificate groups that are empty
emptyDelegationPayloadRatio :: [[DCert]] -> Double
emptyDelegationPayloadRatio =
  lenRatio (filter null)

-- | Ratio of certificates that delegate to _this_ epoch, where
--   each certificate is represented by (current epoch,cert epoch)
thisEpochDelegationsRatio :: [(Epoch, Epoch)] -> Double
thisEpochDelegationsRatio =
  lenRatio (filter thisEpoch)
  where
    thisEpoch = uncurry (==)

-- | Ratio of certificates that delegate to the _next_ epoch, where
--   each certificate is represented by (current epoch,cert epoch)
nextEpochDelegationsRatio :: [(Epoch, Epoch)] -> Double
nextEpochDelegationsRatio =
  lenRatio (filter nextEpoch)
  where
    nextEpoch (e0, e1) = e0 + 1 == e1

-- | Ratio of certificates that "delegate to self", that is,
-- where the delegator and delegate are the same
selfDelegationsRatio :: [DCert] -> Double
selfDelegationsRatio =
  lenRatio (filter selfDeleg . fmap delegatorDelegate)
  where
    selfDeleg (vks, vk) = owner vks == owner vk

-- | Ratio of delegates that have multiple delegators
-- that are delegating to them
multipleDelegationsRatio :: [DCert] -> Double
multipleDelegationsRatio dcerts =
  lenRatio (filter multiple) (delegateCounts dcerts)

-- | The maximum number of delegators to any particular delegate
maxDelegationsTo :: [DCert] -> Int
maxDelegationsTo dcerts =
  case filter multiple (delegateCounts dcerts) of
    [] -> 1
    xs -> List.maximum (snd <$> xs)

-- | Ratio of delegators that have changed their delegations
changedDelegationsRatio :: [DCert] -> Double
changedDelegationsRatio dcerts =
  lenRatio (filter multiple) (delegatorCounts dcerts)

-- | The maximum number of change-of-delegate for any particular delegator
maxChangedDelegations :: [DCert] -> Int
maxChangedDelegations dcerts =
  case filter multiple (delegateCounts dcerts) of
    [] -> 1
    xs -> List.maximum (snd <$> xs)

-- | Ratio of repeated delegations to all delegations
repeatedDelegationsRatio :: [DCert] -> Double
repeatedDelegationsRatio dcerts =
  fmap delegatorDelegate dcerts
    & count
    & lenRatio (filter multiple)

-- | The maximum number of repeated delegations in the given certificates
maxRepeatedDelegations :: [DCert] -> Int
maxRepeatedDelegations dcerts =
  case filter multiple ds of
    [] -> 1
    xs -> List.maximum (snd <$> xs)
  where
    ds = count (fmap delegatorDelegate dcerts)

maxCertsPerBlock :: [[DCert]] -> Int
maxCertsPerBlock groupedCerts =
  case groupedCerts of
    [] -> 0
    _ -> List.maximum (length <$> groupedCerts)

--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

deriveGoblin ''DCert

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''DCert

--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''DSEnv
deriveSeedGoblin ''DIState

--------------------------------------------------------------------------------
-- GoblinData & goblin-tinkered SignalGenerators
--------------------------------------------------------------------------------

mkGoblinGens
  "DELEG"
  [ "SDelegSFailure_SDelegFailure_EpochInThePast",
    "SDelegSFailure_SDelegFailure_EpochPastNextEpoch",
    "SDelegSFailure_SDelegFailure_IsAlreadyScheduled",
    "SDelegSFailure_SDelegFailure_IsNotGenesisKey"
  ]

tamperedDcerts :: DIEnv -> DIState -> Gen [DCert]
tamperedDcerts env st =
  Gen.choice
    [ Gen.list (Range.constant 0 10) (randomDCertGen env),
      do
        sg <- Gen.element goblinGensDELEG
        sg env st
    ]
