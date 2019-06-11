{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Ledger.Delegation
  ( -- * Delegation scheduling
    SDELEG
  , SDELEGS
  , DSState(DSState)
  , _dSStateScheduledDelegations
  , _dSStateKeyEpochDelegations
  , DCert(DCert)
  , _dbody
  , _dwit
  , _dwho
  , _depoch
  , mkDCert
  , delegator
  , delegate
  , dbody
  , dwit
  , dwho
  , depoch
    -- * Delegation activation
  , ADELEG
  , ADELEGS
  , DSEnv
    ( DSEnv
    , _dSEnvAllowedDelegators
    , _dSEnvEpoch
    , _dSEnvSlot
    , _dSEnvK
    )
  , allowedDelegators
  , DState
    ( DState
    , _dStateDelegationMap
    , _dStateLastDelegation
    )
  -- * Delegation interface
  , DELEG
  , DIEnv
  , DIState(DIState)
  , _dIStateDelegationMap
  , _dIStateLastDelegation
  , _dIStateScheduledDelegations
  , _dIStateKeyEpochDelegations
  , PredicateFailure(SDelegSFailure, SDelegFailure, IsAlreadyScheduled)
  , liveAfter
  -- * State lens fields
  , slot
  , delegationMap
  -- * State lens type classes
  , HasScheduledDelegations
  , scheduledDelegations
  , dmsL
  -- * Generators
  , dcertGen
  , dcertsGen
  , dcertGen'
  , dcertsGen'
  -- * Functions on delegation state
  , delegatorOf
  )
where

import Data.AbstractSize
import Data.Bimap (Bimap, (!>))
import qualified Data.Bimap as Bimap
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constant, linear)
import Control.Lens
  ( Lens'
  , (%~)
  , (&)
  , (.~)
  , (<>~)
  , (^.)
  , _1
  , _2
  , lens
  , makeFields
  , makeLenses
  , to
  )


import Control.State.Transition
  ( Embed
  , Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , IRC(IRC)
  , TRC(TRC)
  , (?!)
  , initialRules
  , judgmentContext
  , trans
  , transitionRules
  , wrapFailed
  )
import Control.State.Transition.Generator
  ( HasTrace
  , initEnvGen
  , sigGen
  )
import Ledger.Core
  ( BlockCount
  , Epoch(Epoch)
  , HasHash
  , Hash(Hash)
  , Owner(Owner)
  , Sig(Sig)
  , Slot(Slot)
  , SlotCount(SlotCount)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , (∈)
  , (∉)
  , (⨃)
  , addSlot
  , hash
  , owner
  , owner
  , range
  , unBlockCount
  )
import Ledger.Core.Generators
  ( blockCountGen
  , epochGen
  , slotGen
  , vkGen
  , vkgenesisGen
  )


--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A delegation certificate.
data DCert = DCert
  { -- | Body of the delegation certificate
    _dbody :: (VKey, Epoch)
    -- | Witness for the delegation cerfiticate
  , _dwit :: Sig VKeyGenesis
    -- | Who delegates to whom
  , _dwho :: (VKeyGenesis, VKey)
    -- | Certificate epoch
  , _depoch :: Epoch
  } deriving (Show, Eq, Generic, Hashable)

instance HasTypeReps DCert

instance HasHash [DCert] where
  hash = Hash . H.hash

makeLenses ''DCert

mkDCert
  :: VKeyGenesis
  -> Sig VKeyGenesis
  -> VKey
  -> Epoch
  -> DCert
mkDCert vkg sigVkg vkd e
  = DCert
  { _dbody = (vkd, e)
  , _dwit = sigVkg
  , _dwho = (vkg, vkd)
  , _depoch = e
  }

-- | Key that is delegating.
delegator :: DCert -> VKeyGenesis
delegator c = c ^. dwho . _1

-- | Key being delegated to.
delegate :: DCert -> VKey
delegate c = c ^. dwho . _2

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { _dSEnvAllowedDelegators :: Set VKeyGenesis
  -- ^ Set of allowed delegators
  , _dSEnvEpoch :: Epoch
  -- ^ Current epoch
  , _dSEnvSlot :: Slot
  -- ^ Current slot
  , _dSEnvK :: BlockCount
  -- ^ Chain stability parameter
  } deriving (Show, Eq)

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))]
  , _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  } deriving (Show, Eq)

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dStateDelegationMap :: Bimap VKeyGenesis VKey
    -- | When was the last time each genesis key delegated.
  , _dStateLastDelegation :: Map VKeyGenesis Slot
  } deriving (Eq, Show)

makeFields ''DState

delegatorOf :: Bimap VKeyGenesis VKey -> VKey -> Maybe VKeyGenesis
delegatorOf = flip Bimap.lookupR

-- | Interface environment is the same as scheduling environment.
type DIEnv = DSEnv

data DIState = DIState
  { _dIStateDelegationMap :: Bimap VKeyGenesis VKey
  , _dIStateLastDelegation :: Map VKeyGenesis Slot
  , _dIStateScheduledDelegations :: [(Slot, (VKeyGenesis, VKey))]
  , _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGenesis)
  } deriving (Show, Eq)

makeFields ''DIState

-- | Epoch-genesis key delegation set of the delegation interface state.
eks :: DIState -> Set (Epoch, VKeyGenesis)
eks = _dIStateKeyEpochDelegations

dmsL :: HasDelegationMap a (Bimap VKeyGenesis VKey)
    => Lens' a (Bimap VKeyGenesis VKey)
dmsL = delegationMap

dIStateDSState :: Lens' DIState DSState
dIStateDSState = lens
  (\dis -> DSState (dis ^. scheduledDelegations) (dis ^. keyEpochDelegations))
  (\dis dss ->
    dis
      &  scheduledDelegations
      .~ dss
      ^. scheduledDelegations
      &  keyEpochDelegations
      .~ dss
      ^. keyEpochDelegations
  )

dIStateDState :: Lens' DIState DState
dIStateDState = lens
  (\dis -> DState (dis ^. delegationMap) (dis ^. lastDelegation))
  (\dis dss ->
    dis
      &  delegationMap
      .~ dss
      ^. delegationMap
      &  lastDelegation
      .~ dss
      ^. lastDelegation
  )

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------

-- | Delegation scheduling rules
data SDELEG

instance STS SDELEG where
  type State SDELEG = DSState
  type Signal SDELEG = DCert
  type Environment SDELEG = DSEnv

  data PredicateFailure SDELEG
    = IsNotGenesisKey
    | IsPastEpoch
    | EpochFarInTheFuture
    | HasAlreadyDelegated
    | IsAlreadyScheduled
    | DoesNotVerify
    deriving (Eq, Show)

  initialRules = [ return DSState
                   { _dSStateScheduledDelegations = []
                   , _dSStateKeyEpochDelegations  = Set.empty
                   }
                 ]
  transitionRules =
    [ do
        TRC (env, st, cert) <- judgmentContext
        verify cert ?! DoesNotVerify
        notAlreadyDelegated st cert ?! HasAlreadyDelegated
        let d = liveAfter (env ^. k)
        notAlreadyScheduled d env st cert ?! IsAlreadyScheduled
        Set.member (cert ^. dwho . _1) (env ^. allowedDelegators) ?! IsNotGenesisKey
        let diff = cert ^. depoch - env ^. epoch
        0 <= diff ?! IsPastEpoch
        diff <= 1 ?! EpochFarInTheFuture
        return $ st
          & scheduledDelegations <>~ [((env ^. slot) `addSlot` d
                                      , cert ^. dwho
                                      )]
          & keyEpochDelegations %~ Set.insert (epochDelegator cert)
    ]
    where
      verify :: DCert -> Bool
      verify = const True

      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> Bool
      notAlreadyDelegated st cert =
        Set.notMember (epochDelegator cert) (st ^. keyEpochDelegations)

      epochDelegator :: DCert -> (Epoch, VKeyGenesis)
      epochDelegator cert = (cert ^. depoch, cert ^. dwho . _1)

      -- Check whether there is a scheduled delegation from this key
      notAlreadyScheduled :: SlotCount -> DSEnv -> DSState -> DCert -> Bool
      notAlreadyScheduled d env st cert =
        List.notElem
          ((env ^. slot) `addSlot` d, cert ^. dwho . _1)
          (st ^. scheduledDelegations . to (fmap $ fmap fst))

-- | Compute after which slot the delegation certificate will become live,
-- using the chain stability parameter.
liveAfter :: BlockCount -> SlotCount
liveAfter bc = SlotCount $ 2 * unBlockCount bc

-- | Delegation rules
data ADELEG

instance STS ADELEG where
  type State ADELEG = DState
  type Signal ADELEG = (Slot, (VKeyGenesis, VKey))
  type Environment ADELEG = Set VKeyGenesis

  data PredicateFailure ADELEG
    = BeforeExistingDelegation
      -- | Not actually a failure; this should just trigger the other rule.
    | NoLastDelegation
      -- | Not a failure; this should just pass the other rule
    | AfterExistingDelegation
    -- | The given key is a delegate of the given genesis key.
    | AlreadyADelegateOf VKey VKeyGenesis

    deriving (Eq, Show)

  initialRules = [
    do
      IRC env <- judgmentContext
      return DState
        { _dStateDelegationMap  =
            Bimap.fromList
            $ map (\vkg@(VKeyGenesis key) -> (vkg, key)) (Set.toList env)
        , _dStateLastDelegation = Map.fromSet (const (Slot 0)) env
        }
    ]
  transitionRules =
    [ do
        TRC ( _env
            , DState { _dStateDelegationMap = dms
                     , _dStateLastDelegation = dws
                     }
            , (s, (vks, vkd))
            ) <- judgmentContext
        vkd ∉ range dms ?! AlreadyADelegateOf vkd (dms !> vkd)
        case Map.lookup vks dws of
          Nothing -> pure () -- If vks hasn't delegated, then we proceed and
                             -- update the @ADELEG@ state.
          Just sp -> sp < s ?! BeforeExistingDelegation
        return $!
          DState { _dStateDelegationMap = dms ⨃ [(vks, vkd)]
                 , _dStateLastDelegation = dws ⨃ [(vks, s)]
                 }
    , do
        TRC ( _env
            , st@DState { _dStateDelegationMap = dms
                        , _dStateLastDelegation = dws
                        }
            , (s, (vks, vkd))
            ) <- judgmentContext
        if vkd ∈ range dms
          then return st
          else do
            case Map.lookup vks dws of
              Just sp -> sp >= s ?! AfterExistingDelegation
              Nothing -> False ?! NoLastDelegation
            return st
    ]

-- | Delegation scheduling sequencing
data SDELEGS

instance STS SDELEGS where
  type State SDELEGS = DSState
  type Signal SDELEGS = [DCert]
  type Environment SDELEGS = DSEnv

  data PredicateFailure SDELEGS
    = SDelegFailure (PredicateFailure SDELEG)
    deriving (Eq, Show)

  initialRules = [ do
                     IRC env <- judgmentContext
                     trans @SDELEG $ IRC env
                 ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x:xs) -> do
            dss'  <- trans @SDELEG $ TRC (env, st, x)
            dss'' <- trans @SDELEGS $ TRC (env, dss', xs)
            return dss''
    ]

instance Embed SDELEG SDELEGS where
  wrapFailed = SDelegFailure

-- | Delegation rules sequencing
data ADELEGS

instance STS ADELEGS where
  type State ADELEGS = DState
  type Signal ADELEGS = [(Slot, (VKeyGenesis, VKey))]
  type Environment ADELEGS = Set VKeyGenesis

  data PredicateFailure ADELEGS
    = ADelegFailure (PredicateFailure ADELEG)
    deriving (Eq, Show)

  initialRules = [ do
                     IRC env <- judgmentContext
                     trans @ADELEG $ IRC env
                 ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        case sig of
          [] -> return st
          (x:xs) -> do
            ds'  <- trans @ADELEG $ TRC (env, st, x)
            ds'' <- trans @ADELEGS $ TRC (env, ds', xs)
            return ds''
    ]

instance Embed ADELEG ADELEGS where
  wrapFailed = ADelegFailure

-- | Delegation interface
data DELEG

instance STS DELEG where
  type State DELEG = DIState
  type Signal DELEG = [DCert]
  type Environment DELEG = DIEnv

  data PredicateFailure DELEG
    = SDelegSFailure (PredicateFailure SDELEGS)
    | ADelegSFailure (PredicateFailure ADELEGS)
    deriving (Eq, Show)

  initialRules = [ do
                     IRC env <- judgmentContext
                     initADelegsState <- trans @ADELEGS $ IRC (env ^. allowedDelegators)
                     initSDelegsState <- trans @SDELEGS $ IRC env
                     return DIState
                       { _dIStateDelegationMap  = initADelegsState ^. delegationMap
                       , _dIStateLastDelegation = initADelegsState ^. lastDelegation
                       , _dIStateScheduledDelegations = initSDelegsState ^. scheduledDelegations
                       , _dIStateKeyEpochDelegations  = initSDelegsState ^. keyEpochDelegations
                       }
                 ]
  transitionRules =
    [ do
        TRC (env, st, sig) <- judgmentContext
        sds <- trans @SDELEGS $ TRC (env, st ^. dIStateDSState, sig)
        let slots = filter ((<= (env ^. slot)) . fst) $ sds ^. scheduledDelegations
        as <- trans @ADELEGS $ TRC (env ^. allowedDelegators, st ^. dIStateDState, slots)
        return $ DIState
          (as ^. delegationMap)
          (as ^. lastDelegation)
          (filter (((env ^. slot) `addSlot` 1 <=) . fst)
            $ sds ^. scheduledDelegations)
          (Set.filter ((env ^. epoch <=) . fst)
            $ sds ^. keyEpochDelegations)
    ]

instance Embed SDELEGS DELEG where
  wrapFailed = SDelegSFailure

instance Embed ADELEGS DELEG where
  wrapFailed = ADelegSFailure

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate delegation certificates, using the allowed delegators in the
-- environment passed as parameter.
dcertGen :: DSEnv -> Gen DCert
dcertGen env = do
  -- The generated delegator must be one of the genesis keys in the
  -- environment.
  --
  -- TODO: the delegator should be one of the allowed delegators that can
  -- actually delegate (so it doens't have anything scheduled for this or next
  -- epoch)
  vkS <- Gen.element $ Set.toList (env ^. allowedDelegators)
  vkD <- vkGen
  let Epoch n = env ^. epoch
  m   <- Gen.integral (linear 0 100)
  let epo = Epoch (n + m)
  return DCert { _dbody = (vkD, epo)
               , _dwit = Sig vkS (owner vkS)
               , _dwho = (vkS, vkD)
               , _depoch = epo
               }

dcertGen' :: DSEnv -> DIState -> Gen (Maybe DCert)
dcertGen' env st =
  let
    allowed :: [VKeyGenesis]
    allowed = Set.toList (_dSEnvAllowedDelegators env)
    -- We can generate delegation certificates using the allowed delegators,
    -- and we can delegate for the current or next epoch only.
    preCandidates :: Set (Epoch, VKeyGenesis)
    preCandidates = Set.fromList
                  $  zip (repeat $ _dSEnvEpoch env) allowed
                  ++ zip (repeat $ _dSEnvEpoch env + 1) allowed
    -- We obtain the candidates by removing the ones that already delegated in
    -- this or next epoch.
    candidates :: [(Epoch, VKeyGenesis)]
    candidates = Set.toList $ preCandidates \\ eks st
    -- Next, we choose to whom these keys delegate.
    target :: [VKey]
    -- TODO: we might want to make this configurable for now we chose an upper
    -- bound equal to three time the number of genesis keys to increase the
    -- chance of having two genesis keys delegating to the same key.
    target = VKey . Owner <$> [0 .. (3 * fromIntegral (length allowed))]

    mkDCert' ((e, vkg), vk) = DCert (vk, e) (Sig vkg (owner vkg)) (vkg, vk) e
  in

  if null candidates
    then return Nothing
    else Just <$> Gen.element (mkDCert' <$> zip candidates target)


-- | Generate a list of delegation certificates.
--
-- At the moment the size of the generated list is severely constrained since
-- the longer the list the higher the probability that it will contain
-- conflicting delegation certificates (that will be rejected by the
-- transition-system-rules).
dcertsGen :: DSEnv -> Gen [DCert]
-- NOTE: at the moment we cannot use a linear range that depends on the
-- generator size: the problem is that the more delegation certificates in the
-- resulting list, the higher the probability that this list will be rejected
-- and the generator will have to retry.
--
dcertsGen env = Gen.frequency
  [(95, pure []), (10, Gen.list (constant 1 n) (dcertGen env))]
  where n = env ^. allowedDelegators . to length

dcertsGen' :: DSEnv -> DIState -> Gen [DCert]
dcertsGen' env st =
  -- TODO: alternatively we could define a HasTrace instance for SDELEG and use
  -- trace here.
  catMaybes <$> Gen.list (constant 1 n) (dcertGen' env st)
  where n = env ^. allowedDelegators . to length

instance HasTrace DELEG where

  initEnvGen
    = DSEnv
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
    <$> Gen.set (linear 1 7) vkgenesisGen
    <*> epochGen 0 10
    <*> slotGen 0 100
    <*> blockCountGen 0 100

  sigGen = dcertsGen'
