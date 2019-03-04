{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Control.State.Transition.Examples.RegistryModel where

import Control.Arrow (second)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Either (isLeft, either)
import Control.Exception (ErrorCall, try)
import Control.Monad (foldM, void)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set, member, (\\))
import qualified Data.Set as Set
import Hedgehog
  ( Callback
  , assert
  , Callback(Ensure, Update, Require)
  , Command(Command)
  , Concrete
  , Gen
  , HTraversable
  , MonadTest
  , Property
  , Test
  , Var
  , (===)
  , checkParallel
  , discover
  , evalEither
  , evalIO
  , executeSequential
  , forAll
  , htraverse
  , property
  , withTests
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Control.State.Transition.Examples.Registry

type AThreadId = Int

data SPAWN

instance STS SPAWN where
  type Environment SPAWN = ()

  type State SPAWN = Set AThreadId

  type Signal SPAWN = AThreadId

  data PredicateFailure SPAWN = NoFailure deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), tids, tid) <- judgmentContext
        return $! Set.insert tid tids
    ]

data REGISTER

instance STS REGISTER where
  type Environment REGISTER = Set AThreadId

  type State REGISTER = Map String AThreadId

  type Signal REGISTER = (String, AThreadId)

  data PredicateFailure REGISTER
    = NameAlreadyRegistered String
    | ThreadIdAlreadyRegistered AThreadId
    | ThreadDoesntExist AThreadId
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (tids, regs, (n, t)) <- judgmentContext
        -- Try commenting out some of these checks and see what happens.
        t `member` tids ?! ThreadDoesntExist t
        n `notElem` Map.keys regs ?! NameAlreadyRegistered n
        t `notElem` Map.elems regs ?! ThreadIdAlreadyRegistered t
        return $! Map.insert n t regs
    ]


data UNREGISTER

instance STS UNREGISTER where
  type Environment UNREGISTER = ()

  type State UNREGISTER = Map String AThreadId

  type Signal UNREGISTER = String

  data PredicateFailure UNREGISTER
    = NameNotRegistered String
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), regs, n) <- judgmentContext
        n `elem` Map.keys regs ?! NameNotRegistered n
        return $! Map.delete n regs
    ]

-- | WhereIs becomes a function!
whereIs :: String -> Map String AThreadId -> Maybe AThreadId
whereIs = Map.lookup

data KILL

data REGISTRY

data RegCmd
  = Spawn AThreadId
  | Register String AThreadId
  | Unregister String
  | WhereIs String (Maybe AThreadId)
--  | KillThread AThreadId
  deriving (Eq, Show)

instance STS REGISTRY where

  type Environment REGISTRY = ()

  type State REGISTRY
    = ( Set AThreadId
      , Map String AThreadId
      )

  type Signal REGISTRY = RegCmd

  data PredicateFailure REGISTRY
    = SpawnFailure (PredicateFailure SPAWN)
    | RegFailure (PredicateFailure REGISTER)
    | UnregFailure (PredicateFailure UNREGISTER)
    | WhereIsFailure String
    | WrongWhereIsResult (Maybe AThreadId) (Maybe AThreadId)
    deriving (Eq, Show)

  initialRules =
    [ return ([], []) ]

  transitionRules =
    [ do
        TRC ((), (tids, regs), cmd) <- judgmentContext
        case cmd of
          Spawn tid -> do
            tids' <- trans @SPAWN $ TRC ((), tids, tid)
            return $! (tids', regs)
          Register n t -> do
            regs' <- trans @REGISTER $ TRC (tids, regs, (n, t))
            return $! (tids, regs')
          WhereIs n t -> do
            whereIs n regs == t ?! WrongWhereIsResult t (whereIs n regs)
            return $! (tids, regs)
          Unregister n -> do
            regs' <- trans @UNREGISTER $ TRC ((), regs, n)
            return $! (tids, regs')
    ]

instance Embed SPAWN REGISTRY where
  wrapFailed = SpawnFailure

instance Embed REGISTER REGISTRY where
  wrapFailed = RegFailure

instance Embed UNREGISTER REGISTRY where
  wrapFailed = UnregFailure

allNames :: Set String
allNames = ["a", "b", "c", "d", "e", "f", "g", "h"]

instance HasTrace REGISTRY where
  initEnvGen = return ()

  sigGen () st@(tids, regs) = do
    Gen.choice [ do
                   n <- Gen.integral (Range.constant 0 10000)
                   if n `member` tids
                   then (sigGen @REGISTRY () (tids, regs))
                   else return $! Spawn n
               , if Map.null regs
                 then (sigGen @REGISTRY () (tids, regs))
                 else
                   fmap (uncurry WhereIs)
                     $ fmap (second Just)
                     $ Gen.element (Map.toList regs)
               , do
                   let anames = allNames \\ Set.fromList (Map.keys regs)
                   if Set.null anames
                   then (sigGen @REGISTRY () (tids, regs))
                   else do
                     n <- Gen.element (Set.toList anames)
                     let atids = tids \\ Set.fromList (Map.elems regs)
                     if Set.null atids
                     then (sigGen @REGISTRY () (tids, regs))
                     else do
                       t <- Gen.element (Set.toList atids)
                       return $! Register n t
               , do
                   if (Set.null allNames || Set.null tids)
                   then sigGen @REGISTRY () (tids, regs)
                   else do
                     n <- Gen.element (Set.toList allNames)
                     t <- Gen.element (Set.toList tids)
                     return $! Register n t
               , if null (Map.keys regs)
                 then (sigGen @REGISTRY () (tids, regs))
                 else do
                   n <- Gen.element (Map.keys regs)
                   return $! Unregister n
               ]


prop_generatedTracesAreValid :: Property
prop_generatedTracesAreValid =
  withTests 1000 $ property $ do
    tr <- forAll trace
    _ <- evalIO cleanUp
    executeTrace tr

cleanUp :: IO [Either ErrorCall ()]
cleanUp =
  sequence [ try (unregister name) :: IO (Either ErrorCall ())
           | name <- Set.toList allNames ]

executeTrace
  :: forall m . (MonadTest m, MonadIO m)
  => Trace REGISTRY
  -> m ()
executeTrace tr = void $ res
  where
    res = foldM perform Map.empty (traceSignals OldestFirst tr)

    perform
      :: Map AThreadId ThreadId
      -> RegCmd
      -> m (Map AThreadId ThreadId)
    perform st (Spawn atid) = do
      tid <- evalIO $ forkIO (threadDelay 10000000)
      return $! Map.insert atid tid st
    perform st (Register name atid) = do
      let tid = fromJust $ Map.lookup atid st
      evalIO $ register name tid
      return $! st
    perform st (Unregister name) = do
      evalIO $ unregister name
      return st
    perform st (WhereIs name (Just atid)) = do
      mtid <- evalIO $ whereis name
      let expectedTid = Map.lookup atid st
      mtid === expectedTid
      return st

--------------------------------------------------------------------------------
-- Let's try to hookup the system under test using state machines!
--------------------------------------------------------------------------------

prop_RegistryImplementsModel :: Property
prop_RegistryImplementsModel = withTests 1000 $ property $ do
  tidMapRef <- evalIO $ newIORef []
  env <- forAll $ initEnvGen @REGISTRY
  actions <- forAll $
    Gen.sequential
      (Range.linear 1 100)
      initialState
      [registryCmd tidMapRef env]
  _ <- evalIO cleanUp
  executeSequential initialState actions

newtype AbstractState (v :: * -> *) = AbstractState (State REGISTRY)
  deriving (Show)

initialState :: AbstractState v
initialState = AbstractState ([], [])

data RegCmdW (v :: * -> *) = RegCmdW RegCmd deriving (Eq, Show)

instance HTraversable RegCmdW where
  htraverse _ (RegCmdW cmd) = pure (RegCmdW cmd)

data RegOut
  = Spawned ThreadId
  | Registered
  | Unregistered
  | Found ThreadId
  | Failed ErrorCall
  | NotFound String
  deriving (Eq, Show)

isFailure :: RegOut -> Bool
isFailure (Failed _) = True
isFailure _ = False

wasFound :: RegOut -> Bool
wasFound (Found _) = True
wasFound _ = False

registryCmd
  :: forall m
   . (MonadTest m, MonadIO m)
  => IORef (Map AThreadId ThreadId)
  -> Environment REGISTRY
  -> Command Gen m AbstractState
registryCmd tidMapRef env =
  Command
    gen
    execute
    callbacks
  where
    -- If we're using our STS framework, we should always return a generator. I
    -- don't know whether there are cases in which we can decide that we won't
    -- be able to generate an action based on the initial state.
    gen :: AbstractState v -> Maybe (Gen (RegCmdW v))
    gen (AbstractState st) = Just $ fmap RegCmdW $ sigGen @REGISTRY env st

    execute :: RegCmdW v -> m RegOut
    execute (RegCmdW (Spawn atid)) = do
      etid <- evalIO $ try $ forkIO (threadDelay 10000000)
      case etid of
        Left e -> pure $! Failed e
        Right tid -> do
          evalIO $ modifyIORef tidMapRef $ Map.insert atid tid
          return $! Spawned tid
    execute (RegCmdW (Register name atid)) = do
      -- We need the abstract-thread-id to thread-id mapping! So I guess we
      -- have to resort to IORefs :/
      tidMap <- evalIO $ readIORef tidMapRef
      let tid = fromJust $ Map.lookup atid tidMap
      res <- evalIO $ try $ register name tid
      case res of
        Left e -> pure $! Failed e
        Right _ -> pure $! Registered
    execute (RegCmdW (Unregister name)) = do
      res <- evalIO $ try $ unregister name
      case res of
        Left e -> pure $! Failed e
        Right _ -> pure $! Unregistered
    execute (RegCmdW (WhereIs name atid)) = do
      mtid <- evalIO $ whereis name
      case mtid of
        Nothing -> pure $! NotFound name
        Just tid -> pure $! Found tid

    callbacks :: [Callback RegCmdW RegOut AbstractState]
    callbacks = [Require pre, Update update, Ensure post]

    pre
      :: AbstractState v
      -> RegCmdW v
      -> Bool
    pre (AbstractState (_, regs)) (RegCmdW (WhereIs n (Just t))) =
      -- Here we need to guard against the shrink steps that will cause
      -- @WhereIs n (Just x)@ to be the signal of an empty state.
      --
      -- So here we're saying, it is ok to apply this step, if the state still
      -- contains the information that allowed the generation of this action
      -- (a.k.a signal).
      t `elem` Map.elems regs
    pre (AbstractState (tids, _)) (RegCmdW (Register n t)) =
      -- We start seeing a pattern here: we need to use the precondition to
      -- filter the states that might have not given rise to the given signal.
      t `member` tids
    pre _ _ = True

    update
      :: AbstractState v
      -> RegCmdW v
      -> Var RegOut v
      -> AbstractState v
    update (AbstractState st) (RegCmdW cmd) _ =
      case applySTS @REGISTRY (TRC(env, st, cmd)) of
        Left _ -> AbstractState st
        Right st' -> AbstractState st'

    post
      :: AbstractState Concrete
      -> AbstractState Concrete
      -> RegCmdW Concrete
      -> RegOut
      -> Test ()
    post (AbstractState st) _ (RegCmdW cmd) res =
        isLeft ares === isFailure res
      where
        ares = applySTS @REGISTRY (TRC(env, st, cmd))

tests :: IO Bool
tests =
  checkParallel $$(discover)
