{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.State.Transition.Examples.RegistryModel where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (ErrorCall, try)
import Control.Monad (foldM, void)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set, member, (\\))
import qualified Data.Set as Set
import Hedgehog
  ( MonadTest
  , Property
  , (===)
  , discover
  , evalEither
  , checkParallel
  , evalIO
  , forAll
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
  | WhereIs String AThreadId
  | Register String AThreadId
  | Unregister String
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
    | DoRegFailure (PredicateFailure REGISTER)
    | DoUnregFailure (PredicateFailure UNREGISTER)
    | WhereIsFailure String
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
            case whereIs n regs of
              Nothing -> do
                failBecause $! WhereIsFailure n
                return $! (tids, regs)
              Just t' -> do
                t == t' ?! undefined
                return $! (tids, regs)
          Unregister n -> do
            regs' <- trans @UNREGISTER $ TRC ((), regs, n)
            return $! (tids, regs')

    ]

instance Embed SPAWN REGISTRY where
  wrapFailed = SpawnFailure

instance Embed REGISTER REGISTRY where
  wrapFailed = DoRegFailure

instance Embed UNREGISTER REGISTRY where
  wrapFailed = DoUnregFailure

allNames :: Set String
allNames = ["a", "b", "c", "d", "e"]

instance HasTrace REGISTRY where
  initEnvGen = return ()

  sigGen () (tids, regs) =
    Gen.choice [ do
                   n <- Gen.integral (Range.constant 0 10000)
                   if n `member` tids
                   then (sigGen @REGISTRY () (tids, regs))
                   else return $! Spawn n
               , if Map.null regs
                 then (sigGen @REGISTRY () (tids, regs))
                 else uncurry WhereIs <$> Gen.element (Map.toList regs)
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
  withTests 300 $ property $ do
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
    perform st (WhereIs name atid) = do
      tid <- fmap fromJust $ evalIO $ whereis name
      let expectedTid = fromJust $ Map.lookup atid st
      tid === expectedTid
      return st

tests :: IO Bool
tests =
  checkParallel $$(discover)
