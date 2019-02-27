{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.State.Transition.Examples.Register where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, notMember)
import qualified Data.Set as Set

import Control.State.Transition

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

data DOREG

instance STS DOREG where
  type Environment DOREG = Set AThreadId

  type State DOREG = Map String AThreadId

  type Signal DOREG = (String, AThreadId)

  data PredicateFailure DOREG
    = NameAlreadyRegistered String
    | ThreadIdAlreadyRegistered AThreadId
    | ThreadDoesntExist AThreadId
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (tids, regs, (n, t)) <- judgmentContext
        t `notMember` tids ?! ThreadDoesntExist t
        n `notElem` Map.keys regs ?! NameAlreadyRegistered n
        t `notElem` Map.elems regs ?! ThreadIdAlreadyRegistered t
        return $! Map.insert n t regs
    ]


data DOUNREG

instance STS DOUNREG where
  type Environment DOUNREG = ()

  type State DOUNREG = Map String AThreadId

  type Signal DOUNREG = String

  data PredicateFailure DOUNREG
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

data REGISTER

data RegCmd
  = Spawn AThreadId
  | WhereIs String AThreadId
  | Register String AThreadId
  | Unregister String
--  | KillThread AThreadId
  deriving (Eq, Show)

instance STS REGISTER where

  type Environment REGISTER = ()

  type State REGISTER
    = ( Set AThreadId
      , Map String AThreadId
      )

  type Signal REGISTER = RegCmd

  data PredicateFailure REGISTER
    = SpawnFailure (PredicateFailure SPAWN)
    | DoRegFailure (PredicateFailure DOREG)
    | DoUnregFailure (PredicateFailure DOUNREG)
    | WhereIsFailure String
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((), (tids, regs), cmd) <- judgmentContext
        case cmd of
          Spawn tid -> do
            tids' <- trans @SPAWN $ TRC ((), tids, tid)
            return $! (tids', regs)
          Register n t -> do
            regs' <- trans @DOREG $ TRC (tids, regs, (n, t))
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
            regs' <- trans @DOUNREG $ TRC ((), regs, n)
            return $! (tids, regs')

    ]
instance Embed SPAWN REGISTER where
  wrapFailed = SpawnFailure

instance Embed DOREG REGISTER where
  wrapFailed = DoRegFailure

instance Embed DOUNREG REGISTER where
  wrapFailed = DoUnregFailure
