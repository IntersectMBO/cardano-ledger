{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- The ProvM is a Monad State Transformer, designed for computing provenance
(or metadata) about an underlying computation. By running a computation with type
(ProvM meta m a) one gets the underlying computation of type (m a). Switches
control whether the metadata is computed or not.
-}
module Control.Provenance
  ( ProvM (..),

    -- * Basic Provenance Monad Transformer
    PObject,

    -- * Dynamically typed Provenance objects
    Provenance,

    -- * Type constraint on data stored in PObject
    Prov,

    -- * ProvM instantiated (Map Text PObject)
    BlackBox,

    -- * Abstraction barrier to isolate the provenence type from the result type.

    -- * Operations in ProvM
    lift,
    putM,
    getM,
    modifyM,
    modifyWithBlackBox,
    runProv,
    runWithProv,
    runOtherProv,
    liftProv,
    dump,

    -- * Operations in Prov instantiation
    store,
    push,
    pull,
    update,
    updateWithBlackBox,
    pushOtherProv,
    runWithProvM,
    runProvM,

    -- * Operation on PObject
    find,
    observe,

    -- * For testing invariants
    preservesNothing,
    preservesJust,
  )
where

import Control.Monad.State.Strict (MonadState (..), MonadTrans (..), StateT (..))
import Data.Aeson (ToJSON (..))
import Data.Map.Strict (Map, empty, insert)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import Data.Text (Text, unpack)
import Data.Type.Equality (TestEquality (testEquality))
import NoThunks.Class (NoThunks (..), allNoThunks)
import Type.Reflection (TypeRep, Typeable, typeOf, typeRep, (:~:) (Refl))

-- ======================================================

-- |
-- The Provenance Monad is just the StateT transformer
-- wrapped in a newtype, where the state is (StrictMaybe t).
-- By running the state transformer with an initial state of SNothing
-- we indicate we are not collecting provenance. With an initial
-- state of (SJust t), we are collecting provenance. If we start off
-- as SNothing, then actions on the state are no-ops and are ignored.
-- We maintain several invariants. If we start with SNothing, we get
-- SNothing, and if we start with (SJust s) we end up with (SJust t)
--
-- invariant1 (ProvM m) = do (_,SNothing) <- runStateT m SNothing
-- invariant2 (ProvM m) = do (_,SJust t) <- runStateT m (SJust 1)
--
-- The second invariant is that regardless of whether we compute
-- provenance or not, the non-provenance part is the same.
-- Currently this is enforced by the BlackBox type and its API.
--
-- invariant3 (ProvM m) = do
--   (ans1,SNothing) <- runStateT m SNothing
--   (ans2,SJust p) <- runStateT m (SJust s)
--   pure(ans1 == ans2)
--
-- All operations that read the provenance (i.e the state) return
-- a (BlackBox t). BlackBoxes can only be used to modify provenance.
newtype ProvM t m a = ProvM (StateT (StrictMaybe t) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ProvM t) where
  lift x = ProvM (lift x)

-- | Run and compute the result as well as the provenance. Supply an initial value for the provenance.
runWithProvM :: Monad m => s -> ProvM s m a -> m (a, s)
runWithProvM s (ProvM m) = do
  (a, x) <- runStateT m (SJust s)
  case x of
    SNothing -> error ("(SJust state) returns SNothing in runWithProvM")
    SJust st -> pure (a, st)
{-# INLINE runWithProvM #-}

-- | Run the computation with SNothing. Expend no resources to compute provenance.
runProvM :: (Monad m) => ProvM s m b -> m b
runProvM (ProvM m) = do
  pair <- runStateT m SNothing
  case pair of
    (a, SNothing) -> pure a
    (_, SJust _) -> error ("SNothing returns (SJust p) in runProvM")
{-# INLINE runProvM #-}

-- ======================================================================
-- Helper functions and types

-- | BlackBox is an abstraction barrier. Reading the provenance is always returned
-- in a BlackBox. The only way to open the BlackBox is to use one of the BlackBlox
-- eliminator operations: modifyWithBlackBox or runOtherProv, that merge the contents of the
-- BlackBox into the current provenance. This ensures that there is no easy way for the
-- provenance computation to have an effect on the result of the underlying computation.
data BlackBox t = Box !t | NoBox
  deriving (Show, Functor)

-- Helper for modifyM. This runs in (STateT p m a), modifyM runs in (ProvM p m a)

modifyMState :: Monad m => (t -> t) -> StateT (StrictMaybe t) m ()
modifyMState delta = do
  mstore <- get
  case mstore of
    SNothing -> pure ()
    (SJust st) -> put (SJust (delta st))
{-# INLINE modifyMState #-}

-- | Overwrite the current provenance with something new.
putM :: Monad m => s -> ProvM s m ()
putM s = ProvM (modifyMState (const s))
{-# INLINE putM #-}

-- | Extract the current provenance. The result is wrapped in
-- a BlackBox. This ensures that provenance cannot be used in the
-- non-provenance part of the computation.
getM :: Monad m => ProvM s m (BlackBox s)
getM = ProvM (do m <- get; case m of { SNothing -> pure NoBox; SJust t -> pure (Box t) })
{-# INLINE getM #-}

-- | Modify the provenance if collecting provenance, otherwise do nothing.
modifyM :: Monad m => (t -> t) -> ProvM t m ()
modifyM delta = ProvM (modifyMState delta)
{-# INLINE modifyM #-}

modifyWithBlackBox :: Monad m => BlackBox p -> (p -> t -> t) -> ProvM t m ()
modifyWithBlackBox (Box x) delta = ProvM (modifyMState (delta x))
modifyWithBlackBox NoBox _ = ProvM (pure ())
{-# INLINE modifyWithBlackBox #-}

-- Are we currently collecting provenance? Not to be exported.
active :: Monad m => ProvM s m Bool
active = ProvM (do m <- get; pure (case m of SNothing -> False; SJust _ -> True))
{-# INLINE active #-}

-- | Run a provenance computation, with provenance s1, and lift the
-- result to a provenance computation with provenance s2. Use the active
-- state of the s2 computation to decide if we actually compute the
-- provenance s1. The s1 result is returned in a BlackBox. This ensures that
-- provenance cannot be used in the non-provenance part of the computation.
runOtherProv :: Monad m => s1 -> ProvM s1 m a -> ProvM s2 m (a, BlackBox s1)
runOtherProv initial other = do
  t <- active
  if t
    then ProvM (lift $ do (a, s) <- runWithProvM initial other; pure (a, Box s))
    else ProvM (lift $ do a <- runProvM other; pure (a, NoBox))
{-# INLINE runOtherProv #-}

-- | lift a provenenace computation from one provenance type (s1) to another (s2)
liftProv :: Monad m => ProvM s1 m a -> s1 -> (a -> s1 -> s2 -> s2) -> ProvM s2 m a
liftProv computation inits1 combine =
  do
    (a, blackbox) <- runOtherProv inits1 computation
    modifyWithBlackBox blackbox (combine a)
    pure a
{-# INLINE liftProv #-}

-- =======================================================================

-- | A special case of the ProvM Monad, where the state type is Store
-- a (Map Text PObject), where PObject is a dynamically typed value. This
-- allows the collection of a Set of provenance values, indexed by keys of
-- type Text. As in the ProvM monad, if run with 'runProv' operations on
-- the Store are ignored.
type Prov m a = ProvM Store m a

-- | Run the (Prov m a) computation and ignore the provenance part
runProv :: Monad m => Prov m t -> m t
runProv (ProvM m) = do (a, _) <- runStateT m SNothing; pure a

-- | Run the (Prov m a) computation, compute and return the provenance as well as the result.
runWithProv :: Monad m => Prov m t -> m (t, Store)
runWithProv = runWithProvM empty

-- | Run a computation in the underlying monad (m), return that value in the
-- (Prov m) monad. As a side effect store that value under the given key
-- equivalent to: store key m = do { a <- lift m; push key a; pure a}
store :: forall t m. (Provenance t, Monad m) => Text -> m t -> Prov m t
store key m = ProvM (do a <- lift m; modifyMState (insert key (pobject a)); pure a)
{-# INLINE store #-}

-- | Push a key value pair into the provenance store. Overwrites any
-- existing POjects stored under that key.
push :: (Provenance t, Monad m) => Text -> t -> Prov m ()
push key t = ProvM (modifyMState (insert key (pobject t)))
{-# INLINE push #-}

-- | Modify the value stored at the given key. If the key isn't found
-- or the PObject at that key has the wrong type, do nothing.
update :: forall t m. (Provenance t, Monad m) => Text -> (t -> t) -> Prov m ()
update key delta = ProvM action2
  where
    action2 = do
      m <- get
      case findM @t key m of
        SJust t -> modifyMState (insert key (pobject @t (delta t)))
        SNothing -> pure ()
{-# INLINE update #-}

-- | Modify the value stored at the given key using a value in a BlackBox.
-- if the key isn't found, or the PObject at that key has the wrong type, do nothing.
updateWithBlackBox :: forall t m s. (Provenance t, Monad m) => Text -> BlackBox s -> (s -> t -> t) -> Prov m ()
updateWithBlackBox key (Box s) delta = update key (delta s)
updateWithBlackBox _ NoBox _ = pure ()
{-# INLINE updateWithBlackBox #-}

-- | Reads the provenance value at a key. The value is returned
-- in a BlackBox. There are 3 reasons the BlackBox may be empty.
-- 1) The computation is not collecting provenance.
-- 2) The map does not contain the key
-- 3) The value at the key has the wrong type.
pull :: forall t m. (Monad m, Typeable t) => Text -> Prov m (BlackBox t)
pull key = ProvM (do m <- get; case findM key m of { SNothing -> pure NoBox; SJust t -> pure (Box t) })
{-# INLINE pull #-}

-- | Return a String representation of the current provenance store.
dump :: Monad m => Prov m String
dump =
  ProvM
    ( do
        mstore <- get
        case mstore of
          SJust m -> pure (observe m)
          SNothing -> pure "SNothing Store"
    )

-- | Push the provenance of a computation, under the given key. The
-- computation has provenance s1, and lift the result to a provenance
-- computation with provenance Store. Use the active state of the Store
-- computation to decide if we actually want to compute the
-- provenance s1, and push it, or simply ignore it.
pushOtherProv :: (Provenance s1, Monad m) => Text -> s1 -> ProvM s1 m a -> ProvM Store m a
pushOtherProv key initial other = do
  t <- active
  if t
    then
      ProvM
        ( do
            (a, v) <- lift (runWithProvM initial other)
            modifyMState (insert key (pobject v))
            pure a
        )
    else ProvM (lift $ runProvM other)

-- ================================================================
-- Helper functions to implement the operations on Prov
-- that manipulate PObject values.

-- | Since PObjects are dynamically typed, What operations are
-- required on a type to act as Provenance? We might want to
-- add or subtract some properties from this list.
type Provenance t = (Typeable t, ToJSON t, Show t, NoThunks t)

-- | Provenance Object
data PObject where
  PObject :: Provenance t => !(TypeRep t) -> !t -> PObject

instance NoThunks PObject where
  showTypeOf _ = "PObject"
  wNoThunks ctxt (PObject _ t) = allNoThunks [noThunks ctxt t]

instance Show PObject where
  show (PObject ty t) = "#" ++ show t ++ "::" ++ show ty

-- | extract a value with the given type from a PObject. If the PObject
--   does not have the right type, returns SNothing. If the type context
--   of the call does not fix the type, one can use a type application like
--   extract @[Int] pobject
extract :: forall t. (Typeable t) => PObject -> StrictMaybe t
extract (PObject ty n) = case testEquality ty (typeRep @t) of Just Refl -> SJust n; Nothing -> SNothing

-- | inject a type into the PObject type.
pobject :: Provenance t => t -> PObject
pobject !n = PObject (typeOf n) n

-- ==============================================================
-- Maps of PObjects is how we store Provenance in the ProvM Store monad.
-- Here we define some useful helper functions on these Maps

type Store = Map Text PObject

-- | Find a value for a given key from a Store (Map Text PObject). If
--  the Store does not have that key, or the PObject at that key
--  does not have the right type, returns SNothing. If the type context
--  of the call does not fix the type, one can use a type
--  application like:  find @Bool key map
find :: forall t k. (Ord k, Typeable t) => k -> Map k PObject -> StrictMaybe t
find key m = case Map.lookup key m of Just obj -> extract obj; Nothing -> SNothing

-- A useful helper function
findM :: forall t k. (Ord k, Typeable t) => k -> StrictMaybe (Map k PObject) -> StrictMaybe t
findM _ SNothing = SNothing
findM key (SJust m) = find key m

-- | Turn a Map of PObjects into a String, indicating its contents.
observe :: Store -> String
observe m = unlines (map f (Map.assocs m))
  where
    f (key, PObject _ t) = unpack key ++ " =\n   " ++ show t

-- =======================================================================
-- useful for testing invariants, The type StrictMaybe is local to this
-- module, and is not exported, so these predicates are defined here to
-- support testing these invariants.

preservesNothing :: Monad m => ProvM t m a -> m Bool
preservesNothing (ProvM m) = do
  (_, maybet) <- runStateT m SNothing
  case maybet of SNothing -> pure True; SJust _ -> pure False

preservesJust :: Monad m => t -> ProvM t m a -> m Bool
preservesJust t (ProvM m) = do
  (_, maybet) <- runStateT m (SJust t)
  case maybet of SNothing -> pure False; SJust _ -> pure True
