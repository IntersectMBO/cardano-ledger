{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | This is taken from Edward Kmett's `free` library
-- See: https://hackage.haskell.org/package/free-5.2
module Cardano.Ledger.Ap
  {-# DEPRECATED "Because it is no longer used in Ledger" #-} (
  Ap (..),
  hoistAp,
  runAp,
  runAp_,
) where

--------------------------------------------------------------------------------
-- \|
-- A faster free applicative.
-- Based on <https://www.eyrie.org/~zednenem/2013/05/27/freeapp Dave Menendez's work>.
--------------------------------------------------------------------------------
import Control.Applicative

-- | The free applicative is composed of a sequence of effects,
-- and a pure function to apply that sequence to.
-- The fast free applicative separates these from each other,
-- so that the sequence may be built up independently,
-- and so that 'fmap' can run in constant time by having immediate access to the pure function.
data ASeq f a where
  ANil :: ASeq f ()
  ACons :: f a -> ASeq f u -> ASeq f (a, u)

-- | Interprets the sequence of effects using the semantics for
--   `pure` and `<*>` given by the Applicative instance for 'f'.
reduceASeq :: Applicative f => ASeq f u -> f u
reduceASeq ANil = pure ()
reduceASeq (ACons x xs) = (,) <$> x <*> reduceASeq xs

-- | Given a natural transformation from @f@ to @g@ this gives a natural transformation from @ASeq f@ to @ASeq g@.
hoistASeq :: (forall x. f x -> g x) -> ASeq f a -> ASeq g a
hoistASeq _ ANil = ANil
hoistASeq u (ACons x xs) = ACons (u x) (u `hoistASeq` xs)

-- | It may not be obvious, but this essentially acts like ++,
-- traversing the first sequence and creating a new one by appending the second sequence.
-- The difference is that this also has to modify the return functions and that the return type depends on the input types.
--
-- See the source of 'hoistAp' as an example usage.
rebaseASeq ::
  ASeq f u ->
  (forall x. (x -> y) -> ASeq f x -> z) ->
  (v -> u -> y) ->
  ASeq f v ->
  z
rebaseASeq ANil k f = k (`f` ())
rebaseASeq (ACons x xs) k f =
  rebaseASeq
    xs
    (\g s -> k (\(a, u) -> g u a) (ACons x s))
    (\v u a -> f v (a, u))

-- | The faster free 'Applicative'.
newtype Ap f a = Ap
  { unAp ::
      forall u y z.
      (forall x. (x -> y) -> ASeq f x -> z) ->
      (u -> a -> y) ->
      ASeq f u ->
      z
  }

-- | Given a natural transformation from @f@ to @g@, this gives a canonical monoidal natural transformation from @'Ap' f@ to @g@.
--
-- prop> runAp t == retractApp . hoistApp t
runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
runAp u = retractAp . hoistAp u

-- | Perform a monoidal analysis over free applicative value.
--
-- Example:
--
-- @
-- count :: Ap f a -> Int
-- count = getSum . runAp_ (\\_ -> Sum 1)
-- @
runAp_ :: Monoid m => (forall a. f a -> m) -> Ap f b -> m
runAp_ f = getConst . runAp (Const . f)

instance Functor (Ap f) where
  fmap g x = Ap (\k f -> unAp x k (\s -> f s . g))

instance Applicative (Ap f) where
  pure a = Ap (\k f -> k (`f` a))
  x <*> y = Ap (\k f -> unAp y (unAp x k) (\s a g -> f s (g a)))

-- | Given a natural transformation from @f@ to @g@ this gives a monoidal natural transformation from @Ap f@ to @Ap g@.
hoistAp :: (forall x. f x -> g x) -> Ap f a -> Ap g a
hoistAp g x =
  Ap
    ( \k f s ->
        unAp
          x
          ( \f' s' ->
              rebaseASeq
                (hoistASeq g s')
                k
                (\v u -> f v (f' u))
                s
          )
          (const id)
          ANil
    )

-- | Interprets the free applicative functor over f using the semantics for
--   `pure` and `<*>` given by the Applicative instance for f.
--
--   prop> retractApp == runAp id
retractAp :: Applicative f => Ap f a -> f a
retractAp x = unAp x (\f s -> f <$> reduceASeq s) (\() -> id) ANil
