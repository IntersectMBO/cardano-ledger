{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Monad (
  Typed (..),
  LiftT (..),
  failT,
  explain,
  mergeExplain,
  HasConstraint (..),
  Id (..),
  errorTyped,
  monadTyped,
  requireAll,
) where

-- =================================================

newtype Typed x = Typed {runTyped :: Either [String] x}
  deriving (Functor, Applicative, Monad)

instance MonadFail Typed where
  fail err = failT [err]

failT :: [String] -> Typed a
failT ss = Typed (Left ss)

class LiftT x where
  liftT :: x -> Typed x
  dropT :: Typed x -> x

-- ==================================

explain :: String -> Typed a -> Typed a
explain s (Typed (Left ss)) = Typed (Left (s : ss))
explain _ (Typed (Right x)) = Typed (Right x)

mergeExplain :: (Monoid x, LiftT x) => String -> x -> x -> x
mergeExplain message x y = dropT (explain message (liftT (x <> y)))

requireAll :: [(Bool, [String])] -> Typed a -> Typed a
requireAll xs answer = if null bad then answer else failT (concat msgs)
  where
    bad = filter (not . fst) xs
    msgs = map snd bad

-- ======================================================
-- converting from a (Typed t) to something else

-- The projection from (newtype Typed t = Typed {runTyped :: Either [String] x})
-- runTyped :: Typed t => Either [String] x

-- | Pushes the (Left msgs) into a call to 'error'
errorTyped :: Typed t -> t
errorTyped t = case runTyped t of
  Right x -> x
  Left xs -> error (unlines ("\nSolver-time error" : xs))

-- | Pushes the (Left msgs) into a call to 'error', then injects into a Monad
monadTyped :: Monad m => Typed t -> m t
monadTyped t = pure $! errorTyped t

-- ================================================================
-- Computing runtime evidence of a constraint.

newtype Id x = Id x

-- | runTime evidence that the index 'i' of an indexed type '(s i)'
--   is constrained by the class 'c'. If one has an un-indexed type
--   't' one can always use (Id t) instead. Eg
--   With (Id x) <- hasOrd repT (Id t)
data HasConstraint c t where
  With :: c t => s t -> HasConstraint c (s t)
