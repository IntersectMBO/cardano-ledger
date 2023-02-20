{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Monad (
  Typed (..),
  LiftT (..),
  failT,
  sameRep,
  explain,
  mergeExplain,
  Dyn (..),
  runDyn,
  isDynSet,
  HasCond (..),
  hasOrd,
  Id (..),
  testHasCond,
  errorTyped,
  monadTyped,
) where

import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.Classes (SumCond (..))
import Test.Cardano.Ledger.Constrained.TypeRep (
  Proof (..),
  Rep (..),
  Size (..),
  synopsis,
  testEql,
  (:~:) (Refl),
 )

-- =================================================

newtype Typed x = Typed {runTyped :: Either [String] x}
  deriving (Functor, Applicative, Monad)

failT :: [String] -> Typed a
failT ss = Typed (Left ss)

class LiftT x where
  liftT :: x -> Typed x
  dropT :: Typed x -> x

instance LiftT SumCond where
  liftT (CondNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = CondNever s
  dropT (Typed (Right x)) = x

instance LiftT Size where
  liftT (SzNever xs) = failT xs
  liftT x = pure x
  dropT (Typed (Left s)) = SzNever s
  dropT (Typed (Right x)) = x

-- ==================================

sameRep :: Rep era i -> Rep era j -> Typed (i :~: j)
sameRep r1 r2 = case testEql r1 r2 of
  Just x -> pure x
  Nothing -> failT ["Type error in sameRep:\n  " ++ show r1 ++ " =/=\n  " ++ show r2]

explain :: String -> Typed a -> Typed a
explain s (Typed (Left ss)) = Typed (Left (s : ss))
explain _ (Typed (Right x)) = Typed (Right x)

mergeExplain :: (Monoid x, LiftT x) => String -> x -> x -> x
mergeExplain message x y = dropT (explain message (liftT (x <> y)))

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
monadTyped t = pure (errorTyped t)

-- ========================================================

-- | A Pair of a (Rep era t), and a 't' . Useful to encapsulate 't' in a way
--   one can interogate its type at runtime.
data Dyn era where Dyn :: Rep era t -> t -> Dyn era

instance Show (Dyn era) where
  show (Dyn rep t) = "(Dyn " ++ synopsis rep t ++ ")"

-- | extract the type 't' from a Dyn (if possible).
runDyn :: Rep era t -> Dyn era -> Typed t
runDyn rep1 (Dyn rep2 t) = do
  Refl <- sameRep rep1 rep2
  pure t

-- | Evidence that a (Dyn era) is a (Set rng), where rng has an Ord instance.
--   This is hard to write using (runDyn (SetR x) dyn) because SetR requires an
--   Ord instance, and one often doesn't know any properties of 'x'
isDynSet :: Dyn era -> Rep era rng -> Typed (HasCond Ord (Set rng))
isDynSet (Dyn (SetR r2) set) rng = do
  Refl <- sameRep rng r2
  pure (With set) -- Note pattern matching against (SetR r2) provides the Ord instance
isDynSet dyn rng = failT [show dyn ++ "is not a dynamic (Set " ++ show rng ++ ")"]

-- ================================================================
-- Computing runtime evidence of a constraint.

data Id x = Id x

-- | runTime evidence that the index 'i' of an indexed type '(s i)'
--   is constrained by the class 'c'. If one has an un-indexed type
--   't' one can always use (Id t) instead. Eg
--   With (Id x) <- hasOrd repT (Id t)
data HasCond c t where
  With :: c t => (s t) -> HasCond c (s t)

hasOrd :: Rep era t -> s t -> Typed (HasCond Ord (s t))
hasOrd rep xx = explain ("'hasOrd " ++ show rep ++ "' fails") (help rep xx)
  where
    help :: Rep era t -> s t -> Typed (HasCond Ord (s t))
    help CoinR t = pure $ With t
    help r@(_ :-> _) _ = failT [show r ++ " does not have an Ord instance."]
    help (MapR _ b) m = do
      With _ <- help b undefined
      pure (With m)
    help (SetR _) s = pure $ With s
    help (ListR a) l = do
      With _ <- help a undefined
      pure $ With l
    help CredR c = pure $ With c
    help PoolHashR p = pure $ With p
    help GenHashR p = pure $ With p
    help GenDelegHashR p = pure $ With p
    help WitHashR p = pure $ With p
    help PoolParamsR pp = pure $ With pp
    help EpochR e = pure $ With e
    help RationalR r = pure $ With r
    help Word64R w = pure $ With w
    help IntR i = pure $ With i
    help NaturalR i = pure $ With i
    help FloatR i = pure $ With i
    help TxInR t = pure $ With t
    help StringR s = pure $ With s
    help (ValueR (Shelley _)) v = pure $ With v
    help (ValueR (Allegra _)) v = pure $ With v
    help UnitR v = pure $ With v
    help (ValueR _) _ = failT ["Value does not have Ord instance in post Allegra eras"]
    help (TxOutR _) _ = failT ["TxOut does not have Ord instance"]
    help (UTxOR _) _ = failT ["UTxO does not have Ord instance"]
    help DeltaCoinR v = pure $ With v
    help GenDelegPairR v = pure $ With v
    help FutureGenDelegR v = pure $ With v
    help (PPUPStateR _) _ = failT ["PPUPState does not have Ord instance"]
    help PtrR v = pure $ With v
    help SnapShotsR _ = failT ["SnapShot does not have Ord instance"]
    help IPoolStakeR _ = failT ["IndividualPoolStake does not have Ord instance"]
    help (PParamsR _) _ = failT ["PParams does not have Ord instance"]
    help (PParamsUpdateR _) _ = failT ["PParamsUpdate does not have Ord instance"]
    help RewardR v = pure $ With v
    help (MaybeR a) l = do
      With _ <- help a undefined
      pure $ With l
    help NewEpochStateR _ = failT ["NewEpochStateR does not have Ord instance"]
    help (ProtVerR _) v = pure $ With v
    help SlotNoR v = pure $ With v
    help SizeR v = pure $ With v

-- | Used to test hasOrd
testHasCond :: Rep era [t] -> t -> IO ()
testHasCond (ListR rep) t = case hasOrd rep [t] of
  Typed (Right (With x)) -> print (synopsis (ListR rep) x)
  Typed (Left xs) -> putStrLn $ unlines xs
testHasCond _ _ = error "testHasCond only works on lists"
