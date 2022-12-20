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
  Dyn (..),
  runDyn,
  isDynSet,
  HasCond (..),
  hasOrd,
  Id (..),
  testHasCond,
  ioTyped,
) where

import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Generic.Proof (Proof (..))

-- =================================================

newtype Typed x = Typed {runTyped :: Either [String] x}
  deriving (Functor, Applicative, Monad)

failT :: [String] -> Typed a
failT ss = Typed (Left ss)

class LiftT x where
  liftT :: x -> Typed x
  dropT :: Typed x -> x

sameRep :: Rep era i -> Rep era j -> Typed (i :~: j)
sameRep r1 r2 = case testEql r1 r2 of
  Just x -> pure x
  Nothing -> failT [show r1 ++ " =/= " ++ show r2 ++ " in sameRep."]

explain :: String -> Typed a -> Typed a
explain s (Typed (Left ss)) = Typed (Left (s : ss))
explain _ (Typed (Right x)) = Typed (Right x)

ioTyped :: Typed t -> IO t
ioTyped t = case runTyped t of
  Right x -> pure x
  Left xs -> error (unlines xs)

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
  pure (With set) -- Note pattern matchin against (SetR r2) provides the Ord instance
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
    help PPUPStateR _ = failT ["PPUPState does not have Ord instance"]
    help PtrR v = pure $ With v
    help SnapShotsR _ = failT ["SnapShot does not have Ord instance"]
    help IPoolStakeR _ = failT ["IndividualPoolStake does not have Ord instance"]
    help (PParamsR _) _ = failT ["PParams does not have Ord instance"]
    help (PParamsUpdateR _) _ = failT ["PParamsUpdate does not have Ord instance"]

-- | Used to test hasOrd
testHasCond :: Rep era [t] -> t -> IO ()
testHasCond (ListR rep) t = case hasOrd rep [t] of
  Typed (Right (With x)) -> print (synopsis (ListR rep) x)
  Typed (Left xs) -> putStrLn $ unlines xs
testHasCond _ _ = error "testHasCond only works on lists"
