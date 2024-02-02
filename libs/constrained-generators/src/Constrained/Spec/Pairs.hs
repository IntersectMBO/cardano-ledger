{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Spec.Pairs where

import Constrained.Base
import Constrained.Core
import Constrained.List
import Constrained.Univ

-- HasSpec ----------------------------------------------------------------

cartesian :: (HasSpec fn a, HasSpec fn b) => Spec fn a -> Spec fn b -> Spec fn (Prod a b)
cartesian (MemberSpec []) _ = MemberSpec []
cartesian _ (MemberSpec []) = MemberSpec []
cartesian (ErrorSpec es) _ = ErrorSpec es
cartesian _ (ErrorSpec es) = ErrorSpec es
cartesian s s' = typeSpec $ Cartesian s s'

data PairSpec fn a b = Cartesian (Spec fn a) (Spec fn b)

instance (HasSpec fn a, HasSpec fn b) => HasSpec fn (Prod a b) where
  type TypeSpec fn (Prod a b) = PairSpec fn a b

  type Prerequisites fn (Prod a b) = (HasSpec fn a, HasSpec fn b)

  emptySpec = Cartesian mempty mempty

  combineSpec (Cartesian a b) (Cartesian a' b') = cartesian (a <> a') (b <> b')

  conformsTo (Prod a b) (Cartesian sa sb) = conformsToSpec a sa && conformsToSpec b sb

  genFromTypeSpec (Cartesian sa sb) = Prod <$> genFromSpec sa <*> genFromSpec sb

  toPreds x (Cartesian sf ss) =
    satisfies (app fstFn x) sf
      <> satisfies (app sndFn x) ss

deriving instance (HasSpec fn a, HasSpec fn b) => Show (PairSpec fn a b)

-- Functions for working on pairs -----------------------------------------

instance IsUniverse fn => Functions (PairFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    Fst ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: PairFn fn '[Prod a b] a)
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Prod a b) ->
              cartesian spec TrueSpec
    Snd ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: PairFn fn '[Prod a b] b)
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Prod a b) ->
              cartesian TrueSpec spec
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    Pair
      | HOLE :? Value b :> Nil <- ctx ->
          let sameSnd ps = [a | Prod a b' <- ps, b == b']
           in case spec of
                TypeSpec (Cartesian sa _) cant -> sa <> foldMap notEqualSpec (sameSnd cant)
                MemberSpec es -> MemberSpec (sameSnd es)
      | Value a :! NilCtx HOLE <- ctx ->
          let sameFst ps = [b | Prod a' b <- ps, a == a']
           in case spec of
                TypeSpec (Cartesian _ sb) cant -> sb <> foldMap notEqualSpec (sameFst cant)
                MemberSpec es -> MemberSpec (sameFst es)

  rewriteRules f@Fst =
    -- No TypeAbstractions in ghc-8.10
    case f of
      (_ :: PairFn fn '[Prod a b] a)
        | Evidence <- prerequisites @fn @(Prod a b) ->
            [ rewriteRule_ $ \x y ->
                app fstFn (app (pairFn @fn @a @b) x y) ~> x
            ]
  rewriteRules f@Snd =
    -- No TypeAbstractions in ghc-8.10
    case f of
      (_ :: PairFn fn '[Prod a b] b)
        | Evidence <- prerequisites @fn @(Prod a b) ->
            [ rewriteRule_ $ \x y ->
                app sndFn (app (pairFn @fn @a @b) x y) ~> y
            ]
  rewriteRules _ = []

  mapTypeSpec f ts = case f of
    Fst | Cartesian s _ <- ts -> s
    Snd | Cartesian _ s <- ts -> s
