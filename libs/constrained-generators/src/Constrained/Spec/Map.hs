{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Spec.Map (MapSpec (..), defaultMapSpec, dom_, rng_) where

import Constrained.Base
import Constrained.Core
import Constrained.GenT
import Constrained.Instances ()
import Constrained.List
import Constrained.Spec.Generics
import Constrained.Univ
import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter
import Test.QuickCheck (shrinkList)

------------------------------------------------------------------------
-- HasSpec
------------------------------------------------------------------------

instance Ord a => Sized (Map.Map a b) where
  sizeOf = toInteger . Map.size
  liftSizeSpec sz = typeSpec $ defaultMapSpec {mapSpecSize = typeSpec sz}
  liftMemberSpec xs = typeSpec $ defaultMapSpec {mapSpecSize = MemberSpec xs}
  sizeOfTypeSpec (MapSpec _ mustk mustv size _ _) =
    typeSpec (atLeastSize (sizeOf mustk))
      <> typeSpec (atLeastSize (sizeOf mustv))
      <> size

data MapSpec fn k v = MapSpec
  { mapSpecHint :: Maybe Integer
  , mapSpecMustKeys :: Set k
  , mapSpecMustValues :: [v]
  , mapSpecSize :: Specification fn Integer
  , mapSpecElem :: Specification fn (k, v)
  , mapSpecFold :: FoldSpec fn v
  }

defaultMapSpec :: Ord k => MapSpec fn k v
defaultMapSpec = MapSpec Nothing mempty mempty TrueSpec TrueSpec NoFold

deriving instance
  ( HasSpec fn (k, v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  ) =>
  Show (MapSpec fn k v)
instance Ord k => Forallable (Map k v) (k, v) where
  forAllSpec kvs = typeSpec $ defaultMapSpec {mapSpecElem = kvs}
  forAllToList = Map.toList

instance
  (Ord k, HasSpec fn (Prod k v), HasSpec fn k, HasSpec fn v, HasSpec fn [v]) =>
  HasSpec fn (Map k v)
  where
  type TypeSpec fn (Map k v) = MapSpec fn k v
  type Prerequisites fn (Map k v) = (HasSpec fn k, HasSpec fn v)

  emptySpec = defaultMapSpec

  combineSpec
    (MapSpec mHint mustKeys mustVals size kvs foldSpec)
    (MapSpec mHint' mustKeys' mustVals' size' kvs' foldSpec') = fromGE ErrorSpec $ do
      typeSpec
        . MapSpec
          (unionWithMaybe min mHint mHint')
          (mustKeys <> mustKeys')
          (nub $ mustVals <> mustVals')
          (size <> size')
          (kvs <> kvs')
        <$> combineFoldSpec foldSpec foldSpec'

  conformsTo m (MapSpec _ mustKeys mustVals size kvs foldSpec) =
    and
      [ mustKeys `Set.isSubsetOf` Map.keysSet m
      , all (`elem` Map.elems m) mustVals
      , sizeOf m `conformsToSpec` size
      , all (`conformsToSpec` kvs) (Map.toList m)
      , Map.elems m `conformsToFoldSpec` foldSpec
      ]

  genFromTypeSpec (MapSpec mHint mustKeys mustVals size (simplifySpec -> kvs) foldSpec) = do
    mustMap <- explain ["Make the mustMap"] $ forM (Set.toList mustKeys) $ \k -> do
      let vSpec = constrained $ \v -> satisfies (pair_ (lit k) v) kvs
      v <- explain [show $ "vSpec =" <+> pretty vSpec] $ genFromSpec vSpec
      pure (k, v)
    let haveVals = map snd mustMap
        mustVals' = filter (`notElem` haveVals) mustVals
        size' = constrained $ \sz ->
          -- TODO, we should make sure size' is greater than or equal to 0
          satisfies
            (sz + Lit (sizeOf mustMap))
            (maybe TrueSpec leqSpec mHint <> size <> cardinality (mapSpec fstFn $ mapSpec toGenericFn kvs))
        foldSpec' = case foldSpec of
          NoFold -> NoFold
          FoldSpec fn sumSpec -> FoldSpec fn $ propagateSpecFun (theAddFn @fn) (HOLE :? Value mustSum :> Nil) sumSpec
            where
              mustSum = adds @fn (map (sem fn) haveVals)
    let valsSpec =
          ListSpec
            Nothing
            mustVals'
            size'
            (simplifySpec $ constrained $ \v -> unsafeExists $ \k -> pair_ k v `satisfies` kvs)
            foldSpec'

    restVals <-
      explain
        [ "Make the restVals"
        , "  valsSpec = " ++ show valsSpec
        ]
        $ genFromTypeSpec
        $ valsSpec
    let go m [] = pure m
        go m (v : restVals') = do
          let keySpec = notMemberSpec (Map.keysSet m) <> constrained (\k -> pair_ k (lit v) `satisfies` kvs)
          k <-
            explain
              [ "Make a key"
              , show $ indent 4 $ "keySpec =" <+> pretty keySpec
              ]
              $ genFromSpec keySpec
          go (Map.insert k v m) restVals'
    go (Map.fromList mustMap) restVals

  shrinkWithTypeSpec (MapSpec _ _ _ _ kvs _) m = map Map.fromList $ shrinkList (shrinkWithSpec kvs) (Map.toList m)

  toPreds m (MapSpec mHint mustKeys mustVals size kvs foldSpec) =
    toPred
      [ assert $ lit mustKeys `subset_` dom_ m
      , forAll (Lit mustVals) $ \val ->
          val `elem_` rng_ m
      , sizeOf_ (rng_ m) `satisfies` size
      , forAll m $ \kv -> satisfies kv kvs
      , toPredsFoldSpec (rng_ m) foldSpec
      , maybe TruePred (`genHint` m) mHint
      ]

instance
  (Ord k, HasSpec fn (Prod k v), HasSpec fn k, HasSpec fn v, HasSpec fn [v]) =>
  HasGenHint fn (Map k v)
  where
  type Hint (Map k v) = Integer
  giveHint h = typeSpec $ defaultMapSpec {mapSpecHint = Just h}

------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------

instance BaseUniverse fn => Functions (MapFn fn) fn where
  propagateSpecFun _ _ TrueSpec = TrueSpec
  propagateSpecFun _ _ (ErrorSpec err) = ErrorSpec err
  propagateSpecFun fn ctx spec = case fn of
    _
      | SuspendedSpec v ps <- spec
      , ListCtx pre HOLE suf <- ctx ->
          constrained $ \v' ->
            let args = appendList (mapList (\(Value a) -> Lit a) pre) (v' :> mapList (\(Value a) -> Lit a) suf)
             in Let (App (injectFn fn) args) (v :-> ps)
    Dom ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: MapFn fn '[Map k v] (Set k))
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Map k v) ->
              case spec of
                MemberSpec [s] ->
                  typeSpec $
                    MapSpec Nothing s [] (exactSizeSpec $ sizeOf s) TrueSpec NoFold
                TypeSpec (SetSpec must elemspec size) [] ->
                  typeSpec $
                    MapSpec
                      Nothing
                      must
                      []
                      size
                      (constrained $ \kv -> satisfies (app (fstFn @fn) (app (toGenericFn @fn) kv)) elemspec)
                      NoFold
                _ -> ErrorSpec ["Dom on bad map spec", show spec]
    Rng ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: MapFn fn '[Map k v] [v])
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Map k v) ->
              case spec of
                MemberSpec [r] ->
                  typeSpec $ MapSpec Nothing Set.empty r (equalSpec $ sizeOf r) TrueSpec NoFold
                TypeSpec (ListSpec listHint must size elemspec foldspec) [] ->
                  typeSpec $
                    MapSpec
                      listHint
                      Set.empty
                      must
                      size
                      (constrained $ \kv -> satisfies (app (sndFn @fn) (app (toGenericFn @fn) kv)) elemspec)
                      foldspec
                _ -> ErrorSpec ["Rng on bad map spec", show spec]

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    -- TODO: consider checking against singleton member-specs in the other component
    -- interacting with cant
    Dom ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] (Set k))
          | MapSpec _ mustSet _ sz kvSpec _ <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ SetSpec mustSet (mapSpec (fstFn @fn) $ toSimpleRepSpec kvSpec) sz
    Rng ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] [v])
          | MapSpec _ _ mustList sz kvSpec foldSpec <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ ListSpec Nothing mustList sz (mapSpec (sndFn @fn) $ toSimpleRepSpec kvSpec) foldSpec

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

dom_ ::
  (HasSpec fn (Map k v), HasSpec fn k, Ord k) =>
  Term fn (Map k v) ->
  Term fn (Set k)
dom_ = app domFn

rng_ ::
  (HasSpec fn k, HasSpec fn v, Ord k) =>
  Term fn (Map k v) ->
  Term fn [v]
rng_ = app rngFn
