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
{-# OPTIONS_GHC -Wno-orphans #-}

module Constrained.Spec.Maps (MapSpec (..), dom_, rng_) where

import Constrained.Base
import Constrained.Core
import Constrained.GenT
import Constrained.Instances ()
import Constrained.List
import Constrained.Univ
import Control.Monad
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

------------------------------------------------------------------------
-- HasSpec
------------------------------------------------------------------------

mapSize :: Map k v -> Size
mapSize m = MkSize (Map.size m)

instance Ord a => Sized (Map.Map a b) where
  sizeOf = mapSize
  liftSizeSpec sizespec@(SizeSpec _) = TypeSpec (MapSpec mempty mempty (typeSpec sizespec) TrueSpec NoFold) []
  liftMemberSpec xs = typeSpec (MapSpec mempty mempty (MemberSpec xs) TrueSpec NoFold)
  sizeOfTypeSpec (MapSpec mustk mustv size _ _) =
    typeSpec (atLeastInt (Set.size mustk))
      <> typeSpec (atLeastInt (length mustv))
      <> size

data MapSpec fn k v = MapSpec
  { mapSpecMustKeys :: Set k
  , mapSpecMustValues :: [v]
  , mapSpecSize :: Spec fn Size
  , mapSpecElem :: Spec fn (k, v)
  , mapSpecFold :: FoldSpec fn v
  }

deriving instance
  ( HasSpec fn (k, v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  ) =>
  Show (MapSpec fn k v)
instance Ord k => Forallable (Map k v) (k, v) where
  forAllSpec kvs = typeSpec $ MapSpec mempty [] TrueSpec kvs NoFold
  forAllToList = Map.toList

instance
  (Ord k, HasSpec fn (Prod k v), HasSpec fn k, HasSpec fn v, HasSpec fn [v]) =>
  HasSpec fn (Map k v)
  where
  type TypeSpec fn (Map k v) = MapSpec fn k v
  type Prerequisites fn (Map k v) = (HasSpec fn k, HasSpec fn v)

  emptySpec = MapSpec mempty mempty mempty mempty NoFold

  combineSpec
    (MapSpec mustKeys mustVals size kvs foldSpec)
    (MapSpec mustKeys' mustVals' size' kvs' foldSpec') = fromGE ErrorSpec $ do
      typeSpec
        . MapSpec
          (mustKeys <> mustKeys')
          (nub $ mustVals <> mustVals')
          (size <> size')
          (kvs <> kvs')
        <$> combineFoldSpec foldSpec foldSpec'

  conformsTo m (MapSpec mustKeys mustVals size kvs foldSpec) =
    and
      [ mustKeys `Set.isSubsetOf` Map.keysSet m
      , all (`elem` Map.elems m) mustVals
      , mapSize m `conformsToSpec` size
      , all (`conformsToSpec` kvs) (Map.toList m)
      , Map.elems m `conformsToFoldSpec` foldSpec
      ]

  genFromTypeSpec (MapSpec mustKeys mustVals size kvs foldSpec) = do
    mustMap <- explain ["Make the mustMap"] $ forM (Set.toList mustKeys) $ \k -> do
      let vSpec = constrained $ \v -> satisfies (app (fromGenericFn @fn) $ app (pairFn @fn) (Lit k) v) kvs
      v <- explain [show $ "vSpec =" <+> pretty vSpec] $ genFromSpec vSpec
      pure (k, v)
    let haveVals = map snd mustMap
        mustVals' = filter (`notElem` haveVals) mustVals
        size' = constrained $ \sz ->
          satisfies
            (sz + Lit (listSize mustMap))
            (size <> specIntToSize (cardinality (mapSpec fstFn $ mapSpec toGenericFn kvs)))
        foldSpec' = case foldSpec of
          NoFold -> NoFold
          FoldSpec fn sumSpec -> FoldSpec fn $ propagateSpecFun (theAddFn @fn) (HOLE :? Value mustSum :> Nil) sumSpec
            where
              mustSum = adds @fn (map (sem fn) haveVals)
    let valsSpec =
          ListSpec
            mustVals'
            size'
            ( constrained $ \v ->
                unsafeExists $ \p ->
                  [ satisfies p kvs
                  , assert $ app equalFn v $ app sndFn (app toGenericFn p)
                  ]
            )
            foldSpec'

    restVals <-
      explain ["Make the restVals"] $
        explain [show $ "size' = " <> pretty size'] $
          genFromTypeSpec $
            valsSpec
    let go m [] = pure m
        go m (v : restVals') = do
          let keySpec = constrained $ \k ->
                toPred
                  [ assert $ app (notFn @fn) (app (memberFn @fn) k (Lit $ Map.keysSet m))
                  , satisfies (app (fromGenericFn @fn) $ app (pairFn @fn) k (Lit v)) kvs
                  ]
          k <-
            explain
              [ "Make a key"
              , show $ indent 4 $ "keySpec =" <+> pretty keySpec
              ]
              $ genFromSpec keySpec
          go (Map.insert k v m) restVals'
    go (Map.fromList mustMap) restVals

  toPreds m (MapSpec mustKeys mustVals size kvs foldSpec) =
    toPred
      [ assert $ app (subsetFn @fn) (app (domFn @fn) m) (Lit mustKeys)
      , forAll (Lit mustVals) $ \val ->
          app (elemFn @fn) val (app (rngFn @fn) m)
      , -- TODO: make nice
        satisfies (app (injectFn $ SizeOf @fn) $ app (rngFn @fn) m) size
      , forAll m $ \kv -> satisfies kv kvs
      , toPredsFoldSpec (app (rngFn @fn) m) foldSpec
      ]

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
                    MapSpec s [] (exactSizeSpec $ setSize s) TrueSpec NoFold
                TypeSpec (SetSpec must elemspec size) [] ->
                  typeSpec $
                    MapSpec
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
                  typeSpec $ MapSpec Set.empty r (equalSpec $ listSize r) TrueSpec NoFold
                TypeSpec (ListSpec must size elemspec foldspec) [] ->
                  typeSpec $ MapSpec Set.empty must size (constrained $ \kv -> satisfies (app (sndFn @fn) (app (toGenericFn @fn) kv)) elemspec) foldspec
                _ -> ErrorSpec ["Rng on bad map spec", show spec]

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    -- TODO: consider checking against singleton member-specs in the other component
    -- interacting with cant
    Dom ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] (Set k))
          | MapSpec mustSet _ sz kvSpec _ <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ SetSpec mustSet (mapSpec (fstFn @fn) $ toSimpleRepSpec kvSpec) sz
    Rng ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] [v])
          | MapSpec _ mustList sz kvSpec foldSpec <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ ListSpec mustList sz (mapSpec (sndFn @fn) $ toSimpleRepSpec kvSpec) foldSpec

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
