{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- The pattern completeness checker is much weaker before ghc-9.0. Rather than introducing redundant
-- cases and turning off the overlap check in newer ghc versions we disable the check for old
-- versions.
#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Constrained.Spec.Map (MapSpec (..), defaultMapSpec, dom_, rng_, lookup_, rngSet_) where

import Constrained.Base hiding (count)
import Constrained.Core
import Constrained.GenT
import Constrained.Instances ()
import Constrained.List
import Constrained.Spec.Generics
import Constrained.Univ
import Control.Monad (forM)
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Data.Word
import Debug.Trace
import GHC.Generics
import Prettyprinter
import Test.QuickCheck (Arbitrary (..), frequency, genericShrink, shrinkList)
import Test.QuickCheck hiding (Args (..), forAll)

-- --------------------------------------------------------

------------------------------------------------------------------------
-- HasSpec
------------------------------------------------------------------------

instance Ord a => Sized (Map.Map a b) where
  sizeOf = toInteger . Map.size
  liftSizeSpec sz [0] = typeSpec $ defaultMapSpec {mapSpecSize = TypeSpec (sz <> NumSpecInterval (Just 1) Nothing) []}
  liftSizeSpec sz cant = typeSpec $ defaultMapSpec {mapSpecSize = TypeSpec sz cant}
  liftMemberSpec xs = typeSpec $ defaultMapSpec {mapSpecSize = MemberSpec xs}

data MapSpec fn k v = MapSpec
  { mapSpecHint :: Maybe Integer
  , mapSpecMustKeys :: Set k
  , mapSpecMustValues :: [v]
  , mapSpecSize :: Specification fn Integer
  , mapSpecElem :: Specification fn (k, v)
  , mapSpecFold :: FoldSpec fn v
  , mapSpecMinValSize :: Maybe (MinValSize fn v)
  -- Specifaction of the number of unique Values in the range of the Map that meet the Mapspec.
  -- The Maybe is there because Nothing means (MinValSpec TrueSpec),
  -- but does require the (Ord v) constraint
  }
  deriving (Generic)

-- | Spec of the the number of unique values in the range of a map, needed to conform to the MapSpec
--   Just a Spec, but carries the (Ord v) constraint
data MinValSize fn v where
  MinValSize :: Ord v => Specification fn Integer -> MinValSize fn v

deriving instance BaseUniverse fn => Show (MinValSize fn v)

-- | emptySpec without all the constraints
defaultMapSpec :: Ord k => MapSpec fn k v
defaultMapSpec = MapSpec Nothing mempty mempty TrueSpec TrueSpec NoFold Nothing

instance
  ( HasSpec fn (k, v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  ) =>
  Pretty (WithPrec (MapSpec fn k v))
  where
  pretty (WithPrec d s) =
    parensIf (d > 10) $
      "MapSpec"
        /> vsep
          [ "hint       =" <+> viaShow (mapSpecHint s)
          , "mustKeys   =" <+> viaShow (mapSpecMustKeys s) <+> (pretty (length (mapSpecMustKeys s)))
          , "mustValues =" <+> viaShow (mapSpecMustValues s) <+> (pretty (length (mapSpecMustValues s)))
          , "size       =" <+> pretty (mapSpecSize s)
          , "elem       =" <+> pretty (mapSpecElem s)
          , "fold       =" <+> pretty (mapSpecFold s)
          , "minValSize ="
              <+> ( case (mapSpecMinValSize s) of
                      Nothing -> viaShow (Nothing :: Maybe ())
                      Just (MinValSize p) -> pretty p
                  )
          ]

instance
  ( HasSpec fn (k, v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  ) =>
  Show (MapSpec fn k v)
  where
  showsPrec d = shows . prettyPrec d

instance Ord k => Forallable (Map k v) (k, v) where
  fromForAllSpec kvs = typeSpec $ defaultMapSpec {mapSpecElem = kvs}
  forAllToList = Map.toList

instance
  ( Ord k
  , HasSpec fn (Prod k v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  , IsNormalType v
  , IsNormalType k
  ) =>
  HasSpec fn (Map k v)
  where
  type TypeSpec fn (Map k v) = MapSpec fn k v
  type Prerequisites fn (Map k v) = (HasSpec fn k, HasSpec fn v)

  typeSpecOpt ts [m] | Map.null m = TypeSpec ts {mapSpecSize = mapSpecSize ts <> geqSpec 1} []
  typeSpecOpt ts cant = TypeSpec ts cant

  emptySpec = defaultMapSpec

  combineSpec
    (MapSpec mHint mustKeys mustVals size kvs foldSpec acceptable)
    (MapSpec mHint' mustKeys' mustVals' size' kvs' foldSpec' acceptable') = fromGESpec $ do
      typeSpec
        <$> ( MapSpec
                -- This is min because that allows more compositionality - if a spec specifies a
                -- low upper bound because some part of the spec will be slow it doesn't make sense
                -- to increase it somewhere else because that part isn't slow.
                (unionWithMaybe min mHint mHint')
                (mustKeys <> mustKeys')
                (nub $ mustVals <> mustVals')
                (size <> size')
                (kvs <> kvs')
                <$> (combineFoldSpec foldSpec foldSpec')
                <*> (pure (smallestAcceptableSize acceptable acceptable'))
            )

  conformsTo m (MapSpec _ mustKeys mustVals size kvs foldSpec minval) =
    and
      [ mustKeys `Set.isSubsetOf` Map.keysSet m
      , all (`elem` Map.elems m) mustVals
      , sizeOf m `conformsToSpec` size
      , all (`conformsToSpec` kvs) (Map.toList m)
      , Map.elems m `conformsToFoldSpec` foldSpec
      , conformsToSpec @fn (sizeOf (nub (Map.elems m))) (extractFromMinValSize minval)
      ]

  genFromTypeSpec = genFromMapSpec

  cardinalTypeSpec _ = TrueSpec

  shrinkWithTypeSpec (MapSpec _ _ _ _ kvs _ _) m = map Map.fromList $ shrinkList (shrinkWithSpec kvs) (Map.toList m)

  toPreds m (MapSpec mHint mustKeys mustVals size kvs foldSpec minvalsize) =
    toPred
      [ assert $ lit mustKeys `subset_` dom_ m
      , forAll (Lit mustVals) $ \val ->
          val `elem_` rng_ m
      , sizeOf_ (rng_ m) `satisfies` size
      , case minvalsize of
          Nothing -> TruePred
          Just (MinValSize n) -> sizeOf_ (rngSet_ m) `satisfies` n
      , forAll m $ \kv -> satisfies kv kvs
      , toPredsFoldSpec (rng_ m) foldSpec
      , maybe TruePred (`genHint` m) mHint
      ]

instance
  ( Ord k
  , HasSpec fn (Prod k v)
  , HasSpec fn k
  , HasSpec fn v
  , HasSpec fn [v]
  , IsNormalType v
  , IsNormalType k
  ) =>
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
                  typeSpec $ MapSpec Nothing s [] (equalSpec $ sizeOf s) TrueSpec NoFold Nothing
                TypeSpec (SetSpec must elemspec size) [] ->
                  typeSpec $
                    MapSpec
                      Nothing
                      must
                      []
                      size
                      (constrained $ \kv -> satisfies (app (fstFn @fn) (app (toGenericFn @fn) kv)) elemspec)
                      NoFold
                      Nothing
                _ -> ErrorSpec ["Dom on bad map spec", show spec]
    Rng ->
      -- No TypeAbstractions in ghc-8.10
      case fn of
        (_ :: MapFn fn '[Map k v] [v])
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Map k v) ->
              case spec of
                TypeSpec (ListSpec listHint must size elemspec foldspec) [] ->
                  typeSpec $
                    MapSpec
                      listHint
                      Set.empty
                      must
                      size
                      (constrained $ \kv -> satisfies (snd_ kv) elemspec)
                      foldspec
                      Nothing
                -- NOTE: you'd think `MemberSpec [r]` was a safe and easy case. However, that requires not only that the elements
                -- of the map are fixed to what is in `r`, but they appear in the order that they are in `r`. That's
                -- very difficult to achieve!
                _ -> ErrorSpec ["Rng on bad map spec", show spec]
    Lookup ->
      case fn of
        Lookup :: MapFn fn '[k, Map k v] (Maybe v)
          | HOLE :? Value (m :: Map k v) :> Nil <- ctx ->
              if Nothing `conformsToSpec` spec
                then notMemberSpec [k | (k, v) <- Map.toList m, not $ Just v `conformsToSpec` spec]
                else MemberSpec $ Map.keys $ Map.filter (`conformsToSpec` spec) (Just <$> m)
          | Value k :! NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Map k v) ->
              constrained $ \m ->
                [assert $ lit k `member_` dom_ m | not $ Nothing `conformsToSpec` spec]
                  ++ [ forAll m $ \kv ->
                        letBind (fst_ kv) $ \k' ->
                          letBind (snd_ kv) $ \v ->
                            whenTrue (lit k ==. k') $
                              -- TODO: What you want to write is `cJust_ v `satisfies` spec` but we can't
                              -- do that because we don't have access to `IsNormalType v` here. When
                              -- we refactor the `IsNormalType` machinery we will be able to make
                              -- this nicer.
                              case spec of
                                MemberSpec as -> assert $ v `elem_` lit [a | Just a <- as]
                                TypeSpec (SumSpec _ _ vspec) cant ->
                                  v `satisfies` (vspec <> notMemberSpec [a | Just a <- cant])
                     ]
    RngSet ->
      case fn of
        (_ :: MapFn fn '[Map k v] (Set v))
          | NilCtx HOLE <- ctx
          , Evidence <- prerequisites @fn @(Map k v) ->
              case spec of
                MemberSpec [r] ->
                  typeSpec $
                    MapSpec
                      Nothing
                      Set.empty
                      (Set.toList r)
                      (atLeastSpec (equalSpec $ sizeOf r))
                      TrueSpec
                      NoFold
                      (Just (MinValSize (equalSpec (sizeOf r))))
                TypeSpec (SetSpec must elemspec setsize) [] ->
                  typeSpec $
                    MapSpec
                      Nothing
                      Set.empty
                      (Set.toList must)
                      (atLeastSpec (geqSpec (sizeOf must) <> setsize)) -- This might be overconstrained.
                      (constrained $ \kv -> satisfies (app (sndFn @fn) (app (toGenericFn @fn) kv)) elemspec)
                      NoFold
                      (injectMinValSize setsize)
                _ -> ErrorSpec ["RngSet on bad map spec", show spec]

  -- NOTE: this function over-approximates and returns a liberal spec.
  mapTypeSpec f ts = case f of
    -- TODO: consider checking against singleton member-specs in the other component
    -- interacting with cant
    Dom ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] (Set k))
          | MapSpec _ mustSet _ sz kvSpec _ _ <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ SetSpec mustSet (mapSpec (fstFn @fn) $ toSimpleRepSpec kvSpec) sz
    Rng ->
      -- No TypeAbstractions in ghc-8.10
      case f of
        (_ :: MapFn fn '[Map k v] [v])
          | MapSpec _ _ mustList sz kvSpec foldSpec _ <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $ ListSpec Nothing mustList sz (mapSpec (sndFn @fn) $ toSimpleRepSpec kvSpec) foldSpec
    RngSet ->
      case f of
        (_ :: MapFn fn '[Map k v] (Set v))
          | MapSpec _ _ mustList sz kvSpec _foldSpec _ <- ts
          , Evidence <- prerequisites @fn @(Map k v) ->
              typeSpec $
                SetSpec (Set.fromList mustList) (mapSpec (sndFn @fn) $ toSimpleRepSpec kvSpec) (atMostSpec sz)

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

dom_ ::
  (HasSpec fn (Map k v), HasSpec fn k, Ord k) =>
  Term fn (Map k v) ->
  Term fn (Set k)
dom_ = app domFn

rng_ ::
  (HasSpec fn k, HasSpec fn v, Ord k, IsNormalType v, IsNormalType k) =>
  Term fn (Map k v) ->
  Term fn [v]
rng_ = app rngFn

lookup_ ::
  (HasSpec fn k, HasSpec fn v, Ord k, IsNormalType k, IsNormalType v) =>
  Term fn k ->
  Term fn (Map k v) ->
  Term fn (Maybe v)
lookup_ = app lookupFn

rngSet_ ::
  (HasSpec fn k, HasSpec fn v, Ord k, Ord v, IsNormalType v, IsNormalType k) =>
  Term fn (Map k v) ->
  Term fn (Set v)
rngSet_ = app rngSetFn

-- ======================================================================
-- How to deal with size constraints on the RngSet of a Map
-- There are two parts to this.
--  1) changes to genFromTypeSpec to generate the right thing
--  2) changes to (TypeSpec @Map) to encode the additional constraints needed. See (MinValSize fn v)

-- | Merge two MinValSizes, each from a separate MapSpec, which are being merged.
smallestAcceptableSize ::
  BaseUniverse fn => Maybe (MinValSize fn v) -> Maybe (MinValSize fn v) -> Maybe (MinValSize fn v)
smallestAcceptableSize (Just (MinValSize x)) (Just (MinValSize y)) = Just (MinValSize (x <> y))
smallestAcceptableSize Nothing x = x
smallestAcceptableSize x Nothing = x

-- | Extract a Specification from (Maybe (MinValSize fn v)), Nothing extracts TrueSpec
extractFromMinValSize :: Maybe (MinValSize fn v) -> Specification fn Integer
extractFromMinValSize (Just (MinValSize s)) = s
extractFromMinValSize Nothing = TrueSpec

-- | Inject a Specification into a (Maybe (MinValSize fn v))
injectMinValSize :: Ord v => Specification fn Integer -> Maybe (MinValSize fn v)
injectMinValSize TrueSpec = Nothing
injectMinValSize (MemberSpec []) = Nothing
injectMinValSize spec = Just (MinValSize spec)

-- ========================================================================================================
-- How sequence (GenT m a) until one of them succeeds

-- | Lift any errors in (GenT GE a), into (monadGenError m) so we get back to (GenT m a)
--   This allows us to use 'tryGen' to try different things until one works. See 'tryGenFromList'
liftGE :: forall m a. MonadGenError m => GenT GE a -> GenT m a
liftGE action = do mm <- pureGen action2; mm
  where
    action2 :: Gen (GenT m a)
    action2 = do
      x <- runGenT action Loose
      pure (runGE x)

trySpecs :: (MonadGenError m, HasSpec fn a) => [Specification fn a] -> GenT m a
trySpecs specs = go specs
  where
    go [] = genError ("No spec in trySpecs succeeds" : (map show specs))
    go (x : xs) = do
      m <- (tryGen . genFromSpecT) x
      case m of
        Just ans -> pure ans
        Nothing -> go xs

-- | Try to satisfy some generator on a input list, where the order of the elements
--   in the list may affect the success of the operation. Repeat the operation
--   upto 'n' times, each time with a different order.
tryGenFromList :: (Show b, Show a, MonadGenError m) => Int -> ([a] -> GenT GE b) -> [a] -> GenT m b
tryGenFromList 0 _ _ = genError ["tryGenFromList ran out of tries"]
tryGenFromList _ action [] = liftGE $ action []
tryGenFromList _ action [x] = liftGE $ action [x]
tryGenFromList n action input = do
  m <- liftGE $ tryGen (action input)
  case m of
    Just ans -> pure ans
    Nothing -> do
      input2 <- pureGen (shuffle input)
      tryGenFromList (n - 1) action input2

-- ================================================
-- How to handle a FoldSpec, this is the old style
-- ================================================

genFromMapSpecWithFold ::
  forall fn k v m.
  (HasSpec fn k, HasSpec fn v, Ord k, MonadGenError m) =>
  MapSpec fn k v ->
  GenT m (Map k v)
genFromMapSpecWithFold m@(MapSpec mHint mustKeys mustVals size (simplifySpec -> kvs) foldspec _) = do
  mustMap <-
    explain
      ["Make the mustMap for MapSpec", show m]
      ( forM (Set.toList mustKeys) $ \k -> do
          let vSpec = constrained $ \v -> satisfies (pair_ (lit k) v) kvs
          v <- explain [show $ "vSpec =" <+> pretty vSpec] $ genFromSpecT vSpec
          pure (k, v)
      )
  let haveVals = map snd mustMap
      residualMustVals = filter (`notElem` haveVals) mustVals
      size' = simplifySpec $ constrained $ \sz ->
        satisfies
          (sz + Lit (sizeOf mustMap))
          ( maybe TrueSpec (leqSpec . max 0) mHint
              <> size
              <> maxSpecSize (cardinality (mapSpec fstFn $ mapSpec toGenericFn kvs))
              <> maxSpecSize (cardinalTrueSpec @fn @k)
          )

      foldSpec' = case foldspec of
        NoFold -> NoFold
        FoldSpec fn sumSpec -> FoldSpec fn $ propagateSpecFun (theAddFn @fn) (HOLE :? Value mustSum :> Nil) sumSpec
          where
            mustSum = adds @fn (map (sem fn) haveVals)

      valsSpec =
        ListSpec
          Nothing
          residualMustVals
          size'
          (simplifySpec $ constrained $ \v -> unsafeExists $ \k -> pair_ k v `satisfies` kvs)
          foldSpec'
  restVals <-
    explain
      [ "Generating values for additional pairs for MapSpec"
      , show m
      , "We have already computed " ++ show mustMap
      , "We need additional values (of size " ++ show size' ++ ") that meet the spec"
      , show valsSpec
      ]
      (genFromTypeSpec valsSpec)
  let go mp [] = pure mp
      go mp (v : restVals') = do
        let keySpec = notMemberSpec (Map.keysSet mp) <> constrained (\k -> pair_ k (lit v) `satisfies` kvs)
        k <-
          explain
            [ "Make a key for the value " ++ show v
            , "keySpec =\n  " ++ show (simplifySpec keySpec)
            ]
            $ genFromSpecT keySpec
        go (Map.insert k v mp) restVals'
  go (Map.fromList mustMap) restVals

-- ====================================================================================
-- Compute a combined SizeSpec for a MapSpec. Combines all the size info into one Spec
-- Attempts to explain where things go wrong if the sizes are inconsistent.
-- ===================================================================================

--  | Generate specification that satisfies some things greater than or equal to 'm'
--    Note that sizeSpec can never have zero or negative numbers
genSizeSpec :: BaseUniverse fn => Integer -> Integer -> Gen (Specification fn Integer)
genSizeSpec m maxMember = do
  delta <- choose (1, maxMember) --
  xs <- vectorOf 5 (choose (1, maxMember))
  frequency
    [ (1, pure $ (TypeSpec (rangeSize m (m + delta)) []))
    , (1, pure $ MemberSpec (sort (nub (m : (filter (m <=) xs)))))
    ]

-- | Given a MapSpec, check that all the size specs are consistent, and then returns sizes
--   for the set of keys, the list of values, and the size of value list as a set.
--   That might be smaller than the list of values, because it may contain duplicates.
mapSizes ::
  forall a b fn m.
  (MonadGenError m, Ord a, HasSpec fn a, HasSpec fn b) =>
  MapSpec fn a b ->
  GenT m (Integer, Integer, Integer)
mapSizes m@(MapSpec mHint mustKeys mustVals msize pairspec _foldSpec vSetSize) = do
  let specForRng :: Specification fn b
      specForRng = mapSpec sndFn $ mapSpec toGenericFn (simplifySpec pairspec)
      specForDom :: Specification fn a
      specForDom = mapSpec fstFn $ mapSpec toGenericFn (simplifySpec pairspec)
      cardRng = cardinality specForRng
      cardDom = cardinality specForDom
      rawValSpec = extractFromMinValSize vSetSize
      table =
        [ ("Sizes are nonneagtive ", geqSpec 0)
        , maybe
            ("No hint               ", TrueSpec)
            (\n -> ("The hint             " ++ show n, leqSpec (max 0 n)))
            mHint
        , ("Map size              ", msize)
        , ("Cardinality range     ", atMostSpec cardRng) -- Can't have more than the cardinality of the range 'b'
        , ("Cardinality dom       ", atMostSpec cardDom) -- Can't have more than the cardinality of the range 'b'
        , ("MustKeys              ", geqSpec (sizeOf mustKeys))
        , ("Mustvals              ", geqSpec (sizeOf mustVals))
        , ("ValSet (atLeast " ++ show rawValSpec ++ ")", atLeastSpec rawValSpec)
        ]
      describe (msg, spec) = "  " ++ msg ++ " = " ++ show spec
      size =
        explainSpec
          (["Map spec sizes are inconsistent. Relevant Size Specs"] ++ map describe table)
          (foldMap snd table)
  keysize <- genFromSpecT size
  (valsize, valSetSize) <-
    explain
      ["Picking the size of the set of values, given the size of the set of keys: " ++ show keysize]
      $ case vSetSize of
        Nothing -> pure (keysize, keysize)
        Just (MinValSize minvalspec) -> do
          sizeAsSet <- genFromSpecT (geqSpec 1 <> minvalspec <> geqSpec (sizeOf mustVals))
          pure (keysize, sizeAsSet)
  pure (keysize, valsize, valSetSize)

-- ==============================================================
-- How to hande the size constraints on the RngSet of a Map
-- This is the new style. It calls the old style if a FoldSpec is used.
-- ==============================================================

genFromMapSpec ::
  forall fn k v m.
  ( Ord k
  , HasSpec fn k
  , IsNormalType k
  , HasSpec fn v
  , IsNormalType v
  , MonadGenError m
  ) =>
  MapSpec fn k v ->
  GenT m (Map k v)
genFromMapSpec m@(MapSpec _mHint mustKeys mustVals msize pairspec (FoldSpec _ _) (Just _)) =
  fatalError ["MapSpec with both Fold and MinValSize", show m]
genFromMapSpec m@(MapSpec _mHint mustKeys mustVals msize pairspec (FoldSpec _ _) Nothing) = genFromMapSpecWithFold m
genFromMapSpec m@(MapSpec _mHint mustKeys mustVals msize pairspec NoFold Nothing) = genFromMapSpecWithFold m
genFromMapSpec m@(MapSpec _mHint mustKeys mustVals msize pairspec NoFold (Just (MinValSize (MemberSpec [])))) =
  fatalError ["MemberSpec [] in MinValSpec", show m]
genFromMapSpec m@(MapSpec _mHint mustKeys mustVals msize pairspec NoFold vsize) = do
  zs@(keysize, valsize, valAsSetSize) <- mapSizes m
  -- let !_ = trace ("(keysize, valsize,valAsSetSize) " ++ show zs) True
  (kvPairsForMustVals, residualKeys, badvs, badks) <-
    explain
      [ "\nTrying to pair up the mustVals "
      , "   " ++ show (nub mustVals)
      , "with the must keys"
      , "   " ++ show mustKeys
      , "In a way that meets the key-val-spec"
      , show (simplifySpec pairspec)
      , "This may not be possible."
      , "msize = " ++ show msize
      , "(keysize, valsize, valAsSetSize) = "
          ++ show (keysize, valsize, valAsSetSize)
      , ""
      ]
      (tryGenFromList 10 (loopV (sizeOf (nub mustVals)) mustKeys [] Set.empty pairspec) (nub mustVals)) -- DO WE NEED nub here?
      -- let !_ = trace ("mustval pairs = "++show kvPairsForMustVals++", residual keys = "++ show residualKeys) True
  let
    -- \| The number of additional pairs we must generate
    morePairs = keysize - (sizeOf kvPairsForMustVals)
  -- \| The set of vals we have already generated, this should be equal (as a set) to the mustVal

  kvPairsForResidualKeys <-
    explain
      [ "\nWe are trying to expand the unmatched keys " ++ show (Set.toList residualKeys)
      , "to a list of pairs with length " ++ show keysize
      , "And the (pair key value) must meet the spec " ++ show pairspec
      , "This is not always possible."
      , "MapSpec is "
      , show m
      , ""
      ]
      ( loopK
          kvPairsForMustVals
          (Set.toList residualKeys)
          morePairs
          valAsSetSize
          badvs
          (Set.toList badks)
          pairspec
      )
  pure (Map.fromList kvPairsForResidualKeys)

-- | Match mustVals with satisfying mustKeys
--   Given a val 'v', try to find a satisfying k in 'mustks' (satisfy (k,v) ok)
--   if you can't find a satisfying k, then generate one.
--   Remember each 'k' you found or generated in 'usedKs',  so you won't pick it again.
--   Remember each 'v' you process in 'usedVs', so you won't pick it again
--   When you run out of mustVals, add 'count' additional fresh pairs that satisfy 'ok'
loopV ::
  forall fn m k v.
  (Ord k, HasSpec fn k, IsNormalType k, IsNormalType v, HasSpec fn v, MonadGenError m) =>
  Integer ->
  Set k ->
  [v] ->
  Set k ->
  Specification fn (k, v) ->
  [v] ->
  GenT m ([(k, v)], Set k, [v], Set k)
loopV count mustKeys usedVs usedKs ok mustVals =
  -- trace ("LOOPV "++show(count,mustVals,mustKeys,usedVs,usedKs)) $
  explain
    [ "Matching mustVals " ++ show mustVals
    , "with satisfying mustKeys " ++ show mustKeys
    ]
    $ case (count, mustVals, mustKeys) of
      (0, [], residualks) -> pure ([], residualks, usedVs, usedKs) -- We are Done
      (n, [], mustks) -> step3 n (Set.toList mustks) usedVs usedKs -- Add 'n' additional pairs choosing 'k' from 'mustks'
      (n, v : vs, mustks) | Set.null mustks -> step1 n usedVs usedKs v vs -- Generate a match for 'v' (since 'mustks' is null)
      (n, v : vs, mustks) -> step2 n mustks usedVs usedKs v vs -- Find (in 'mustks') or genrate an unused key, such that ok(key,'v')
  where
    -- mustks exhausted, generate a fresh k
    step1 n usedvs usedks v vs = do
      k <-
        genFromSpecT @fn
          ( constrained $ \k ->
              [ assert $ not_ (member_ k (lit usedks))
              , satisfies (pair_ k (lit v)) ok
              ]
          )
      (pairs, _, usedvals, usedkeys) <- loopV (n - 1) Set.empty (v : usedvs) (Set.insert k usedks) ok vs
      pure ((k, v) : pairs, Set.empty, usedvals, usedkeys)

    -- Still some mustkeys. Pick one (if possible), or generate a fresh one if not.
    step2 n mustks usedvs usedks v vs = do
      k <- genOrFindMatchFor v mustks usedks ok
      (pairs, mustkeys, usedvals, usedkeys) <-
        loopV (n - 1) (Set.delete k mustks) (v : usedvs) (Set.insert k usedks) ok vs
      pure ((k, v) : pairs, mustkeys, usedvals, usedkeys)

    -- Add 'n' additional pairs
    step3 0 mustks usedvs usedks =
      -- We are done
      pure ([], Set.fromList mustks, usedvs, usedks)
    step3 n [] usedvs usedks = do
      -- Still have 'n' pairs to choose, but both 'k' and 'v' are unused random
      (k, v) <-
        genFromSpecT @fn
          ( constrained $ \p -> match p $ \k v ->
              [ assert $ not_ (member_ k (lit usedks))
              , assert $ not_ (elem_ v (lit usedvs))
              , satisfies (pair_ k v) ok
              ]
          )
      (pairs, mustks, usedvals, usedkeys) <- step3 (n - 1) [] (v : usedvs) (Set.insert k usedks)
      pure ((k, v) : pairs, mustks, usedvals, usedkeys)
    step3 n (k : ks) usedvs usedks = do
      -- Still have 'n' pairs to choose, but 'k' is fixed, 'v' is unused random
      v <-
        genFromSpecT @fn
          ( constrained $ \v ->
              [ assert $ not_ (elem_ v (lit usedvs))
              , satisfies (pair_ (lit k) v) ok
              ]
          )
      (pairs, mustks, usedvals, usedkeys) <- step3 (n - 1) ks (v : usedvs) (Set.insert k usedks)
      pure ((k, v) : pairs, mustks, usedvals, usedkeys)

-- | Generate a random [(k,v)] from the residual 'mustKeys' (those not yet paired with some mustVal)
--   'newVcount' is how many random vs must be generated
--   'dups' how may duplicate vs must be generated.
--   This is because the size of the Range as a set, might be smaller smaller than the size of the Range as a list.
--   'usedVs' have already been chosen to be in the range of result.
--   'usedKs' have already been chosen to be in the domain of the result.
loopK ::
  forall fn m k v.
  (HasSpec fn k, IsNormalType k, IsNormalType v, HasSpec fn v, MonadGenError m) =>
  [(k, v)] ->
  [k] ->
  Integer ->
  Integer ->
  [v] ->
  [k] ->
  Specification fn (k, v) ->
  GenT m [(k, v)]
loopK ans mustKeys newVcount dups usedVs usedKs ok =
  -- trace ("LOOPK "++show(mustKeys,newVcount,dups,usedVs,usedKs)++"  "++show ans) $
  case (mustKeys, newVcount) of
    ([], 0) -> pure ans
    ([], n) ->
      if sizeOf usedVs >= dups && dups /= 0
        then randomKusedV (n - 1) usedVs usedKs
        else randomKrandomV (n - 1) usedVs usedKs
    (k : ks, n) ->
      if sizeOf usedVs >= dups && dups /= 0
        then fixedKusedV ks (n - 1) usedVs usedKs k
        else fixedKrandomV ks (n - 1) usedVs usedKs k
  where
    -- Random unused k, duplicate v
    randomKusedV n usedvs usedks = do
      (k, v) <-
        genFromSpecT @fn @_ @m
          ( constrained $ \p -> match p $ \k v ->
              [ assert $ not_ (elem_ k (lit usedks))
              , assert $ elem_ v (lit usedvs)
              , satisfies (pair_ k v) ok
              ]
          )
      loopK ((k, v) : ans) [] n dups usedvs (k : usedks) ok

    -- Random unused k, Random unused v
    randomKrandomV n usedvs usedks = do
      (k, v) <-
        genFromSpecT @fn @_ @m
          ( constrained $ \p -> match p $ \k v ->
              [ assert $ not_ (elem_ k (lit usedks))
              , assert $ not_ (elem_ v (lit usedvs))
              , satisfies (pair_ k v) ok
              ]
          )
      loopK ((k, v) : ans) [] n dups (v : usedvs) (k : usedks) ok

    -- Given k, duplicate v
    fixedKusedV ks n usedvs usedks k = do
      v <-
        genFromSpecT @fn @_ @m
          ( constrained $ \v ->
              [ assert $ elem_ v (lit usedvs)
              , satisfies (pair_ (lit k) v) ok
              ]
          )
      loopK ((k, v) : ans) ks n dups usedvs (k : usedks) ok

    -- Given k, Random unused v
    fixedKrandomV ks n usedvs usedks k = do
      v <-
        genFromSpecT @fn @_ @m
          ( constrained $ \v ->
              [ assert $ not_ (elem_ v (lit usedvs))
              , satisfies (pair_ (lit k) v) ok
              ]
          )
      loopK ((k, v) : ans) ks n dups (v : usedvs) (k : usedks) ok

-- | Generate, or find a match , k, (in 'mustks') for v, that satisfies ok (k,v)
genOrFindMatchFor ::
  (MonadGenError m, HasSpec fn v, HasSpec fn k, Ord k) =>
  v ->
  Set k ->
  Set k ->
  Specification fn (k, v) ->
  GenT m k
genOrFindMatchFor v mustks usedks ok =
  explain
    [ "Find a k-match for v=" ++ show v ++ " where k in " ++ show (Set.toList mustks)
    , "  or"
    , "Gen a k-match for v="
        ++ show v
        ++ " where k not in "
        ++ show (Set.toList (Set.union mustks usedks))
    ]
    (trySpecs [findMatch, genMatch])
  where
    genMatch =
      constrained $ \key ->
        [ satisfies (pair_ key (lit v)) ok
        , assert $ not_ (member_ key (lit (Set.union mustks usedks)))
        ]
    findMatch =
      constrained $ \key ->
        [ satisfies (pair_ key (lit v)) ok
        , assert $ member_ key (lit mustks)
        ]

-- =================================================================================
-- More accurate than the Arbitrary instance
-- =================================================================================

-- | Usefull for generating (MapSpec fn Integer Integer) for use in tests
--   Most of the MapSpec's generated should be valid, with a good distribution
--   of different sizes for mustVals and mustKeys, and a high probability of
--   satisfying kvSpec (key >= val)
genMapSpec :: forall fn. BaseUniverse fn => Gen (MapSpec fn Integer Integer)
genMapSpec = do
  (mustKeyLen, mustValLen, _slack) <- genSizes
  mapsize <- genSizeSpec @fn (mustKeyLen + mustValLen) 6
  let biggest = maxSize mapsize
  let ok x = x <= biggest && x > mustValLen
  let genOk = if mustValLen == biggest then pure biggest else arbitrary `suchThat` ok -- ((getSmall <$> arbitrary) `suchThat` ok)
  valsize <-
    frequency
      [ (1, pure $ Just (MinValSize (typeSpec (NumSpecInterval (Just mustValLen) (Just biggest)))))
      , (1, pure $ Just (MinValSize (typeSpec (NumSpecInterval Nothing (Just biggest)))))
      , (1, pure Nothing)
      , (1, Just . MinValSize . MemberSpec . (mustValLen :) <$> vectorOf 5 genOk)
      ]
  mustval <- vectorOf (fromInteger mustValLen) arbitrary
  let avg =
        if null mustval
          then 0 :: Integer
          else sum mustval `div` toInteger (length mustval)
      goodMust = constrained (\n -> n >=. (lit avg))
  mustkey <- genFromGenT $ (Set.fromList <$> genDistinctList @_ @fn goodMust mustKeyLen [])
  let kvSpec =
        constrained
          ( \p ->
              match p (\key val -> [assert $ key >=. val])
          )
  pure (MapSpec Nothing mustkey mustval mapsize kvSpec NoFold valsize)

maxSize :: Specification fn Integer -> Integer
maxSize (TypeSpec (NumSpecInterval _ (Just n)) _) = n
maxSize (TypeSpec (NumSpecInterval _ Nothing) _) = 8
maxSize (MemberSpec xs) = maximum xs
maxSize _ = 9

-- | Gen 3 Integers They correspond to the
--   1) size of the mustKeys,
--   2) size of the mustVals
--   3) The minimum size, one might get from the SizeSpec for the Map
--   All of them could be 0
genSizes :: Gen (Integer, Integer, Integer)
genSizes =
  genFromGenT
    ( genFromSpecT @BaseFn
        ( constrained
            ( \triple ->
                match
                  triple
                  ( \k v slack ->
                      [ assert $ 0 <=. v
                      , assert $ v <=. 6
                      , assert $ 0 <=. k
                      , assert $ k <=. 7
                      , assert $ 0 <=. slack
                      , assert $ slack <=. 5
                      ]
                  )
            )
        )
    )

-- TODO: consider making this more interesting to get fewer discarded tests in `prop_gen_sound`.
-- See genMapSpec as a first attempt, but we would have to generalize Integer to k and v
instance
  ( Arbitrary k
  , Arbitrary v
  , Arbitrary (TypeSpec fn k)
  , Arbitrary (TypeSpec fn v)
  , Ord k
  , Ord v
  , HasSpec fn k
  , Foldy fn v
  ) =>
  Arbitrary (MapSpec fn k v)
  where
  arbitrary =
    MapSpec
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> frequency [(1, pure NoFold), (1, arbitrary)]
      <*> frequency [(2, pure Nothing), (1, Just <$> arbitrary)]
  shrink = genericShrink

instance (BaseUniverse fn, Ord v) => Arbitrary (MinValSize fn v) where
  arbitrary = MinValSize <$> arbitrary

instance Arbitrary (FoldSpec fn (Map k v)) where
  arbitrary = pure NoFold

-- ==============================================================================

-- Like conformsTo, but tells which of the sub conformance components fail
-- Nothing means success, (Just s), means 's' tells what went wrong.
conformsToMapMaybe ::
  forall fn k v.
  (HasSpec fn (Map k v), HasSpec fn k, HasSpec fn v, Ord k) =>
  Map k v ->
  MapSpec fn k v ->
  Maybe String
conformsToMapMaybe m (MapSpec _ mustKeys mustVals size kvs foldSpec minval) =
  allOf
    [ tell "mustKeys" $ mustKeys `Set.isSubsetOf` Map.keysSet m
    , tell "mustVals" $ all (`elem` Map.elems m) mustVals
    , tell ("size (" ++ show size ++ ") on map with size" ++ show (Map.size m)) $
        sizeOf m `conformsToSpec` size
    , tell "kvs" $ all (`conformsToSpec` kvs) (Map.toList m)
    , tell "foldSpec" $ Map.elems m `conformsToFoldSpec` foldSpec
    , tell "minValSize" $ conformsToSpec @fn (sizeOf (nub (Map.elems m))) (extractFromMinValSize minval)
    ]
  where
    tell :: String -> Bool -> Maybe String
    tell _ True = Nothing
    tell msg False = Just msg

    allOf :: [Maybe String] -> Maybe String
    allOf xs = foldr accum Nothing xs
      where
        accum Nothing ans = ans
        accum (Just s) Nothing = Just s
        accum (Just s) (Just more) = Just (s ++ "\n" ++ more)
