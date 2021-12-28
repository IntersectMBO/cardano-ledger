{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Compact.KeyMap where

import Cardano.Prelude (HeapWords (..), (<|>))
import qualified Data.Compact.HashMap as HM
import Data.Compact.KeyMap as KeyMap
import Data.Foldable (foldl')
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import System.Random (mkStdGen)
import System.Random.Stateful (runStateGen_, uniformListM)
import Test.QuickCheck
import Test.QuickCheck.Property (mapTotalResult, maybeNumTests)
import Test.Tasty
import Test.Tasty.HUnit (testCaseInfo)
import Test.Tasty.QuickCheck

-- ==============================================================

instance Arbitrary Key where
  arbitrary =
    frequency
      [ (1, Key <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary),
        (20, Key <$> chooseAny <*> chooseAny <*> chooseAny <*> chooseAny)
      ]

instance Arbitrary a => Arbitrary (KeyMap a) where
  arbitrary = do
    let go i m
          | i > 0 = do
            key <- arbitrary
            val <- arbitrary
            go (i - 1) $! insert key val m
          | otherwise = pure m
    NonNegative n <- arbitrary
    go (n :: Int) KeyMap.Empty

instance (HM.Keyed k, Arbitrary v) => Arbitrary (HM.HashMap k v) where
  arbitrary = HM.HashMap <$> arbitrary

prop_RountripToFromList :: KeyMap Int -> Property
prop_RountripToFromList km = KeyMap.fromList (KeyMap.toList km) === km

-- ==================================================
-- Map functions obey the map laws

roundtripFromList ::
  forall k v m.
  (Show (m k v), Eq (m k v)) =>
  (m k v -> [(k, v)]) ->
  ([(k, v)] -> m k v) ->
  m k v ->
  Property
roundtripFromList toL fromL mp = fromL (toL mp) === mp

insertDelete ::
  forall k v m.
  (Eq (m k v), Show (m k v)) =>
  (k -> v -> m k v -> m k v) ->
  (k -> m k v -> m k v) ->
  k ->
  v ->
  m k v ->
  Property
insertDelete insertF deleteF k v mp = deleteF k (insertF k v mp) === deleteF k mp

foldlkey ::
  forall k v m.
  (Eq v, Num v, Show v) =>
  (m k v -> [(k, v)]) ->
  ((v -> k -> v -> v) -> v -> m k v -> v) ->
  m k v ->
  Property
foldlkey toL foldlF hm = foldlF accum1 100 hm === List.foldl' accum2 100 (toL hm)
  where
    accum1 ans _key v = ans + v
    accum2 ans (_key, v) = ans + v

sizeProp :: forall k v m. (m k v -> [(k, v)]) -> (m k v -> Int) -> m k v -> Property
sizeProp toL sz hm = sz hm === length (toL hm)

setprop ::
  forall k v m.
  (Eq (m k v), Show (m k v)) =>
  ((k -> v -> v -> v) -> m k v -> m k v -> m k v) ->
  (m k v -> m k v -> m k v) ->
  m k v ->
  m k v ->
  m k v ->
  Property
setprop unionWithF intersct a b c =
  intersct a (unionWithF (\_k x _y -> x) b c)
    === unionWithF (\_k x _y -> x) (intersct a b) (intersct a c)

lookupinsert ::
  forall k a t.
  (Show a, Eq a) =>
  (k -> t k a -> Maybe a) ->
  (k -> a -> t k a -> t k a) ->
  k ->
  a ->
  t k a ->
  Property
lookupinsert look ins k v km = look k (ins k v km) === Just v

lookupdelete ::
  forall k a t.
  (Show a, Eq a) =>
  (k -> t k a -> Maybe a) ->
  (k -> t k a -> t k a) ->
  k ->
  t k a ->
  Property
lookupdelete look del k km = look k (del k km) === Nothing

assoc ::
  forall k v t.
  (Eq (t k v), Show (t k v)) =>
  (t k v -> t k v -> t k v) ->
  t k v ->
  t k v ->
  t k v ->
  Property
assoc oper x y z = oper (oper x y) z === oper x (oper y z)

commutes ::
  forall k v t.
  (Eq (t k v), Show (t k v)) =>
  (t k v -> t k v -> t k v) ->
  t k v ->
  t k v ->
  Property
commutes oper x y = oper x y === oper y x

ascFoldDescFold :: KeyMap Int -> Property
ascFoldDescFold x =
  KeyMap.foldWithAscKey (\ans _key v -> ans + v) 0 x
    === KeyMap.foldWithDescKey (\_key v ans -> v + ans) 0 x

allKey :: (Key -> Bool) -> KeyMap t -> Bool
allKey p = KeyMap.foldWithDescKey (\key _v ans -> p key && ans) True

allVal :: (t -> Bool) -> KeyMap t -> Bool
allVal p = KeyMap.foldWithDescKey (\_key v ans -> p v && ans) True

minKey :: Show a => KeyMap a -> Maybe Property
minKey x = do
  (k, _v) <- KeyMap.lookupMin x
  Just $ counterexample ("min=" ++ show k ++ "map=\n" ++ show x) (allKey (>= k) x)

maxKey :: Show a => KeyMap a -> Maybe Property
maxKey x = do
  (k, _v) <- KeyMap.lookupMax x
  Just $ counterexample ("max=" ++ show k ++ "map=\n" ++ show x) (allKey (<= k) x)

mapWorks :: KeyMap Int -> Property
mapWorks x = allVal (== (99 :: Int)) (KeyMap.mapWithKey (\_key _x -> 99) x) === True

foldintersect :: KeyMap Int -> KeyMap Int -> Property
foldintersect x y =
  foldOverIntersection (\ans _key u _v -> ans + u) 0 x y
    === foldWithDescKey (\_key u ans -> ans + u) 0 (intersection x y)

withoutRestrict :: KeyMap Int -> Set Key -> Bool
withoutRestrict m domset = union (withoutKeys m domset) (restrictKeys m domset) == m

splitwhole :: Key -> KeyMap Int -> Property
splitwhole k m =
  case splitLookup k m of
    (m1, Nothing, m2) -> m === union m1 m2
    (m1, Just v, m2) -> m === insert k v (union m1 m2)

-- =========================================================

withMaxTimesSuccess :: Testable prop => Int -> prop -> Property
withMaxTimesSuccess !n =
  mapTotalResult $ \res -> res {maybeNumTests = (n *) <$> (maybeNumTests res <|> Just 100)}

testPropertyN :: Testable prop => Int -> TestName -> prop -> TestTree
testPropertyN n name = testProperty name . withMaxTimesSuccess n

keyMapTests :: TestTree
keyMapTests =
  testGroup
    "KeyMap has Map properties"
    [ testProperty "to/fromList" prop_RountripToFromList,
      testProperty "HashMap-to-from-list" $ roundtripFromList @Key @Int HM.toList HM.fromList,
      testPropertyN 5 "HashMap-insert-delete" $ insertDelete @Key @Int HM.insert HM.delete,
      testProperty "HashMap-foldl" $ foldlkey @Key @Int HM.toList HM.foldlWithKey',
      testProperty "size-length-toList" $ sizeProp @Key @Int HM.toList HM.size,
      testPropertyN 10 "union-intersect-property" $ setprop @Key @Int HM.unionWithKey HM.intersection,
      testProperty "lookup-insert" $ lookupinsert @Key @Int HM.lookup HM.insert,
      testProperty "lookup-delete" $ lookupdelete @Key @Int HM.lookup HM.delete,
      testPropertyN 10 "union is associative" $ assoc @Key @Int HM.union,
      -- (unionwith f) is commutative if 'f' is commutative
      testPropertyN 10 "unionWith is commutative" $ commutes @Key @Int (HM.unionWith (+)),
      testPropertyN 10 "intersect is associative" $ assoc @Key @Int HM.intersection,
      -- (unionwith f) is commutative if 'f' is commutative
      testPropertyN 10 "intersectWith is commutative" $ commutes @Key @Int (HM.intersectionWith (+)),
      testProperty "ascending fold == descending fold with commutative operator" ascFoldDescFold,
      testProperty "lookupMin finds the smallest key" (minKey @Int),
      testProperty "lookupMax finds the largest key" (maxKey @Int),
      testProperty "(mapWithKey f) applies 'f' to every value" mapWorks,
      testProperty "foldOverIntersection folds over the intersection" foldintersect,
      testProperty "restrictKeys and withoutKeys partition a KeyMap" withoutRestrict,
      testPropertyN 5 "splitLookup pieces add to the whole" splitwhole,
      testCaseInfo "Keys are uniformly distributed" (testStatistics 100000)
    ]

-- =========================================================
-- Map functions behave the same as Data.Map

infix 4 $==$

($==$) :: (HM.Keyed k, Eq k, Eq v, Show k, Show v) => HM.HashMap k v -> Map.Map k v -> Property
($==$) x y =
  counterexample
    ("\nkeymap\n" ++ show x ++ "\ndatamap\n" ++ show y)
    (HM.toList x === Map.toList y)

insertHMDATA :: forall v. (Eq v, Show v) => Key -> v -> [(Key, v)] -> Property
insertHMDATA k v m = HM.insert k v (HM.fromList m) $==$ Map.insert k v (Map.fromList m)

deleteHMDATA :: forall v. (Eq v, Show v) => Key -> [(Key, v)] -> Property
deleteHMDATA k m = HM.delete k (HM.fromList m) $==$ Map.delete k (Map.fromList m)

unionHMDATA :: forall v. (Eq v, Show v) => [(Key, v)] -> [(Key, v)] -> Property
unionHMDATA n m =
  HM.union (HM.fromList n) (HM.fromList m)
    $==$ Map.union (Map.fromList n) (Map.fromList m)

intersectHMDATA :: forall v. (Eq v, Show v) => [(Key, v)] -> [(Key, v)] -> Property
intersectHMDATA n m =
  HM.intersection (HM.fromList n) (HM.fromList m)
    $==$ Map.intersection (Map.fromList n) (Map.fromList m)

lookupHMDATA :: forall v. (Eq v, Show v) => Key -> [(Key, v)] -> Property
lookupHMDATA k m = HM.lookup k (HM.fromList m) === Map.lookup k (Map.fromList m)

restrictHMDATA :: (Show a, Eq a) => [(Key, a)] -> Set Key -> Property
restrictHMDATA m s = HM.restrictKeys (HM.fromList m) s $==$ Map.restrictKeys (Map.fromList m) s

withoutHMDATA :: (Show a, Eq a) => [(Key, a)] -> Set Key -> Property
withoutHMDATA m s = HM.withoutKeys (HM.fromList m) s $==$ Map.withoutKeys (Map.fromList m) s

minHMDATA :: (Eq a, Show a) => [(Key, a)] -> Property
minHMDATA m = HM.lookupMin (HM.fromList m) === Map.lookupMin (Map.fromList m)

maxHMDATA :: (Eq a, Show a) => [(Key, a)] -> Property
maxHMDATA m = HM.lookupMax (HM.fromList m) === Map.lookupMax (Map.fromList m)

splitHMDATA :: (Eq a, Show a) => Key -> [(Key, a)] -> Property
splitHMDATA k m = case Map.splitLookup k (Map.fromList m) of
  (a, b, c) -> (fromList (Map.toList a), b, fromList (Map.toList c)) === splitLookup k (fromList m)

minViewHMDATA :: [(Key, Int)] -> Property
minViewHMDATA m = case Map.minViewWithKey (Map.fromList m) of
  Nothing -> minViewWithKey (fromList m) === Nothing
  Just (b, c) -> Just (b, fromList (Map.toList c)) === minViewWithKey (fromList m)

maxViewHMDATA :: [(Key, Int)] -> Property
maxViewHMDATA m = case Map.maxViewWithKey (Map.fromList m) of
  Nothing -> maxViewWithKey (fromList m) === Nothing
  Just (b, c) -> Just (b, fromList (Map.toList c)) === maxViewWithKey (fromList m)

keyMapEquivDataMap :: TestTree
keyMapEquivDataMap =
  testGroup
    "KeyMap behaves like Data.Map"
    [ testPropertyN 5 "insert" (insertHMDATA @Int),
      testPropertyN 5 "delete" (deleteHMDATA @Int),
      testPropertyN 5 "union" (unionHMDATA @Int),
      testPropertyN 5 "intersection" (intersectHMDATA @Int),
      testPropertyN 5 "lookup" (lookupHMDATA @Int),
      testPropertyN 5 "withoutKeys" (withoutHMDATA @Int),
      testPropertyN 5 "restrictKeys" (restrictHMDATA @Int),
      testPropertyN 5 "lookupMin" (minHMDATA @Int),
      testPropertyN 5 "lookupMax" (maxHMDATA @Int),
      testPropertyN 5 "splitLookup" (splitHMDATA @Int),
      testPropertyN 5 "minViewWithKey" minViewHMDATA,
      testPropertyN 5 "maxViewWithKey" maxViewHMDATA
    ]

-- ====================================================
-- Generating random KeyMaps

-- | Generate a list of random Key's starting with 'seed' of length 'cnt'
--   The keys should be uniformly distributed across the domain of Keys
makeKeys :: Int -> Int -> [Key]
makeKeys seed cnt = runStateGen_ (mkStdGen seed) (uniformListM cnt)

-- ==================================================================
-- Data structure for storing statictic like values about KeyMaps

data Stat n = Stat
  { statCount :: n,
    statSum :: n,
    statMax :: Maybe n,
    statMin :: Maybe n
  }

liftMaybes :: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
liftMaybes f mx my = (f <$> mx <*> my) <|> my <|> mx

instance (Ord n, Num n) => Semigroup (Stat n) where
  (Stat c1 s1 mx1 mn1) <> (Stat c2 s2 mx2 mn2) =
    Stat (c1 + c2) (s1 + s2) (liftMaybes max mx1 mx2) (liftMaybes min mn1 mn2)

instance (Ord n, Num n) => Monoid (Stat n) where
  mempty = Stat 0 0 Nothing Nothing

instance (Integral n, Show n) => Show (Stat n) where
  show (Stat c s mx mn) =
    "{count=" ++ show c ++ ", sum=" ++ show s ++ ", max=" ++ show mx
      ++ ", min="
      ++ show mn
      ++ (if c == 0 then "}" else ", avg=" ++ show (div s c) ++ "}")

add :: (Num n, Ord n) => n -> Stat n -> Stat n
add n stat = Stat 1 n (Just n) (Just n) <> stat

-- =========================================================================
-- Computing statitics about KeyMaps

count :: KeyMap v -> (Int, Int, Int, Stat Int, Stat Int, Int)
count = go 0 (0, 0, 0, mempty, mempty, 0)
  where
    go _ (e, o, t, l, b, f) Empty = (e + 1, o, t, l, b, f)
    go d (e, o, t, l, b, f) (One _ y) = go (1 + d) (e, 1 + o, t, l, b, f) y
    go d (e, o, t, l, b, f) (Two _ z y) = go (1 + d) (go (1 + d) (e, o, 1 + t, l, b, f) z) y
    go d (e, o, t, l, b, f) (Leaf _ _) = (e, o, t, add d l, b, f)
    go d (e, o, t, l, b, f) (BitmapIndexed _ arr) =
      foldl' (go (length arr + d)) (e, o, t, l, add (length arr) b, f) arr
    go d (e, o, t, l, b, f) (Full arr) = foldl' (go (length arr + d)) (e, o, t, l, b, f + 1) arr

-- KeyMap is designed for values of type Key which are true cryptographic hashes. This means they
-- are close to uniformly distributed in their 32 byte range. One way to test this is to compare the
-- actual depth of the tree with the log of its size. With Non uniform distrubution the depth of
-- the tree can be many times the log of its size (up to 'bitsPerSegment' times larger). It they are
-- even close to uniformly distributed it is highly likely (amost certain) that depth < log size.
keysAreUniform :: Int -> Int -> String -> String
keysAreUniform depth sz stats = if ok then stats else error message
  where
    ok = depth < ceiling (log (fromIntegral sz :: Double))
    message =
      "Keys are not uniformly distributed error: 'depth' is not less than '(log size)'\n" ++ stats

statisticString :: HeapWords a => KeyMap a -> String
statisticString keymap =
  keysAreUniform depth sz $
    unlines
      [ "Statistics for KeyMap of size " ++ show sz,
        "bits per level = " ++ show bitsPerSegment,
        "actual depth = " ++ show depth,
        "empty = " ++ show empty',
        "leaf  = " ++ show leaf,
        "one   = " ++ show one,
        "two   = " ++ show two,
        "bits  = " ++ show bit,
        "full  = " ++ show full,
        "hwords = " ++ show hwords,
        "branching factor histogram (they range from [0.."
          ++ show ((2 :: Int) ^ bitsPerSegment)
          ++ "]) =\n  "
          ++ show (histogram keymap)
      ]
  where
    (empty', one, two, leaf, bit, full) = count keymap
    hwords = heapWords keymap
    depth = hdepth keymap
    sz = size keymap

testStatistics :: Int -> IO String
testStatistics n = pure $ statisticString keymap
  where
    keys = makeKeys 199 n
    pairs = zip keys ([0 ..] :: [Int])
    keymap = fromList pairs

-- ============================================================

alltests :: TestTree
alltests =
  testGroup
    "KeyMap tests"
    [keyMapTests, keyMapEquivDataMap]

-- =======================================================
