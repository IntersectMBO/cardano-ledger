{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- surpresses orphan warnings on Arbitray (Value era),  Arbitrary AssetName,  Arbitrary (PolicyID C)

module Test.Cardano.Ledger.Mary.Value (valTests, ass, pol) where

import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
    insert,
    lookup,
  )
import Cardano.Ledger.Val (Val (..), invert)
import Data.ByteString (ByteString)
import Data.CanonicalMaps
  ( CanonicalZero (..),
    canonicalInsert,
    canonicalMapUnion,
  )
import Data.Map.Strict (empty, singleton)
import qualified Data.Map.Strict as Map
import Shelley.Spec.Ledger.Coin (Coin (..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)
import Prelude hiding (lookup)

-- =================================================================================
-- Alternate implementations of insert to be benchmarked.
-- Also used in testing below. to show that the executable spec: insert is correct.
-- We compute Values 3 ways and show all are equivalent.
-- =================================================================================

-- Use canonicalUnion and canonicalInsert

insert3 :: (Integer -> Integer -> Integer) -> PolicyID crypto -> AssetName -> Integer -> Value crypto -> Value crypto
insert3 combine pid aid new (Value c m1) =
  case Map.lookup pid m1 of
    Nothing -> Value c (canonicalInsert (canonicalMapUnion combine) pid (canonicalInsert combine aid new zeroC) m1)
    Just m2 -> case Map.lookup aid m2 of
      Nothing -> Value c (canonicalInsert (canonicalMapUnion combine) pid (singleton aid new) m1)
      Just old -> Value c (canonicalInsert (\_o n -> n) pid (canonicalInsert (\_o n -> n) aid (combine old new) m2) m1)

-- | Make a Value with no coin, and just one token.
unit :: PolicyID crypto -> AssetName -> Integer -> Value crypto
unit pid aid n = Value 0 (canonicalInsert (\_old new -> new) pid (canonicalInsert (\_old new -> new) aid n empty) empty)

-- Use <+> and <->

insert2 :: CC.Crypto crypto => (Integer -> Integer -> Integer) -> PolicyID crypto -> AssetName -> Integer -> Value crypto -> Value crypto
insert2 combine pid aid new m1 =
  case (lookup pid aid m1, new == 0) of -- The trick is to correctly not store a zero. Several ways to get a zero
    (0, True) -> m1 -- input is zero, and its not in the map
    (0, False) -> m1 <+> unit pid aid new -- input is not zero, its not in the map, so add it to the map
    (old, _) ->
      let n = combine old new -- it is in the map, use combine to get the correct value
       in m1 <+> unit pid aid (n - old) -- make the correction
      -- equivalent to: (m1 <->  unit pid aid old) <+> unit pid aid n

-- 3 functions that build Values from Policy Asset triples.

valueFromList :: [(PolicyID C_Crypto, AssetName, Integer)] -> Integer -> Value C_Crypto
valueFromList list c = foldr acc (Value c empty) list
  where
    acc (policy, asset, count) m = insert (+) policy asset count m

valueFromList3 :: [(PolicyID C_Crypto, AssetName, Integer)] -> Integer -> Value C_Crypto
valueFromList3 list c = foldr acc (Value c empty) list
  where
    acc (policy, asset, count) m = insert3 (+) policy asset count m

valueFromList2 :: [(PolicyID C_Crypto, AssetName, Integer)] -> Integer -> Value C_Crypto
valueFromList2 list c = foldr acc (Value c empty) list
  where
    acc (policy, asset, count) m = insert2 (+) policy asset count m

-- Test that all tree functions build the same values.

insertTests :: TestTree
insertTests =
  testGroup
    "insert == insert2 == insert3"
    [ testProperty "insert=insert2" $ \vs c -> (valueFromList vs c === valueFromList2 vs c),
      testProperty "insert=insert3" $ \vs c -> (valueFromList vs c === valueFromList3 vs c),
      testProperty "insert2=insert3" $ \vs c -> (valueFromList2 vs c === valueFromList3 vs c)
    ]

-- ============================================================================================
-- Arbitray instances

genB :: Gen ByteString
genB = resize 4 arbitrary

genID :: Gen AssetName
genID = fmap AssetName genB

-- we want a limited number of AssetName and (ScriptHash C)
assetChoices :: [AssetName]
assetChoices = unsafePerformIO (generate (vectorOf 8 genID))

ass :: Int -> AssetName
ass n = assetChoices !! n

policyChoices :: [PolicyID C_Crypto]
policyChoices = unsafePerformIO (generate (vectorOf 8 (PolicyID <$> arbitrary)))

pol :: Int -> PolicyID C_Crypto
pol n = policyChoices !! n

genAssetName :: Gen AssetName
genAssetName = oneof (map return assetChoices)

genPolicyID :: Gen (PolicyID C_Crypto)
genPolicyID = oneof (map return policyChoices)

genTriple :: Gen (PolicyID C_Crypto, AssetName, Integer)
genTriple = (,,) <$> genPolicyID <*> genAssetName <*> choose (-2, 4)

genMap :: Gen [(PolicyID C_Crypto, AssetName, Integer)] -- Most maps have 1 or 2 Assets
genMap =
  frequency
    [ (1, vectorOf 0 genTriple),
      (4, vectorOf 1 genTriple),
      (5, vectorOf 2 genTriple),
      (2, vectorOf 3 genTriple),
      (1, vectorOf 4 genTriple)
    ]

genValue :: Gen (Value C_Crypto)
genValue = valueFromList <$> genMap <*> choose (-2, 10)

instance Arbitrary (Value C_Crypto) where
  arbitrary = genValue
  shrink _ = []

instance Arbitrary AssetName where
  arbitrary = genAssetName

instance Arbitrary (PolicyID C_Crypto) where
  arbitrary = genPolicyID

-- ===========================================================================
-- Tests that Val instances really align with the Albelian Group Supertype

albelianlist :: forall v. (Show v, Val v) => [(v -> v -> Property, String)]
albelianlist =
  [ (\x y -> x <+> y === x <> y, "<+> is <>"),
    (\_ _ -> (zero @v) === mempty, "zero is mempty"),
    (\_ _ -> isZero (zero @v) === True, "isZero zero"),
    (\_ _ -> isZero (mempty @v) === True, "isZero mempty"),
    (\x _ -> isZero x === (x == mempty), "isZero is (== mempty)")
  ]

albelianTests :: TestTree
albelianTests =
  testGroup
    "albelian test"
    [ testGroup "albelian Coin" (map (\(prop, name) -> testProperty name prop) (albelianlist @Coin)),
      testGroup "albelian Value" (map (\(prop, name) -> testProperty name prop) (albelianlist @(Value C_Crypto)))
    ]

-- ===================================================================
-- Generic tests that should hold for all Val instances.
-- We will instantiate these twice. Once with Coin, once with Value.

proplist :: forall v. Val v => [(Integer -> Integer -> v -> v -> v -> Bool, String)]
proplist =
  -- (\ r s x y z -> prop , name)
  [ (\_ _ x y _ -> x <-> y == x <+> (invert y), "defMinus"),
    (\_ _ x _ _ -> invert x == (-1 :: Integer) <×> x, "defInvert"),
    (\_ _ x y _ -> x <+> y == y <+> x, "commute"),
    (\_ _ x y z -> x <+> (y <+> z) == (y <+> x) <+> z, "assoc"),
    (\_ _ x _ _ -> (zero <+> x == x <+> zero) && (zero <+> x == x), "addIdent"),
    (\_ _ _ _ _ -> (zero @v <+> zero) == zero, "zero-zero"),
    (\_ _ x _ _ -> x <-> x == zero, "cancel"),
    (\r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "distr1"),
    (\r s x _ _ -> (r + s) <×> x == (r <×> x) <+> (s <×> x), "dist2"),
    (\r s x _ _ -> (r * s) <×> x == r <×> (s <×> x), "distr3"),
    (\_ _ x _ _ -> (1 :: Integer) <×> x == x, "scaleIdenity"),
    (\_ _ x _ _ -> (x <-> x) == zero, "minusCancel"),
    (\_ _ x y _ -> ((x <+> y) <-> y == x <+> (y <-> y)) && (x <+> (y <-> y) == x), "plusMinusAssoc"),
    (\_ _ x _ _ -> (x <+> (invert x) == (x <-> x)) && (x <-> x == zero), "plusInvertCancel"),
    (\_ _ x _ _ -> (x <-> zero) == x, "minusZero"),
    (\_ _ x _ _ -> (zero <-> x) == invert x, "zeroMinus"),
    (\_ _ x _ _ -> invert x == (-1 :: Integer) <×> x, "invertScale"),
    (\_ _ x _ _ -> (0 :: Integer) <×> x == zero, "scaleZero"),
    (\r _ _ _ _ -> r <×> zero @v == zero @v, "zeroScale"),
    (\r s _ _ _ -> r <×> (inject @v (Coin s)) == inject @v (r <×> (Coin s)), "scaleInject"),
    (\_ _ x _ _ -> (1 :: Integer) <×> x == x, "scaleOne"),
    (\r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "scalePlus"),
    (\r s x _ _ -> r <×> (s <×> x) == (r * s) <×> x, "scaleScale"),
    (\r _ x _ _ -> r <×> (coin x) == coin (r <×> x), "scaleCoin"),
    (\_ _ x _ _ -> (3 :: Integer) <×> x == x <+> x <+> x, "unfoldScale"),
    (\_ _ _ _ _ -> coin (zero @v) == zero, "coinZero"),
    (\_ _ x y _ -> coin (x <+> y) == coin x <+> coin y, "coinPlus"),
    (\r _ x _ _ -> coin (r <×> x) == r <×> (coin x), "coinScale"),
    (\r _ _ _ _ -> coin @v (inject @v (Coin r)) == (Coin r), "coinInject"),
    (\_ _ _ _ _ -> pointwise (==) (zero @v) zero, "pointwise zero")
  ]

polyCoinTests :: TestTree
polyCoinTests = testGroup "polyCoinTests" (map f (proplist @Coin))
  where
    f (fun, name) = testProperty name fun

polyValueTests :: TestTree
polyValueTests = testGroup "polyValueTests" (map f (proplist @(Value C_Crypto)))
  where
    f (fun, name) = testProperty name fun

-- ============================================================================
-- Tests that hold only in the Value class.
-- Testing that insert, lookup, and coin interact properly

valuePropList :: [(Integer -> Integer -> Value C_Crypto -> PolicyID C_Crypto -> AssetName -> Bool, String)]
valuePropList =
  [ (\_ _ x _ _ -> coin (modifyCoin f x) == modifyCoin f (coin x), "coinModify"),
    (\_ _ _ p a -> insert (\old _new -> old) p a 0 zero == zero, "Nozeros"),
    (\_ _ x p a -> insert (\old _new -> old) p a 0 x == insert2 (\old _new -> old) p a 0 x, "insert==insert2A"),
    (\_ _ x p a -> insert (\_old new -> new) p a 0 x == insert2 (\_old new -> new) p a 0 x, "insert==insert2B"),
    (\_ _ x p a -> insert (\old _new -> old) p a 0 x == insert3 (\old _new -> old) p a 0 x, "insert==insert3A"),
    (\_ _ x p a -> insert (\_old new -> new) p a 0 x == insert3 (\_old new -> new) p a 0 x, "insert==insert3B"),
    ( \n _ _ p a ->
        insert (\_old new -> new) p a n zero
          == insert (\old _new -> old) p a n zero,
      "comb doesn't matter on zero"
    ),
    -- the following 4 laws only holds for non zero n and m, and when not(n==m).
    -- Zeros cause the inserts to be no-ops in that case.
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || (insert (\old _new -> old) p a m (insert (\_old new -> new) p a n zero))
          == (insert (\_old new -> new) p a n zero),
      "retains-old"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || (insert (\_old new -> new) p a m (insert (\_old new -> new) p a n zero))
          == (insert (\_old new -> new) p a m zero),
      "new-overrides"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || lookup p a (insert (\old _new -> old) p a m (insert (\_old new -> new) p a n zero)) == n,
      "oldVsNew"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || lookup p a (insert (\_old new -> new) p a m (insert (\_old new -> new) p a n zero)) == m,
      "newVsOld"
    ),
    (\n _ x p a -> lookup p a (insert (\_old new -> new) p a n x) == n, "lookup-insert-overwrite"),
    ( \n _ x p a ->
        lookup p a x == 0
          || lookup p a (insert (\old _new -> old) p a n x) == lookup p a x,
      "lookup-insert-retain"
    ),
    (\n _ x p a -> coin (insert (\old _new -> old) p a n x) == coin x, "coinIgnores1"),
    (\n _ x p a -> coin (insert (\_old new -> new) p a n x) == coin x, "coinIgnores2"),
    (\n _ x p a -> coin (insert (\old _new -> old + n) p a n x) == coin x, "coinIgnores3")
  ]
  where
    f (Coin n) = Coin (n + 3)

monoValueTests :: TestTree
monoValueTests = testGroup "Value specific tests" (map (\(f, n) -> testProperty n f) valuePropList)

valueGroup :: [(Integer -> Value C_Crypto -> Value C_Crypto -> PolicyID C_Crypto -> AssetName -> Property, String)]
valueGroup =
  [ (\_ x y p a -> lookup p a (x <+> y) === lookup p a x + lookup p a y, "lookup over <+>"),
    (\_ x y p a -> lookup p a (x <-> y) === lookup p a x - lookup p a y, "lookup over <->"),
    (\n x _ p a -> lookup p a (n <×> x) === n * lookup p a x, "lookup over <×>"),
    (\_ _ _ p a -> lookup p a zero === 0, "lookup over zero"),
    (\_ x _ p a -> lookup p a (invert x) === (-1) * lookup p a x, "lookup over invert")
  ]

valueGroupTests :: TestTree
valueGroupTests = testGroup "value is a group" (map (\(f, n) -> testProperty n f) valueGroup)

-- ===========================================
-- All the value tests

valTests :: TestTree
valTests =
  testGroup
    "allValTests"
    [ insertTests,
      albelianTests,
      polyCoinTests,
      polyValueTests,
      monoValueTests,
      valueGroupTests
    ]
