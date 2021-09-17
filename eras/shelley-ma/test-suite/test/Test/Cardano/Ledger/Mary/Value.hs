{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- surpresses orphan warnings on Arbitray (Value era),  Arbitrary AssetName,  Arbitrary (PolicyID C)

module Test.Cardano.Ledger.Mary.Value (valTests) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
    insert,
    lookup,
  )
import Cardano.Ledger.Val (Val (..), invert)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.CanonicalMaps
  ( CanonicalZero (..),
    canonicalInsert,
    canonicalMapUnion,
  )
import Data.Map.Strict (empty, singleton)
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)
import Prelude hiding (lookup)

-- =================================================================================
-- Alternate implementations of insert to be benchmarked.
-- Also used in testing below. to show that the executable spec: insert is correct.
-- We compute Values 3 ways and show all are equivalent.
-- =================================================================================

-- Use canonicalUnion and canonicalInsert

pickNew, pickOld :: a -> a -> a
pickNew _o n = n
pickOld o _n = o

insert3 ::
  (Integer -> Integer -> Integer) ->
  PolicyID crypto ->
  AssetName ->
  Integer ->
  Value crypto ->
  Value crypto
insert3 combine pid aid new (Value c m1) =
  case Map.lookup pid m1 of
    Nothing ->
      Value c $
        canonicalInsert (canonicalMapUnion combine) pid (canonicalInsert combine aid new zeroC) m1
    Just m2 -> case Map.lookup aid m2 of
      Nothing ->
        Value c $
          canonicalInsert (canonicalMapUnion combine) pid (singleton aid new) m1
      Just old ->
        Value c $
          canonicalInsert pickNew pid (canonicalInsert pickNew aid (combine old new) m2) m1

-- | Make a Value with no coin, and just one token.
unit :: PolicyID crypto -> AssetName -> Integer -> Value crypto
unit pid aid n = Value 0 (canonicalInsert pickNew pid (canonicalInsert pickNew aid n empty) empty)

-- Use <+> and <->

insert2 ::
  CC.Crypto crypto =>
  (Integer -> Integer -> Integer) ->
  PolicyID crypto ->
  AssetName ->
  Integer ->
  Value crypto ->
  Value crypto
insert2 combine pid aid new m1 =
  -- The trick is to correctly not store a zero. Several ways to get a zero
  case (lookup pid aid m1, new == 0) of
    -- input is zero, and its not in the map
    (0, True) -> m1
    -- input is not zero, its not in the map, so add it to the map
    (0, False) -> m1 <+> unit pid aid new
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
    [ testProperty "insert=insert2" $ \vs c -> valueFromList vs c === valueFromList2 vs c,
      testProperty "insert=insert3" $ \vs c -> valueFromList vs c === valueFromList3 vs c,
      testProperty "insert2=insert3" $ \vs c -> valueFromList2 vs c === valueFromList3 vs c
    ]

-- ============================================================================================
-- Arbitray instances

genB :: Gen ByteString
genB = resize 4 arbitrary

genAssetName :: Gen AssetName
genAssetName = fmap AssetName genB

genPolicyID :: Gen (PolicyID C_Crypto)
genPolicyID = PolicyID <$> arbitrary

genTriple :: Gen Integer -> Gen (PolicyID C_Crypto, AssetName, Integer)
genTriple genAmount = (,,) <$> genPolicyID <*> genAssetName <*> genAmount

-- Most maps have 1 or 2 Assets
genMap :: Gen Integer -> Gen [(PolicyID C_Crypto, AssetName, Integer)]
genMap genAmount = do
  len <- frequency [(1, pure 0), (4, pure 1), (5, pure 2), (2, pure 3), (1, pure 4)]
  vectorOf len (genTriple genAmount)

genValue :: Gen Integer -> Gen (Value C_Crypto)
genValue genAmount = valueFromList <$> genMap genAmount <*> genAmount

instance Arbitrary (Value C_Crypto) where
  arbitrary = genValue (choose (-2, 10))
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
    [ testGroup "albelian Coin" $
        map (\(prop, name) -> testProperty name prop) (albelianlist @Coin),
      testGroup "albelian Value" $
        map (\(prop, name) -> testProperty name prop) (albelianlist @(Value C_Crypto))
    ]

-- ===================================================================
-- Generic tests that should hold for all Val instances.
-- We will instantiate these twice. Once with Coin, once with Value.

proplist :: forall v. Val v => [(Integer -> Integer -> v -> v -> v -> Bool, String)]
proplist =
  -- (\ r s x y z -> prop , name)
  [ (\_ _ x y _ -> x <-> y == x <+> invert y, "defMinus"),
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
    (\_ _ x _ _ -> (x <+> invert x == (x <-> x)) && (x <-> x == zero), "plusInvertCancel"),
    (\_ _ x _ _ -> (x <-> zero) == x, "minusZero"),
    (\_ _ x _ _ -> (zero <-> x) == invert x, "zeroMinus"),
    (\_ _ x _ _ -> invert x == (-1 :: Integer) <×> x, "invertScale"),
    (\_ _ x _ _ -> (0 :: Integer) <×> x == zero, "scaleZero"),
    (\r _ _ _ _ -> r <×> zero @v == zero @v, "zeroScale"),
    (\r s _ _ _ -> r <×> inject @v (Coin s) == inject @v (r <×> Coin s), "scaleInject"),
    (\_ _ x _ _ -> (1 :: Integer) <×> x == x, "scaleOne"),
    (\r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "scalePlus"),
    (\r s x _ _ -> r <×> (s <×> x) == (r * s) <×> x, "scaleScale"),
    (\r _ x _ _ -> r <×> coin x == coin (r <×> x), "scaleCoin"),
    (\_ _ x _ _ -> (3 :: Integer) <×> x == x <+> x <+> x, "unfoldScale"),
    (\_ _ _ _ _ -> coin (zero @v) == zero, "coinZero"),
    (\_ _ x y _ -> coin (x <+> y) == coin x <+> coin y, "coinPlus"),
    (\r _ x _ _ -> coin (r <×> x) == r <×> coin x, "coinScale"),
    (\r _ _ _ _ -> coin @v (inject @v (Coin r)) == Coin r, "coinInject"),
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

valuePropList ::
  [(Integer -> Integer -> Value C_Crypto -> PolicyID C_Crypto -> AssetName -> Bool, String)]
valuePropList =
  [ (\_ _ x _ _ -> coin (modifyCoin f x) == modifyCoin f (coin x), "coinModify"),
    (\_ _ _ p a -> insert pickOld p a 0 zero == zero, "Nozeros"),
    (\_ _ x p a -> insert pickOld p a 0 x == insert2 pickOld p a 0 x, "insert==insert2A"),
    (\_ _ x p a -> insert pickNew p a 0 x == insert2 pickNew p a 0 x, "insert==insert2B"),
    (\_ _ x p a -> insert pickOld p a 0 x == insert3 pickOld p a 0 x, "insert==insert3A"),
    (\_ _ x p a -> insert pickNew p a 0 x == insert3 pickNew p a 0 x, "insert==insert3B"),
    ( \n _ _ p a -> insert pickNew p a n zero == insert pickOld p a n zero,
      "comb doesn't matter on zero"
    ),
    -- the following 4 laws only holds for non zero n and m, and when not(n==m).
    -- Zeros cause the inserts to be no-ops in that case.
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || (insert pickOld p a m (insert pickNew p a n zero))
          == (insert pickNew p a n zero),
      "retains-old"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || (insert pickNew p a m (insert pickNew p a n zero))
          == (insert pickNew p a m zero),
      "new-overrides"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || lookup p a (insert pickOld p a m (insert pickNew p a n zero)) == n,
      "oldVsNew"
    ),
    ( \n m _ p a ->
        n == 0 || m == 0 || n == m
          || lookup p a (insert pickNew p a m (insert pickNew p a n zero)) == m,
      "newVsOld"
    ),
    (\n _ x p a -> lookup p a (insert pickNew p a n x) == n, "lookup-insert-overwrite"),
    ( \n _ x p a ->
        lookup p a x == 0
          || lookup p a (insert pickOld p a n x) == lookup p a x,
      "lookup-insert-retain"
    ),
    (\n _ x p a -> coin (insert pickOld p a n x) == coin x, "coinIgnores1"),
    (\n _ x p a -> coin (insert pickNew p a n x) == coin x, "coinIgnores2"),
    (\n _ x p a -> coin (insert (\o _n -> o + n) p a n x) == coin x, "coinIgnores3")
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

compactRoundTrip :: Property
compactRoundTrip = forAll gen $ \v ->
  counterexample
    (show $ toCompact v)
    (Just v === fmap fromCompact (toCompact v))
  where
    gen = do
      pids <- replicateM 3 (pure <$> genPolicyID)
      ans <- replicateM 3 (pure <$> genAssetName)
      -- this ensures we get some collisions among asset names and among pids
      numTriples <- QC.choose (3, 30)
      triples <- replicateM numTriples $ do
        pid <- QC.oneof pids
        an <- QC.oneof ans
        q <- QC.choose (0, 100)
        pure (pid, an, q)
      q <- QC.choose (0, 100)
      pure (valueFromList triples q)

compactTest :: TestTree
compactTest = testProperty "fromCompact . toCompact == id" compactRoundTrip

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
      valueGroupTests,
      compactTest
    ]
