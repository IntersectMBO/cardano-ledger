{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-}
   -- surpresses orphan warnings on Arbitray (Value era),  Arbitrary AssetID,  Arbitrary (PolicyID C)


module Test.Shelley.Spec.Ledger.ValProp(valTests,ass,pol) where

import Prelude hiding (lookup)
import System.IO.Unsafe(unsafePerformIO)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(empty,singleton)
import Cardano.Ledger.Era
import Cardano.Ledger.Val( Val (..), invert )
import Cardano.Ledger.ShelleyMA.ValueInternal
  ( Value,
    ASSET(Value),
    AssetID(..),
    PolicyID(..),
    lookup,
    insert,
    cannonicalInsert,
    cannonicalMapUnion,
    CanonicalZero(..),
  )
import Shelley.Spec.Ledger.Coin (Coin, ASSET(Coin))
import Test.Shelley.Spec.Ledger.Serialisation.Generators() -- get: instance Era era => Arbitrary (ScriptHash era)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(C)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)

-- =================================================================================
-- Alternate implementations of insert to be benchmarked.
-- Also used in testing below. to show that the executable spec: insert is correct.
-- We compute Values 3 ways and show all are equivalent.
-- =================================================================================

-- Use cannonicalUnion and cannonicalInsert

insert3 :: (Integer -> Integer -> Integer) -> PolicyID era -> AssetID -> Integer -> Value era -> Value era
insert3 combine pid aid new (Value c m1) =
  case Map.lookup pid m1 of
    Nothing -> Value c (cannonicalInsert (cannonicalMapUnion combine) pid (cannonicalInsert combine aid new zeroC) m1)
    Just m2 -> case Map.lookup aid m2 of
      Nothing -> Value c (cannonicalInsert (cannonicalMapUnion combine) pid (singleton aid new) m1)
      Just old -> Value c (cannonicalInsert (\ _o n -> n) pid (cannonicalInsert (\ _o n -> n) aid (combine old new) m2) m1)


-- | Make a Value with no coin, and just one token.
unit :: PolicyID era -> AssetID -> Integer -> Value era
unit pid aid n = Value 0 (cannonicalInsert (\ _old new -> new) pid (cannonicalInsert (\ _old new -> new) aid n empty) empty)


-- Use <+> and <->

insert2 :: Era era => (Integer -> Integer -> Integer) -> PolicyID era -> AssetID -> Integer -> Value era -> Value era
insert2 combine pid aid new m1 =
  case (lookup pid aid m1, new == 0) of -- The trick is to correctly not store a zero. Several ways to get a zero
    (0, True) -> m1                                -- input is zero, and its not in the map
    (0, False) -> m1 <+> unit pid aid new          -- input is not zero, its not in the map, so add it to the map
    (old, _) -> let n = combine old new            -- it is in the map, use combine to get the correct value
                in  m1 <+> unit pid aid (n - old)  -- make the correction
                                                   -- equivalent to: (m1 <->  unit pid aid old) <+> unit pid aid n

-- 3 functions that build Values from Policy Asset triples.

valueFromList :: [(PolicyID C, AssetID, Integer)] -> Integer -> Value C
valueFromList list c = foldr acc (Value c empty) list
  where acc (policy,asset,count) m = insert (+) policy asset count m

valueFromList3 :: [(PolicyID C, AssetID, Integer)] -> Integer -> Value C
valueFromList3 list c = foldr acc (Value c empty) list
  where acc (policy,asset,count) m = insert3 (+) policy asset count m

valueFromList2 :: [(PolicyID C, AssetID, Integer)] -> Integer -> Value C
valueFromList2 list c = foldr acc (Value c empty) list
  where acc (policy,asset,count) m = insert2 (+) policy asset count m

-- Test that all tree functions build the same values.

insertTests :: TestTree
insertTests = testGroup "insert == insert2 == insert3"
                 [ testProperty "insert=insert2" $ \ vs c ->  (valueFromList vs c === valueFromList2 vs c)
                 , testProperty "insert=insert3" $ \ vs c ->  (valueFromList vs c === valueFromList3 vs c)
                 , testProperty "insert2=insert3" $ \ vs c -> (valueFromList2 vs c === valueFromList3 vs c)
                 ]

-- ============================================================================================
-- Arbitray instances

genB :: Gen ByteString
genB = resize 4 arbitrary

genID :: Gen AssetID
genID = fmap AssetID genB

-- we want a limited number of AssetID and (ScriptHash C)
assetChoices:: [ AssetID ]
assetChoices = unsafePerformIO(generate(vectorOf 8 genID))

ass :: Int -> AssetID
ass n = assetChoices !! n

policyChoices :: [ PolicyID C ]
policyChoices = unsafePerformIO(generate(vectorOf 8 (PolicyID <$> arbitrary)))

pol :: Int -> PolicyID C
pol n = policyChoices !! n

genAssetID :: Gen AssetID
genAssetID =  oneof (map return assetChoices)

genPolicyID :: Gen (PolicyID C)
genPolicyID = oneof (map return policyChoices)

genTriple :: Gen (PolicyID C, AssetID, Integer)
genTriple = (,,) <$> genPolicyID  <*> genAssetID <*> choose (-2,4)

genMap :: Gen [(PolicyID C, AssetID, Integer)]  -- Most maps have 1 or 2 Assets
genMap = frequency [(1,vectorOf 0 genTriple),
                    (4,vectorOf 1 genTriple),
                    (5,vectorOf 2 genTriple),
                    (2,vectorOf 3 genTriple),
                    (1,vectorOf 4 genTriple)]

genValue :: Gen (Value C)
genValue = valueFromList <$> genMap <*> choose(-2,10)

instance Arbitrary (Value C) where
  arbitrary = genValue
  shrink _ = []

instance Arbitrary AssetID where
  arbitrary = genAssetID

instance Arbitrary (PolicyID C) where
  arbitrary = genPolicyID

-- ===========================================================================
-- Tests that Val instances really align with the Albelian Group Supertype

albelianlist :: forall v. (Show v,Val v) => [( v -> v -> Property,String)]
albelianlist =
  [ (\ x y -> x<+>y === x <> y, "<+> is <>")
  , (\ _ _ -> (zero @v) === mempty, "zero is mempty")
  , (\ _ _ -> isZero (zero @v) === True, "isZero zero")
  , (\ _ _ -> isZero (mempty @v) === True, "isZero mempty")
  , (\ x _ -> isZero x ===  (x == mempty), "isZero is (== mempty)")
  ]

albelianTests :: TestTree
albelianTests =
    testGroup "albelian test"
       [ testGroup "albelian Coin" (map (\ (prop,name) -> testProperty name prop) (albelianlist @Coin))
       , testGroup "albelian Value" (map (\ (prop,name) -> testProperty name prop) (albelianlist @(Value C)))
       ]

-- ===================================================================
-- Generic tests that should hold for all Val instances.
-- We will instantiate these twice. Once with Coin, once with Value.

proplist :: forall v. Val v => [(Integer -> Integer -> v -> v -> v -> Bool,String)]
proplist =
  -- (\ r s x y z -> prop , name)
   [ (\ _ _ x y _ -> x <-> y == x <+> (invert y),"defMinus")
   , (\ _ _ x _ _ -> invert x == (-1 ::Integer) <×> x,"defInvert")
   , (\ _ _ x y _ -> x <+> y == y <+> x, "commute")
   , (\ _ _ x y z -> x <+> (y <+> z) == (y <+> x) <+> z, "assoc")
   , (\ _ _ x _ _ -> (zero <+> x == x <+> zero) && (zero <+> x == x),"addIdent")
   , (\ _ _ _ _ _ -> (zero @v  <+> zero) == zero, "zero-zero")
   , (\ _ _ x _ _ -> x <-> x == zero, "cancel")
   , (\ r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y),"distr1")
   , (\ r s x _ _ -> (r + s) <×> x == (r <×> x) <+> (s <×> x),"dist2")
   , (\ r s x _ _ -> (r * s) <×> x == r <×> (s <×> x), "distr3")
   , (\ _ _ x _ _ -> (1::Integer) <×> x == x, "scaleIdenity")
   , (\ _ _ x _ _ -> (x <-> x) == zero, "minusCancel")
   , (\ _ _ x y _ -> ((x <+> y) <-> y == x <+> (y <-> y)) && (x <+> (y <-> y) == x), "plusMinusAssoc")
   , (\ _ _ x _ _ -> (x <+> (invert x) == (x <-> x)) && (x <-> x == zero), "plusInvertCancel")
   , (\ _ _ x _ _ -> (x <-> zero) == x,"minusZero")
   , (\ _ _ x _ _ -> (zero <-> x) == invert x, "zeroMinus")
   , (\ _ _ x _ _ -> invert x == (-1 :: Integer)  <×> x, "invertScale")
   , (\ _ _ x _ _ -> (0 ::Integer) <×> x == zero, "scaleZero")
   , (\ r _ _ _ _ -> r <×> zero @v  == zero @v , "zeroScale")
   , (\ r s _ _ _ -> r <×> (inject @v (Coin s)) == inject @v (r <×> (Coin s)), "scaleInject")
   , (\ _ _ x _ _ -> (1::Integer) <×> x == x, "scaleOne")
   , (\ r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "scalePlus")
   , (\ r s x _ _ -> r <×> (s <×> x) == (r * s) <×> x, "scaleScale")
   , (\ r _ x _ _ -> r <×> (coin x) == coin (r <×> x), "scaleCoin")
   , (\ _ _ x _ _ -> (3::Integer) <×> x == x <+> x <+> x, "unfoldScale")
   , (\ _ _ _ _ _ -> coin (zero @v) == zero, "coinZero")
   , (\ _ _ x y _ -> coin (x <+> y) == coin x <+> coin y, "coinPlus")
   , (\ r _ x _ _ -> coin (r <×> x) == r <×> (coin x), "coinScale")
   , (\ r _ _ _ _ -> coin @v (inject @v (Coin r)) == (Coin r), "coinInject")
   , (\ _ _ _ _ _ -> pointwise (==) (zero @v) zero, "pointwise zero")

   ]

polyCoinTests :: TestTree
polyCoinTests = testGroup "polyCoinTests" (map f (proplist @Coin))
   where f (fun,name) = testProperty name fun

polyValueTests :: TestTree
polyValueTests = testGroup "polyValueTests" (map f (proplist @(Value C)))
   where f (fun,name) = testProperty name fun

-- ============================================================================
-- Tests that hold only in the Value class.
-- Testing that insert, lookup, and coin interact properly

valuePropList :: [(Integer -> Integer -> Value C -> PolicyID C -> AssetID -> Bool,String)]
valuePropList =
    [ (\ _ _ x _ _ -> coin (modifyCoin f x) == modifyCoin f (coin x), "coinModify")
    , (\ _ _ _ p a -> insert (\ old _new -> old) p a 0 zero == zero,"Nozeros")
    , (\ _ _ x p a -> insert (\ old _new -> old) p a 0 x == insert2 (\ old _new -> old) p a 0 x, "insert==insert2A")
    , (\ _ _ x p a -> insert (\ _old new -> new) p a 0 x == insert2 (\ _old new -> new) p a 0 x, "insert==insert2B")
    , (\ _ _ x p a -> insert (\ old _new -> old) p a 0 x == insert3 (\ old _new -> old) p a 0 x, "insert==insert3A")
    , (\ _ _ x p a -> insert (\ _old new -> new) p a 0 x == insert3 (\ _old new -> new) p a 0 x, "insert==insert3B")
    , (\ n _ _ p a -> insert (\ _old new -> new) p a n zero ==
                    insert (\ old _new -> old) p a n zero, "comb doesn't matter on zero")
      -- the following 4 laws only holds for non zero n and m, and when not(n==m).
      -- Zeros cause the inserts to be no-ops in that case.
    , (\ n m _ p a -> n==0 || m==0 || n==m ||
                      (insert (\ old _new -> old) p a m (insert (\ _old new -> new) p a n zero)) ==
                      (insert (\ _old new -> new) p a n zero), "retains-old")
    , (\ n m _ p a -> n==0 || m==0 || n==m ||
                      (insert (\ _old new -> new) p a m (insert (\ _old new -> new) p a n zero)) ==
                      (insert (\ _old new -> new) p a m zero), "new-overrides")
    , (\ n m _ p a ->  n==0 || m==0 || n==m ||
                       lookup p a (insert (\ old _new -> old) p a m (insert (\ _old new -> new) p a n zero)) == n, "oldVsNew")
    , (\ n m _ p a ->  n==0 || m==0 || n==m ||
                       lookup p a (insert (\ _old new -> new) p a m (insert (\ _old new -> new) p a n zero)) == m, "newVsOld")
    , (\ n _ x p a -> lookup p a (insert (\ _old new -> new) p a n x) == n, "lookup-insert-overwrite")
    , (\ n _ x p a -> lookup p a x == 0 ||
                      lookup p a (insert (\ old _new -> old) p a n x) == lookup p a x, "lookup-insert-retain")
    , (\ n _ x p a -> coin (insert (\ old _new -> old) p a n x) == coin x, "coinIgnores1")
    , (\ n _ x p a -> coin (insert (\ _old new -> new) p a n x) == coin x, "coinIgnores2")
    , (\ n _ x p a -> coin (insert (\ old _new -> old+n) p a n x) == coin x, "coinIgnores3")
    ]
  where f (Coin n) = Coin (n+3)

monoValueTests :: TestTree
monoValueTests = testGroup "Value specific tests" (map (\ (f,n) -> testProperty n f) valuePropList)

valueGroup :: [(Integer -> Value C -> Value C -> PolicyID C -> AssetID -> Property,String)]
valueGroup =
    [ (\ _ x y p a -> lookup p a (x <+> y) === lookup p a x + lookup p a y, "lookup over <+>")
    , (\ _ x y p a -> lookup p a (x <-> y) === lookup p a x - lookup p a y, "lookup over <->")
    , (\ n x _ p a -> lookup p a (n <×> x) === n * lookup p a x, "lookup over <×>")
    , (\ _ _ _ p a -> lookup p a zero === 0, "lookup over zero")
    , (\ _ x _ p a -> lookup p a (invert x) === (-1) * lookup p a x, "lookup over invert")
    ]

valueGroupTests :: TestTree
valueGroupTests = testGroup "value is a group" (map (\ (f,n) -> testProperty n f) valueGroup)

-- ===========================================
-- All the value tests

valTests :: TestTree
valTests = testGroup "allValTests"
             [ insertTests,
               albelianTests,
               polyCoinTests,
               polyValueTests,
               monoValueTests,
               valueGroupTests]
