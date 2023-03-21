{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Mary.Value (valTests) where

import Cardano.Crypto.Hash.Class (castHash, hashFromStringAsHex)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.Value (
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  PolicyID (..),
  insertMultiAsset,
  lookupMultiAsset,
 )
import Cardano.Ledger.Val (Val (..), invert)
import Control.DeepSeq (rnf)
import Control.Monad (replicateM)
import Data.ByteString.Short (ShortByteString)
import Data.CanonicalMaps (
  CanonicalZero (..),
  canonicalInsert,
  canonicalMapUnion,
 )
import qualified Data.Group as G
import Data.Map.Strict (empty, singleton)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Test.Cardano.Ledger.Mary.Arbitrary ()
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

insertValue ::
  (Integer -> Integer -> Integer) ->
  PolicyID c ->
  AssetName ->
  Integer ->
  MaryValue c ->
  MaryValue c
insertValue combine pid aid new (MaryValue c m) = MaryValue c $ insertMultiAsset combine pid aid new m

insert3 ::
  (Integer -> Integer -> Integer) ->
  PolicyID c ->
  AssetName ->
  Integer ->
  MaryValue c ->
  MaryValue c
insert3 combine pid aid new (MaryValue c (MultiAsset m1)) =
  case Map.lookup pid m1 of
    Nothing ->
      MaryValue c $
        MultiAsset $
          canonicalInsert (canonicalMapUnion combine) pid (canonicalInsert combine aid new zeroC) m1
    Just m2 -> case Map.lookup aid m2 of
      Nothing ->
        MaryValue c $
          MultiAsset $
            canonicalInsert (canonicalMapUnion combine) pid (singleton aid new) m1
      Just old ->
        MaryValue c $
          MultiAsset $
            canonicalInsert pickNew pid (canonicalInsert pickNew aid (combine old new) m2) m1

-- | Make a Value with no coin, and just one token.
unit :: PolicyID c -> AssetName -> Integer -> MaryValue c
unit pid aid n = MaryValue 0 $ MultiAsset (canonicalInsert pickNew pid (canonicalInsert pickNew aid n empty) empty)

-- Use <+> and <->

insert2 ::
  CC.Crypto c =>
  (Integer -> Integer -> Integer) ->
  PolicyID c ->
  AssetName ->
  Integer ->
  MaryValue c ->
  MaryValue c
insert2 combine pid aid new m1 =
  -- The trick is to correctly not store a zero. Several ways to get a zero
  case (lookupMultiAsset pid aid m1, new == 0) of
    -- input is zero, and its not in the map
    (0, True) -> m1
    -- input is not zero, its not in the map, so add it to the map
    (0, False) -> m1 <+> unit pid aid new
    (old, _) ->
      let n = combine old new -- it is in the map, use combine to get the correct value
       in m1 <+> unit pid aid (n - old) -- make the correction
      -- equivalent to: (m1 <->  unit pid aid old) <+> unit pid aid n

-- 3 functions that build Values from Policy Asset triples.

valueFromList :: [(PolicyID StandardCrypto, AssetName, Integer)] -> Integer -> MaryValue StandardCrypto
valueFromList list c = MaryValue c $ foldr acc mempty list
  where
    acc (policy, asset, count) m = insertMultiAsset (+) policy asset count m

valueFromList3 :: [(PolicyID StandardCrypto, AssetName, Integer)] -> Integer -> MaryValue StandardCrypto
valueFromList3 list c = foldr acc (MaryValue c mempty) list
  where
    acc (policy, asset, count) m = insert3 (+) policy asset count m

valueFromList2 :: [(PolicyID StandardCrypto, AssetName, Integer)] -> Integer -> MaryValue StandardCrypto
valueFromList2 list c = foldr acc (MaryValue c mempty) list
  where
    acc (policy, asset, count) m = insert2 (+) policy asset count m

-- Test that all tree functions build the same values.

insertTests :: TestTree
insertTests =
  testGroup
    "insert == insert2 == insert3"
    [ testProperty "insert=insert2" $ \vs c -> valueFromList vs c === valueFromList2 vs c
    , testProperty "insert=insert3" $ \vs c -> valueFromList vs c === valueFromList3 vs c
    , testProperty "insert2=insert3" $ \vs c -> valueFromList2 vs c === valueFromList3 vs c
    ]

-- ============================================================================================
-- Arbitray instances

genB :: Gen ShortByteString
genB = resize 4 arbitrary

genAssetName :: Gen AssetName
genAssetName = AssetName <$> genB

genPolicyID :: Gen (PolicyID StandardCrypto)
genPolicyID = PolicyID <$> arbitrary

-- ===========================================================================
-- Tests that Val instances really align with the Albelian Group Supertype

albelianlist :: forall v. Val v => [(v -> v -> Property, String)]
albelianlist =
  [ (\x y -> x <+> y === x <> y, "<+> is <>")
  , (\_ _ -> (zero @v) === mempty, "zero is mempty")
  , (\_ _ -> isZero (zero @v) === True, "isZero zero")
  , (\_ _ -> isZero (mempty @v) === True, "isZero mempty")
  , (\x _ -> isZero x === (x == mempty), "isZero is (== mempty)")
  ]

albelianTests :: TestTree
albelianTests =
  testGroup
    "albelian test"
    [ testGroup "albelian Coin" $
        map (\(prop, name) -> testProperty name prop) (albelianlist @Coin)
    , testGroup "albelian Value" $
        map (\(prop, name) -> testProperty name prop) (albelianlist @(MaryValue StandardCrypto))
    ]

-- ===================================================================
-- Generic tests that should hold for all Val instances.
-- We will instantiate these twice. Once with Coin, once with Value.

proplist :: forall v. Val v => [(Integer -> Integer -> v -> v -> v -> Bool, String)]
proplist =
  -- (\ r s x y z -> prop , name)
  [ (\_ _ x y _ -> x <-> y == x <+> invert y, "defMinus")
  , (\_ _ x _ _ -> invert x == (-1 :: Integer) <×> x, "defInvert")
  , (\_ _ x y _ -> x <+> y == y <+> x, "commute")
  , (\_ _ x y z -> x <+> (y <+> z) == (y <+> x) <+> z, "assoc")
  , (\_ _ x _ _ -> (zero <+> x == x <+> zero) && (zero <+> x == x), "addIdent")
  , (\_ _ _ _ _ -> (zero @v <+> zero) == zero, "zero-zero")
  , (\_ _ x _ _ -> x <-> x == zero, "cancel")
  , (\r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "distr1")
  , (\r s x _ _ -> (r + s) <×> x == (r <×> x) <+> (s <×> x), "dist2")
  , (\r s x _ _ -> (r * s) <×> x == r <×> (s <×> x), "distr3")
  , (\_ _ x _ _ -> (1 :: Integer) <×> x == x, "scaleIdenity")
  , (\_ _ x _ _ -> (x <-> x) == zero, "minusCancel")
  , (\_ _ x y _ -> ((x <+> y) <-> y == x <+> (y <-> y)) && (x <+> (y <-> y) == x), "plusMinusAssoc")
  , (\_ _ x _ _ -> (x <+> invert x == (x <-> x)) && (x <-> x == zero), "plusInvertCancel")
  , (\_ _ x _ _ -> (x <-> zero) == x, "minusZero")
  , (\_ _ x _ _ -> (zero <-> x) == invert x, "zeroMinus")
  , (\_ _ x _ _ -> invert x == (-1 :: Integer) <×> x, "invertScale")
  , (\_ _ x _ _ -> (0 :: Integer) <×> x == zero, "scaleZero")
  , (\r _ _ _ _ -> r <×> zero @v == zero @v, "zeroScale")
  , (\r s _ _ _ -> r <×> inject @v (Coin s) == inject @v (r <×> Coin s), "scaleInject")
  , (\_ _ x _ _ -> (1 :: Integer) <×> x == x, "scaleOne")
  , (\r _ x y _ -> r <×> (x <+> y) == (r <×> x) <+> (r <×> y), "scalePlus")
  , (\r s x _ _ -> r <×> (s <×> x) == (r * s) <×> x, "scaleScale")
  , (\r _ x _ _ -> r <×> coin x == coin (r <×> x), "scaleCoin")
  , (\_ _ x _ _ -> (3 :: Integer) <×> x == x <+> x <+> x, "unfoldScale")
  , (\_ _ _ _ _ -> coin (zero @v) == zero, "coinZero")
  , (\_ _ x y _ -> coin (x <+> y) == coin x <+> coin y, "coinPlus")
  , (\r _ x _ _ -> coin (r <×> x) == r <×> coin x, "coinScale")
  , (\r _ _ _ _ -> coin @v (inject @v (Coin r)) == Coin r, "coinInject")
  , (\_ _ _ _ _ -> pointwise (==) (zero @v) zero, "pointwise zero")
  ]

polyCoinTests :: TestTree
polyCoinTests = testGroup "polyCoinTests" (map f (proplist @Coin))
  where
    f (fun, name) = testProperty name fun

polyValueTests :: TestTree
polyValueTests = testGroup "polyValueTests" (map f (proplist @(MaryValue StandardCrypto)))
  where
    f (fun, name) = testProperty name fun

-- ============================================================================
-- Tests that hold only in the Value class.
-- Testing that insert, lookup, and coin interact properly

valuePropList ::
  [(Integer -> Integer -> MaryValue StandardCrypto -> PolicyID StandardCrypto -> AssetName -> Bool, String)]
valuePropList =
  [ (\_ _ x _ _ -> coin (modifyCoin f x) == modifyCoin f (coin x), "coinModify")
  , (\_ _ _ p a -> insertValue pickOld p a 0 zero == zero, "Nozeros")
  , (\_ _ x p a -> insertValue pickOld p a 0 x == insert2 pickOld p a 0 x, "insert==insert2A")
  , (\_ _ x p a -> insertValue pickNew p a 0 x == insert2 pickNew p a 0 x, "insert==insert2B")
  , (\_ _ x p a -> insertValue pickOld p a 0 x == insert3 pickOld p a 0 x, "insert==insert3A")
  , (\_ _ x p a -> insertValue pickNew p a 0 x == insert3 pickNew p a 0 x, "insert==insert3B")
  ,
    ( \n _ _ p a -> insertValue pickNew p a n zero == insertValue pickOld p a n zero
    , "comb doesn't matter on zero"
    )
  , -- the following 4 laws only holds for non zero n and m, and when not(n==m).
    -- Zeros cause the inserts to be no-ops in that case.

    ( \n m _ p a ->
        n == 0
          || m == 0
          || n == m
          || (insertValue pickOld p a m (insertValue pickNew p a n zero))
            == (insertValue pickNew p a n zero)
    , "retains-old"
    )
  ,
    ( \n m _ p a ->
        n == 0
          || m == 0
          || n == m
          || (insertValue pickNew p a m (insertValue pickNew p a n zero))
            == (insertValue pickNew p a m zero)
    , "new-overrides"
    )
  ,
    ( \n m _ p a ->
        n == 0
          || m == 0
          || n == m
          || lookupMultiAsset p a (insertValue pickOld p a m (insertValue pickNew p a n zero)) == n
    , "oldVsNew"
    )
  ,
    ( \n m _ p a ->
        n == 0
          || m == 0
          || n == m
          || lookupMultiAsset p a (insertValue pickNew p a m (insertValue pickNew p a n zero)) == m
    , "newVsOld"
    )
  , (\n _ x p a -> lookupMultiAsset p a (insertValue pickNew p a n x) == n, "lookup-insert-overwrite")
  ,
    ( \n _ x p a ->
        lookupMultiAsset p a x == 0
          || lookupMultiAsset p a (insertValue pickOld p a n x) == lookupMultiAsset p a x
    , "lookup-insert-retain"
    )
  , (\n _ x p a -> coin (insertValue pickOld p a n x) == coin x, "coinIgnores1")
  , (\n _ x p a -> coin (insertValue pickNew p a n x) == coin x, "coinIgnores2")
  , (\n _ x p a -> coin (insertValue (\o _n -> o + n) p a n x) == coin x, "coinIgnores3")
  ]
  where
    f (Coin n) = Coin (n + 3)

monoValueTests :: TestTree
monoValueTests = testGroup "Value specific tests" (map (\(f, n) -> testProperty n f) valuePropList)

valueGroup ::
  [ ( Integer ->
      MaryValue StandardCrypto ->
      MaryValue StandardCrypto ->
      PolicyID StandardCrypto ->
      AssetName ->
      Property
    , String
    )
  ]
valueGroup =
  [ (\_ x y p a -> lookupMultiAsset p a (x <+> y) === lookupMultiAsset p a x + lookupMultiAsset p a y, "lookup over <+>")
  , (\_ x y p a -> lookupMultiAsset p a (x <-> y) === lookupMultiAsset p a x - lookupMultiAsset p a y, "lookup over <->")
  , (\n x _ p a -> lookupMultiAsset p a (n <×> x) === n * lookupMultiAsset p a x, "lookup over <×>")
  , (\_ _ _ p a -> lookupMultiAsset p a zero === 0, "lookup over zero")
  , (\_ x _ p a -> lookupMultiAsset p a (invert x) === (-1) * lookupMultiAsset p a x, "lookup over invert")
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

-- | Create a script hash of length 28 with 27 leading zeros followed by one hex-encoded byte
-- supplied by the caller.
makeScriptHash :: String -> ScriptHash StandardCrypto
makeScriptHash str =
  ScriptHash $ castHash (fromMaybe (error "Impossible") $ hashFromStringAsHex (pad <> str))
  where
    pad = replicate 54 '0'

oneNonameAsset :: Map.Map AssetName Integer
oneNonameAsset = Map.fromList [(AssetName "", 1)]

makeMultiAsset :: ScriptHash StandardCrypto -> MultiAsset StandardCrypto
makeMultiAsset sh = MultiAsset (Map.singleton (PolicyID sh) oneNonameAsset)

s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12 :: MultiAsset StandardCrypto
s0 = makeMultiAsset $ makeScriptHash "00"
s1 = makeMultiAsset $ makeScriptHash "01"
s2 = makeMultiAsset $ makeScriptHash "02"
s3 = makeMultiAsset $ makeScriptHash "03"
s4 = makeMultiAsset $ makeScriptHash "04"
s5 = makeMultiAsset $ makeScriptHash "05"
s6 = makeMultiAsset $ makeScriptHash "06"
s7 = makeMultiAsset $ makeScriptHash "07"
s8 = makeMultiAsset $ makeScriptHash "08"
s9 = makeMultiAsset $ makeScriptHash "09"
s10 = makeMultiAsset $ makeScriptHash "10"
s11 = makeMultiAsset $ makeScriptHash "11"
s12 = makeMultiAsset $ makeScriptHash "12"

exampleMultiAssets :: [MultiAsset StandardCrypto]
exampleMultiAssets = [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12]

-- | Test that the subtraction of Multi-assets (and the underlying 'CanonicalMaps')
-- is a total function.
-- This was used to diagnose https://github.com/input-output-hk/cardano-node/issues/4826
subtractionIsTotal :: TestTree
subtractionIsTotal = testProperty "multi-asset subtraction is total" $
  QC.withMaxSuccess 100000 $
    do
      shuffle1 <- take 12 <$> QC.shuffle exampleMultiAssets
      shuffle2 <- take 2 <$> QC.shuffle exampleMultiAssets
      let a = mconcat [m | MultiAsset m <- shuffle1]
          -- \^ here we chose to perform addition on the CanonicalMaps, as this is what
          -- happens during deserialization, giving us insight into how node-4826 could
          -- have occurred on mainnet (since we care about how the addition is associated).
          -- Note that the ledger does not manipulate instances of
          -- 'Value' and then store them in memory, since outputs are created by the user
          -- and only deserialized. In other words, it is only in the ledger rules themselves
          -- that we manipulate 'Value'.
          b = mconcat shuffle2
      pure $! rnf (MultiAsset a <> G.invert b)

-- | The test below was discovered by a failure of 'subtractionIsTotal'
-- using git sha bd359d3f745ca72242b2cd1208780c2787992b5f and --quickcheck-replay=649941
node4826Reproducible :: TestTree
node4826Reproducible =
  testProperty "node4826Reproducible" $
    let shuffle1 =
          [ makeMultiAsset $ makeScriptHash suffix
          | suffix <- ["10", "09", "11", "08", "01", "06", "03", "05", "04", "07", "02", "00"]
          ]
        shuffle2 =
          [ makeMultiAsset $ makeScriptHash suffix
          | suffix <- ["04", "08"]
          ]
        multiAssetMap = mconcat [m | MultiAsset m <- shuffle1]
        reproducible = MultiAsset multiAssetMap <> G.invert (mconcat shuffle2)
     in rnf reproducible

-- ===========================================
-- All the value tests

valTests :: TestTree
valTests =
  testGroup
    "allValTests"
    [ insertTests
    , albelianTests
    , polyCoinTests
    , polyValueTests
    , monoValueTests
    , valueGroupTests
    , compactTest
    , subtractionIsTotal
    , node4826Reproducible
    ]
