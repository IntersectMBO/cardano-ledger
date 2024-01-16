{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Data.OSet.StrictSpec where

import Cardano.Ledger.Binary (natVersion)
import Control.Monad (forM_)
import Data.OSet.Strict hiding (empty)
import Data.Proxy
import Data.Sequence.Strict (StrictSeq, (><))
import qualified Data.Sequence.Strict as SSeq (fromList)
import Data.Set (Set, elems, empty)
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip (cborTrip, embedTripSpec, roundTripCborSpec)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes

spec :: Spec
spec =
  describe "OSet.Strict" $ do
    context "membership checks work" $ do
      prop "unconsed" $ \(s :: OSet Int) -> case s of
        Empty -> pure ()
        x :<|: _xs -> x `shouldSatisfy` (`member` s)
      prop "unsnoced" $ \(s :: OSet Int) -> case s of
        Empty -> pure ()
        _xs :|>: x -> x `shouldSatisfy` (`member` s)
    context "when cons-ing" $ do
      prop "adding a duplicate results in a no-op" $ \(s :: OSet Int) -> do
        case s of
          Empty -> pure ()
          x :<|: _xs -> s `shouldBe` x <| s
        case s of
          Empty -> pure ()
          _xs :|>: x -> s `shouldBe` x <| s
      prop "new values get added" $ \((s, i) :: (OSet Int, Int)) -> do
        if i `member` s
          then i <| s `shouldBe` s
          else i <| s `shouldBe` i :<|: s
    context "when snoc-ing" $ do
      prop "adding a duplicate results in a no-op" $ \(s :: OSet Int) -> do
        case s of
          Empty -> pure ()
          x :<|: _xs -> s `shouldBe` s |> x
        case s of
          Empty -> pure ()
          _xs :|>: x -> s `shouldBe` s |> x
      prop "new values get added" $ \((s, i) :: (OSet Int, Int)) -> do
        if i `member` s
          then s |> i `shouldBe` s
          else s |> i `shouldBe` s :|>: i
    context "mappend preserves uniqueness" $ do
      prop "mappending with itself should be a no-op" $ \(i :: OSet Int) -> do
        i |>< i `shouldBe` i
        i ><| i `shouldBe` i
      prop "mappending with duplicates: left-preserving" $ \((i, j) :: (OSet Int, OSet Int)) -> do
        case j of
          Empty -> i `shouldBe` i |>< j
          j' :<|: _js -> i |>< j `shouldBe` (i |> j') |>< j
      prop "mappending with duplicates: right-preserving" $ \((i, j) :: (OSet Int, OSet Int)) -> do
        case i of
          Empty -> i ><| j `shouldBe` j
          _is :|>: i' -> i ><| j `shouldBe` i ><| (i' <| j)
    prop "operations preserve invariant" $
      \((oset, oset', sseq, set) :: (OSet Int, OSet Int, StrictSeq Int, Set Int)) -> do
        oset |>< oset' `shouldSatisfy` invariantHolds'
        oset ><| oset' `shouldSatisfy` invariantHolds'
        fromStrictSeq sseq `shouldSatisfy` invariantHolds'
        fromSet set `shouldSatisfy` invariantHolds'
    prop "fromList preserves order" $
      \(set :: Set Int) ->
        let sseq = SSeq.fromList $ elems set
         in toStrictSeq (fromList (elems set)) `shouldBe` sseq
    context "fromStrictSeqDuplicates" $ do
      prop "with duplicates" $ \(set :: (Set Int)) ->
        let sseq = SSeq.fromList $ elems set
            oset = fromStrictSeq sseq
         in fromStrictSeqDuplicates (sseq >< sseq) `shouldBe` (set, oset)
      prop "without duplicates" $ \(set :: (Set Int)) ->
        let sseq = SSeq.fromList $ elems set
            oset = fromStrictSeq sseq
         in fromStrictSeqDuplicates sseq `shouldBe` (empty, oset)
    context "CBOR round-trip" $ do
      roundTripCborSpec @(OSet Int)
      roundTripCborSpec @(OSet (OSet Int))
      forM_ [natVersion @9 .. maxBound] $ \v -> do
        embedTripSpec v v (cborTrip @(Set Word) @(OSet Word)) $
          \oset set -> set `shouldBe` toSet oset
        embedTripSpec v v (cborTrip @(OSet Word) @(Set Word)) $
          \set oset -> toSet oset `shouldBe` set
    context "Typeclass laws" $ do
      it "Type" $
        lawsCheckOne
          (Proxy :: Proxy (OSet Int))
          [ eqLaws
          , ordLaws
          , isListLaws
          , semigroupLaws
          , monoidLaws
          , semigroupMonoidLaws
          ]
