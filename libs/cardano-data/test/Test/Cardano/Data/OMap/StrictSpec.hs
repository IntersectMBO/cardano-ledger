{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Data.OMap.StrictSpec where

import Data.OMap.Strict
import Data.Proxy (Proxy (Proxy))
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import Lens.Micro hiding (set)
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Classes
import Prelude hiding (elem, filter, lookup, null)

spec :: Spec
spec =
  describe "OMap.Strict" $ do
    context "membership checks work" $ do
      prop "unconsed" $
        \(m :: OMap Int Int) -> case m of
          Empty -> pure ()
          v :<|: _kv -> v ^. okeyL `shouldSatisfy` (`member` m)
      prop "unsnoced" $
        \(m :: OMap Int Int) -> case m of
          Empty -> pure ()
          _kv :|>: v -> v ^. okeyL `shouldSatisfy` (`member` m)
    context "when cons-ing" $ do
      prop "adding a duplicate results in a no-op" $
        \(m :: OMap Int Int) -> do
          case m of
            Empty -> pure ()
            v :<|: _kv -> m `shouldBe` v <| m
          case m of
            Empty -> pure ()
            _kv :|>: v -> m `shouldBe` v <| m
      prop "new values get added" $
        \((m, v) :: (OMap Int Int, Int)) ->
          if v `elem` m
            then v <| m `shouldBe` m
            else v <| m `shouldBe` v :<|: m
    context "when snoc-ing" $ do
      prop "adding a duplicate results in a no-op" $
        \(m :: OMap Int Int) -> do
          case m of
            Empty -> pure ()
            v :<|: _kv -> m `shouldBe` m |> v
          case m of
            Empty -> pure ()
            _kv :|>: v -> m `shouldBe` m |> v
      prop "new values get added" $
        \((m, v) :: (OMap Int Int, Int)) ->
          if v `elem` m
            then m |> v `shouldBe` m
            else m |> v `shouldBe` m :|>: v
    context "mappend preserves uniqueness" $ do
      prop "mappending with itself should be a no-op" $
        \(i :: OMap Int Int) -> do
          let il = i |>< i
              ir = i ><| i
          il `shouldBe` i
          ir `shouldBe` i
          il `shouldSatisfy` invariantHolds'
          ir `shouldSatisfy` invariantHolds'
      prop "mappending with duplicates: left-preserving" $
        \((i, j) :: (OMap Int Int, OMap Int Int)) ->
          case j of
            Empty -> i `shouldBe` i |>< j
            j' :<|: _js -> do
              let result = i |>< j
              result `shouldBe` (i |> j') |>< j
              result `shouldSatisfy` invariantHolds'
      prop "mappending with duplicates: right-preserving" $
        \((i, j) :: (OMap Int Int, OMap Int Int)) ->
          case i of
            Empty -> i ><| j `shouldBe` j
            _is :|>: i' -> do
              let result = i ><| j
              result `shouldBe` i ><| (i' <| j)
              result `shouldSatisfy` invariantHolds'
    prop "extractKeys should satisfy membership" $
      \((omap, set) :: (OMap Int Int, Set.Set Int)) -> do
        let result = extractKeys set omap
        result `shouldSatisfy` (all (`Set.notMember` set) . fst)
        result `shouldSatisfy` (all (`Set.member` set) . snd)
        result `shouldSatisfy` invariantHolds' . fst
    prop "filter" $
      \((omap, i) :: (OMap Int Int, Int)) -> do
        let result = filter (< i) omap
        result `shouldSatisfy` all (< i)
        result `shouldSatisfy` invariantHolds'
    prop "adjust" $
      \((omap, i) :: (OMap Int OMapTest, Int)) -> do
        let adjustingFn omt@OMapTest {omSnd} = omt {omSnd = omSnd + 1}
            overwritingAdjustingFn omt@OMapTest {omFst} = omt {omFst = omFst + 1} -- Changes the `okeyL`.
        adjust adjustingFn i omap `shouldSatisfy` invariantHolds'
        adjust overwritingAdjustingFn i omap `shouldSatisfy` invariantHolds'
    context "overwriting" $ do
      prop "cons' - (<||)" $
        \((omap, i) :: (OMap Int OMapTest, OMapTest)) -> do
          let consed = i <|| omap
              k = i ^. okeyL
          if k `member` omap
            then consed `shouldBe` adjust (const i) (i ^. okeyL) omap
            else consed `shouldBe` i <| omap
      prop "snoc' - (||>)" $
        \((omap, i) :: (OMap Int OMapTest, OMapTest)) -> do
          let snoced = omap ||> i
              k = i ^. okeyL
          if k `member` omap
            then snoced `shouldBe` adjust (const i) (i ^. okeyL) omap
            else snoced `shouldBe` omap |> i
    prop "fromFoldable preserves order" $
      \(set :: Set.Set Int) -> do
        let sseq = SSeq.fromList $ Set.elems set
            omap = fromFoldable sseq
        toStrictSeq omap `shouldBe` sseq
        omap `shouldSatisfy` invariantHolds'
    context "fromFoldableDuplicates preserves order" $ do
      prop "with duplicates" $
        \(set :: Set.Set Int) -> do
          let sseq = SSeq.fromList $ Set.elems set
              omap = fromFoldable sseq
              result = fromFoldableDuplicates (sseq SSeq.>< sseq)
          toStrictSeq (snd result) `shouldBe` sseq
          result `shouldBe` (set, omap)
          snd result `shouldSatisfy` invariantHolds'
      prop "without duplicates" $
        \(set :: Set.Set Int) -> do
          let sseq = SSeq.fromList $ Set.elems set
              omap = fromFoldable sseq
              result = fromFoldableDuplicates sseq
          toStrictSeq (snd result) `shouldBe` sseq
          result `shouldBe` (Set.empty, omap)
          snd result `shouldSatisfy` invariantHolds'
    context "CBOR round-trip" $ do
      roundTripCborSpec @(OMap Int Int)
    context "Typeclass laws" $ do
      it "Type" $
        lawsCheckOne
          (Proxy :: Proxy (OMap Int Int))
          [ eqLaws
          , isListLaws
          , semigroupLaws
          , monoidLaws
          , semigroupMonoidLaws
          ]

instance HasOKey Int Int where
  okeyL = lens id const

data OMapTest = OMapTest {omFst :: Int, omSnd :: Int}
  deriving (Eq, Show, Ord)

instance HasOKey Int OMapTest where
  okeyL = lens omFst $ \om u -> om {omFst = u}

instance Arbitrary OMapTest where
  arbitrary = OMapTest <$> arbitrary <*> arbitrary
