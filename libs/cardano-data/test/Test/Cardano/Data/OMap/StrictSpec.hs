{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Test.QuickCheck.Classes.Base

spec :: Spec
spec =
  describe "OMap.Strict" $ do
    context "membership checks work" $ do
      prop "unconsed" $ \(m :: OMap Int Int) -> case m of
        Empty -> pure ()
        (k, _v) :<|: _kv -> k `shouldSatisfy` (`member` m)
      prop "unsnoced" $ \(m :: OMap Int Int) -> case m of
        Empty -> pure ()
        _kv :|>: (k, _v) -> k `shouldSatisfy` (`member` m)
    context "when cons-ing" $ do
      prop "adding a duplicate results in a no-op" $ \(m :: OMap Int Int) -> do
        case m of
          Empty -> pure ()
          (k, v) :<|: _kv -> m `shouldBe` (k, v) <| m
        case m of
          Empty -> pure ()
          _kv :|>: (k, v) -> m `shouldBe` (k, v) <| m
      prop "new values get added" $ \((m, k, v) :: (OMap Int Int, Int, Int)) -> do
        if k `member` m
          then (k, v) <| m `shouldBe` m
          else (k, v) <| m `shouldBe` (k, v) :<|: m
    context "when snoc-ing" $ do
      prop "adding a duplicate results in a no-op" $ \(m :: OMap Int Int) -> do
        case m of
          Empty -> pure ()
          (k, v) :<|: _kv -> m `shouldBe` m |> (k, v)
        case m of
          Empty -> pure ()
          _kv :|>: (k, v) -> m `shouldBe` m |> (k, v)
      prop "new values get added" $ \((m, k, v) :: (OMap Int Int, Int, Int)) -> do
        if k `member` m
          then m |> (k, v) `shouldBe` m
          else m |> (k, v) `shouldBe` m :|>: (k, v)
    context "mappend preserves uniqueness" $ do
      prop "mappending with itself should be a no-op" $ \(i :: OMap Int Int) -> do
        i |>< i `shouldBe` i
        i ><| i `shouldBe` i
      prop "mappending with duplicates: left-preserving" $ \((i, j) :: (OMap Int Int, OMap Int Int)) -> do
        case j of
          Empty -> i `shouldBe` i |>< j
          j' :<|: _js -> i |>< j `shouldBe` (i |> j') |>< j
      prop "mappending with duplicates: right-preserving" $ \((i, j) :: (OMap Int Int, OMap Int Int)) -> do
        case i of
          Empty -> i ><| j `shouldBe` j
          _is :|>: i' -> i ><| j `shouldBe` i ><| (i' <| j)
    prop "operations preserve invariant" $
      \((omap, omap', sseq, set) :: (OMap Int Int, OMap Int Int, SSeq.StrictSeq Int, Set.Set Int)) -> do
        omap |>< omap' `shouldSatisfy` invariantHolds'
        omap ><| omap' `shouldSatisfy` invariantHolds'
        fromStrictSeq sseq `shouldSatisfy` invariantHolds'
        fromSet set `shouldSatisfy` invariantHolds'
    prop "fromStrictSeq preserves order" $
      \(set :: Set.Set Int) ->
        let sseq = SSeq.fromList $ Set.elems set
         in toStrictSeq (fromStrictSeq sseq) `shouldBe` sseq
    context "fromStrictSeqDuplicates" $ do
      prop "with duplicates" $ \(set :: (Set.Set Int)) ->
        let sseq = SSeq.fromList $ Set.elems set
            omap = fromStrictSeq sseq
         in fromStrictSeqDuplicates (sseq SSeq.>< sseq) `shouldBe` (set, omap)
      prop "without duplicates" $ \(set :: (Set.Set Int)) ->
        let sseq = SSeq.fromList $ Set.elems set
            omap = fromStrictSeq sseq
         in fromStrictSeqDuplicates sseq `shouldBe` (Set.empty, omap)
    context "CBOR round-trip" $ do
      roundTripCborSpec @(OMap Int Int)
    context "Typeclass laws" $ do
      it "Type" $
        lawsCheckOne
          (Proxy :: Proxy (OMap Int Int))
          [ isListLaws
          , semigroupLaws
          , monoidLaws
          , semigroupMonoidLaws
          ]

instance HasOKey Int Int where
  okeyL = lens id const
