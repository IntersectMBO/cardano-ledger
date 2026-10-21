{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Data.SimpleOMap.StrictSpec where

import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (encCBOR), encodeMapLen)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence.Strict qualified as SSeq
import Data.Set qualified as Set
import Data.SimpleOMap.Strict
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripFailureExpectation, mkTrip, roundTripCborSpec)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Classes
import Prelude hiding (elem, filter, lookup, null)

spec :: Spec
spec =
  describe "SimpleOMap.Strict" $ do
    context "membership checks work" $ do
      prop "unconsed" $
        \(m :: OMap Int Int) -> case m of
          Empty -> pure ()
          (k, _) :<|: _kv -> k `shouldSatisfy` (`member` m)
      prop "unsnoced" $
        \(m :: OMap Int Int) -> case m of
          Empty -> pure ()
          _kv :|>: (k, _) -> k `shouldSatisfy` (`member` m)
    context "when cons-ing" $ do
      prop "adding a duplicate results in a no-op" $
        \(m :: OMap Int Int) -> do
          case m of
            Empty -> pure ()
            kv :<|: _kv -> m `shouldBe` kv <| m
          case m of
            Empty -> pure ()
            _kv :|>: kv -> m `shouldBe` kv <| m
      prop "new values get added" $
        \((m, kv@(k, _)) :: (OMap Int Int, (Int, Int))) ->
          if k `member` m
            then kv <| m `shouldBe` m
            else kv <| m `shouldBe` kv :<|: m
    context "when snoc-ing" $ do
      prop "adding a duplicate results in a no-op" $
        \(m :: OMap Int Int) -> do
          case m of
            Empty -> pure ()
            kv :<|: _kv -> m `shouldBe` m |> kv
          case m of
            Empty -> pure ()
            _kv :|>: kv -> m `shouldBe` m |> kv
      prop "new values get added" $
        \((m, kv@(k, _)) :: (OMap Int Int, (Int, Int))) ->
          if k `member` m
            then m |> kv `shouldBe` m
            else m |> kv `shouldBe` m :|>: kv
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
        result `shouldSatisfy` (all (`Set.notMember` set) . toStrictSeqOKeys . fst)
        result `shouldSatisfy` (all (`Set.member` set) . Map.keys . snd)
        result `shouldSatisfy` invariantHolds' . fst
    prop "filter" $
      \((omap, i) :: (OMap Int Int, Int)) -> do
        let result = filter (< i) omap
        result `shouldSatisfy` all (< i)
        result `shouldSatisfy` invariantHolds'
    prop "adjust" $
      \((omap, i) :: (OMap Int Int, Int)) ->
        adjust (+ 1) i omap `shouldSatisfy` invariantHolds'
    context "overwriting" $ do
      prop "cons' - (<||)" $
        \((omap, kv@(k, v)) :: (OMap Int Int, (Int, Int))) -> do
          let consed = kv <|| omap
          if k `member` omap
            then consed `shouldBe` adjust (const v) k omap
            else consed `shouldBe` kv <| omap
      prop "snoc' - (||>)" $
        \((omap, kv@(k, v)) :: (OMap Int Int, (Int, Int))) -> do
          let snoced = omap ||> kv
          if k `member` omap
            then snoced `shouldBe` adjust (const v) k omap
            else snoced `shouldBe` omap |> kv
    prop "fromFoldable preserves order" $
      \(m :: Map.Map Int Int) -> do
        let sseq = SSeq.fromList $ Map.toList m
            omap = fromFoldable sseq
        toStrictSeqOfPairs omap `shouldBe` sseq
        omap `shouldSatisfy` invariantHolds'
    context "fromFoldableDuplicates preserves order" $ do
      prop "with duplicates" $
        \(m :: Map.Map Int Int) -> do
          let sseq = SSeq.fromList $ Map.toList m
              omap = fromFoldable sseq
              result = fromFoldableDuplicates (sseq SSeq.>< sseq)
          toStrictSeqOfPairs (snd result) `shouldBe` sseq
          result `shouldBe` (Set.fromList (Map.toList m), omap)
          snd result `shouldSatisfy` invariantHolds'
      prop "without duplicates" $
        \(m :: Map.Map Int Int) -> do
          let sseq = SSeq.fromList $ Map.toList m
              omap = fromFoldable sseq
              result = fromFoldableDuplicates sseq
          toStrictSeqOfPairs (snd result) `shouldBe` sseq
          result `shouldBe` (Set.empty, omap)
          snd result `shouldSatisfy` invariantHolds'
    context "CBOR round-trip" $ do
      roundTripCborSpec @(OMap Int Int)
      prop "duplicate keys fail to decode" $
        \((omap, key, value1, value2) :: (OMap Int Int, Int, Int, Int)) -> do
          let encodeAsMap kvs =
                encodeMapLen (fromIntegral (length kvs))
                  <> foldMap (\(k, v) -> encCBOR k <> encCBOR v) kvs
          embedTripFailureExpectation
            (mkTrip encodeAsMap (decCBOR @(OMap Int Int)))
            ((key, value1) : assocList omap <> [(key, value2)])
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
