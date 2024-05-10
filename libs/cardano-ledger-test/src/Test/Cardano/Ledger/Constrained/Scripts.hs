{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Scripts (
  allPlutusScripts,
  genCoreScript,
  spendPlutusScripts,
  sufficientScript,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Core (Era (..), NativeScript, Script, getNativeScript, hashScript)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyRole (..),
 )
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural
import Test.Cardano.Ledger.Constrained.Combinators (genFromMap)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Generic.Functions (
  alwaysFalse,
  alwaysTrue,
  primaryLanguage,
 )
import Test.Cardano.Ledger.Generic.GenState
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock,
  ValidityInterval (..),
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )

-- ======================================

type KeyMap era = Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))

genMultiSig ::
  forall era.
  (ShelleyEraScript era, NativeScript era ~ MultiSig era) =>
  KeyMap era ->
  Proof era ->
  Gen (MultiSig era)
genMultiSig keymap _proof = do
  let genNestedMultiSig :: Natural -> Gen (MultiSig era)
      genNestedMultiSig k
        | k > 0 =
            oneof $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = oneof nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature =
        RequireSignature @era . fst <$> genFromMap ["from requiresSignature in genMultiSig"] keymap
      requireAllOf k = do
        n <- nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- nonNegativeSingleDigitInt
        m <- choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedMultiSig (k - 1))
  genNestedMultiSig (2 :: Natural)

genTimelock ::
  forall era.
  (AllegraEraScript era, NativeScript era ~ Timelock era) =>
  KeyMap era ->
  ValidityInterval ->
  Proof era ->
  Gen (Timelock era)
genTimelock keymap (ValidityInterval mBefore mAfter) _proof = do
  -- We need to limit how deep these timelocks can go, otherwise this generator will
  -- diverge. It also has to stay very shallow because it grows too fast.
  let genNestedTimelock k
        | k > 0 =
            oneof $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = oneof nonRecTimelocks
      nonRecTimelocks :: [Gen (Timelock era)]
      nonRecTimelocks =
        [ r
        | SJust r <-
            [ requireTimeStart <$> mBefore
            , requireTimeExpire <$> mAfter
            , SJust requireSignature
            ]
        ]
      requireSignature = RequireSignature . fst <$> genFromMap ["from requiresSignature in genTimelock"] keymap
      requireAllOf k = do
        n <- nonNegativeSingleDigitInt
        RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        n <- positiveSingleDigitInt
        RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        n <- nonNegativeSingleDigitInt
        m <- choose (0, n)
        RequireMOf m . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireTimeStart (SlotNo validFrom) = do
        minSlotNo <- choose (minBound, validFrom)
        pure $ RequireTimeStart (SlotNo minSlotNo)
      requireTimeExpire (SlotNo validTill) = do
        maxSlotNo <- choose (validTill, maxBound)
        pure $ RequireTimeExpire (SlotNo maxSlotNo)
  genNestedTimelock (2 :: Natural)

genPlutusScript ::
  forall era.
  PlutusPurposeTag ->
  Proof era ->
  Gen (Bool, Script era)
genPlutusScript tag proof = do
  isValid <- frequency [(5, pure False), (95, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (proof, tag) of
        (Conway, Spending) -> 2
        (Conway, _) -> 1
        (Babbage, Spending) -> 2
        (Babbage, _) -> 1
        (_, Spending) -> 3
        (_, _) -> 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  let mlanguage = primaryLanguage proof
  if isValid
    then (,) isValid <$> alwaysTrue proof mlanguage . (+ numArgs) <$> (elements [0, 1, 2, 3 :: Natural])
    else pure $ (isValid, alwaysFalse proof mlanguage numArgs)

genCoreScript ::
  forall era.
  Proof era ->
  PlutusPurposeTag ->
  KeyMap era ->
  ValidityInterval ->
  Gen (Script era)
genCoreScript proof tag keymap vi = case proof of
  Conway ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      -- TODO Add this once scripts are working in Conway
      -- , (1, snd <$> genPlutusScript tag proof)
      ]
  Babbage ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      , (1, snd <$> genPlutusScript tag proof)
      ]
  Alonzo ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      , (1, snd <$> genPlutusScript tag proof)
      ]
  Mary -> genTimelock keymap vi proof
  Allegra -> genTimelock keymap vi proof
  Shelley -> genMultiSig keymap proof

-- | For any given Era, there are only a finite number of Plutus scripts.
--   This function computes all of them. There will be two failing scripts
--   One for the Spend Tag, and another for all other Tags (Mint, Cert, Rewrd).
--   The non-failing Spend scripts have varying number of arguments (0, 1, 2, 3)
--   The non-failing (Mint Cert Rewrd) scripts are identical.
--   Any Plutus script generated by 'genCoreScript' will be in this Map.
allPlutusScripts ::
  Reflect era => Proof era -> Map (ScriptHash (EraCrypto era)) (IsValid, Script era)
allPlutusScripts proof =
  Map.fromList $
    map hash (concatMap (plutusByTag proof) (plutusPurposeTags proof))
  where
    hash (b, s) = (hashScript s, (b, s))

-- | There are only 5 plutus scripts that can be used in a Spend context
spendPlutusScripts ::
  Reflect era => Proof era -> Map (ScriptHash (EraCrypto era)) (IsValid, Script era)
spendPlutusScripts proof = Map.fromList (map hash (plutusByTag proof Spending))
  where
    hash (b, s) = (hashScript s, (b, s))

plutusByTag :: Proof era -> PlutusPurposeTag -> [(IsValid, Script era)]
plutusByTag proof tag = trueS ++ falseS
  where
    numArgs = case (proof, tag) of
      (Conway, Spending) -> 2
      (Conway, _) -> 1
      (Babbage, Spending) -> 2
      (Babbage, _) -> 1
      (_, Spending) -> 3
      (_, _) -> 2
    trueS = [(IsValid True, alwaysTrue proof mlanguage (numArgs + n)) | n <- [0, 1, 2, 3 :: Natural]]
    falseS = [(IsValid False, alwaysFalse proof mlanguage numArgs)]
    mlanguage = primaryLanguage proof

-- ===================================================

-- The function 'witsVKeyNeededFromBody' computes the necesary (but not sufficient)
-- key witnesses. Some of the missing ones have to do with MultiSig (and Timelock) scripts
-- So we need to compute the smallest set possible for Scripts. A MultiSig (Timelock) scripts
-- needs enough key witnesses, so that some subset of the Signature scripts to make it True.

sufficientMultiSig ::
  (ShelleyEraScript era, NativeScript era ~ MultiSig era) =>
  MultiSig era ->
  Set (KeyHash 'Witness (EraCrypto era))
sufficientMultiSig x = case x of
  RequireSignature kh -> Set.singleton kh
  RequireAllOf xs -> Set.unions (sufficientMultiSig <$> xs)
  RequireAnyOf xs ->
    case List.sortBy p (filter (not . Set.null) (sufficientMultiSig <$> (toList xs))) of
      [] -> Set.empty
      (s : _) -> s
  RequireMOf n xs -> Set.unions (take n (List.sortBy p (sufficientMultiSig <$> (toList xs))))
  _ -> error "Impossible: All NativeScripts should have been accounted for"
  where
    p a b = compare (Set.size a) (Set.size b)

-- | Return sufficient KeyHash to make the Timelock succeed. Note that some Timelock
--   scripts need no KeyHashes to succeed (RequireTimeExpire, RequireTimeStart)
sufficientTimelock ::
  (AllegraEraScript era, NativeScript era ~ Timelock era) =>
  Timelock era ->
  Set (KeyHash 'Witness (EraCrypto era))
sufficientTimelock x = case x of
  RequireSignature kh -> Set.singleton kh
  RequireAllOf xs -> Set.unions (fmap sufficientTimelock xs)
  RequireAnyOf xs ->
    case List.sortBy p (filter (not . Set.null) (map sufficientTimelock (toList xs))) of
      [] -> Set.empty
      (s : _) -> s
  RequireMOf n xs -> Set.unions (take n (List.sortBy p (map sufficientTimelock (toList xs))))
  RequireTimeExpire {} -> Set.empty
  RequireTimeStart {} -> Set.empty
  where
    p a b = compare (Set.size a) (Set.size b)

sufficientScript :: Proof era -> Script era -> Set (KeyHash 'Witness (EraCrypto era))
sufficientScript p s = case p of
  Shelley -> sufficientMultiSig s
  Allegra -> sufficientTimelock s
  Mary -> sufficientTimelock s
  Alonzo -> maybe Set.empty sufficientTimelock (getNativeScript s)
  Babbage -> maybe Set.empty sufficientTimelock (getNativeScript s)
  Conway -> maybe Set.empty sufficientTimelock (getNativeScript s)
