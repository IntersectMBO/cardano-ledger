{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Scripts where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import qualified Cardano.Ledger.Allegra.Scripts as Time (Timelock (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Core (Era (..), Script, hashScript)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
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

-- ======================================

type KeyMap era = Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))

genKeyMap :: Era era => Proof era -> Gen (KeyMap era)
genKeyMap _proof = do
  keyPairs <- vectorOf 100 arbitrary
  let keyHash keyPair = ((coerceKeyRole . hashKey . vKey) keyPair, keyPair)
  pure (Map.fromList (map keyHash keyPairs))

genMultiSig :: forall era. Era era => KeyMap era -> Proof era -> Gen (Shelley.MultiSig era)
genMultiSig keymap _proof = do
  let genNestedMultiSig :: Natural -> Gen (Shelley.MultiSig era)
      genNestedMultiSig k
        | k > 0 =
            oneof $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = oneof nonRecTimelocks
      nonRecTimelocks = [requireSignature]
      requireSignature = Shelley.RequireSignature @era . fst <$> genFromMap ["from requiresSignature in genMultiSig"] keymap
      requireAllOf k = do
        n <- nonNegativeSingleDigitInt
        Shelley.RequireAllOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireAnyOf k = do
        n <- positiveSingleDigitInt
        Shelley.RequireAnyOf <$> replicateM n (genNestedMultiSig (k - 1))
      requireMOf k = do
        n <- nonNegativeSingleDigitInt
        m <- choose (0, n)
        Shelley.RequireMOf m <$> replicateM n (genNestedMultiSig (k - 1))
  genNestedMultiSig (2 :: Natural)

genTimelock :: forall era. Era era => KeyMap era -> ValidityInterval -> Proof era -> Gen (Time.Timelock era)
genTimelock keymap (ValidityInterval mBefore mAfter) _proof = do
  -- We need to limit how deep these timelocks can go, otherwise this generator will
  -- diverge. It also has to stay very shallow because it grows too fast.
  let genNestedTimelock k
        | k > 0 =
            oneof $
              nonRecTimelocks ++ [requireAllOf k, requireAnyOf k, requireMOf k]
        | otherwise = oneof nonRecTimelocks
      nonRecTimelocks :: [Gen (Time.Timelock era)]
      nonRecTimelocks =
        [ r
        | SJust r <-
            [ requireTimeStart <$> mBefore
            , requireTimeExpire <$> mAfter
            , SJust requireSignature
            ]
        ]
      requireSignature = Time.RequireSignature . fst <$> genFromMap ["from requiresSignature in genTimelock"] keymap
      requireAllOf k = do
        n <- nonNegativeSingleDigitInt
        Time.RequireAllOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireAnyOf k = do
        n <- positiveSingleDigitInt
        Time.RequireAnyOf . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireMOf k = do
        n <- nonNegativeSingleDigitInt
        m <- choose (0, n)
        Time.RequireMOf m . Seq.fromList <$> replicateM n (genNestedTimelock (k - 1))
      requireTimeStart (SlotNo validFrom) = do
        minSlotNo <- choose (minBound, validFrom)
        pure $ Time.RequireTimeStart (SlotNo minSlotNo)
      requireTimeExpire (SlotNo validTill) = do
        maxSlotNo <- choose (validTill, maxBound)
        pure $ Time.RequireTimeExpire (SlotNo maxSlotNo)
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
        (Conway _, Spending) -> 2
        (Conway _, _) -> 1
        (Babbage _, Spending) -> 2
        (Babbage _, _) -> 1
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
  Conway _ ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      -- TODO Add this once scripts are working in Conway
      -- , (1, snd <$> genPlutusScript tag proof)
      ]
  Babbage _ ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      , (1, snd <$> genPlutusScript tag proof)
      ]
  Alonzo _ ->
    frequency
      [ (1, TimelockScript <$> genTimelock keymap vi proof)
      , (1, snd <$> genPlutusScript tag proof)
      ]
  Mary _ -> genTimelock keymap vi proof
  Allegra _ -> genTimelock keymap vi proof
  Shelley _ -> genMultiSig keymap proof

-- | For any given Era, there are only a finite number of Plutus scripts.
--   This function computes all of them. There will be two failing scripts
--   One for the Spend Tag, and another for all other Tags (Mint, Cert, Rewrd).
--   The non-failing Spend scripts have varying number of arguments (0, 1, 2, 3)
--   The non-failing (Mint Cert Rewrd) scripts are identical.
--   Any Plutus script generated by 'genCoreScript' will be in this Map.
allPlutusScripts :: Reflect era => Proof era -> Map (ScriptHash (EraCrypto era)) (IsValid, Script era)
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
      (Conway _, Spending) -> 2
      (Conway _, _) -> 1
      (Babbage _, Spending) -> 2
      (Babbage _, _) -> 1
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

sufficientMultiSig :: Era era => Shelley.MultiSig era -> Set (KeyHash 'Witness (EraCrypto era))
sufficientMultiSig x = case x of
  Shelley.RequireSignature kh -> Set.singleton kh
  Shelley.RequireAllOf xs -> Set.unions (map sufficientMultiSig xs)
  Shelley.RequireAnyOf xs ->
    case List.sortBy p (filter (not . Set.null) (map sufficientMultiSig (toList xs))) of
      [] -> Set.empty
      (s : _) -> s
  Shelley.RequireMOf n xs -> Set.unions (take n (List.sortBy p (map sufficientMultiSig xs)))
  where
    p a b = compare (Set.size a) (Set.size b)

smallest :: [Set x] -> Set x
smallest xs = help Set.empty xs
  where
    help small1 [] = small1
    help small1 (y : ys) =
      if Set.size small1 < Set.size y
        then help small1 ys
        else help y ys

-- | Return sufficient KeyHash to make the Timelock succeed. Note that some Timelock
--   scripts need no KeyHashes to succeed (RequireTimeExpire, RequireTimeStart)
sufficientTimelock :: Era era => Time.Timelock era -> Set (KeyHash 'Witness (EraCrypto era))
sufficientTimelock x = case x of
  Time.RequireSignature kh -> Set.singleton kh
  Time.RequireAllOf xs -> Set.unions (fmap sufficientTimelock xs)
  Time.RequireAnyOf xs ->
    case List.sortBy p (filter (not . Set.null) (map sufficientTimelock (toList xs))) of
      [] -> Set.empty
      (s : _) -> s
  Time.RequireMOf n xs -> Set.unions (take n (List.sortBy p (map sufficientTimelock (toList xs))))
  Time.RequireTimeExpire {} -> Set.empty
  Time.RequireTimeStart {} -> Set.empty
  where
    p a b = compare (Set.size a) (Set.size b)

sufficientScript :: Proof era -> Script era -> Set (KeyHash 'Witness (EraCrypto era))
sufficientScript p s = case whichScript p of
  ScriptShelleyToShelley -> sufficientMultiSig s
  ScriptAllegraToMary -> sufficientTimelock s
  ScriptAlonzoToConway -> case s of
    TimelockScript tl -> sufficientTimelock tl
    PlutusScript _ -> Set.empty
