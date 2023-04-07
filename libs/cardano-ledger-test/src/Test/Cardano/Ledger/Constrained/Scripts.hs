{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Scripts where

import Cardano.Ledger.Allegra.Scripts (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), Tag (..))
import Cardano.Ledger.Core (Era (..), Script)
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import qualified Cardano.Ledger.Shelley.Scripts as Shelley (MultiSig (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as Seq
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
  keyPairs <- vectorOf 10 arbitrary
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

genTimelock :: forall era. Era era => KeyMap era -> ValidityInterval -> Proof era -> Gen (Timelock era)
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
  Tag ->
  Proof era ->
  Gen (Script era)
genPlutusScript tag proof = do
  isValid <- frequency [(5, pure False), (95, pure True)]
  -- Plutus scripts alwaysSucceeds needs at least numArgs, while
  -- alwaysFails needs exactly numArgs to have the desired affect.
  -- For reasons unknown, this number differs from Alonzo to Babbage
  -- Perhaps because Babbage is using PlutusV2 scripts?
  let numArgs = case (proof, tag) of
        (Conway _, Spend) -> 2
        (Conway _, _) -> 1
        (Babbage _, Spend) -> 2
        (Babbage _, _) -> 1
        (_, Spend) -> 3
        (_, _) -> 2
  -- While using varying number of arguments for alwaysSucceeds we get
  -- varying script hashes, which helps with the fuzziness
  let mlanguage = primaryLanguage proof
  if isValid
    then alwaysTrue proof mlanguage . (+ numArgs) <$> (elements [0, 1, 2, 3 :: Natural])
    else pure $ alwaysFalse proof mlanguage numArgs

genCoreScript :: forall era. Proof era -> Tag -> KeyMap era -> ValidityInterval -> Gen (Script era)
genCoreScript proof tag keymap vi = case proof of
  Conway _ -> frequency [(2, TimelockScript <$> genTimelock keymap vi proof), (1, genPlutusScript tag proof)]
  Babbage _ -> frequency [(2, TimelockScript <$> genTimelock keymap vi proof), (1, genPlutusScript tag proof)]
  Alonzo _ -> frequency [(2, TimelockScript <$> genTimelock keymap vi proof), (1, genPlutusScript tag proof)]
  Mary _ -> genTimelock keymap vi proof
  Allegra _ -> genTimelock keymap vi proof
  Shelley _ -> genMultiSig keymap proof

{-

import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars(validityInterval)
import Test.Cardano.Ledger.Generic.PrettyCore (
 pcKeyHash,
 pcScript,
 prettyScript,
 )
import Cardano.Ledger.Pretty(
 ppMap,
 ppString,
 ppList,
 )

-- ==================
main = do
  m <- generate (genKeyMap (Babbage Standard))
  putStrLn(show (ppMap pcKeyHash (ppString . show) m))
  ss <- generate $ do
      tag <- arbitrary
      vi <- arbitrary
      vectorOf 10 (genCoreScript (Babbage Standard) tag m vi)
  putStrLn (show (ppList prettyScript ss))

scriptT:: Proof era -> Tag -> Target era (Gen (Script era))
scriptT p tag = Constr "GenCoreScript" (genCoreScript p tag)
            ^$ keymapUniv
            ^$ validityInterval

keymapUniv = Var (V "keymapUniv" (MapR WitHashR KeyPairR) No)

-- spendMapUniv = Var (V "spendMapUniv"

-}
