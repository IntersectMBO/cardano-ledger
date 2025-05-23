{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Shelley.Generator.ScriptClass (
  ScriptClass (..),
  Quantifier (..),
  exponential,
  anyOf,
  allOf,
  mOf,
  keyPairs,
  mkPayScriptHashMap,
  mkStakeScriptHashMap,
  mkScriptsFromKeyPair,
  mkKeyPairs,
  mkScripts,
  mkScriptCombinations,
  combinedScripts,
  baseScripts,
  scriptKeyCombinations,
  scriptKeyCombination,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Keys (asWitness)
import Data.List (permutations)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Tuple (swap)
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), KeyPairs, vKey)
import Test.Cardano.Ledger.Shelley.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

{------------------------------------------------------------------------------
  ScriptClass defines the operations that enable an Era's scripts to
  be adapated to property tests. This is a key component of the EraGen class.
------------------------------------------------------------------------------}

class EraScript era => ScriptClass era where
  basescript :: Proxy era -> KeyHash 'Witness -> Script era
  isKey :: Proxy era -> Script era -> Maybe (KeyHash 'Witness)
  isOnePhase :: Proxy era -> Script era -> Bool
  isOnePhase _proxy _ = True -- Many Eras have only OnePhase Scripts.
  quantify :: Proxy era -> Script era -> Quantifier (Script era)
  unQuantify :: Proxy era -> Quantifier (Script era) -> Script era

{------------------------------------------------------------------------------
  Abstracts the quantifier structure of (Script era)
  used in the 'quantify' and 'unQuantify' methods of ScriptClass.
 -----------------------------------------------------------------------------}

data Quantifier t = AllOf [t] | AnyOf [t] | MOf Int [t] | Leaf t
  deriving (Functor)

anyOf :: forall era. ScriptClass era => Proxy era -> [Script era] -> Script era
anyOf prox xs = unQuantify prox $ AnyOf xs

allOf :: forall era. ScriptClass era => Proxy era -> [Script era] -> Script era
allOf prox xs = unQuantify prox $ AllOf xs

mOf :: forall era. ScriptClass era => Proxy era -> Int -> [Script era] -> Script era
mOf prox m xs = unQuantify prox $ MOf m xs

{------------------------------------------------------------------------------
  Compute lists of keyHashes
------------------------------------------------------------------------------}

-- | return the first sublist that meets the predicate p.
getFirst :: ([a] -> Bool) -> [[a]] -> [a]
getFirst _p [] = []
getFirst p (xs : xss) = if p xs then xs else getFirst p xss

-- | Produce a valid list of key hashes that appear in a Script.
-- Note: in the case of AnyOf, we just take the first script in the expression.
-- This only works if we generate AnyOf scripts such that all script options
-- are valid scripts (that is, valid in the context of a transaction, at generation time
-- and execution/spend time).
scriptKeyCombination ::
  forall era.
  ScriptClass era =>
  Proxy era ->
  Script era ->
  [KeyHash 'Witness]
scriptKeyCombination prox script = case quantify prox script of
  AllOf xs -> concatMap (scriptKeyCombination prox) xs
  AnyOf xs -> getFirst (not . null) (map (scriptKeyCombination prox) xs)
  MOf m xs -> concatMap (scriptKeyCombination prox) (take m xs)
  Leaf t -> case isKey prox t of
    Just hk -> [hk]
    Nothing -> []

-- | Return all valid lists of KeyHashes that appear in a Script
--   used in testing.
scriptKeyCombinations ::
  forall era.
  ScriptClass era =>
  Proxy era ->
  Script era ->
  [[KeyHash 'Witness]]
scriptKeyCombinations prox script = case quantify prox script of
  AllOf xs -> [concat $ concatMap (scriptKeyCombinations prox) xs]
  AnyOf xs -> concatMap (scriptKeyCombinations prox) xs
  MOf m xs ->
    let perms = map (take m) $ permutations xs
     in map (concat . concatMap (scriptKeyCombinations prox)) perms
  Leaf t -> case isKey prox t of
    Just hk -> [[hk]]
    Nothing -> [[]]

-- | Make a simple (non-combined, ie NO quantifer like All, Any, MofN, etc.) script.
--   'basescript' is a method of ScriptClass, and is different for every Era.
mkScriptFromKey :: forall era. ScriptClass era => KeyPair 'Witness -> Script era
mkScriptFromKey = basescript (Proxy :: Proxy era) . hashKey . vKey

mkScriptsFromKeyPair ::
  forall era.
  ScriptClass era =>
  (KeyPair 'Payment, KeyPair 'Staking) ->
  (Script era, Script era)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey @era $ asWitness k0, mkScriptFromKey @era $ asWitness k1)

-- | make Scripts based on the given key pairs
mkScripts ::
  forall era.
  ScriptClass era =>
  KeyPairs ->
  [(Script era, Script era)]
mkScripts = map (mkScriptsFromKeyPair @era)

mkPayScriptHashMap ::
  forall era.
  ScriptClass era =>
  [(Script era, Script era)] ->
  Map.Map ScriptHash (Script era, Script era)
mkPayScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(pay, _ssStake) = (hashScript @era pay, script)

-- | Generate a mapping from stake script hash to script pair.
mkStakeScriptHashMap ::
  forall era.
  ScriptClass era =>
  [(Script era, Script era)] ->
  Map.Map ScriptHash (Script era, Script era)
mkStakeScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(_pay, stake) = (hashScript @era stake, script)

-- | Combine a list of script pairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkScriptCombinations ::
  forall era.
  ScriptClass era =>
  [(Script era, Script era)] ->
  [(Script era, Script era)]
mkScriptCombinations msigs =
  if length msigs < 3
    then error "length of input msigs must be at least 3"
    else
      ( List.foldl' (++) [] $
          do
            (k1, k2) <- msigs
            (k3, k4) <- msigs List.\\ [(k1, k2)]
            (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

            pure
              [ (pay, stake)
              | pay <-
                  [ anyOf (Proxy @era) [k1, k3, k5]
                  , allOf (Proxy @era) [k1, k3, k5]
                  , mOf (Proxy @era) 1 [k1, k3, k5]
                  , mOf (Proxy @era) 2 [k1, k3, k5]
                  , mOf (Proxy @era) 3 [k1, k3, k5]
                  ]
              , stake <-
                  [ anyOf (Proxy @era) [k2, k4, k6]
                  , allOf (Proxy @era) [k2, k4, k6]
                  , mOf (Proxy @era) 1 [k2, k4, k6]
                  , mOf (Proxy @era) 2 [k2, k4, k6]
                  , mOf (Proxy @era) 3 [k2, k4, k6]
                  ]
              ]
      ) ::
        [(Script era, Script era)]

-- | Make list of script pairs (payment,staking). These are non-combined scripts
--   Ie NO quantifer like All, Any, MofN, etc.) scripts.
--   In post Shelley Eras, either Keylock or Require Start-Finish scripts.
baseScripts ::
  forall era.
  ScriptClass era =>
  Constants ->
  [(Script era, Script era)]
baseScripts c = mkScripts @era (keyPairs c)

-- | Make a list of script pairs (payment,staking). Each of these are combined scripts.
--   I.e.  All, Any, MofN, etc. These come from combining the the first N (numBaseScripts) baseScripts
--   When N==3, we get about 150 combined scripts.
combinedScripts ::
  forall era.
  ScriptClass era =>
  Constants ->
  [(Script era, Script era)]
combinedScripts c@(Constants {numBaseScripts}) =
  mkScriptCombinations @era . take numBaseScripts $ baseScripts @era c

-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: Constants -> KeyPairs
keyPairs Constants {numKeyPairs} = mkKeyPairs <$> [1 .. numKeyPairs]

mkKeyPairs ::
  Word64 ->
  (KeyPair kr, KeyPair kr')
mkKeyPairs n =
  (mkKeyPair_ (2 * n), mkKeyPair_ (2 * n + 1))
  where
    mkKeyPair_ n_ =
      (uncurry KeyPair . swap)
        (mkKeyPair (RawSeed n_ n_ n_ n_ n_))

{------------------------------------------------------------------------------
  How to be a Generic Value
------------------------------------------------------------------------------}

exponential :: Integer -> Integer -> Gen Integer
exponential minc maxc = QC.frequency spread
  where
    width = (maxc - minc) `div` n
    deltas = [QC.choose (minc + (i - 1) * width, minc + i * width) | i <- [1 .. n]]
    scales = [1, 2, 4, 6, 4, 2, 1]
    n = fromIntegral (length scales)
    spread = zip scales deltas
