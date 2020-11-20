{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- =============================================================================

-- | We gather together all the operations on the type family Script that are
--   necessary to generate consistent transactions to run property tests.
--   The key component is the class ScriptClass.
module Test.Shelley.Spec.Ledger.Generator.Scripts
  ( ScriptClass(..),
    Quantifier(..),
    ValueClass(..),
    TxBodyClass(..),
    genCoin,
    exponential,
    anyOf, allOf, mOf,
    ScriptPairs,
    keyPairs,
    mkPayScriptHashMap,
    mkStakeScriptHashMap,
    mkScriptsFromKeyPair,
    mkKeyPairs,
    mkScripts,
    mkScriptCombinations,
    combinedScripts,
    someScripts,
    scriptKeyCombinations,
    scriptKeyCombination,
    genesisId,
  )
  where

import Shelley.Spec.Ledger.Scripts(MultiSig(..),ScriptHash,hashMultiSigScript)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Core(Script,Value,TxBody,ChainData, SerialisableData, AnnotatedData)
import Cardano.Ledger.Era (Era(..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Shelley.Spec.Ledger.Keys (Hash, KeyHash, KeyRole (..),VKey(..), hashKey, asWitness, KeyPair(..), vKey)
import Cardano.Binary(ToCBOR(..),FromCBOR(..),Annotator)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Compactible(Compactible(..),CompactForm(..))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody, HashAnnotated (..))
import Data.Proxy
import qualified Data.Map as Map
import qualified Data.List as List
import Cardano.Ledger.Val(Val(..))
import qualified Cardano.Crypto.Hash as Hash
import Shelley.Spec.Ledger.TxBody(TxId(..))  -- also to import instances
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import Data.List(concatMap,permutations)
import Shelley.Spec.Ledger.LedgerState( KeyPairs )
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants,
  )
import Test.QuickCheck (Gen,generate)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Utils( mkKeyPair )
import Data.Word(Word64)
import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import Cardano.Ledger.Crypto (DSIGN)
import Data.Tuple (swap)
import Shelley.Spec.Ledger.Tx( ValidateScript (..) )
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))


-- ==============================================================================

class ( Show (Script era),
        Eq (Script era),
        ValidateScript era,
        HashIndex (Core.TxBody era) ~ EraIndependentTxBody,
        Eq (Core.TxBody era),
        Show (Core.TxBody era),
        CC.Crypto(Crypto era),
        Era era ) => ScriptClass era where
  basescript :: Proxy era -> KeyHash 'Witness (Crypto era) -> Script era
  isKey :: Proxy era -> Script era -> Maybe (KeyHash 'Witness (Crypto era))
  quantify :: Proxy era -> Script era -> Quantifier (Script era)
  unQuantify:: Proxy era -> Quantifier (Script era) -> Script era

type ScriptPairs era = [(Core.Script era, Core.Script era)]

-- ====================================

-- | Abstracts the quantifier structure of (Script era)
--   used in the 'quantify' and 'unQuantify' methods of ScriptClass.
data Quantifier t = AllOf [t] | AnyOf [t] | MOf Int [t] | Leaf t

anyOf :: forall era. ScriptClass era => Proxy era -> [Script era] -> Script era
anyOf prox xs = unQuantify prox $ AnyOf xs

allOf :: forall era. ScriptClass era => Proxy era -> [Script era] -> Script era
allOf prox xs = unQuantify prox $ AllOf xs

mOf :: forall era. ScriptClass era => Proxy era -> Int -> [Script era] -> Script era
mOf prox m xs = unQuantify prox $ MOf m xs

-- =======================================================================
-- Compute lists of keyHashes

-- | return the first sublist that meets the predicate p.
getFirst :: ([a] -> Bool) -> [[a]] -> [a]
getFirst _p [] = []
getFirst p (xs:xss) = if p xs then xs else getFirst p xss

-- | Return some valid list of KeyHashes that appear in a Script
--   Try not to return the empty list if there is at least on
--   Leaf that requires a key hash.
scriptKeyCombination :: forall era. ScriptClass era =>
   Proxy era -> Script era -> [KeyHash 'Witness (Crypto era)]
scriptKeyCombination prox script = case quantify prox script of
  AllOf xs ->  concatMap (scriptKeyCombination prox) xs
  AnyOf xs -> getFirst (not . null) (map (scriptKeyCombination prox) xs)
  MOf m xs -> concatMap (scriptKeyCombination prox) (take m xs)
  Leaf t -> case isKey prox t of
              Just hk -> [ hk ]
              Nothing -> []

-- | Return all valid lists of KeyHashes that appear in a Script
--   used in testing.
scriptKeyCombinations :: forall era. ScriptClass era =>
   Proxy era -> Script era -> [[KeyHash 'Witness (Crypto era)]]
scriptKeyCombinations prox script = case quantify prox script of
  AllOf xs -> [concat $ concatMap (scriptKeyCombinations prox) xs]
  AnyOf xs ->  concatMap (scriptKeyCombinations prox) xs
  MOf m xs ->
     let perms = map (take m) $ permutations xs
     in map (concat . concatMap (scriptKeyCombinations prox)) perms
  Leaf t -> case isKey prox t of
              Just hk -> [[hk]]
              Nothing -> [[]]

-- ================================================================

mkScriptFromKey :: forall era. (ScriptClass era) => KeyPair 'Witness (Crypto era) -> Core.Script era
mkScriptFromKey = (basescript (Proxy :: Proxy era) . hashKey . vKey)

mkScriptsFromKeyPair :: forall era.
  (ScriptClass era) =>
  (KeyPair 'Payment (Crypto era), KeyPair 'Staking (Crypto era)) ->
  (Core.Script era, Core.Script era)
mkScriptsFromKeyPair (k0, k1) =
  (mkScriptFromKey @era $ asWitness k0, mkScriptFromKey @era $ asWitness k1)


-- | make Scripts based on the given key pairs
mkScripts :: forall era. (ScriptClass era) => KeyPairs (Crypto era) -> ScriptPairs era
mkScripts = map (mkScriptsFromKeyPair @era)

mkPayScriptHashMap ::
  (ScriptClass era) =>
  [(Core.Script era, Core.Script era)] ->
  Map.Map (ScriptHash era) (Core.Script era, Core.Script era)
mkPayScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(pay, _stake) = (hashScript pay, script)


-- | Generate a mapping from stake script hash to script pair.
mkStakeScriptHashMap ::
  (ScriptClass era) =>
  [(Core.Script era, Core.Script era)] ->
  Map.Map (ScriptHash era) (Core.Script era, Core.Script era)
mkStakeScriptHashMap scripts =
  Map.fromList (f <$> scripts)
  where
    f script@(_pay, stake) = (hashScript stake, script)


-- =========================================================================

-- | Combine a list of ScriptPairs into hierarchically structured multi-sig
-- scripts, list must have at least length 3. Be careful not to call with too
-- many pairs in order not to create too many of the possible combinations.
mkScriptCombinations :: forall era. (ScriptClass era) => ScriptPairs era -> ScriptPairs era
mkScriptCombinations msigs =
  if length msigs < 3
    then error "length of input msigs must be at least 3"
    else (List.foldl' (++) [] $
      do
        (k1, k2) <- msigs
        (k3, k4) <- msigs List.\\ [(k1, k2)]
        (k5, k6) <- msigs List.\\ [(k1, k2), (k3, k4)]

        pure
          [ (pay, stake)
            | pay <-
                [ anyOf (Proxy @era) [k1, k3, k5],
                  allOf (Proxy @era) [k1, k3, k5],
                  mOf (Proxy @era) 1 [k1, k3, k5],
                  mOf (Proxy @era) 2 [k1, k3, k5],
                  mOf (Proxy @era) 3 [k1, k3, k5]
                ],
              stake <-
                [ anyOf (Proxy @era) [k2, k4, k6],
                  allOf (Proxy @era) [k2, k4, k6],
                  mOf (Proxy @era) 1 [k2, k4, k6],
                  mOf (Proxy @era) 2 [k2, k4, k6],
                  mOf (Proxy @era) 3 [k2, k4, k6]
                ]
          ]) :: ScriptPairs era



baseScripts :: forall era. ScriptClass era => Proxy era -> Constants -> ScriptPairs era
baseScripts _proxy c = mkScripts @era (keyPairs c)

combinedScripts :: forall era. ScriptClass era => Proxy era -> Constants -> ScriptPairs era
combinedScripts proxy c@(Constants {numBaseScripts}) =
  mkScriptCombinations @era . take numBaseScripts $ baseScripts proxy c

-- | Select between _lower_ and _upper_ scripts from the possible combinations
-- of the first `numBaseScripts` multi-sig scripts of `mSigScripts`.
someScripts :: ScriptClass era => Proxy era -> Constants -> Int -> Int -> Gen (ScriptPairs era)
someScripts proxy c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (combinedScripts proxy c)


-- | Constant list of KeyPairs intended to be used in the generators.
keyPairs :: CC.Crypto crypto => Constants -> KeyPairs crypto
keyPairs Constants {maxNumKeyPairs} = mkKeyPairs <$> [1 .. maxNumKeyPairs]

mkKeyPairs ::
  (DSIGNAlgorithm (DSIGN crypto)) =>
  Word64 ->
  (KeyPair kr crypto, KeyPair kr' crypto)
mkKeyPairs n =
  (mkKeyPair_ (2 * n), mkKeyPair_ (2 * n + 1))
  where
    mkKeyPair_ n_ =
      (uncurry KeyPair . swap)
        (mkKeyPair (n_, n_, n_, n_, n_))

-- ==========================================================
-- The (ShelleyEra c) instance of ScriptClass and EraGen

instance CC.Crypto c => ScriptClass (ShelleyEra c) where
  basescript _proxy = RequireSignature
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  quantify _ (RequireAllOf xs) = AllOf xs
  quantify _ (RequireAnyOf xs) = AnyOf xs
  quantify _ (RequireMOf n xs) = MOf n xs
  quantify _ t = Leaf t
  unQuantify _  (AllOf xs) = (RequireAllOf xs)
  unQuantify _  (AnyOf xs) = (RequireAnyOf xs)
  unQuantify _  (MOf n xs) = (RequireMOf n xs)
  unQuantify _  (Leaf t) = t

-- ===========================================================
-- How to be a Generic Value

class Val(Core.Value era) => ValueClass era where
   genValue:: [ScriptHash era] -> Integer -> Integer -> Gen(Core.Value era)

genCoin :: Integer -> Integer -> Gen Coin
genCoin minCoin maxCoin = Coin <$> exponential minCoin maxCoin

exponential :: Integer -> Integer -> Gen Integer
exponential minc maxc = QC.frequency spread
  where width = (maxc - minc) `div` n
        deltas = [QC.choose (minc + (i-1)*width, minc + i*width) | i <- [1 .. n]]
        scales = [1,2,4,6,4,2,1]
        n = fromIntegral(length scales)
        spread = zip scales deltas

instance ValueClass (ShelleyEra c) where
   genValue _ = genCoin


-- ===========================================================
-- How to be a Generic TxBody

-- data Field old new = Field (d -> old -> new) (Gen d)

class ( HashIndex (Core.TxBody era) ~ EraIndependentTxBody,
        HashAnnotated (TxBody era) era)
       =>
       TxBodyClass era where
   emptyTxBody:: Core.TxBody era
   liftTxBody :: Gen (Shelley.TxBody era) -> Gen(Core.TxBody era)

instance CC.Crypto c => TxBodyClass (ShelleyEra c) where
   emptyTxBody = Shelley.TxBody
          Set.empty
          StrictSeq.Empty
          StrictSeq.Empty
          (Shelley.Wdrl Map.empty)
          (Coin 0)
          (SlotNo 0)
          SNothing
          SNothing
   liftTxBody x = x

genesisId :: forall era. TxBodyClass era => TxId era
genesisId = TxId (hashAnnotated @(Core.TxBody era) @era (emptyTxBody @era))
