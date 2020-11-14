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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- =============================================================================

module Test.Shelley.Spec.Ledger.Generator.GenEra
  ( ScriptClass(..),
    Quantifier(..),
    anyOf, allOf, mOf,
    EraGen(),
    proxy,
  )
  where

import Shelley.Spec.Ledger.Scripts(MultiSig(..),ScriptHash,getKeyCombination,hashMultiSigScript)
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Core(Script,Value,TxBody,ChainData, SerialisableData, AnnotatedData)
import Cardano.Ledger.Era (Era(..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Shelley.Spec.Ledger.Keys (Hash, KeyHash, KeyRole (..),VKey(..), hashKey, asWitness)
import Cardano.Binary(ToCBOR(..),FromCBOR(..),Annotator)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Compactible(Compactible(..),CompactForm(..))
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Hashing (EraIndependentTxBody, HashAnnotated (..))
import Data.Proxy
import Cardano.Ledger.Val(Val(..))
import qualified Cardano.Crypto.Hash as Hash
import Shelley.Spec.Ledger.TxBody() -- import instances only
import Data.List(concatMap,permutations)

-- ==============================================================================

{-
data Field t era where
  Field :: (a -> t -> t) -> (EraGen era => Gen a) -> Field t era
-}

proxy :: forall t. Proxy t
proxy = Proxy

class (Show (Script era)) => ScriptClass era where
  hashScript :: Era era => Script era -> ScriptHash era
  scriptKeyCombination :: Era era => Proxy era -> Script era -> [KeyHash 'Witness (Crypto era)]
  scriptKeyCombination prox scr = scriptCombo prox scr
  basescript :: Proxy era -> KeyHash 'Witness (Crypto era) -> Script era
  isKey :: Proxy era -> Script era -> Maybe (KeyHash 'Witness (Crypto era))
  quantify :: Proxy era -> Script era -> Quantifier (Script era)
  unQuantify:: Proxy era -> Quantifier (Script era) -> Script era

class (Era era,

       ScriptClass era,
       ToCBOR(Core.Script era),
       FromCBOR(Annotator (Core.Script era)),
       Eq (Core.Script era),
       Show (Core.Script era),
       NoThunks (Core.Script era),

       Eq (Core.TxBody era),
       Show (Core.TxBody era),
       HashIndex (Core.TxBody era) ~ EraIndependentTxBody,
       ToCBOR (Core.TxBody era),
       FromCBOR(Annotator (Core.TxBody era)),
       HashAnnotated(Core.TxBody era) era,
       NoThunks (Core.TxBody era),


       Val (Core.Value era),
       Eq (Core.Value era),
       Show (Core.Value era),
       NoThunks (Core.Value era),
       Compactible (Core.Value era),
       Torsor (Core.Value era),
       ToCBOR (Core.Value era),
       ToCBOR(Delta (Core.Value era)),
       ToCBOR(CompactForm (Core.Value era)),
       FromCBOR(Core.Value era),
       FromCBOR(Delta (Core.Value era)),
       FromCBOR(CompactForm (Core.Value era))

      ) => EraGen era where

-- ==========================================================
-- The (ShelletEra c) instance of ScriptClass and EraGen

instance CC.Crypto c => ScriptClass (ShelleyEra c) where
  hashScript = hashMultiSigScript
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

instance CC.Crypto c => EraGen (ShelleyEra c) where

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

-- | return the first sublist that meets the predicate p.
getFirst :: ([a] -> Bool) -> [[a]] -> [a]
getFirst _p [] = []
getFirst p (xs:xss) = if p xs then xs else getFirst p xss

-- | Return some valid list of KeyHashes that appear in a Script
--   Try not to return the empty list if there is at least on
--   Leaf that requires a key hash.

scriptCombo :: forall era. ScriptClass era =>
   Proxy era -> Script era -> [KeyHash 'Witness (Crypto era)]
scriptCombo prox script = case quantify prox script of
  AllOf xs ->  concatMap (scriptCombo prox) xs
  AnyOf xs -> getFirst (not . null) (map (scriptCombo prox) xs)
  MOf m xs -> concatMap (scriptCombo prox) (take m xs)
  Leaf t -> case isKey prox t of
              Just hk -> [ hk ]
              Nothing -> []

-- | Return all valid lists of KeyHashes that appear in a Script
--   used in testing.

scriptCombos :: forall era. ScriptClass era =>
   Proxy era -> Script era -> [[KeyHash 'Witness (Crypto era)]]
scriptCombos prox script = case quantify prox script of
  AllOf xs -> [concat $ concatMap (scriptCombos prox) xs]
  AnyOf xs ->  concatMap (scriptCombos prox) xs
  MOf m xs ->
     let perms = map (take m) $ permutations xs
     in map (concat . concatMap (scriptCombos prox)) perms
  Leaf t -> case isKey prox t of
              Just hk -> [[hk]]
              Nothing -> [[]]
