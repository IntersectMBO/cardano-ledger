{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Generic.Scriptic where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import qualified Cardano.Ledger.Shelley.Scripts as Multi
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq (fromList)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Proof

-- =============================================
-- Making era parameterized Scripts

theSlot :: Int -> SlotNo
theSlot n = SlotNo (fromIntegral n)

class (EraScript era, Show (Script era)) => Scriptic era where
  always :: Natural -> Proof era -> Script era
  alwaysAlt :: Natural -> Proof era -> Script era
  never :: Natural -> Proof era -> Script era
  require :: KeyHash 'Witness (EraCrypto era) -> Proof era -> NativeScript era
  allOf :: [Proof era -> NativeScript era] -> Proof era -> NativeScript era
  anyOf :: [Proof era -> NativeScript era] -> Proof era -> NativeScript era
  mOf :: Int -> [Proof era -> NativeScript era] -> Proof era -> NativeScript era

class Scriptic era => PostShelley era where
  before :: Int -> Proof era -> NativeScript era
  after :: Int -> Proof era -> NativeScript era

class HasTokens era where
  forge :: Integer -> Script era -> MultiAsset (EraCrypto era)

instance Crypto c => Scriptic (ShelleyEra c) where
  never _ Shelley = Multi.RequireAnyOf mempty -- always False
  always _ Shelley = Multi.RequireAllOf mempty -- always True
  alwaysAlt _ Shelley = Multi.RequireAllOf mempty -- always True
  require key Shelley = Multi.RequireSignature key
  allOf xs Shelley = Multi.RequireAllOf (map ($ Shelley) xs)
  anyOf xs Shelley = Multi.RequireAnyOf (map ($ Shelley) xs)
  mOf n xs Shelley = Multi.RequireMOf n (map ($ Shelley) xs)

-- Make Scripts in AllegraEra

instance Crypto c => Scriptic (AllegraEra c) where
  never _ Allegra = RequireAnyOf mempty -- always False
  always _ Allegra = RequireAllOf mempty -- always True
  alwaysAlt _ Allegra = RequireAllOf mempty -- always True
  require key Allegra = RequireSignature key
  allOf xs proof = RequireAllOf (Seq.fromList (map ($ proof) xs))
  anyOf xs proof = RequireAnyOf (Seq.fromList (map ($ proof) xs))
  mOf n xs proof = RequireMOf n (Seq.fromList (map ($ proof) xs))

instance Crypto c => PostShelley (AllegraEra c) where
  before n Allegra = RequireTimeStart (theSlot n)
  after n Allegra = RequireTimeExpire (theSlot n)

-- Make Scripts in Mary era

instance Crypto c => Scriptic (MaryEra c) where
  never _ Mary = RequireAnyOf mempty -- always False
  always _ Mary = RequireAllOf mempty -- always True
  alwaysAlt _ Mary = RequireAllOf mempty -- always True
  require key Mary = RequireSignature key
  allOf xs proof = RequireAllOf (Seq.fromList (map ($ proof) xs))
  anyOf xs proof = RequireAnyOf (Seq.fromList (map ($ proof) xs))
  mOf n xs proof = RequireMOf n (Seq.fromList (map ($ proof) xs))

instance Crypto c => PostShelley (MaryEra c) where
  before n Mary = RequireTimeStart (theSlot n)
  after n Mary = RequireTimeExpire (theSlot n)

instance forall c. Crypto c => HasTokens (MaryEra c) where
  forge n s = MultiAsset $ Map.singleton pid (Map.singleton an n)
    where
      pid = PolicyID (hashScript @(MaryEra c) s)
      an = AssetName "an"

instance forall c. Crypto c => HasTokens (AlonzoEra c) where
  forge n s = MultiAsset $ Map.singleton pid (Map.singleton an n)
    where
      pid = PolicyID (hashScript @(AlonzoEra c) s)
      an = AssetName "an"

instance forall c. Crypto c => HasTokens (BabbageEra c) where
  forge n s = MultiAsset $ Map.singleton pid (Map.singleton an n)
    where
      pid = PolicyID (hashScript @(BabbageEra c) s)
      an = AssetName "an"

instance forall c. Crypto c => HasTokens (ConwayEra c) where
  forge n s = MultiAsset $ Map.singleton pid (Map.singleton an n)
    where
      pid = PolicyID (hashScript @(ConwayEra c) s)
      an = AssetName "an"

-- =================================
-- Make Scripts in Alonzo era

instance Crypto c => Scriptic (AlonzoEra c) where
  never n Alonzo = alwaysFails @'PlutusV1 n -- always False
  always n Alonzo = alwaysSucceeds @'PlutusV1 n -- always True
  alwaysAlt n Alonzo = alwaysSucceeds @'PlutusV1 n -- always True
  require key Alonzo = RequireSignature key
  allOf xs proof = RequireAllOf (Seq.fromList (($ proof) <$> xs))
  anyOf xs proof = RequireAnyOf (Seq.fromList (($ proof) <$> xs))
  mOf n xs proof = RequireMOf n (Seq.fromList (($ proof) <$> xs))

instance Crypto c => PostShelley (AlonzoEra c) where
  before n Alonzo = RequireTimeStart (theSlot n)
  after n Alonzo = RequireTimeExpire (theSlot n)

-- =================================

instance Crypto c => Scriptic (BabbageEra c) where
  never n Babbage = alwaysFails @'PlutusV1 n -- always False
  always n Babbage = alwaysSucceeds @'PlutusV1 n -- always True
  alwaysAlt n Babbage = alwaysSucceeds @'PlutusV2 n -- always True
  require key Babbage = RequireSignature key
  allOf xs proof = RequireAllOf (Seq.fromList (($ proof) <$> xs))
  anyOf xs proof = RequireAnyOf (Seq.fromList (($ proof) <$> xs))
  mOf n xs proof = RequireMOf n (Seq.fromList (($ proof) <$> xs))

instance Crypto c => PostShelley (BabbageEra c) where
  before n Babbage = RequireTimeStart (theSlot n)
  after n Babbage = RequireTimeExpire (theSlot n)

-- =================================

instance Crypto c => Scriptic (ConwayEra c) where
  never n Conway = alwaysFails @'PlutusV1 n -- always False
  always n Conway = alwaysSucceeds @'PlutusV1 n -- always True
  alwaysAlt n Conway = alwaysSucceeds @'PlutusV2 n -- always True
  require key Conway = RequireSignature key
  allOf xs proof = RequireAllOf (Seq.fromList (($ proof) <$> xs))
  anyOf xs proof = RequireAnyOf (Seq.fromList (($ proof) <$> xs))
  mOf n xs proof = RequireMOf n (Seq.fromList (($ proof) <$> xs))

instance Crypto c => PostShelley (ConwayEra c) where
  before n Conway = RequireTimeStart (theSlot n)
  after n Conway = RequireTimeExpire (theSlot n)

-- =======================================
-- Some examples that work in multiple Eras
matchkey :: Scriptic era => Int -> Proof era -> NativeScript era
matchkey n era = require (theKeyHash n) era

test21 :: Scriptic era => Proof era -> Script era
test21 wit = fromNativeScript $ allOf [matchkey 1, anyOf [matchkey 2, matchkey 3]] wit

test22 :: PostShelley era => Proof era -> Script era
test22 wit = fromNativeScript $ mOf 2 [matchkey 1, before 100, anyOf [matchkey 2, matchkey 3]] wit
