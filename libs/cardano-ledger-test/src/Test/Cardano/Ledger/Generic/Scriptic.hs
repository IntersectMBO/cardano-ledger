{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Generic.Scriptic where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (..), ValidateScript (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary (MaryEra)
import qualified Cardano.Ledger.Mary.Value as Mary (AssetName (..), PolicyID (..), Value (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.Scripts as Multi
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq (fromList)
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Generic.Indexed (theKeyHash)
import Test.Cardano.Ledger.Generic.Proof

-- =============================================
-- Making era parameterized Scripts

theSlot :: Int -> SlotNo
theSlot n = SlotNo (fromIntegral n)

class (Era era, ValidateScript era, Eq (Core.Script era), Show (Core.Script era)) => Scriptic era where
  always :: Natural -> Proof era -> (Core.Script era)
  alwaysAlt :: Natural -> Proof era -> (Core.Script era)
  never :: Natural -> Proof era -> (Core.Script era)
  require :: KeyHash 'Witness (Crypto era) -> Proof era -> (Core.Script era)
  allOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  anyOf :: [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)
  mOf :: Int -> [Proof era -> (Core.Script era)] -> Proof era -> (Core.Script era)

class Scriptic era => PostShelley era where
  before :: Int -> Proof era -> Core.Script era
  after :: Int -> Proof era -> Core.Script era

class HasTokens era where
  forge :: Integer -> Core.Script era -> Core.Value era

instance CC.Crypto c => Scriptic (ShelleyEra c) where
  never _ (Shelley _) = Multi.RequireAnyOf mempty -- always False
  always _ (Shelley _) = Multi.RequireAllOf mempty -- always True
  alwaysAlt _ (Shelley _) = Multi.RequireAllOf mempty -- always True
  require key (Shelley _) = Multi.RequireSignature key
  allOf xs (Shelley c) = (Multi.RequireAllOf (map ($ Shelley c) xs))
  anyOf xs (Shelley c) = (Multi.RequireAnyOf (map ($ Shelley c) xs))
  mOf n xs (Shelley c) = (Multi.RequireMOf n (map ($ Shelley c) xs))

-- Make Scripts in AllegraEra

instance CC.Crypto c => Scriptic (AllegraEra c) where
  never _ (Allegra _) = RequireAnyOf mempty -- always False
  always _ (Allegra _) = RequireAllOf mempty -- always True
  alwaysAlt _ (Allegra _) = RequireAllOf mempty -- always True
  require key (Allegra _) = RequireSignature key
  allOf xs (Allegra c) = (RequireAllOf (Seq.fromList (map ($ Allegra c) xs)))
  anyOf xs (Allegra c) = (RequireAnyOf (Seq.fromList (map ($ Allegra c) xs)))
  mOf n xs (Allegra c) = (RequireMOf n (Seq.fromList (map ($ Allegra c) xs)))

instance CC.Crypto c => PostShelley (AllegraEra c) where
  before n (Allegra _) = RequireTimeStart (theSlot n)
  after n (Allegra _) = RequireTimeExpire (theSlot n)

-- Make Scripts in Mary era

instance CC.Crypto c => Scriptic (MaryEra c) where
  never _ (Mary _) = RequireAnyOf mempty -- always False
  always _ (Mary _) = RequireAllOf mempty -- always True
  alwaysAlt _ (Mary _) = RequireAllOf mempty -- always True
  require key (Mary _) = RequireSignature key
  allOf xs (Mary c) = (RequireAllOf (Seq.fromList (map ($ Mary c) xs)))
  anyOf xs (Mary c) = (RequireAnyOf (Seq.fromList (map ($ Mary c) xs)))
  mOf n xs (Mary c) = (RequireMOf n (Seq.fromList (map ($ Mary c) xs)))

instance CC.Crypto c => PostShelley (MaryEra c) where
  before n (Mary _) = RequireTimeStart (theSlot n)
  after n (Mary _) = RequireTimeExpire (theSlot n)

instance forall c. CC.Crypto c => HasTokens (MaryEra c) where
  forge n s = Mary.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Mary.PolicyID (hashScript @(MaryEra c) s)
      an = Mary.AssetName $ BS.pack "an"

instance forall c. CC.Crypto c => HasTokens (AlonzoEra c) where
  forge n s = Mary.Value 0 $ Map.singleton pid (Map.singleton an n)
    where
      pid = Mary.PolicyID (hashScript @(AlonzoEra c) s)
      an = Mary.AssetName $ BS.pack "an"

-- =================================
-- Make Scripts in Alonzo era

-- | Not every Alonzo Script can be used in a Timelock context.
unTime :: CC.Crypto (Crypto era) => Proof era -> (Proof era -> Script era) -> Timelock (Crypto era)
unTime wit f = case f wit of
  (TimelockScript x) -> x
  (PlutusScript _ "\SOH\NUL\NUL \ACK\SOH") -> (RequireAnyOf mempty)
  (PlutusScript _ "\SOH\NUL\NUL \STX\NUL\NUL\DC1") -> (RequireAllOf mempty)
  (PlutusScript _ _) -> error ("Plutus script in Timelock context")

instance CC.Crypto c => Scriptic (AlonzoEra c) where
  never n (Alonzo _) = alwaysFails PlutusV1 n -- always False
  always n (Alonzo _) = alwaysSucceeds PlutusV1 n -- always True
  alwaysAlt n (Alonzo _) = alwaysSucceeds PlutusV2 n -- always True
  require key (Alonzo _) = TimelockScript (RequireSignature key)
  allOf xs (Alonzo c) = TimelockScript (RequireAllOf (Seq.fromList (map (unTime (Alonzo c)) xs)))
  anyOf xs (Alonzo c) = TimelockScript (RequireAnyOf (Seq.fromList (map (unTime (Alonzo c)) xs)))
  mOf n xs (Alonzo c) = TimelockScript (RequireMOf n (Seq.fromList (map (unTime (Alonzo c)) xs)))

instance CC.Crypto c => PostShelley (AlonzoEra c) where
  before n (Alonzo _) = TimelockScript $ RequireTimeStart (theSlot n)
  after n (Alonzo _) = TimelockScript $ RequireTimeExpire (theSlot n)

-- =================================

instance CC.Crypto c => Scriptic (BabbageEra c) where
  never n (Babbage _) = alwaysFails PlutusV1 n -- always False
  always n (Babbage _) = alwaysSucceeds PlutusV1 n -- always True
  alwaysAlt n (Babbage _) = alwaysSucceeds PlutusV2 n -- always True
  require key (Babbage _) = TimelockScript (RequireSignature key)
  allOf xs (Babbage c) = TimelockScript (RequireAllOf (Seq.fromList (map (unTime (Babbage c)) xs)))
  anyOf xs (Babbage c) = TimelockScript (RequireAnyOf (Seq.fromList (map (unTime (Babbage c)) xs)))
  mOf n xs (Babbage c) = TimelockScript (RequireMOf n (Seq.fromList (map (unTime (Babbage c)) xs)))

instance CC.Crypto c => PostShelley (BabbageEra c) where
  before n (Babbage _) = TimelockScript $ RequireTimeStart (theSlot n)
  after n (Babbage _) = TimelockScript $ RequireTimeExpire (theSlot n)

-- =======================================
-- Some examples that work in multiple Eras
matchkey :: Scriptic era => Int -> Proof era -> Core.Script era
matchkey n era = require (theKeyHash n) era

test21 :: Scriptic era => Proof era -> Core.Script era
test21 wit = allOf [always 1, matchkey 1, anyOf [matchkey 2, matchkey 3]] $ wit

test22 :: PostShelley era => Proof era -> Core.Script era
test22 wit = mOf 2 [matchkey 1, before 100, anyOf [matchkey 2, matchkey 3]] $ wit
