{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Generic.Indexed where

import Cardano.Crypto.DSIGN.Class ()
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (Witness),
  SignKeyDSIGN,
  VKey,
  hashKey,
 )
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as Mary (MultiAsset (..))
import Cardano.Ledger.Pretty (
  PrettyA (..),
  PrettyAnn (Width), ppRecord,
 )
import Cardano.Ledger.Pretty.Alonzo ()
import Cardano.Ledger.Pretty.Mary ()
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.Scripts (MultiSig)
import qualified Cardano.Ledger.Shelley.Scripts as Multi
import Cardano.Ledger.Shelley.TxBody (WitVKey (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString.Short (ShortByteString, pack, unpack)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as Seq (fromList)
import Prettyprinter (reAnnotate, viaShow)
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessVKey)
import Test.Cardano.Ledger.Generic.Proof (
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  Evidence (..),
  GoodCrypto,
  Mock,
  Proof (..),
  Reflect (..),
 )
import Test.Cardano.Ledger.Shelley.Utils (RawSeed (..), mkKeyPair)

-- ===========================================================================
-- Classes for "picking" the unique element of a type associated with an Int

class PrettyA t => Fixed t where
  unique :: Int -> t
  size :: Proxy t -> Maybe Int
  size _ = Nothing

class Era e => IndexedE t e where
  pickE :: Int -> Proof e -> t e

pickCbyCrypto :: Fixed (t c) => Int -> Evidence c -> t c
pickCbyCrypto n Standard = unique n
pickCbyCrypto n Mock = unique n

pickCbyEra :: Fixed (t (EraCrypto era)) => Int -> Proof era -> t (EraCrypto era)
pickCbyEra n _ = unique n

-- =======================================================
-- Examples where the type is independent of Era

names :: ShortByteString
names = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

allnames :: [ShortByteString]
allnames = map (\x -> pack [x]) (unpack names)

instance Fixed AssetName where
  unique n = map AssetName allnames !! n
  size _ = Just (length allnames)

instance Fixed Coin where
  size _ = Nothing
  unique n = Coin (fromIntegral n)

-- =======================================================
-- Examples where type depends on Crypto

data MultiAsset era where
  MultiAsset :: Era era => MaryValue (EraCrypto era) -> MultiAsset era

unMulti :: MultiAsset era -> MaryValue (EraCrypto era)
unMulti (MultiAsset x) = x

deriving instance Show (MultiAsset era)

instance (Reflect era, EraScript era, Fixed (Script era)) => Fixed (MultiAsset era) where
  unique n =
    MultiAsset
      ( MaryValue
          (fromIntegral n)
          ( Mary.MultiAsset
              ( Map.singleton
                  (lift (pickPolicyID @era n))
                  (Map.singleton (unique @AssetName n) (fromIntegral n))
              )
          )
      )
  size _ = lift (scriptsize @era)

scriptsize :: forall era. Fixed (Script era) => Proof era -> Maybe Int
scriptsize _ = size (Proxy @(Script era))

instance CC.Crypto c => Fixed (MaryValue c) where
  size _ = Nothing
  unique n = MaryValue (fromIntegral n) (Mary.MultiAsset Map.empty)

-- =======================================================
-- Keys and KeyHashes

-- | A signing key
newtype SKey (kr :: KeyRole) c = SKey (SignKeyDSIGN c)

instance CC.Crypto c => Fixed (KeyPair kr c) where
  unique n = KeyPair a b
    where
      m1 = fromIntegral n
      (b, a) = mkKeyPair (RawSeed 0 0 0 0 m1)

theKeyPair :: CC.Crypto c => Int -> KeyPair kr c
theKeyPair = unique

theVKey :: CC.Crypto c => Int -> VKey kr c
theVKey n = vKey (theKeyPair n)

theSKey :: forall c kr. CC.Crypto c => Int -> SKey kr c
theSKey n = SKey (sKey (theKeyPair @c n))

theKeyHash :: CC.Crypto c => Int -> KeyHash kr c
theKeyHash n = hashKey (theVKey n)

theWitVKey :: (GoodCrypto c) => Int -> SafeHash c EraIndependentTxBody -> WitVKey 'Witness c
theWitVKey n hash = mkWitnessVKey hash (theKeyPair n)

theKeyHashObj :: CC.Crypto c => Int -> Credential kr c
theKeyHashObj n = KeyHashObj . hashKey . vKey $ theKeyPair n

aScriptHashObj :: forall era kr. EraScript era => Proof era -> Script era -> Credential kr (EraCrypto era)
aScriptHashObj _wit s = ScriptHashObj . hashScript @era $ s

theStakeReference :: CC.Crypto c => Int -> StakeReference c
theStakeReference n = (StakeRefBase . KeyHashObj . hashKey) (theVKey n)

-- ====================================================
-- SlotNo

instance Fixed SlotNo where
  unique n = SlotNo (fromIntegral n)

-- ==========================================================================
-- An example where there is no easy algorithmic way to pick a (T era) from
-- an Int, so we compute a "basket" of (T era) and then just choose one from
-- the "basket".  We do this for all of the kinds of scripts.
-- ==========================================================================
-- MultiSig Scripts

multisigSimple :: forall era. Era era => Proof era -> [MultiSig era]
multisigSimple _c =
  [ Multi.RequireAnyOf mempty -- always False
  , Multi.RequireAllOf mempty -- always True
  ]

multisigFrom :: forall era. Era era => Proof era -> Int -> [MultiSig era]
multisigFrom _c n =
  [ Multi.RequireSignature (theKeyHash n)
  ]

multisigCompound :: forall era. Era era => Proof era -> Int -> [MultiSig era]
multisigCompound c n =
  [ Multi.RequireAnyOf (multisigFrom c n)
  , Multi.RequireAllOf (multisigFrom c n)
  , Multi.RequireMOf 1 (multisigFrom c n)
  , Multi.RequireMOf 2 (multisigFrom c n)
  ]

somemultisigs :: Era era => Proof era -> [MultiSig era]
somemultisigs c =
  multisigSimple c
    ++ concat [multisigFrom c i | i <- [1 .. 5]]
    ++ concat [multisigCompound c i | i <- [1 .. 5]]

instance (Era era, Reflect era) => Fixed (MultiSig era) where
  unique n = (somemultisigs reify) !! n
  size _ = Just multisiglength

multisiglength :: Int
multisiglength = length (somemultisigs (Shelley Mock)) - 1

-- ====================================================
-- Timelock Scripts

timelockSimple :: forall era. Era era => Proof era -> [Timelock era]
timelockSimple _c =
  [ RequireAnyOf mempty -- always False
  , RequireAllOf mempty -- always True
  ]

timelockFrom :: forall era. Era era => Proof era -> Int -> [Timelock era]
timelockFrom _c n =
  [ RequireSignature (theKeyHash n)
  , RequireTimeExpire (unique @SlotNo n)
  , RequireTimeStart (unique @SlotNo n)
  ]

timelockCompound :: forall era. Era era => Proof era -> Int -> [Timelock era]
timelockCompound c n =
  [ RequireAnyOf (Seq.fromList (timelockFrom c n))
  , RequireAllOf (Seq.fromList (timelockFrom c n))
  , RequireMOf 1 (Seq.fromList (timelockFrom c n))
  , RequireMOf 2 (Seq.fromList (timelockFrom c n))
  ]

sometimelocks :: Era era => Proof era -> [Timelock era]
sometimelocks c =
  timelockSimple c
    ++ concat [timelockFrom c i | i <- [1 .. 5]]
    ++ concat [timelockCompound c i | i <- [1 .. 5]]

timelocklength :: Int
timelocklength = length (sometimelocks (Shelley Mock)) - 1

instance (Era era, Reflect era) => Fixed (Timelock era) where
  unique n = lift sometimelocks !! n
  size _ = Just timelocklength

-- ====================================================
-- Alonzo Scripts

alonzoSimple :: forall era. [AlonzoScript era]
alonzoSimple =
  [ alwaysFails PlutusV1 1 -- always False
  , alwaysSucceeds PlutusV1 1 -- always True
  ]

somealonzo :: Era era => Proof era -> [AlonzoScript era]
somealonzo c =
  alonzoSimple
    ++ ( fmap
          TimelockScript
          ( concat [timelockFrom c i | i <- [1 .. 5]]
              ++ concat [timelockCompound c i | i <- [1 .. 5]]
          )
       )

alonzolength :: Int
alonzolength = length (somealonzo (Alonzo Mock) :: [Script (AlonzoEra Mock)]) - 1

instance Reflect (AlonzoEra c) => Fixed (AlonzoScript (AlonzoEra c)) where
  unique n = lift somealonzo !! n
  size _ = Just alonzolength

instance Reflect (BabbageEra c) => Fixed (AlonzoScript (BabbageEra c)) where
  unique n = lift somealonzo !! n
  size _ = Just alonzolength

instance Reflect (ConwayEra c) => Fixed (AlonzoScript (ConwayEra c)) where
  unique n = lift somealonzo !! n
  size _ = Just alonzolength

-- ==============================================
-- Type families (and other Types uniquely determined from type families like Hashes)
-- Because we can't make instances over type families, we can't say things like
-- instance IndexedE (Value era) where pickE x wit = ...
-- So instead we make a special pickXXX function where XXX is the name of a type family

pickValue :: forall era. Reflect era => Int -> Proof era -> Value era
pickValue n (Shelley _) = unique @Coin n
pickValue n (Allegra _) = unique @Coin n
pickValue n (Mary _) = unMulti (unique @(MultiAsset era) n)
pickValue n (Alonzo _) = unMulti (unique @(MultiAsset era) n)
pickValue n (Babbage _) = unMulti (unique @(MultiAsset era) n)
pickValue n (Conway _) = unMulti (unique @(MultiAsset era) n)

pickScript :: Int -> Proof era -> Script era
pickScript n p@(Shelley _) = somemultisigs p !! n
pickScript n p@(Allegra _) = sometimelocks p !! n
pickScript n p@(Mary _) = sometimelocks p !! n
pickScript n p@(Alonzo _) = somealonzo p !! n
pickScript n p@(Babbage _) = somealonzo p !! n
pickScript n p@(Conway _) = somealonzo p !! n

pickScriptHash :: forall era. Reflect era => Int -> Proof era -> ScriptHash (EraCrypto era)
pickScriptHash n wit = hashScript @era (pickScript n wit)

pickPolicyID :: Reflect era => Int -> Proof era -> PolicyID (EraCrypto era)
pickPolicyID n wit = PolicyID (pickScriptHash n wit)

-- ===========================================================================
-- An example where the 'pick' is the same type across all Crypto Evidence

data PublicSecret kr kr' c = PublicSecret (KeyPair kr c) (KeyPair kr' c)

instance CC.Crypto c => Fixed (PublicSecret kr kr' c) where
  unique n = PublicSecret (KeyPair a b) (KeyPair c d)
    where
      m1 = fromIntegral (2 * n)
      m2 = fromIntegral (2 * n + 1)
      (b, a) = mkKeyPair (RawSeed m1 m1 m1 m1 m1)
      (d, c) = mkKeyPair (RawSeed m2 m2 m2 m2 m2)

-- ===============================================================
-- PrettyA instances

instance Crypto c => PrettyA (KeyPair r c) where
  prettyA (KeyPair x y) =
    ppRecord "KeyPair" [("vKey", prettyA x), ("sKey", reAnnotate (Width 5 :) (viaShow y))]

instance (CC.Crypto c) => PrettyA (PublicSecret kr kr' c) where
  prettyA (PublicSecret x y) = prettyA (x, y)

instance PrettyA (SKey kr c) where
  prettyA (SKey _x) = "SKey"

instance PrettyA (MultiAsset era) where
  prettyA (MultiAsset v) = prettyA v
