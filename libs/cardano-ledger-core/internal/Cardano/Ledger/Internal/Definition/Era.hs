{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Internal.Definition.Era (
  Era (..),
  EraHasName (..),
  ByronEra,
  ShelleyEra,
  AllegraEra,
  MaryEra,
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  DijkstraEra,
) where

import Cardano.Ledger.Binary (MaxVersion, MinVersion)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

--------------------------------------------------------------------------------
-- Era
--------------------------------------------------------------------------------

class
  ( Typeable era
  , KnownNat (ProtVerLow era)
  , KnownNat (ProtVerHigh era)
  , ProtVerLow era <= ProtVerHigh era
  , MinVersion <= ProtVerLow era
  , MinVersion <= ProtVerHigh era
  , -- We need to make sure that there is never a case that MaxVersion equals to the highest
    -- protocol version for the era, otherwise we can never upgrade to the next version:
    CmpNat (ProtVerLow era) MaxVersion ~ 'LT
  , CmpNat (ProtVerHigh era) MaxVersion ~ 'LT
  ) =>
  Era era
  where
  type EraName era :: Symbol

  -- | Map an era to its predecessor.
  --
  -- For example:
  --
  -- > type instance PreviousEra AllegraEra = ShelleyEra
  type PreviousEra era = (r :: Type) | r -> era

  -- | Lowest major protocol version for this era
  type ProtVerLow era :: Nat

  -- | Highest major protocol version for this era. By default se to `ProtVerLow`
  type ProtVerHigh era :: Nat

  type ProtVerHigh era = ProtVerLow era

  -- | Textual name of the current era.
  --
  -- Designed to be used with @TypeApplications@:
  --
  -- >>> eraName @ByronEra
  -- "Byron"
  eraName :: String
  default eraName :: KnownSymbol (EraName era) => String
  eraName = symbolVal' (proxy# :: Proxy# (EraName era))

-- | This is a non-existent era and is defined for satisfying the `PreviousEra` type family injectivity
data VoidEra

-- | This is the era that preceded Shelley era. It cannot have any other class instances,
-- except for `Era` type class.
data ByronEra

instance Era ByronEra where
  type EraName ByronEra = "Byron"
  type PreviousEra ByronEra = VoidEra
  type ProtVerLow ByronEra = 0
  type ProtVerHigh ByronEra = 1

data ShelleyEra

instance Era ShelleyEra where
  type EraName ShelleyEra = "Shelley"
  type PreviousEra ShelleyEra = ByronEra
  type ProtVerLow ShelleyEra = 2

data AllegraEra

instance Era AllegraEra where
  type EraName AllegraEra = "Allegra"
  type PreviousEra AllegraEra = ShelleyEra
  type ProtVerLow AllegraEra = 3

data MaryEra

instance Era MaryEra where
  type EraName MaryEra = "Mary"
  type PreviousEra MaryEra = AllegraEra
  type ProtVerLow MaryEra = 4

data AlonzoEra

instance Era AlonzoEra where
  type EraName AlonzoEra = "Alonzo"
  type PreviousEra AlonzoEra = MaryEra
  type ProtVerLow AlonzoEra = 5
  type ProtVerHigh AlonzoEra = 6

data BabbageEra

instance Era BabbageEra where
  type EraName BabbageEra = "Babbage"
  type PreviousEra BabbageEra = AlonzoEra
  type ProtVerLow BabbageEra = 7
  type ProtVerHigh BabbageEra = 8

data ConwayEra

instance Era ConwayEra where
  type EraName ConwayEra = "Conway"
  type PreviousEra ConwayEra = BabbageEra
  type ProtVerLow ConwayEra = 9
  type ProtVerHigh ConwayEra = 11

data DijkstraEra

instance Era DijkstraEra where
  type EraName DijkstraEra = "Dijkstra"
  type PreviousEra DijkstraEra = ConwayEra
  type ProtVerLow DijkstraEra = 12
  type ProtVerHigh DijkstraEra = 12

-- `EraHasName` type class must not be exported from any of the era packages and is only safe to
-- export from `cardano-ledger-api`, or any other package that can depend on all of the
-- cardano-ledegr-[era] packages.

-- | This class exists in order to be able to derive the protocol version range for an era from its
-- name. It is achieved by the means of injective definition of an era type from its name. This
-- effectively closes the world for eras to only the ones that are deined in this module, however it
-- does have to be that way. In other words, if there is ever a need to define custom eras that
-- build on top of existing ledger eras, then we will need to remove injectivity from this type
-- class as well as from the @PreviousEra@ type family.
class
  ( KnownSymbol eraName
  , Era (EraFromName eraName)
  , EraName (EraFromName eraName) ~ eraName
  ) =>
  EraHasName eraName
  where
  type EraFromName eraName = (era :: Type) | era -> eraName

instance EraHasName "Byron" where
  type EraFromName "Byron" = ByronEra

instance EraHasName "Shelley" where
  type EraFromName "Shelley" = ShelleyEra

instance EraHasName "Allegra" where
  type EraFromName "Allegra" = AllegraEra

instance EraHasName "Mary" where
  type EraFromName "Mary" = MaryEra

instance EraHasName "Alonzo" where
  type EraFromName "Alonzo" = AlonzoEra

instance EraHasName "Babbage" where
  type EraFromName "Babbage" = BabbageEra

instance EraHasName "Conway" where
  type EraFromName "Conway" = ConwayEra

instance EraHasName "Dijkstra" where
  type EraFromName "Dijkstra" = DijkstraEra
