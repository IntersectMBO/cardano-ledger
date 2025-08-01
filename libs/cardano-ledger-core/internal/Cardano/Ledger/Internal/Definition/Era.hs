{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Internal.Definition.Era (
  Era (..),
  ToEraName,
  FromEraName,
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
  , KnownSymbol (ToEraName era)
  , FromEraName (ToEraName era) ~ era
  ) =>
  Era era
  where
  -- | Map an era to its predecessor.
  --
  -- For example:
  --
  -- > type instance PreviousEra (AllegraEra c) = ShelleyEra c
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
  eraName = symbolVal' (proxy# :: Proxy# (ToEraName era))

-- | This is a non-existent era and is defined for satisfying the `PreviousEra` type family injectivity
data VoidEra

-- | This is the era that preceded Shelley era. It cannot have any other class instances,
-- except for `Era` type class.
data ByronEra

instance Era ByronEra where
  type PreviousEra ByronEra = VoidEra
  type ProtVerLow ByronEra = 0
  type ProtVerHigh ByronEra = 1

data ShelleyEra

instance Era ShelleyEra where
  type PreviousEra ShelleyEra = ByronEra
  type ProtVerLow ShelleyEra = 2

data AllegraEra

instance Era AllegraEra where
  type PreviousEra AllegraEra = ShelleyEra
  type ProtVerLow AllegraEra = 3

data MaryEra

instance Era MaryEra where
  type PreviousEra MaryEra = AllegraEra
  type ProtVerLow MaryEra = 4

data AlonzoEra

instance Era AlonzoEra where
  type PreviousEra AlonzoEra = MaryEra
  type ProtVerLow AlonzoEra = 5
  type ProtVerHigh AlonzoEra = 6

data BabbageEra

instance Era BabbageEra where
  type PreviousEra BabbageEra = AlonzoEra
  type ProtVerLow BabbageEra = 7
  type ProtVerHigh BabbageEra = 8

data ConwayEra

instance Era ConwayEra where
  type PreviousEra ConwayEra = BabbageEra
  type ProtVerLow ConwayEra = 9
  type ProtVerHigh ConwayEra = 11

data DijkstraEra

instance Era DijkstraEra where
  type PreviousEra DijkstraEra = ConwayEra
  type ProtVerLow DijkstraEra = 12
  type ProtVerHigh DijkstraEra = 12

type family ToEraName (era :: Type) :: Symbol where
  ToEraName ByronEra = "Byron"
  ToEraName ShelleyEra = "Shelley"
  ToEraName AllegraEra = "Allegra"
  ToEraName MaryEra = "Mary"
  ToEraName AlonzoEra = "Alonzo"
  ToEraName BabbageEra = "Babbage"
  ToEraName ConwayEra = "Conway"
  ToEraName DijkstraEra = "Dijkstra"
  ToEraName era = TypeError ('Text "Unrecognized era " ':<>: 'ShowType era)

type family FromEraName (name :: Symbol) :: Type where
  FromEraName "Byron" = ByronEra
  FromEraName "Shelley" = ShelleyEra
  FromEraName "Allegra" = AllegraEra
  FromEraName "Mary" = MaryEra
  FromEraName "Alonzo" = AlonzoEra
  FromEraName "Babbage" = BabbageEra
  FromEraName "Conway" = ConwayEra
  FromEraName "Dijkstra" = DijkstraEra
  FromEraName name =
    TypeError
      ( 'Text "Era with name "
          ':<>: 'Text name
          ':<>: 'Text " is unknown"
      )
