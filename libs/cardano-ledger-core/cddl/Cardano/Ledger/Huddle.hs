{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Huddle (
  HuddleRule (..),
  HuddleGroup (..),
  HuddleGRule (..),
  HuddleRule1 (..),
  Era,
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol)

class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRule :: Proxy era -> Rule

class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroup :: Proxy era -> GroupDef

class (KnownSymbol name, Era era) => HuddleGRule (name :: Symbol) era where
  huddleGRule :: Proxy era -> GRuleDef

class (KnownSymbol name, Era era) => HuddleRule1 (name :: Symbol) era where
  huddleRule1 :: IsType0 a => Proxy era -> a -> GRuleCall
