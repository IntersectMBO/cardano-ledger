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
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol)

class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRule :: Proxy era -> Rule

class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroup :: Proxy era -> Named Group

class (KnownSymbol name, Era era) => HuddleGRule (name :: Symbol) era where
  huddleGRule :: Proxy era -> GRuleDef
