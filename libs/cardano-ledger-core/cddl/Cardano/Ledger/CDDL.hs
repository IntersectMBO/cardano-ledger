{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.CDDL (
  HasCDDL (..),
  huddleRule,
  huddleGroup,
  huddleGRule,
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

class Era era => HasCDDL (name :: Symbol) era where
  huddleItem :: HuddleItem

huddleRule :: forall name era. (HasCDDL name era, KnownSymbol name) => Rule
huddleRule =
  case huddleItem @name @era of
    HIRule rule -> rule
    _ -> error $ "Expected Rule for " ++ symbolVal (undefined :: proxy name)

huddleGroup :: forall name era. (HasCDDL name era, KnownSymbol name) => Named Group
huddleGroup =
  case huddleItem @name @era of
    HIGroup g -> g
    _ -> error $ "Expected Group for " ++ symbolVal (undefined :: proxy name)

huddleGRule :: forall name era. (HasCDDL name era, KnownSymbol name) => GRuleDef
huddleGRule =
  case huddleItem @name @era of
    HIGRule grule -> grule
    _ -> error $ "Expected GRule for " ++ symbolVal (undefined :: proxy name)
