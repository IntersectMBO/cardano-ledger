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
  huddleRule,
  huddleGroup,
  huddleGRule,
  huddleRule1,
  (=.=),
  (=.~),
  Era,
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRuleNamed :: Proxy name -> Proxy era -> Rule

class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroupNamed :: Proxy name -> Proxy era -> GroupDef

class (KnownSymbol name, Era era) => HuddleGRule (name :: Symbol) era where
  huddleGRuleNamed :: Proxy name -> Proxy era -> GRuleDef

class (KnownSymbol name, Era era) => HuddleRule1 (name :: Symbol) era where
  huddleRule1Named :: IsType0 a => Proxy name -> Proxy era -> a -> GRuleCall

huddleRule :: forall name era. HuddleRule name era => Proxy era -> Rule
huddleRule = huddleRuleNamed (Proxy @name)

huddleGroup :: forall name era. HuddleGroup name era => Proxy era -> GroupDef
huddleGroup = huddleGroupNamed (Proxy @name)

huddleGRule :: forall name era. HuddleGRule name era => Proxy era -> GRuleDef
huddleGRule = huddleGRuleNamed (Proxy @name)

huddleRule1 :: forall name era a. (HuddleRule1 name era, IsType0 a) => Proxy era -> a -> GRuleCall
huddleRule1 = huddleRule1Named (Proxy @name)

(=.=) :: (KnownSymbol name, IsType0 t) => Proxy (name :: Symbol) -> t -> Rule
(=.=) pname t = Name (T.pack (symbolVal pname)) =:= t

infixr 0 =.=

(=.~) :: KnownSymbol name => Proxy (name :: Symbol) -> Group -> GroupDef
(=.~) pname group = Name (T.pack (symbolVal pname)) =:~ group

infixr 0 =.~
