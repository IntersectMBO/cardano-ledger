{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Huddle (
  Type0 (..),
  HuddleRule (..),
  HuddleGroup (..),
  huddleRule,
  huddleGroup,
  Era,
) where

import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle (Group, IsType0, Named, Rule, comment, (=:=), (=:~))
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data Type0 where
  Type0 :: IsType0 t => t -> Type0

class (KnownSymbol name, Era era) => HuddleRule (name :: Symbol) era where
  huddleRuleBody :: Proxy era -> Type0
  huddleRuleComment :: Proxy era -> Maybe T.Text
  huddleRuleComment _ = Nothing

class (KnownSymbol name, Era era) => HuddleGroup (name :: Symbol) era where
  huddleGroupBody :: Proxy era -> Group
  huddleGroupComment :: Proxy era -> Maybe T.Text
  huddleGroupComment _ = Nothing

huddleRule :: forall name era. HuddleRule name era => Proxy era -> Rule
huddleRule p =
  let name' = T.pack $ symbolVal $ Proxy @name
      baseRule = case huddleRuleBody @name p of
        Type0 body -> name' =:= body
   in case huddleRuleComment @name p of
        Nothing -> baseRule
        Just txt -> comment txt baseRule

huddleGroup :: forall name era. HuddleGroup name era => Proxy era -> Named Group
huddleGroup p =
  let name' = T.pack $ symbolVal $ Proxy @name
      body = huddleGroupBody @name p
      baseGroup = name' =:~ body
   in case huddleGroupComment @name p of
        Nothing -> baseGroup
        Just txt -> comment txt baseGroup
