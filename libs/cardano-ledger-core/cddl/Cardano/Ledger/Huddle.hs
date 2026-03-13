{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Huddle (
  module Huddle,
  (//-),
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
  genArrayTerm,
  genBytesTerm,
  genStringTerm,
  pickOne,
) where

import Cardano.Ledger.Binary (Term (..))
import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import qualified Codec.CBOR.Cuddle.Huddle as Huddle hiding ((=:=), (=:~))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Random.Stateful (StatefulGen, UniformRange (..))
import Test.QuickCheck (Gen, elements)

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

genArrayTerm :: [Term] -> Gen Term
genArrayTerm es = elements [TList es, TListI es]

pickOne :: StatefulGen g m => NonEmpty a -> g -> m a
pickOne es g = do
  i <- uniformRM (0, length es - 1) g
  pure $ es NE.!! i

genBytesTerm :: StatefulGen g m => ByteString -> g -> m Term
genBytesTerm bs = pickOne [TBytes bs, TBytesI $ LBS.fromStrict bs]

genStringTerm :: StatefulGen g m => T.Text -> g -> m Term
genStringTerm t = pickOne [TString t, TStringI $ LT.fromStrict t]
