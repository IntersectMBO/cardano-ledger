{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  SpecNormalize (..),
  OpaqueErrorString (..),
  SpecTransM,
  runSpecTransM,
  withSpecTransM,
  withCtxSpecTransM,
  askSpecTransM,
  unComputationResult,
  unComputationResult_,
  toSpecRepTuple,
  toSpecRepTupleGen,
  toSpecRepOMap,
  toSpecRepMap,
) where

import Cardano.Ledger.BaseTypes (NonNegativeInterval, UnitInterval, unboundRational)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Compactible (Compactible (..), fromCompact)
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError (..), mapExceptT, runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, ask, runReader, withReaderT)
import Data.Bitraversable (bimapM)
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.OMap.Strict (OMap)
import qualified Data.OMap.Strict as OMap
import Data.OSet.Strict (OSet)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import qualified MAlonzo.Code.Ledger.Core.Foreign.API as Agda
import Test.Cardano.Ledger.TreeDiff (Expr (..), ToExpr (..))

newtype SpecTransM era ctx a
  = SpecTransM (ExceptT Text (Reader ctx) a)
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader ctx)

runSpecTransM :: ctx -> SpecTransM era ctx a -> Either Text a
runSpecTransM ctx (SpecTransM m) = runReader (runExceptT m) ctx

withSpecTransM :: (ctx -> ctx') -> SpecTransM era ctx' a -> SpecTransM era ctx a
withSpecTransM f (SpecTransM m) = SpecTransM (mapExceptT (withReaderT f) m)

withCtxSpecTransM :: ctx -> SpecTransM era ctx a -> SpecTransM era ctx' a
withCtxSpecTransM ctx = withSpecTransM (const ctx)

askSpecTransM :: SpecTransM era ctx ctx
askSpecTransM = ask

class SpecTranslate era a where
  type SpecRep era a :: Type

  type SpecContext era a :: Type
  type SpecContext era a = ()

  toSpecRep :: a -> SpecTransM era (SpecContext era a) (SpecRep era a)

instance SpecTranslate era () where
  type SpecRep era () = ()

  toSpecRep = pure

instance SpecTranslate era Bool where
  type SpecRep era Bool = Bool

  toSpecRep = pure

instance SpecTranslate era Integer where
  type SpecRep era Integer = Integer

  toSpecRep = pure

instance SpecTranslate era Void where
  type SpecRep era Void = Void

  toSpecRep = absurd

instance SpecTranslate era Word16 where
  type SpecRep era Word16 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate era Word32 where
  type SpecRep era Word32 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate era Word64 where
  type SpecRep era Word64 = Integer

  toSpecRep = pure . toInteger

toSpecRepTupleGen ::
  forall era a b c d ctx.
  (a -> SpecTransM era ctx c) ->
  (b -> SpecTransM era ctx d) ->
  (a, b) ->
  SpecTransM era ctx (c, d)
toSpecRepTupleGen f g (a, b) = (,) <$> f a <*> g b

toSpecRepTuple ::
  forall era a b ctx.
  (SpecTranslate era a, SpecTranslate era b, SpecContext era a ~ ctx, SpecContext era b ~ ctx) =>
  (a, b) -> SpecTransM era ctx (SpecRep era a, SpecRep era b)
toSpecRepTuple = toSpecRepTupleGen toSpecRep toSpecRep

instance SpecTranslate era a => SpecTranslate era [a] where
  type SpecRep era [a] = [SpecRep era a]
  type SpecContext era [a] = SpecContext era a

  toSpecRep = traverse toSpecRep

instance SpecTranslate era a => SpecTranslate era (StrictMaybe a) where
  type SpecRep era (StrictMaybe a) = Maybe (SpecRep era a)
  type SpecContext era (StrictMaybe a) = SpecContext era a

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate era a => SpecTranslate era (Maybe a) where
  type SpecRep era (Maybe a) = Maybe (SpecRep era a)
  type SpecContext era (Maybe a) = SpecContext era a

  toSpecRep = traverse toSpecRep

instance SpecTranslate era a => SpecTranslate era (StrictSeq a) where
  type SpecRep era (StrictSeq a) = [SpecRep era a]
  type SpecContext era (StrictSeq a) = SpecContext era a

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate era a => SpecTranslate era (Seq a) where
  type SpecRep era (Seq a) = [SpecRep era a]
  type SpecContext era (Seq a) = SpecContext era a

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate era a => SpecTranslate era (OSet a) where
  type SpecRep era (OSet a) = [SpecRep era a]
  type SpecContext era (OSet a) = SpecContext era a

  toSpecRep = traverse toSpecRep . toList

toSpecRepOMap ::
  forall era k v ctx.
  (Ord k, SpecTranslate era k, SpecTranslate era v, SpecContext era k ~ ctx, SpecContext era v ~ ctx) =>
  OMap k v -> SpecTransM era ctx [(SpecRep era k, SpecRep era v)]
toSpecRepOMap = traverse (bimapM toSpecRep toSpecRep) . OMap.assocList

instance (SpecTranslate era a, Compactible a) => SpecTranslate era (CompactForm a) where
  type SpecRep era (CompactForm a) = SpecRep era a
  type SpecContext era (CompactForm a) = SpecContext era a

  toSpecRep = toSpecRep . fromCompact

instance SpecTranslate era a => SpecTranslate era (Sized a) where
  type SpecRep era (Sized a) = SpecRep era a
  type SpecContext era (Sized a) = SpecContext era a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate era a => SpecTranslate era (Set a) where
  type SpecRep era (Set a) = Agda.HSSet (SpecRep era a)
  type SpecContext era (Set a) = SpecContext era a

  toSpecRep = fmap Agda.MkHSSet . traverse toSpecRep . Set.toList

instance SpecTranslate era UnitInterval where
  type SpecRep era UnitInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance SpecTranslate era NonNegativeInterval where
  type SpecRep era NonNegativeInterval = Agda.Rational

  toSpecRep = pure . unboundRational

toSpecRepMap ::
  forall era k v ctx.
  (SpecTranslate era k, SpecTranslate era v, SpecContext era k ~ ctx, SpecContext era v ~ ctx) =>
  Map k v -> SpecTransM era ctx (Agda.HSMap (SpecRep era k) (SpecRep era v))
toSpecRepMap =
  fmap Agda.MkHSMap
    . traverse (bimapM toSpecRep toSpecRep)
    . Map.toList

class GSpecNormalize f where
  genericSpecNormalize :: f a -> f a

instance GSpecNormalize U1 where
  genericSpecNormalize U1 = U1

instance GSpecNormalize V1 where
  genericSpecNormalize = \case {}

instance (GSpecNormalize f, GSpecNormalize g) => GSpecNormalize (f :*: g) where
  genericSpecNormalize (f :*: g) = genericSpecNormalize f :*: genericSpecNormalize g

instance (GSpecNormalize f, GSpecNormalize g) => GSpecNormalize (f :+: g) where
  genericSpecNormalize (L1 f) = L1 $ genericSpecNormalize f
  genericSpecNormalize (R1 f) = R1 $ genericSpecNormalize f

instance GSpecNormalize a => GSpecNormalize (M1 i c a) where
  genericSpecNormalize (M1 x) = M1 $ genericSpecNormalize x

instance SpecNormalize a => GSpecNormalize (K1 i a) where
  genericSpecNormalize (K1 x) = K1 $ specNormalize x

class SpecNormalize a where
  specNormalize :: a -> a
  default specNormalize :: (Generic a, GSpecNormalize (Rep a)) => a -> a
  specNormalize = to . genericSpecNormalize . from

-- | OpaqueErrorString behaves like unit in comparisons, but contains an
-- error string that can be displayed.
newtype OpaqueErrorString = OpaqueErrorString (NonEmpty Text)
  deriving (Generic, Show, Semigroup)

-- | This implementation violates referential transparency. Do not rely on it
-- unless you know what you're doing.
instance Eq OpaqueErrorString where
  _ == _ = True

instance ToExpr OpaqueErrorString where
  toExpr (OpaqueErrorString x) = App "OpaqueErrorString" [toExpr x]

instance NFData OpaqueErrorString

unComputationResult :: Agda.ComputationResult Text a -> Either Text a
unComputationResult (Agda.Success x) = Right x
unComputationResult (Agda.Failure e) = Left e

unComputationResult_ :: Agda.ComputationResult Void a -> Either e a
unComputationResult_ (Agda.Success x) = Right x
unComputationResult_ (Agda.Failure x) = case x of {}
