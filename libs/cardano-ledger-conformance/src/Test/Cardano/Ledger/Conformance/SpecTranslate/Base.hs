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
  askSpecTransM,
  withCtx,
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
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.TreeDiff (Expr (..), ToExpr (..))

newtype SpecTransM ctx a
  = SpecTransM (ExceptT Text (Reader ctx) a)
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader ctx)

runSpecTransM :: ctx -> SpecTransM ctx a -> Either Text a
runSpecTransM ctx (SpecTransM m) = runReader (runExceptT m) ctx

withSpecTransM :: (ctx -> ctx') -> SpecTransM ctx' a -> SpecTransM ctx a
withSpecTransM f (SpecTransM m) = SpecTransM (mapExceptT (withReaderT f) m)

askSpecTransM :: SpecTransM ctx ctx
askSpecTransM = ask

class SpecTranslate a where
  type SpecRep a :: Type

  type SpecContext a :: Type
  type SpecContext a = ()

  toSpecRep :: a -> SpecTransM (SpecContext a) (SpecRep a)

instance SpecTranslate () where
  type SpecRep () = ()

  toSpecRep = pure

instance SpecTranslate Bool where
  type SpecRep Bool = Bool

  toSpecRep = pure

instance SpecTranslate Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

instance SpecTranslate Void where
  type SpecRep Void = Void

  toSpecRep = absurd

instance SpecTranslate Word16 where
  type SpecRep Word16 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate Word32 where
  type SpecRep Word32 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

toSpecRepTupleGen ::
  (a -> SpecTransM ctx c) ->
  (b -> SpecTransM ctx d) ->
  (a, b) ->
  SpecTransM ctx (c, d)
toSpecRepTupleGen f g (a, b) = (,) <$> f a <*> g b

toSpecRepTuple ::
  (SpecTranslate a, SpecTranslate b, SpecContext a ~ ctx, SpecContext b ~ ctx) =>
  (a, b) -> SpecTransM ctx (SpecRep a, SpecRep b)
toSpecRepTuple = toSpecRepTupleGen toSpecRep toSpecRep

instance SpecTranslate a => SpecTranslate [a] where
  type SpecRep [a] = [SpecRep a]
  type SpecContext [a] = SpecContext a

  toSpecRep = traverse toSpecRep

instance SpecTranslate a => SpecTranslate (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)
  type SpecContext (StrictMaybe a) = SpecContext a

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate a => SpecTranslate (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)
  type SpecContext (Maybe a) = SpecContext a

  toSpecRep = traverse toSpecRep

instance SpecTranslate a => SpecTranslate (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]
  type SpecContext (StrictSeq a) = SpecContext a

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate a => SpecTranslate (Seq a) where
  type SpecRep (Seq a) = [SpecRep a]
  type SpecContext (Seq a) = SpecContext a

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate a => SpecTranslate (OSet a) where
  type SpecRep (OSet a) = [SpecRep a]
  type SpecContext (OSet a) = SpecContext a
  toSpecRep = traverse toSpecRep . toList

toSpecRepOMap ::
  (Ord k, SpecTranslate k, SpecTranslate v, SpecContext k ~ ctx, SpecContext v ~ ctx) =>
  OMap k v -> SpecTransM ctx [(SpecRep k, SpecRep v)]
toSpecRepOMap = traverse (bimapM toSpecRep toSpecRep) . OMap.assocList

instance (SpecTranslate a, Compactible a) => SpecTranslate (CompactForm a) where
  type SpecRep (CompactForm a) = SpecRep a
  type SpecContext (CompactForm a) = SpecContext a

  toSpecRep = toSpecRep . fromCompact

instance SpecTranslate a => SpecTranslate (Sized a) where
  type SpecRep (Sized a) = SpecRep a
  type SpecContext (Sized a) = SpecContext a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate a => SpecTranslate (Set a) where
  type SpecRep (Set a) = Agda.HSSet (SpecRep a)
  type SpecContext (Set a) = SpecContext a

  toSpecRep = fmap Agda.MkHSSet . traverse toSpecRep . Set.toList

instance SpecTranslate UnitInterval where
  type SpecRep UnitInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance SpecTranslate NonNegativeInterval where
  type SpecRep NonNegativeInterval = Agda.Rational

  toSpecRep = pure . unboundRational

toSpecRepMap ::
  (SpecTranslate k, SpecTranslate v, SpecContext k ~ ctx, SpecContext v ~ ctx) =>
  Map k v -> SpecTransM ctx (Agda.HSMap (SpecRep k) (SpecRep v))
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

withCtx :: ctx -> SpecTransM ctx a -> SpecTransM ctx' a
withCtx ctx m = do
  case runSpecTransM ctx m of
    Right x -> pure x
    Left e -> throwError e

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
