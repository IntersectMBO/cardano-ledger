{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  askCtx,
  withCtx,
  unComputationResult,
  unComputationResult_,
  toSpecRep_,
) where

import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.BaseTypes (Inject (..), NonNegativeInterval, UnitInterval, unboundRational)
import Cardano.Ledger.Compactible (Compactible (..), fromCompact)
import Data.List.NonEmpty (NonEmpty)
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
import Data.Bitraversable (bimapM)
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.OMap.Strict (OMap)
import qualified Data.OMap.Strict as OMap
import Data.OSet.Strict (OSet)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
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

class SpecTranslate ctx a where
  type SpecRep a :: Type

  toSpecRep :: a -> SpecTransM ctx (SpecRep a)

instance SpecTranslate ctx () where
  type SpecRep () = ()

  toSpecRep = pure

instance SpecTranslate ctx Bool where
  type SpecRep Bool = Bool

  toSpecRep = pure

instance SpecTranslate ctx Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

instance SpecTranslate ctx Void where
  type SpecRep Void = Void

  toSpecRep = absurd

instance SpecTranslate ctx Word16 where
  type SpecRep Word16 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx Word32 where
  type SpecRep Word32 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

instance
  ( SpecTranslate ctx a
  , SpecTranslate ctx b
  ) =>
  SpecTranslate ctx (a, b)
  where
  type SpecRep (a, b) = (SpecRep a, SpecRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y

instance SpecTranslate ctx a => SpecTranslate ctx [a] where
  type SpecRep [a] = [SpecRep a]

  toSpecRep = traverse toSpecRep

instance SpecTranslate ctx a => SpecTranslate ctx (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate ctx a => SpecTranslate ctx (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep

instance SpecTranslate ctx a => SpecTranslate ctx (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate ctx a => SpecTranslate ctx (Seq a) where
  type SpecRep (Seq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate ctx a => SpecTranslate ctx (OSet a) where
  type SpecRep (OSet a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  , Ord k
  ) =>
  SpecTranslate ctx (OMap k v)
  where
  type SpecRep (OMap k v) = [(SpecRep k, SpecRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . OMap.assocList

instance (SpecTranslate ctx a, Compactible a) => SpecTranslate ctx (CompactForm a) where
  type SpecRep (CompactForm a) = SpecRep a

  toSpecRep = toSpecRep . fromCompact

instance SpecTranslate ctx a => SpecTranslate ctx (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ctx a => SpecTranslate ctx (Set a) where
  type SpecRep (Set a) = Agda.HSSet (SpecRep a)

  toSpecRep = fmap Agda.MkHSSet . traverse toSpecRep . Set.toList

instance SpecTranslate ctx UnitInterval where
  type SpecRep UnitInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance SpecTranslate ctx NonNegativeInterval where
  type SpecRep NonNegativeInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  ) =>
  SpecTranslate ctx (Map k v)
  where
  type SpecRep (Map k v) = Agda.HSMap (SpecRep k) (SpecRep v)

  toSpecRep = fmap Agda.MkHSMap . traverse (bimapM toSpecRep toSpecRep) . Map.toList

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

askCtx :: forall b ctx. Inject ctx b => SpecTransM ctx b
askCtx = asks inject

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

toSpecRep_ ::
  SpecTranslate () a =>
  a ->
  SpecRep a
toSpecRep_ x = case runSpecTransM () $ toSpecRep x of
  Right res -> res
  Left v -> error $ "Failed to translate:\n" <> T.unpack v
