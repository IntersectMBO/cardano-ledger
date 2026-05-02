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
  askCtx,
  withCtx,
  unComputationResult,
  unComputationResult_,
  toSpecRep_,
) where

import Cardano.Ledger.BaseTypes (Inject (..), NonNegativeInterval, UnitInterval, unboundRational)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Compactible (Compactible (..), fromCompact)
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
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

class SpecTranslate ctx era a where
  type SpecRep era a :: Type

  toSpecRep :: a -> SpecTransM ctx (SpecRep era a)

instance SpecTranslate ctx era () where
  type SpecRep era () = ()

  toSpecRep = pure

instance SpecTranslate ctx era Bool where
  type SpecRep era Bool = Bool

  toSpecRep = pure

instance SpecTranslate ctx era Integer where
  type SpecRep era Integer = Integer

  toSpecRep = pure

instance SpecTranslate ctx era Void where
  type SpecRep era Void = Void

  toSpecRep = absurd

instance SpecTranslate ctx era Word16 where
  type SpecRep era Word16 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx era Word32 where
  type SpecRep era Word32 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx era Word64 where
  type SpecRep era Word64 = Integer

  toSpecRep = pure . toInteger

instance
  ( SpecTranslate ctx era a
  , SpecTranslate ctx era b
  ) =>
  SpecTranslate ctx era (a, b)
  where
  type SpecRep era (a, b) = (SpecRep era a, SpecRep era b)

  toSpecRep (x, y) = (,) <$> toSpecRep @ctx @era x <*> toSpecRep @ctx @era y

instance SpecTranslate ctx era a => SpecTranslate ctx era [a] where
  type SpecRep era [a] = [SpecRep era a]

  toSpecRep = traverse (toSpecRep @ctx @era)

instance SpecTranslate ctx era a => SpecTranslate ctx era (StrictMaybe a) where
  type SpecRep era (StrictMaybe a) = Maybe (SpecRep era a)

  toSpecRep = toSpecRep @ctx @era . strictMaybeToMaybe

instance SpecTranslate ctx era a => SpecTranslate ctx era (Maybe a) where
  type SpecRep era (Maybe a) = Maybe (SpecRep era a)

  toSpecRep = traverse (toSpecRep @ctx @era)

instance SpecTranslate ctx era a => SpecTranslate ctx era (StrictSeq a) where
  type SpecRep era (StrictSeq a) = [SpecRep era a]

  toSpecRep = traverse (toSpecRep @ctx @era) . toList

instance SpecTranslate ctx era a => SpecTranslate ctx era (Seq a) where
  type SpecRep era (Seq a) = [SpecRep era a]

  toSpecRep = traverse (toSpecRep @ctx @era) . toList

instance SpecTranslate ctx era a => SpecTranslate ctx era (OSet a) where
  type SpecRep era (OSet a) = [SpecRep era a]

  toSpecRep = traverse (toSpecRep @ctx @era) . toList

instance
  ( SpecTranslate ctx era k
  , SpecTranslate ctx era v
  , Ord k
  ) =>
  SpecTranslate ctx era (OMap k v)
  where
  type SpecRep era (OMap k v) = [(SpecRep era k, SpecRep era v)]

  toSpecRep = traverse (bimapM (toSpecRep @ctx @era) (toSpecRep @ctx @era)) . OMap.assocList

instance (SpecTranslate ctx era a, Compactible a) => SpecTranslate ctx era (CompactForm a) where
  type SpecRep era (CompactForm a) = SpecRep era a

  toSpecRep = toSpecRep @ctx @era . fromCompact

instance SpecTranslate ctx era a => SpecTranslate ctx era (Sized a) where
  type SpecRep era (Sized a) = SpecRep era a

  toSpecRep (Sized x _) = toSpecRep @ctx @era x

instance SpecTranslate ctx era a => SpecTranslate ctx era (Set a) where
  type SpecRep era (Set a) = Agda.HSSet (SpecRep era a)

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep @ctx @era) . Set.toList

instance SpecTranslate ctx era UnitInterval where
  type SpecRep era UnitInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance SpecTranslate ctx era NonNegativeInterval where
  type SpecRep era NonNegativeInterval = Agda.Rational

  toSpecRep = pure . unboundRational

instance
  ( SpecTranslate ctx era k
  , SpecTranslate ctx era v
  ) =>
  SpecTranslate ctx era (Map k v)
  where
  type SpecRep era (Map k v) = Agda.HSMap (SpecRep era k) (SpecRep era v)

  toSpecRep = fmap Agda.MkHSMap . traverse (bimapM (toSpecRep @ctx @era) (toSpecRep @ctx @era)) . Map.toList

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
  forall era a.
  SpecTranslate () era a =>
  a ->
  SpecRep era a
toSpecRep_ x = case runSpecTransM () $ toSpecRep @() @era x of
  Right res -> res
  Left v -> error $ "Failed to translate:\n" <> T.unpack v
