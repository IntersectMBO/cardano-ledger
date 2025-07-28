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

import Cardano.Ledger.BaseTypes (Inject (..))
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
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
