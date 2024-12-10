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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Core (
  SpecTranslationError,
  SpecTranslate (..),
  FixupSpecRep (..),
  OpaqueErrorString (..),
  SpecTransM,
  runSpecTransM,
  askCtx,
  withCtx,
  toTestRep,
  showOpaqueErrorString,
  unComputationResult,
  unComputationResult_,
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Constrained.Base ()
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
import Data.Foldable (Foldable (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import qualified Lib as Agda
import Test.Cardano.Ledger.TreeDiff (Expr (..), ToExpr (..), showExpr)

type SpecTranslationError = Text

newtype SpecTransM ctx a
  = SpecTransM (ExceptT SpecTranslationError (Reader ctx) a)
  deriving (Functor, Applicative, Monad, MonadError SpecTranslationError, MonadReader ctx)

runSpecTransM :: ctx -> SpecTransM ctx a -> Either SpecTranslationError a
runSpecTransM ctx (SpecTransM m) = runReader (runExceptT m) ctx

class SpecTranslate ctx a where
  type SpecRep a :: Type

  toSpecRep :: a -> SpecTransM ctx (SpecRep a)

class GFixupSpecRep f where
  genericFixupSpecRep :: f a -> f a

instance GFixupSpecRep U1 where
  genericFixupSpecRep U1 = U1

instance GFixupSpecRep V1 where
  genericFixupSpecRep = \case {}

instance (GFixupSpecRep f, GFixupSpecRep g) => GFixupSpecRep (f :*: g) where
  genericFixupSpecRep (f :*: g) = genericFixupSpecRep f :*: genericFixupSpecRep g

instance (GFixupSpecRep f, GFixupSpecRep g) => GFixupSpecRep (f :+: g) where
  genericFixupSpecRep (L1 f) = L1 $ genericFixupSpecRep f
  genericFixupSpecRep (R1 f) = R1 $ genericFixupSpecRep f

instance GFixupSpecRep a => GFixupSpecRep (M1 i c a) where
  genericFixupSpecRep (M1 x) = M1 $ genericFixupSpecRep x

instance FixupSpecRep a => GFixupSpecRep (K1 i a) where
  genericFixupSpecRep (K1 x) = K1 $ fixup x

class FixupSpecRep a where
  fixup :: a -> a
  default fixup :: (Generic a, GFixupSpecRep (Rep a)) => a -> a
  fixup = to . genericFixupSpecRep . from

toTestRep :: (SpecTranslate ctx a, FixupSpecRep (SpecRep a)) => a -> SpecTransM ctx (SpecRep a)
toTestRep = fmap fixup . toSpecRep

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
  toExpr (OpaqueErrorString x) = App (T.unpack . T.unlines $ toList x) []

instance NFData OpaqueErrorString

showOpaqueErrorString :: ToExpr a => a -> OpaqueErrorString
showOpaqueErrorString = OpaqueErrorString . pure . T.pack . showExpr

unComputationResult :: Agda.ComputationResult Text a -> Either OpaqueErrorString a
unComputationResult (Agda.Success x) = Right x
unComputationResult (Agda.Failure e) = Left (OpaqueErrorString $ pure e)

unComputationResult_ :: Agda.ComputationResult Void a -> Either e a
unComputationResult_ (Agda.Success x) = Right x
unComputationResult_ (Agda.Failure x) = case x of {}
