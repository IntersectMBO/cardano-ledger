{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Core (
  SpecTranslationError,
  SpecTranslate (..),
  SpecTransM,
  runSpecTransM,
  askTransCtx,
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Constrained.Base ()
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Test.Cardano.Ledger.Common (NFData)
import Test.Cardano.Ledger.TreeDiff (ToExpr)

type SpecTranslationError = Text

newtype SpecTransM ctx a
  = SpecTransM (ExceptT SpecTranslationError (Reader ctx) a)
  deriving (Functor, Applicative, Monad, MonadError SpecTranslationError, MonadReader ctx)

runSpecTransM :: ctx -> SpecTransM ctx a -> Either SpecTranslationError a
runSpecTransM ctx (SpecTransM m) = runReader (runExceptT m) ctx

class
  ( Eq (TestRep a)
  , ToExpr (TestRep a)
  , NFData (TestRep a)
  , NFData (SpecRep a)
  , Inject ctx (SpecTransContext a)
  ) =>
  SpecTranslate ctx a
  where
  type SpecRep a :: Type

  type TestRep a :: Type
  type TestRep a = SpecRep a

  type SpecTransContext a :: Type
  type SpecTransContext a = ()

  toSpecRep :: a -> SpecTransM ctx (SpecRep a)

  specToTestRep :: SpecRep a -> TestRep a
  default specToTestRep :: Coercible (SpecRep a) (TestRep a) => SpecRep a -> TestRep a
  specToTestRep = coerce

  toTestRep :: a -> SpecTransM ctx (TestRep a)
  toTestRep x = specToTestRep @ctx @a <$> toSpecRep x

askTransCtx :: forall a ctx. SpecTranslate ctx a => SpecTransM ctx (SpecTransContext a)
askTransCtx = asks inject
