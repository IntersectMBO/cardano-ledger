{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
  SpecTransM,
  runSpecTransM,
  askCtx,
  toTestRep,
  OpaqueErrorString (..),
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Constrained.Base ()
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader (..), Reader, asks, runReader)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, (:*:) (..), (:+:) (..))
import Test.Cardano.Ledger.TreeDiff (Expr (..), ToExpr (..))

-- | OpaqueErrorString behaves like unit in comparisons, but contains an
-- error string that can be displayed.
newtype OpaqueErrorString = OpaqueErrorString String
  deriving (Generic)

instance Eq OpaqueErrorString where
  _ == _ = True

instance ToExpr OpaqueErrorString where
  -- Using `toExpr` on a `String` displays escape codes in place of unicode
  -- characters (e.g. "≡" becomes "\8802")
  -- TODO figure out a less hacky way to solve this problem
  toExpr (OpaqueErrorString x) = App "OpaqueErrorString" [App x []]

instance NFData OpaqueErrorString

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
