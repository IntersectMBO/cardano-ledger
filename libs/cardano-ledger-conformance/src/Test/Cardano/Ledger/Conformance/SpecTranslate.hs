{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate
  ( SpecTranslationError
  , SpecTranslate (..)
  ) where
import Data.Text (Text)
import Data.Coerce (Coercible, coerce)

type SpecTranslationError = Text

class SpecTranslate a where
  type SpecRep a
  type TestRep a
  type TestRep a = SpecRep a

  toSpecRep :: a -> Either SpecTranslationError (SpecRep a)

  specToTestRep :: SpecRep a -> TestRep a
  default specToTestRep :: Coercible (SpecRep a) (TestRep a) => SpecRep a -> TestRep a
  specToTestRep = coerce

  toTestRep :: a -> Either SpecTranslationError (TestRep a)
  toTestRep x = specToTestRep @a <$> toSpecRep x
