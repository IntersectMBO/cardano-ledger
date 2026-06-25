{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.QuickCheck (
  module QC,
  QuickCheckMaxRatio (..),
  testProperty,
) where

import GHC.Stack
import Test.Hspec
import Test.Hspec.QuickCheck
-- Using re-exported operators causes fourmolu not to know their fixity
import Test.QuickCheck as QC hiding ((.&&.), (.&.), (.||.), (=/=), (===), (==>), (><))

newtype QuickCheckMaxRatio = QuickCheckMaxRatio Int
  deriving (Num, Ord, Eq, Real, Enum, Integral)

testProperty :: (HasCallStack, Testable prop) => String -> prop -> Spec
testProperty = prop
