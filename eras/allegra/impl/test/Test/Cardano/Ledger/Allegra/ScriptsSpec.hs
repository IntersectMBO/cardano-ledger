{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.ScriptsSpec (spec) where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..), inInterval)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.QuickCheck.Classes (
  commutativeMonoidLaws,
  commutativeSemigroupLaws,
  exponentialSemigroupLaws,
  idempotentSemigroupLaws,
  monoidLaws,
  semigroupLaws,
 )

spec :: Spec
spec = describe "ValidityInterval" $ do
  describe "Semigroup and Monoid laws" $
    testLawsGroup @ValidityInterval
      [ semigroupLaws
      , commutativeSemigroupLaws
      , idempotentSemigroupLaws
      , exponentialSemigroupLaws
      , monoidLaws
      , commutativeMonoidLaws
      ]
  -- This is the property `PV4.TxInfoSimplified.ttisValidRange` relies on: a top
  -- transaction is valid exactly at the slots where every sub-transaction in it
  -- is valid. This property is stated again `inInterval`.
  prop "<> is the intersection of the two intervals" $ \vi1 vi2 slot ->
    inInterval slot (vi1 <> vi2) === (inInterval slot vi1 && inInterval slot vi2)
