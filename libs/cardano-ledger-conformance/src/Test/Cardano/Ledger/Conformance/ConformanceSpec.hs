{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.ConformanceSpec (spec) where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.TxIn (TxId)
import Data.List (isInfixOf)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (FixupSpecRep, SpecTranslate (..), runSpecTransM, toTestRep)
import Test.Cardano.Ledger.Conformance.Spec.Conway ()

hashDisplayProp ::
  forall a.
  ( Typeable a
  , Arbitrary a
  , SpecTranslate () a
  , FixupSpecRep (SpecRep a)
  , ToExpr (SpecRep a)
  , ToExpr a
  ) =>
  Spec
hashDisplayProp = prop (show $ typeRep (Proxy @a)) $ do
  someHash <- arbitrary @a
  let
    specRes =
      case runSpecTransM () (toTestRep someHash) of
        Left e -> error $ "Failed to translate hash: " <> show e
        Right x -> x
  pure
    . counterexample ("impl expr: " <> showExpr someHash)
    . counterexample ("spec expr: " <> showExpr specRes)
    $ takeWhile (/= '"') (dropWhile (/= '"') (showExpr specRes)) `isInfixOf` showExpr someHash

-- TODO think of a more sensible way to test this

spec :: Spec
spec =
  describe "Translation" $ do
    describe "Hashes are displayed in the same way in the implementation and in the spec" $ do
      hashDisplayProp @(TxId StandardCrypto)
