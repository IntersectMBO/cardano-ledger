{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.ConformanceSpec (spec) where

import Cardano.Ledger.Hashes (ADDRHASH)
import Cardano.Ledger.TxIn (TxId)
import Data.List (isInfixOf)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (
  SpecNormalize (..),
  SpecTranslate (..),
  hashToInteger,
  integerToHash,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.Spec.Conway ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (vkeyFromInteger, vkeyToInteger)

hashDisplayProp ::
  forall a.
  ( Typeable a
  , Arbitrary a
  , SpecTranslate () a
  , SpecNormalize (SpecRep a)
  , ToExpr (SpecRep a)
  , ToExpr a
  ) =>
  Spec
hashDisplayProp = prop (show $ typeRep (Proxy @a)) $ do
  someHash <- arbitrary @a
  let
    specRes =
      case runSpecTransM () (specNormalize <$> toSpecRep someHash) of
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
      hashDisplayProp @TxId
    describe "Utility properties" $ do
      prop "vkeyToInteger and vkeyFromInteger are inverses" $
        \vk -> vkeyFromInteger (vkeyToInteger vk) === Just vk
      prop "hashToInteger and integerToHash are inverses" $
        \h -> integerToHash (hashToInteger @ADDRHASH h) === Just h
