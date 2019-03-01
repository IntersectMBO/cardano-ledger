{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Chain.Update.Properties
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.Char (isAscii)
import qualified Data.Text as T

import qualified Hedgehog as H
import Hedgehog (Property, property, discover, withTests, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Update
  ( ApplicationName(..)
  , SoftwareVersion(..)
  , SystemTag(..)
  , applicationNameMaxLength
  , checkApplicationName
  , checkSoftwareVersion
  , checkSystemTag
  , systemTagMaxLength
  )

import Test.Cardano.Chain.Update.Gen
  (genApplicationName, genSoftwareVersion, genSystemTag)

-- Make sure `checkApplicationName` works for allowed values.
prop_checkApplicationName :: Property
prop_checkApplicationName = withTests 100 . property $ do
  aName <- forAll genApplicationName
  assertEitherIsRight checkApplicationName aName

-- Make sure `checkApplicationName` fails on names that are too long.
prop_checkApplicationNameTooLong :: Property
prop_checkApplicationNameTooLong = withTests 100 . property $ do
  (ApplicationName aName) <- forAll $ Gen.filter
    (\name -> T.length (getApplicationName name) >= applicationNameMaxLength)
    genApplicationName
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  assertEitherIsLeft
    checkApplicationName
    (ApplicationName $ aName `T.append` moreText)

-- Make sure `checkApplicationName` fails on names that are non-ascii.
prop_checkApplicationNameNotAscii :: Property
prop_checkApplicationNameNotAscii = withTests 100 . property $ do
  nonAscii <- forAll $ Gen.filter
    (all (== True) . map (not . isAscii))
    (Gen.string (Range.linear 1 applicationNameMaxLength) Gen.unicodeAll)
  assertEitherIsLeft checkApplicationName (ApplicationName $ T.pack nonAscii)

-- Make sure `checkSoftwareVersion` works for allowed values.
prop_checkSoftwareVersion :: Property
prop_checkSoftwareVersion = withTests 100 . property $ do
  sVer <- forAll genSoftwareVersion
  assertEitherIsRight checkSoftwareVersion sVer

-- Make sure `checkSoftwareVersion` fails on names that are too long.
prop_checkSoftwareVersionTooLong :: Property
prop_checkSoftwareVersionTooLong = withTests 100 . property $ do
  (ApplicationName aName) <- forAll $ Gen.filter
    (\name -> T.length (getApplicationName name) >= applicationNameMaxLength)
    genApplicationName
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  let appNameTooLong = ApplicationName $ aName `T.append` moreText
  sVersion <- forAll genSoftwareVersion
  let sVersion' = sVersion { svAppName = appNameTooLong }
  assertEitherIsLeft checkSoftwareVersion sVersion'

-- Make sure `checkSoftwareVersion` fails on names that are non-ascii.
prop_checkSoftwareVersionNotAscii :: Property
prop_checkSoftwareVersionNotAscii = withTests 100 . property $ do
  nonAscii <- forAll $ Gen.filter
    (all (== True) . map (not . isAscii))
    (Gen.string (Range.linear 1 applicationNameMaxLength) Gen.unicodeAll)
  let appNameNonascii = ApplicationName $ T.pack nonAscii
  sVersion <- forAll genSoftwareVersion
  let sVersion' = sVersion { svAppName = appNameNonascii }
  assertEitherIsLeft checkSoftwareVersion sVersion'

-- Make sure `checkSystemTag` works for allowed values.
prop_checkSystemTag :: Property
prop_checkSystemTag = withTests 100 . property $ do
  sTag <- forAll genSystemTag
  assertEitherIsRight checkSystemTag sTag

-- Make sure `checkSystemTag` fails on tags that are too long.
prop_checkSystemTagTooLong :: Property
prop_checkSystemTagTooLong = withTests 100 . property $ do
  (SystemTag tag) <- forAll $ Gen.filter
    (\sysTag -> T.length (getSystemTag sysTag) >= systemTagMaxLength)
    genSystemTag
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  let sysTagTooLong = SystemTag (tag `T.append` moreText)
  assertEitherIsLeft checkSystemTag sysTagTooLong

-- Make sure `checkSystemTag` fails on names that are non-ascii.
prop_checkSystemTagNotAscii :: Property
prop_checkSystemTagNotAscii = withTests 100 . property $ do
  nonAscii <- forAll $ Gen.filter
    (all (== True) . map (not . isAscii))
    (Gen.string (Range.linear 1 applicationNameMaxLength) Gen.unicodeAll)
  let sysTagNonascii = SystemTag $ T.pack nonAscii
  assertEitherIsLeft checkSystemTag sysTagNonascii

tests :: IO Bool
tests = H.checkParallel $$(discover)
