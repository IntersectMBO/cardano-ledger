{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Update.Properties
  ( tests,
  )
where

import Cardano.Chain.Update
  ( ApplicationName (..),
    ApplicationNameError (..),
    SoftwareVersion (..),
    SoftwareVersionError (..),
    SystemTag (..),
    SystemTagError (..),
    applicationNameMaxLength,
    checkApplicationName,
    checkSoftwareVersion,
    checkSystemTag,
    systemTagMaxLength,
  )
import Cardano.Prelude
import Data.Data (Constr, toConstr)
import qualified Data.Text as T
import Hedgehog (forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Cardano.Chain.Update.Gen
  ( genApplicationName,
    genSoftwareVersion,
    genSystemTag,
  )
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)

-- Make sure `checkApplicationName` works for allowed values.
ts_prop_checkApplicationName :: TSProperty
ts_prop_checkApplicationName = withTestsTS 100 . property $ do
  aName <- forAll genApplicationName
  assertIsRight $ checkApplicationName aName

-- Make sure `checkApplicationName` fails on names that are too long.
ts_prop_checkApplicationNameTooLong :: TSProperty
ts_prop_checkApplicationNameTooLong = withTestsTS 100 . property $ do
  (ApplicationName aName) <-
    forAll $
      Gen.filter
        (\name -> T.length (unApplicationName name) >= applicationNameMaxLength)
        genApplicationName
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  assertIsLeftConstr
    dummyAppNameTooLong
    (checkApplicationName . ApplicationName $ aName `T.append` moreText)

-- Make sure `checkApplicationName` fails on names that are non-ascii.
ts_prop_checkApplicationNameNotAscii :: TSProperty
ts_prop_checkApplicationNameNotAscii = withTestsTS 100 . property $ do
  nonAscii <-
    forAll $
      Gen.filter
        (all (== True) . map (not . isAscii))
        (Gen.string (Range.linear 1 applicationNameMaxLength) Gen.unicodeAll)
  assertIsLeftConstr
    dummyAppNameNotAscii
    (checkApplicationName $ ApplicationName $ T.pack nonAscii)

-- Make sure `checkSoftwareVersion` works for allowed values.
ts_prop_checkSoftwareVersion :: TSProperty
ts_prop_checkSoftwareVersion = withTestsTS 100 . property $ do
  sVer <- forAll genSoftwareVersion
  assertIsRight $ checkSoftwareVersion sVer

-- Make sure `checkSoftwareVersion` fails on names that are too long.
ts_prop_checkSoftwareVersionTooLong :: TSProperty
ts_prop_checkSoftwareVersionTooLong = withTestsTS 100 . property $ do
  (ApplicationName aName) <-
    forAll $
      Gen.filter
        (\name -> T.length (unApplicationName name) >= applicationNameMaxLength)
        genApplicationName
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  let appNameTooLong = ApplicationName $ aName `T.append` moreText
  sVersion <- forAll genSoftwareVersion
  let sVersion' = sVersion {svAppName = appNameTooLong}
  assertIsLeftConstr dummySoftVerTooLong (checkSoftwareVersion sVersion')

-- Make sure `checkSoftwareVersion` fails on names that are non-ascii.
ts_prop_checkSoftwareVersionNotAscii :: TSProperty
ts_prop_checkSoftwareVersionNotAscii = withTestsTS 100 . property $ do
  nonAscii <-
    forAll $
      Gen.filter
        (all (== True) . map (not . isAscii))
        (Gen.string (Range.linear 1 applicationNameMaxLength) Gen.unicodeAll)
  let appNameNonascii = ApplicationName $ T.pack nonAscii
  sVersion <- forAll genSoftwareVersion
  let sVersion' = sVersion {svAppName = appNameNonascii}
  assertIsLeftConstr dummySoftVerNotAscii (checkSoftwareVersion sVersion')

-- Make sure `checkSystemTag` works for allowed values.
ts_prop_checkSystemTag :: TSProperty
ts_prop_checkSystemTag = withTestsTS 100 . property $ do
  sTag <- forAll genSystemTag
  assertIsRight $ checkSystemTag sTag

-- Make sure `checkSystemTag` fails on tags that are too long.
ts_prop_checkSystemTagTooLong :: TSProperty
ts_prop_checkSystemTagTooLong = withTestsTS 100 . property $ do
  (SystemTag tag) <-
    forAll $
      Gen.filter
        (\sysTag -> T.length (getSystemTag sysTag) >= systemTagMaxLength)
        genSystemTag
  moreText <- forAll $ Gen.text (Range.linear 1 20) Gen.ascii
  let sysTagTooLong = SystemTag (tag `T.append` moreText)
  assertIsLeftConstr dummySysTagTooLong (checkSystemTag sysTagTooLong)

-- Make sure `checkSystemTag` fails on names that are non-ascii.
ts_prop_checkSystemTagNotAscii :: TSProperty
ts_prop_checkSystemTagNotAscii = withTestsTS 100 . property $ do
  nonAscii <-
    forAll $
      Gen.filter
        (all (== True) . map (not . isAscii))
        (Gen.string (Range.linear 1 systemTagMaxLength) Gen.unicodeAll)
  let sysTagNonascii = SystemTag $ T.pack nonAscii
  assertIsLeftConstr dummySysTagNotAscii (checkSystemTag sysTagNonascii)

tests :: TSGroup
tests = $$discoverPropArg

--------------------------------------------------------------------------------
-- Dummy values for constructor comparison in assertIsLeftConstr tests
--------------------------------------------------------------------------------

dummyAppNameNotAscii :: Constr
dummyAppNameNotAscii = toConstr $ ApplicationNameNotAscii "dummyValue"

dummyAppNameTooLong :: Constr
dummyAppNameTooLong = toConstr $ ApplicationNameTooLong "dummyValue"

dummySoftVerNotAscii :: Constr
dummySoftVerNotAscii =
  toConstr . SoftwareVersionApplicationNameError $
    ApplicationNameNotAscii
      "dummyValue"

dummySoftVerTooLong :: Constr
dummySoftVerTooLong =
  toConstr . SoftwareVersionApplicationNameError $
    ApplicationNameTooLong
      "dummyValue"

dummySysTagNotAscii :: Constr
dummySysTagNotAscii = toConstr $ SystemTagNotAscii "dummyValue"

dummySysTagTooLong :: Constr
dummySysTagTooLong = toConstr $ SystemTagTooLong "dummyValue"
