{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for use in golden testing of datatypes

module Test.Cardano.Prelude.Golden
       ( discoverGolden
       , eachOf
       , goldenTestJSON
       , getText
       ) where

import           Cardano.Prelude

import           Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import           Hedgehog (Gen, Group, Property, PropertyT, TestLimit,
                     discoverPrefix, forAll, property, withTests, (===))
import           Hedgehog.Internal.Property (failWith)
import           Hedgehog.Internal.TH (TExpQ)


discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden_"

-- | Check that @eachOf@ @testLimit@ generated @things@ @hasProperty@
eachOf
  :: (Show a, HasCallStack)
  => TestLimit
  -> Gen a
  -> (a -> PropertyT IO ())
  -> Property
eachOf testLimit things hasProperty =
  withFrozenCallStack
    $   withTests testLimit
    .   property
    $   forAll things
    >>= hasProperty

goldenTestJSON
  :: (Eq a, FromJSON a, HasCallStack, Show a, ToJSON a)
  => a
  -> FilePath
  -> Property
goldenTestJSON x path = withFrozenCallStack $ withTests 1 . property $ do
  bs <- liftIO (LB.readFile path)
  encode x === bs
  case eitherDecode bs of
    Left  err -> failWith Nothing $ "could not decode: " <> show err
    Right x'  -> x === x'

-- | Text used for example values in a number of golden tests
--
--   Changing existing values in this string will break existing golden
--   tests, but it us OK to append more data to the end.
staticText :: Text
staticText
  = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText
