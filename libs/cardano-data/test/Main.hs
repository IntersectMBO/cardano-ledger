{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
  hSetEncoding,
  stdout,
  utf8,
 )
import Test.Cardano.Data.Map.NonEmpty qualified as Map.NonEmpty
import Test.Cardano.Data.MapExtrasSpec (mapExtrasSpec)
import Test.Cardano.Data.OMap.StrictSpec qualified as OMap
import Test.Cardano.Data.OSet.StrictSpec qualified as OSet
import Test.Cardano.Data.Set.NonEmpty qualified as Set.NonEmpty
import Test.Hspec
import Test.Hspec.Runner

conf :: Config
conf =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

spec :: Spec
spec =
  describe "cardano-data" $ do
    mapExtrasSpec
    Set.NonEmpty.spec
    OSet.spec
    Map.NonEmpty.spec
    OMap.spec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec
