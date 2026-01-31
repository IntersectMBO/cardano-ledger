module Main where

import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
  hSetEncoding,
  stdout,
  utf8,
 )
import Test.Hspec
import Test.Hspec.Core.Runner (ColorMode (ColorAlways), Config (..), defaultConfig, hspecWith)
import Test.VMap

-- ====================================================================================

customSpecConfig :: Config
customSpecConfig =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

customSpecMainWithConfig :: Config -> Spec -> IO ()
customSpecMainWithConfig conf spec = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec

customSpecMain :: Spec -> IO ()
customSpecMain = customSpecMainWithConfig customSpecConfig

-- ====================================================================================

tests :: Spec
tests =
  describe "vector-map" $ do
    vMapTests

main :: IO ()
main = customSpecMain tests
