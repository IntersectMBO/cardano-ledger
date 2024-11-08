module Test.ImpSpec.Main (
  impSpecMain,
  impSpecConfig,
  impSpecMainWithConfig,
) where

import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
  hSetEncoding,
  stdout,
  utf8,
 )
import Test.Hspec
import Test.Hspec.Core.Runner (ColorMode (ColorAlways), Config (..), defaultConfig, hspecWith)

impSpecConfig :: Config
impSpecConfig =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

impSpecMainWithConfig :: Config -> Spec -> IO ()
impSpecMainWithConfig conf spec = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec

impSpecMain :: Spec -> IO ()
impSpecMain = impSpecMainWithConfig impSpecConfig
