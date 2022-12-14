module Test.Cardano.Ledger.Common
  ( module X,
    ledgerTestMain,
    ledgerTestMainWith,
    ledgerHspecConfig,
  )
where

import Control.Monad as X (forM_, unless, when)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    hSetEncoding,
    stdout,
    utf8,
  )
import Test.Hspec as X
import Test.Hspec.QuickCheck as X
import Test.Hspec.Runner
import Test.QuickCheck as X

ledgerHspecConfig :: Config
ledgerHspecConfig =
  defaultConfig
    { configTimes = True,
      configColorMode = ColorAlways
    }

ledgerTestMainWith :: Config -> Spec -> IO ()
ledgerTestMainWith conf spec = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec

ledgerTestMain :: Spec -> IO ()
ledgerTestMain = ledgerTestMainWith ledgerHspecConfig
