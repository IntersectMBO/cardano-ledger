{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Shelley.Spec.Ledger.MetaData.ShowSpec
  ( tests
  , prop_MetaData_Show
  ) where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Hedgehog
import           Language.Haskell.Interpreter (OptionVal (..))
import           System.FilePath.Posix        ((</>))

import qualified Data.Map                     as M
import qualified GHC.Paths                    as GHC
import qualified Hedgehog                     as H
import qualified Language.Haskell.Interpreter as HI
import qualified Shelley.Spec.Ledger.MetaData as MD
import qualified System.Directory             as IO
import qualified System.Environment           as IO

tests :: IO Bool
tests = checkParallel $$discover

prop_MetaData_Show :: Property
prop_MetaData_Show = property $ do
  maybeProjectRoot <- liftIO $ IO.lookupEnv "SHELLEY_SPEC_LEDGER_TEST_ROOT"
  H.annotateShow maybeProjectRoot
  H.annotateShow GHC.libdir
  expected <- forAll . pure $ MD.MetaData (M.singleton 0 (MD.List [MD.I 5, MD.S "hello"]))
  forM_ maybeProjectRoot $ \projectRoot -> do
    ls <- H.evalM . liftIO $ IO.listDirectory projectRoot
    H.annotateShow ls
  result <- liftIO $ HI.runInterpreter $ do
    forM_ maybeProjectRoot $ \projectRoot -> do
      HI.set
        [ HI.searchPath :=
          [ projectRoot </> "shelley" </> "chain-and-ledger" </> "executable-spec" </> "src"
          -- , projectRoot </> "shelley" </> "chain-and-ledger" </> "shelley-spec-ledger-test" </> "test"
          ]
        ]
    HI.setImports ["Prelude", "Shelley.Spec.Ledger.MetaData"]
    HI.interpret (show expected) expected
  actual <- evalEither result
  actual === expected
