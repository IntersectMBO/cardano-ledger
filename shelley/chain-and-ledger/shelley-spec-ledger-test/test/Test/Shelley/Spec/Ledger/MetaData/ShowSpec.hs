{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Shelley.Spec.Ledger.MetaData.ShowSpec
  ( tests
  , prop_MetaData_Show
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Hedgehog

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as HI
import qualified Shelley.Spec.Ledger.MetaData as MD

tests :: IO Bool
tests = checkParallel $$discover

prop_MetaData_Show :: Property
prop_MetaData_Show = property $ do
  expected <- forAll . pure $ MD.MetaData (M.singleton 0 (MD.List [MD.I 5, MD.S "hello"]))
  result <- liftIO $ HI.runInterpreter $ do
    HI.setImports ["Prelude", "Shelley.Spec.Ledger.MetaData"]
    HI.interpret (show expected) expected
  actual <- evalEither result
  actual === expected
