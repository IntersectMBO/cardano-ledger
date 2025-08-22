{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Era.Spec (
  everyEraSpec,
  goldenFilePath,
  goldenJsonFilePath,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Genesis
import Control.Monad.IO.Class
import Data.Aeson (eitherDecodeFileStrict', encode)
import Data.Char (toLower)
import System.FilePath ((</>))
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.ImpTest

goldenFilePath :: FilePath
goldenFilePath = "golden"

goldenJsonFilePath :: FilePath
goldenJsonFilePath = goldenFilePath </> "json"

-- | This spec is applicable to all eras and will be executed for every era starting with Shelley.
everyEraSpec :: forall era. EraImp era => Spec
everyEraSpec =
  describe "Spec for every Era" $ do
    let eraLowerName = map toLower $ eraName @era
    describe "JSON" $ do
      describe "Golden" $ do
        withImpInit @KeyPairSpec $ do
          it "Genesis" $ do
            let decodeJsonGenesis = do
                  eitherGenesis <-
                    liftIO $ do
                      genesisFilePath <-
                        getEraDataFileName @era $
                          goldenJsonFilePath </> eraLowerName <> "-genesis.json"
                      eitherDecodeFileStrict' genesisFilePath
                  expectRightDeep eitherGenesis
            genesis <- impAnn "Initializing Genesis" $ initGenesis @era
            mkGenesisWith @era decodeJsonGenesis `shouldReturn` genesis
