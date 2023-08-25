{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.GenesisSpec (spec) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (Committee (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys
import Cardano.Ledger.Slot (EpochNo (..))
import Data.Aeson hiding (Encoding)
import Data.Default.Class (Default (def))
import Data.Map as Map
import Data.Ratio ((%))
import Paths_cardano_ledger_conway (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)

spec :: Spec
spec = do
  describe "Genesis Golden Spec" $ do
    goldenConwayGenesisJSON

goldenConwayGenesisJSON :: Spec
goldenConwayGenesisJSON =
  it "should deserialize to the default value" $ do
    let fileName = "test/data/conway-genesis.json"
        credMember =
          KeyHashObj
            (KeyHash "4e88cc2d27c364aaf90648a87dfb95f8ee103ba67fa1f12f5e86c42a") ::
            Credential 'ColdCommitteeRole StandardCrypto
        scriptMember =
          ScriptHashObj
            (ScriptHash "4e88cc2d27c364aaf90648a87dfb95f8ee103ba67fa1f12f5e86c42a") ::
            Credential 'ColdCommitteeRole StandardCrypto
        comm =
          Committee
            ( Map.fromList
                [
                  ( credMember
                  , EpochNo 1
                  )
                ,
                  ( scriptMember
                  , EpochNo 2
                  )
                ]
            )
            (unsafeBoundRational (1 % 2)) ::
            Committee Conway
    file <- getDataFileName fileName
    dec <- eitherDecodeFileStrict' file
    cg <- case dec of
      Left err -> error ("Failed to deserialize JSON: " ++ err)
      Right x -> pure x
    let expectedCg = def {cgCommittee = comm}
    cg `shouldBe` expectedCg
