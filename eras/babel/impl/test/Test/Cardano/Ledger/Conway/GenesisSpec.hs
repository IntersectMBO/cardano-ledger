{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Babel.GenesisSpec (spec, expectedBabelGenesis) where

import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babel (Babel)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.PParams
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import Data.Aeson hiding (Encoding)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Paths_cardano_ledger_Babel (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Babel.Genesis (expectedBabelGenesis)
import Test.Cardano.Slotting.Numeric ()

spec :: Spec
spec = do
  describe "BabelGenesis" $ do
    describe "Golden Spec" goldenBabelGenesisJSON
    prop "Upgrades" propBabelPParamsUpgrade

fileName :: String
fileName = "test/data/Babel-genesis.json"

goldenBabelGenesisJSON :: Spec
goldenBabelGenesisJSON =
  it "should deserialize to the default value" $ do
    file <- getDataFileName fileName
    dec <- eitherDecodeFileStrict' file
    cg <- case dec of
      Left err -> error ("Failed to deserialize JSON: " ++ err)
      Right x -> pure x
    cg `shouldBe` expectedBabelGenesis

propBabelPParamsUpgrade :: UpgradeBabelPParams Identity -> PParams Babbage -> Property
propBabelPParamsUpgrade ppu pp = property $ do
  let pp' = upgradePParams ppu pp :: PParams Babel
  pp' ^. ppPoolVotingThresholdsL `shouldBe` ucppPoolVotingThresholds ppu
  pp' ^. ppDRepVotingThresholdsL `shouldBe` ucppDRepVotingThresholds ppu
  pp' ^. ppCommitteeMinSizeL `shouldBe` ucppCommitteeMinSize ppu
  pp' ^. ppCommitteeMaxTermLengthL `shouldBe` ucppCommitteeMaxTermLength ppu
  pp' ^. ppGovActionLifetimeL `shouldBe` ucppGovActionLifetime ppu
  pp' ^. ppGovActionDepositL `shouldBe` ucppGovActionDeposit ppu
  pp' ^. ppDRepDepositL `shouldBe` ucppDRepDeposit ppu
  pp' ^. ppDRepActivityL `shouldBe` ucppDRepActivity ppu
  pp' ^. ppMinFeeRefScriptCostPerByteL `shouldBe` ucppMinFeeRefScriptCostPerByte ppu
  Map.lookup PlutusV3 (costModelsValid (pp' ^. ppCostModelsL))
    `shouldBe` Just (ucppPlutusV3CostModel ppu)
