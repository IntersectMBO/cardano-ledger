{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Mary (
  ledgerExamplesMary,
  exampleMultiAssetValue,
  exampleMultiAsset,
) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import qualified Data.Map.Strict as Map (singleton)
import Data.Proxy
import Lens.Micro
import Test.Cardano.Ledger.Api.Examples.Consensus.Allegra
import Test.Cardano.Ledger.Api.Examples.Consensus.Shelley

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesMary :: ShelleyLedgerExamples MaryEra
ledgerExamplesMary =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @MaryEra))
    id
    (exampleMultiAssetValue 1)
    ((exampleAllegraTxBody (exampleMultiAssetValue 1)) & mintTxBodyL .~ exampleMultiAsset 1)
    exampleAllegraTxAuxData
    NoGenesis

exampleMultiAssetValue :: Int -> MaryValue
exampleMultiAssetValue x = MaryValue (Coin 100) $ exampleMultiAsset x

exampleMultiAsset :: Int -> MultiAsset
exampleMultiAsset x =
  MultiAsset (Map.singleton policyId $ Map.singleton couttsCoin 1000)
  where
    policyId = PolicyID $ mkScriptHash x
    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"
