{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Mary (
  ledgerExamplesMary,
  exampleMultiAssetValue,
  exampleMultiAsset,
) where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
import Test.Cardano.Ledger.Api.Examples.Consensus.Shelley

-- | ShelleyLedgerExamples for Mary era
ledgerExamplesMary :: ShelleyLedgerExamples MaryEra
ledgerExamplesMary = undefined

exampleMultiAssetValue :: Int -> MaryValue
exampleMultiAssetValue = undefined

exampleMultiAsset :: Int -> MultiAsset
exampleMultiAsset = undefined
