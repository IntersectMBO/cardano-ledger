{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Examples.Consensus where

import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import qualified Data.Map.Strict as Map (singleton)
import Data.Proxy
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Examples.Consensus
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.Examples.Consensus

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesMary :: ShelleyLedgerExamples Mary
ledgerExamplesMary =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @Mary))
    id
    (exampleMultiAssetValue 1)
    ((exampleAllegraTxBody (exampleMultiAssetValue 1)) & mintTxBodyL .~ exampleMultiAsset 1)
    exampleAllegraTxAuxData
    mempty

exampleMultiAssetValue :: Crypto c => Int -> MaryValue c
exampleMultiAssetValue x = MaryValue (Coin 100) $ exampleMultiAsset x

exampleMultiAsset :: forall c. Crypto c => Int -> MultiAsset c
exampleMultiAsset x =
  MultiAsset (Map.singleton policyId $ Map.singleton couttsCoin 1000)
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash x

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"
