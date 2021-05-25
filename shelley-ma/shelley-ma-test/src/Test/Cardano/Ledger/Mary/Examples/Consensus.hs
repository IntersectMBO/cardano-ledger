{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Mary.Examples.Consensus where

import qualified Data.Map.Strict as Map (singleton)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary (MaryEra)
import Data.Proxy
import Test.Cardano.Ledger.Allegra.Examples.Consensus
import Test.Shelley.Spec.Ledger.Examples.Consensus
import Test.Shelley.Spec.Ledger.Orphans ()
import Cardano.Ledger.Mary.Value

type StandardMary = MaryEra StandardCrypto

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesMary :: ShelleyLedgerExamples StandardMary
ledgerExamplesMary =
    defaultShelleyLedgerExamples
      (mkWitnessesPreAlonzo (Proxy @StandardMary))
      id
      exampleMultiAssetValue
      exampleTxBodyMary
      exampleAuxiliaryDataMA

exampleMultiAssetValue :: forall c. Cardano.Ledger.Crypto.Crypto c => Cardano.Ledger.Mary.Value.Value c
exampleMultiAssetValue =
    Value 100 $ Map.singleton policyId $ Map.singleton couttsCoin 1000
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash 1

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"

exampleTxBodyMary :: Cardano.Ledger.Core.TxBody StandardMary
exampleTxBodyMary = exampleTxBodyMA exampleMultiAssetValue
