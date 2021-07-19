{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Examples.Consensus where

import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Value
import qualified Data.Map.Strict as Map (singleton)
import Data.Proxy
import Test.Cardano.Ledger.Allegra.Examples.Consensus
import Test.Shelley.Spec.Ledger.Examples.Consensus
import Test.Shelley.Spec.Ledger.Orphans ()

type StandardMary = MaryEra StandardCrypto

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesMary :: ShelleyLedgerExamples StandardMary
ledgerExamplesMary =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @StandardMary))
    id
    (exampleMultiAssetValue 1)
    exampleTxBodyMary
    exampleAuxiliaryDataMA

exampleMultiAssetValue ::
  forall c.
  Cardano.Ledger.Crypto.Crypto c =>
  Int ->
  Cardano.Ledger.Mary.Value.Value c
exampleMultiAssetValue x =
  Value 100 $ Map.singleton policyId $ Map.singleton couttsCoin 1000
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash x

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"

exampleTxBodyMary :: Cardano.Ledger.Core.TxBody StandardMary
exampleTxBodyMary = exampleTxBodyMA (exampleMultiAssetValue 1)
