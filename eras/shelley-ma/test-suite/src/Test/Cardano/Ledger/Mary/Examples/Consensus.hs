{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Examples.Consensus where

import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody)
import qualified Data.Map.Strict as Map (singleton)
import Data.Proxy
import Test.Cardano.Ledger.Allegra.Examples.Consensus
import Test.Cardano.Ledger.Shelley.Examples.Consensus
import Test.Cardano.Ledger.Shelley.Orphans ()

type StandardMary = MaryEra CC.StandardCrypto

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesMary :: ShelleyLedgerExamples StandardMary
ledgerExamplesMary =
  defaultShelleyLedgerExamples
    (mkWitnessesPreAlonzo (Proxy @StandardMary))
    id
    (exampleMultiAssetValue 1)
    exampleTxBodyMary
    exampleAuxiliaryDataMA
    ()

exampleMultiAssetValue ::
  forall c.
  CC.Crypto c =>
  Int ->
  MaryValue c
exampleMultiAssetValue x =
  MaryValue 100 $ MultiAsset (Map.singleton policyId $ Map.singleton couttsCoin 1000)
  where
    policyId :: PolicyID c
    policyId = PolicyID $ mkScriptHash x

    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"

exampleTxBodyMary :: MATxBody StandardMary
exampleTxBodyMary = exampleTxBodyMA (exampleMultiAssetValue 1)
