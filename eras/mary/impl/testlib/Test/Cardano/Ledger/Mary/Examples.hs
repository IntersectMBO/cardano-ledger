{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Mary.Examples (
  ledgerExamples,
  exampleMaryBasedShelleyTxBody,
  exampleMaryBasedTxBody,
  exampleMultiAsset,
  exampleMultiAssetValue,
) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary (ApplyTxError (MaryApplyTxError), MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyDelegsPredFailure (DelplFailure),
  ShelleyDelplPredFailure (DelegFailure),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import qualified Data.Map.Strict as Map (singleton)
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Examples (
  exampleAllegraBasedShelleyTxBody,
  exampleAllegraBasedTxBody,
  mkAllegraBasedExampleTx,
 )
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  mkKeyHash,
  mkScriptHash,
  mkShelleyBasedLedgerExamples,
 )

ledgerExamples :: LedgerExamples MaryEra
ledgerExamples =
  mkShelleyBasedLedgerExamples
    ( MaryApplyTxError . pure . DelegsFailure . DelplFailure . DelegFailure $
        DelegateeNotRegisteredDELEG @MaryEra (mkKeyHash 1)
    )
    (exampleMultiAssetValue 1)
    (mkAllegraBasedExampleTx exampleMaryBasedShelleyTxBody)
    NoGenesis

exampleMaryBasedShelleyTxBody ::
  forall era.
  ( MaryEraTxBody era
  , ShelleyEraTxBody era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleMaryBasedShelleyTxBody =
  mkMaryBasedExampleTxBody (exampleAllegraBasedShelleyTxBody (exampleMultiAssetValue 1))

exampleMaryBasedTxBody ::
  forall era.
  ( MaryEraTxBody era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleMaryBasedTxBody =
  mkMaryBasedExampleTxBody (exampleAllegraBasedTxBody (exampleMultiAssetValue 1))

mkMaryBasedExampleTxBody ::
  forall era.
  MaryEraTxBody era =>
  TxBody TopTx era ->
  TxBody TopTx era
mkMaryBasedExampleTxBody txBody =
  txBody
    & mintTxBodyL .~ exampleMultiAsset 1

exampleMultiAssetValue :: Int -> MaryValue
exampleMultiAssetValue x = MaryValue (Coin 100) $ exampleMultiAsset x

exampleMultiAsset :: Int -> MultiAsset
exampleMultiAsset x =
  MultiAsset (Map.singleton policyId $ Map.singleton couttsCoin 1000)
  where
    policyId = PolicyID $ mkScriptHash x
    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"
