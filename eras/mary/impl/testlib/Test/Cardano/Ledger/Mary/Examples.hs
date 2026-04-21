{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Mary.Examples (
  ledgerExamples,
  exampleMaryBasedTx,
  exampleMaryToBabbageTx,
  exampleMaryToConwayTx,
  exampleMultiAsset,
  exampleMultiAssetValue,
) where

import Cardano.Ledger.Allegra.Scripts (AllegraEraScript)
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
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Examples (
  exampleAllegraBasedTx,
  exampleAllegraToBabbageTx,
  exampleAllegraToConwayTx,
 )
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  examplePayKey,
  exampleStakeKey,
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
    exampleMaryToBabbageTx
    NoGenesis

-- Complete transaction which is compatible with any era starting with Mary.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleMaryBasedTx ::
  forall era.
  ( EraTx era
  , MaryEraTxBody era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era
exampleMaryBasedTx =
  addMaryBasedTxFeatures exampleAllegraBasedTx

-- Complete Mary transaction that is compatible until Babbage era
-- ('ConwayEra' is not an instance of 'ShelleyEraTxBody').
exampleMaryToBabbageTx ::
  forall era.
  ( EraTx era
  , MaryEraTxBody era
  , ShelleyEraTxBody era
  , Value era ~ MaryValue
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  ) =>
  Tx TopTx era
exampleMaryToBabbageTx =
  addMaryBasedTxFeatures exampleAllegraToBabbageTx

-- Complete Mary transaction that is compatible until Conway era
-- ('DijkstraEra' is not an instance of 'ShelleyEraTxCert').
exampleMaryToConwayTx ::
  forall era.
  ( EraTx era
  , MaryEraTxBody era
  , ShelleyEraTxCert era
  , Value era ~ MaryValue
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  ) =>
  Tx TopTx era
exampleMaryToConwayTx =
  addMaryBasedTxFeatures exampleAllegraToConwayTx

addMaryBasedTxFeatures ::
  forall era.
  ( EraTx era
  , MaryEraTxBody era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addMaryBasedTxFeatures tx =
  tx
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut (mkAddr examplePayKey exampleStakeKey) $ exampleMultiAssetValue 1
        ]
    & bodyTxL . mintTxBodyL .~ exampleMultiAsset 1

exampleMultiAssetValue :: Int -> MaryValue
exampleMultiAssetValue x = MaryValue (Coin 100) $ exampleMultiAsset x

exampleMultiAsset :: Int -> MultiAsset
exampleMultiAsset x =
  MultiAsset (Map.singleton policyId $ Map.singleton couttsCoin 1000)
  where
    policyId = PolicyID $ mkScriptHash x
    couttsCoin :: AssetName
    couttsCoin = AssetName "couttsCoin"
