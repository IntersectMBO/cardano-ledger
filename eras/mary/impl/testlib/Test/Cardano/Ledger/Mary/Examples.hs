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
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Allegra.Examples (
  exampleAllegraBasedTx,
 )
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  addShelleyBasedTopTxExampleFee,
  addShelleyToBabbageExampleProposedPUpdates,
  addShelleyToBabbageTxCerts,
  addShelleyToConwayTxCerts,
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
    exampleMaryTx
    NoGenesis
  where
    exampleMaryTx :: Tx TopTx MaryEra
    exampleMaryTx =
      exampleMaryBasedTx
        & addShelleyBasedTopTxExampleFee
        & addShelleyToBabbageExampleProposedPUpdates
        & addShelleyToBabbageTxCerts
        & addShelleyToConwayTxCerts

-- Complete transaction which is compatible with any era starting with Mary.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleMaryBasedTx ::
  forall era l.
  ( EraTx era
  , MaryEraTxBody era
  , Value era ~ MaryValue
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , Typeable l
  ) =>
  Tx l era
exampleMaryBasedTx =
  exampleAllegraBasedTx
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
