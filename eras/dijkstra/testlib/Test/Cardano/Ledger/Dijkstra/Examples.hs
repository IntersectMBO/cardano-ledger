{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
  exampleDijkstraGenesis,
) where

import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  ApplyTxError (..),
  RewardAccount (..),
  TxId (..),
 )
import Cardano.Ledger.TxIn (mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  exampleTx,
  mkLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState, exampleCollateralOutput)
import Test.Cardano.Ledger.Conway.Examples (exampleConwayCerts)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era ()
import Test.Cardano.Ledger.Dijkstra.ImpTest (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  keyToCredential,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples DijkstraEra
ledgerExamples =
  mkLedgerExamples
    ( ApplyTxError $
        pure $
          wrapFailed @(ConwayDELEG DijkstraEra) @(ConwayLEDGER DijkstraEra) $
            DelegateeStakePoolNotRegisteredDELEG @DijkstraEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxDijkstra
    exampleDijkstraGenesis

exampleTxDijkstra :: Tx DijkstraEra
exampleTxDijkstra = exampleTx exampleTxBodyDijkstra (ConwaySpending $ AsIx 0)

exampleTxBodyDijkstra :: TxBody DijkstraEra
exampleTxBodyDijkstra =
  DijkstraTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @DijkstraEra) $
            BabbageTxOut
              (mkAddr examplePayKey exampleStakeKey)
              (exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData exampleDatum) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @DijkstraEra) exampleCollateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    exampleConwayCerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (keyToCredential exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    (Set.singleton $ mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
    (VotingProcedures mempty)
    mempty
    (SJust $ Coin 867530900000) -- current treasury value
    mempty
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3
