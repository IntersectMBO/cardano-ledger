{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
) where

import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules (DijkstraLEDGER, DijkstraMEMPOOL)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  Credential (..),
  NewEpochState,
  ProposedPPUpdates (ProposedPPUpdates),
  RewardAccount (..),
  TxId (..),
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (mkTxInPartial)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
 )
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  exampleTx,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState, exampleCollateralOutput)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era ()
import Test.Cardano.Ledger.Dijkstra.ImpTest (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  exampleNonMyopicRewards,
  examplePayKey,
  examplePoolDistr,
  exampleStakeKey,
  exampleStakePoolParams,
  keyToCredential,
  mkKeyHash,
  mkScriptHash,
  testShelleyGenesis,
 )

ledgerExamples :: LedgerExamples "MEMPOOL" DijkstraEra
ledgerExamples =
  mkLedgerExamples
    ( pure $
        wrapFailed @(DijkstraLEDGER DijkstraEra) @(DijkstraMEMPOOL DijkstraEra) $
          wrapFailed @(ConwayDELEG DijkstraEra) @(DijkstraLEDGER DijkstraEra) $
            DelegateeStakePoolNotRegisteredDELEG @DijkstraEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxDijkstra
    exampleDijkstraGenesis

mkLedgerExamples ::
  forall era.
  AlonzoEraPParams era =>
  NonEmpty (PredicateFailure (EraRule "MEMPOOL" era)) ->
  NewEpochState era ->
  Tx TopTx era ->
  TranslationContext era ->
  LedgerExamples "MEMPOOL" era
mkLedgerExamples
  applyTxError
  newEpochState
  tx
  translationContext =
    LedgerExamples
      { leTx = tx
      , leApplyTxError = applyTxError
      , lePParams = def
      , leProposedPPUpdates =
          ProposedPPUpdates $
            Map.singleton
              (mkKeyHash 0)
              (emptyPParamsUpdate & ppuCollateralPercentageL .~ SJust 150)
      , leNewEpochState = newEpochState
      , lePoolDistr = examplePoolDistr
      , leRewardsCredentials =
          Set.fromList
            [ Left (Coin 100)
            , Right (ScriptHashObj (mkScriptHash 1))
            , Right (KeyHashObj (mkKeyHash 2))
            ]
      , leNonMyopicRewards = exampleNonMyopicRewards
      , leTranslationContext = translationContext
      , leShelleyGenesis = testShelleyGenesis
      }

exampleTxDijkstra :: Tx TopTx DijkstraEra
exampleTxDijkstra =
  exampleTx
    exampleTxBodyDijkstra
    (DijkstraSpending $ AsIx 0)
    (RequireAllOf @DijkstraEra mempty)

exampleTxBodyDijkstra :: TxBody TopTx DijkstraEra
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
    exampleDijkstraCerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (keyToCredential exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    (OSet.fromList [KeyHashObj $ mkKeyHash 212, ScriptHashObj $ mkScriptHash 213]) -- guards
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
    (VotingProcedures mempty)
    mempty
    (SJust $ Coin 867530900000) -- current treasury value
    mempty
    mempty
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleDijkstraCerts :: OSet.OSet (DijkstraTxCert era)
exampleDijkstraCerts =
  OSet.fromList -- TODO should I add the new certs here?
    [ DijkstraTxCertPool (RegPool exampleStakePoolParams)
    ]
