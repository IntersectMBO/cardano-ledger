{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (ApplyTxError (..), DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules (DijkstraLEDGER, DijkstraMEMPOOL)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceIntervals (..), DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxBody (
  accountBalanceIntervalsTxBodyL,
  directDepositsTxBodyL,
  guardsTxBodyL,
  subTransactionsTxBodyL,
 )
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  exampleTx,
  mkLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState, exampleCollateralOutput)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Dijkstra.Era ()
import Test.Cardano.Ledger.Dijkstra.ImpTest (exampleDijkstraGenesis)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  exampleStakeKey,
  exampleStakePoolParams,
  keyToCredential,
  mkKeyHash,
  mkScriptHash,
 )

ledgerExamples :: LedgerExamples DijkstraEra
ledgerExamples =
  mkLedgerExamples
    ( DijkstraApplyTxError $
        pure $
          wrapFailed @(DijkstraLEDGER DijkstraEra) @(DijkstraMEMPOOL DijkstraEra) $
            wrapFailed @(ConwayDELEG DijkstraEra) @(DijkstraLEDGER DijkstraEra) $
              DelegateeStakePoolNotRegisteredDELEG (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxDijkstra
    exampleDijkstraGenesis

exampleTxDijkstra :: Tx TopTx DijkstraEra
exampleTxDijkstra =
  exampleTx
    exampleTxBodyDijkstra
    (DijkstraSpending $ AsIx 0)
    (RequireAllOf mempty)

exampleTxBodyDijkstra :: TxBody TopTx DijkstraEra
exampleTxBodyDijkstra =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]
    & collateralInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]
    & referenceInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]
    & outputsTxBodyL
      .~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV2 3)
        ]
    & collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & totalCollateralTxBodyL .~ SJust (Coin 8675309)
    & certsTxBodyL .~ StrictSeq.fromList (Set.toList $ OSet.toSet exampleDijkstraCerts)
    & withdrawalsTxBodyL
      .~ Withdrawals
        ( Map.singleton
            (AccountAddress Testnet (AccountId (keyToCredential exampleStakeKey)))
            (Coin 100)
        )
    & feeTxBodyL .~ Coin 999
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
    & guardsTxBodyL .~ OSet.fromList [KeyHashObj $ mkKeyHash 212, ScriptHashObj $ mkScriptHash 213]
    & mintTxBodyL .~ exampleMultiAsset
    & scriptIntegrityHashTxBodyL .~ SJust (mkDummySafeHash 42)
    & auxDataHashTxBodyL .~ SJust (TxAuxDataHash $ mkDummySafeHash 42)
    & networkIdTxBodyL .~ SJust Mainnet
    & votingProceduresTxBodyL .~ VotingProcedures mempty
    & proposalProceduresTxBodyL .~ mempty
    & currentTreasuryValueTxBodyL .~ SJust (Coin 867530900000)
    & treasuryDonationTxBodyL .~ mempty
    & subTransactionsTxBodyL .~ mempty
    & directDepositsTxBodyL .~ DirectDeposits mempty
    & accountBalanceIntervalsTxBodyL .~ AccountBalanceIntervals mempty
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleDijkstraCerts :: OSet.OSet (DijkstraTxCert era)
exampleDijkstraCerts =
  OSet.fromList -- TODO should I add the new certs here?
    [ DijkstraTxCertPool (RegPool exampleStakePoolParams)
    ]
