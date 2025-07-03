{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Dijkstra (
  ledgerExamplesDijkstra,
  collateralOutput,
  exampleTxBodyDijkstra,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleDijkstraNewEpochState,
  exampleDijkstraGenesis,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.TxBody (TxBody (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
  hashData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  ApplyTxError (..),
  NewEpochState (..),
  ProposedPPUpdates (..),
  RewardAccount (..),
  TxId (..),
 )
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import Data.Default (Default (def))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Conway as SLE
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Mary as MarySLE
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)

-- ==============================================================

ledgerExamplesDijkstra ::
  SLE.ShelleyLedgerExamples DijkstraEra
ledgerExamplesDijkstra =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            wrapFailed @(ConwayDELEG DijkstraEra) @(ConwayLEDGER DijkstraEra) $
              DelegateeStakePoolNotRegisteredDELEG @DijkstraEra (SLE.mkKeyHash 1)
    , SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (SLE.mkScriptHash 1))
          , Right (KeyHashObj (SLE.mkKeyHash 2))
          ]
    , SLE.sleResultExamples = resultExamples
    , SLE.sleNewEpochState = exampleDijkstraNewEpochState
    , SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    , SLE.sleTranslationContext = exampleDijkstraGenesis
    }
  where
    resultExamples =
      SLE.ShelleyResultExamples
        { SLE.srePParams = def
        , SLE.sreProposedPPUpdates = examplePPPU
        , SLE.srePoolDistr = SLE.examplePoolDistr
        , SLE.sreNonMyopicRewards = SLE.exampleNonMyopicRewards
        , SLE.sreShelleyGenesis = SLE.testShelleyGenesis
        }
    examplePPPU =
      ProposedPPUpdates $
        Map.singleton
          (SLE.mkKeyHash 0)
          (emptyPParamsUpdate & ppuCollateralPercentageL .~ SJust 150)

collateralOutput :: BabbageTxOut DijkstraEra
collateralOutput =
  BabbageTxOut
    (mkAddr SLE.examplePayKey SLE.exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
    NoDatum
    SNothing

exampleTxBodyDijkstra :: TxBody DijkstraEra
exampleTxBodyDijkstra =
  DijkstraTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @DijkstraEra) $
            BabbageTxOut
              (mkAddr SLE.examplePayKey SLE.exampleStakeKey)
              (MarySLE.exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData datumExample) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @DijkstraEra) collateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    SLE.exampleConwayCerts -- txcerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    (Set.singleton $ SLE.mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
    (VotingProcedures mempty)
    mempty
    (SJust $ Coin 867530900000) -- current treasury value
    mempty
  where
    MaryValue _ exampleMultiAsset = MarySLE.exampleMultiAssetValue 3

datumExample :: Data DijkstraEra
datumExample = Data (P.I 191)

redeemerExample :: Data DijkstraEra
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx DijkstraEra
exampleTx =
  ShelleyTx
    exampleTxBodyDijkstra
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyDijkstra) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @DijkstraEra $ alwaysSucceeds @'PlutusV1 3)
            (alwaysSucceeds @'PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (ConwaySpending $ AsIx 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        mkAlonzoTxAuxData
          SLE.exampleAuxDataMap -- metadata
          [alwaysFails @'PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
    )

exampleTransactionInBlock :: AlonzoTx DijkstraEra
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a
  where
    ShelleyTx b w a = exampleTx

exampleDijkstraNewEpochState :: NewEpochState DijkstraEra
exampleDijkstraNewEpochState =
  SLE.exampleNewEpochState
    (MarySLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1))

exampleDijkstraGenesis :: DijkstraGenesis
exampleDijkstraGenesis = DijkstraGenesis
