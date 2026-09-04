{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Dijkstra.Examples (
  ledgerExamples,
  exampleDijkstraTx,
  exampleDijkstraBasedTopTx,
  exampleDijkstraBasedSubTx,
  exampleDijkstraOnwardsEraPParams,
  exampleDijkstraOnwardsEraPParamsUpdate,
  exampleDijkstraGenesis,
  exampleBlsKey,
) where

import Cardano.Crypto.DSIGN (
  BLS12381MinSigDSIGN,
  DSIGNAggregatable (createPossessionProofDSIGN),
  DSIGNAlgorithm (deriveVerKeyDSIGN, genKeyDSIGNWithContext),
  seedSizeDSIGN,
 )
import Cardano.Crypto.DSIGN.BLS12381.Internal (minSigPoPDST)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (
  Exclusive (..),
  Inclusive (..),
  Milliseconds32 (..),
  Network (..),
  StrictMaybe (..),
  boundRational,
  knownNonZeroBounded,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (ApplyTxError (..), DijkstraEra)
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (
  DijkstraEraPParams,
  UpgradeDijkstraPParams (..),
  ppLeiosAnnouncementPeriodLengthL,
  ppLeiosCommitteeSizeL,
  ppLeiosDiffusionPeriodLengthL,
  ppLeiosQuorumStakeThresholdL,
  ppLeiosVotePeriodLengthL,
  ppMaxEndorserBlockExUnitsL,
  ppMaxEndorserBlockReferencesSizeL,
  ppMaxEndorserBlockTxsSizeL,
  ppMaxRefScriptSizePerBlockL,
  ppMaxRefScriptSizePerEndorserBlockL,
  ppMaxRefScriptSizePerTxL,
  ppRefScriptCostMultiplierL,
  ppRefScriptCostStrideL,
  ppuLeiosAnnouncementPeriodLengthL,
  ppuLeiosCommitteeSizeL,
  ppuLeiosDiffusionPeriodLengthL,
  ppuLeiosQuorumStakeThresholdL,
  ppuLeiosVotePeriodLengthL,
  ppuMaxEndorserBlockExUnitsL,
  ppuMaxEndorserBlockReferencesSizeL,
  ppuMaxEndorserBlockTxsSizeL,
  ppuMaxRefScriptSizePerBlockL,
  ppuMaxRefScriptSizePerEndorserBlockL,
  ppuMaxRefScriptSizePerTxL,
  ppuRefScriptCostMultiplierL,
  ppuRefScriptCostStrideL,
 )
import qualified Cardano.Ledger.Dijkstra.Rules as Dijkstra
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraEraScript,
  pattern GuardingPurpose,
 )
import Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody,
  accountBalanceIntervalsTxBodyL,
  directDepositsTxBodyL,
  guardsTxBodyL,
  requiredTopLevelGuardsTxBodyL,
  subTransactionsTxBodyL,
 )
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus (OrdExUnits (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Cardano.Ledger.State (
  BlsKey (..),
  StakePoolParams (..),
 )
import qualified Data.ByteString as Strict
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro ((%~), (&), (.~), (<>~))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  mkAlonzoBasedLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState)
import Test.Cardano.Ledger.Conway.Examples (
  exampleConwayBasedTopTx,
  exampleConwayBasedTx,
  exampleConwayOnwardsEraPParams,
  exampleConwayOnwardsEraPParamsUpdate,
 )
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (alwaysSucceedsPlutus, testingCostModel)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  addShelleyBasedTopTxExampleFee,
  examplePayKey,
  exampleStakeKey,
  exampleStakePoolParams,
  mkKeyHash,
  mkScriptHash,
 )

ledgerExamples :: LedgerExamples DijkstraEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( DijkstraApplyTxError $
        pure $
          Dijkstra.LedgerFailure $
            injectFailure $
              Conway.DelegateeStakePoolNotRegisteredDELEG (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleDijkstraTx
    exampleDijkstraGenesis

exampleDijkstraTx :: Tx TopTx DijkstraEra
exampleDijkstraTx =
  exampleDijkstraBasedTopTx
    & addShelleyBasedTopTxExampleFee

exampleDijkstraGenesis :: DijkstraGenesis
exampleDijkstraGenesis =
  DijkstraGenesis
    { dgUpgradePParams =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1024 * 1024 -- 1MiB
          , udppMaxRefScriptSizePerTx = 200 * 1024 -- 200KiB
          , udppRefScriptCostStride = knownNonZeroBounded @25_600 -- 25 KiB
          , udppRefScriptCostMultiplier = fromJust $ boundRational 1.2
          , udppMaxPledgeLeverage = MaxPledgeLeverage SNothing
          , udppMinPoolMargin = fromJust $ boundRational 0.015
          , udppPlutusV4CostModel = testingCostModel PlutusV4
          , -- Feasible values of CIP-164 Table 7
            udppLeiosAnnouncementPeriodLength = Milliseconds32 1_000 -- L_hdr
          , udppLeiosVotePeriodLength = Milliseconds32 4_000 -- L_vote
          , udppLeiosDiffusionPeriodLength = Milliseconds32 7_000 -- L_diff
          , udppLeiosCommitteeSize = 900 -- N_c
          , udppLeiosQuorumStakeThreshold = fromJust $ boundRational 0.75 -- tau
          , udppMaxEndorserBlockReferencesSize = 512 * 1024 -- 512 KiB
          , udppMaxEndorserBlockTxsSize = 12 * 1024 * 1024 -- 12 MiB
          , udppMaxEndorserBlockExUnits = OrdExUnits $ ExUnits 7_000_000_000 2_000_000_000_000
          , udppMaxRefScriptSizePerEndorserBlock = 12 * 1024 * 1024 -- 12 MiB
          }
    }

exampleDijkstraBasedTopTx ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , Value era ~ MaryValue
  , DijkstraEraScript era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , EraPlutusTxInfo PlutusV4 era
  ) =>
  Tx TopTx era
exampleDijkstraBasedTopTx =
  exampleConwayBasedTopTx
    & addDijkstraBasedTxFeatures
    & addDijkstraBasedTopTxFeatures

exampleDijkstraBasedSubTx ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , Value era ~ MaryValue
  , DijkstraEraScript era
  , EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , EraPlutusTxInfo PlutusV4 era
  ) =>
  Tx SubTx era
exampleDijkstraBasedSubTx =
  exampleConwayBasedTx
    & addDijkstraBasedTxFeatures
    & addDijkstraBasedSubTxFeatures

addDijkstraBasedTopTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV2 era
  , EraPlutusTxInfo 'PlutusV3 era
  , EraPlutusTxInfo 'PlutusV4 era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addDijkstraBasedTopTxFeatures tx =
  tx
    & bodyTxL . subTransactionsTxBodyL .~ OMap.fromFoldable [exampleDijkstraBasedSubTx]

addDijkstraBasedSubTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  ) =>
  Tx SubTx era ->
  Tx SubTx era
addDijkstraBasedSubTxFeatures tx =
  tx
    & bodyTxL . requiredTopLevelGuardsTxBodyL
      <>~ Map.fromList
        [ (KeyHashObj $ mkKeyHash 212, SNothing)
        , (ScriptHashObj $ mkScriptHash 213, SJust $ exampleDatum @era)
        ]

addDijkstraBasedTxFeatures ::
  forall era l.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraScript era
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxInfo 'PlutusV4 era
  , Value era ~ MaryValue
  ) =>
  Tx l era ->
  Tx l era
addDijkstraBasedTxFeatures tx =
  tx
    & witsTxL
      <>~ ( mkBasicTxWits
              -- NOTE: PlutusV4 scripts are NOT part of Dijkstra's transaction_witness_set
              -- CDDL (only V1/V2/V3 are). Including them here would cause a roundtrip
              -- failure as they get silently dropped during serialization. See
              -- TODO in 'Cardano.Ledger.Dijkstra.HuddleSpec'.
              -- & scriptTxWitsL <>~ Map.fromElems hashScript [alwaysSucceeds @'PlutusV4 3]
              & rdmrsTxWitsL <>~ redeemers
          )
    & modifyTxAuxData
      ( plutusScriptsTxAuxDataL
          %~ Map.insertWith
            (<>)
            PlutusV4
            (NE.singleton (plutusBinary (alwaysSucceedsPlutus @'PlutusV4 3)))
      )
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV4 3)
        ]
    & bodyTxL . guardsTxBodyL
      .~ OSet.fromList
        [ KeyHashObj $ mkKeyHash 211
        , KeyHashObj $ mkKeyHash 212
        , ScriptHashObj $ mkScriptHash 213
        ]
    & bodyTxL . directDepositsTxBodyL .~ exampleDirectDeposits
    & bodyTxL . accountBalanceIntervalsTxBodyL .~ exampleAccountBalanceIntervals
    & bodyTxL . certsTxBodyL
      <>~ StrictSeq.fromList
        [ RegPoolTxCert exampleStakePoolParamsWithBlsKey
        ]
  where
    exampleStakePoolParamsWithBlsKey =
      exampleStakePoolParams
        { sppBlsKey = SJust exampleBlsKey
        }
    redeemers =
      Redeemers $
        Map.fromList
          [ (GuardingPurpose $ AsIx 3, (redeemerData, ExUnits 5000 5000))
          ]
    redeemerData = Data @era (P.Constr 1 [P.List [P.I 1], P.Map [(P.I 2, P.B "2")]])

exampleDirectDeposits :: DirectDeposits
exampleDirectDeposits =
  DirectDeposits $
    Map.singleton
      (AccountAddress Mainnet (AccountId $ KeyHashObj $ mkKeyHash 300))
      (Coin 1_000_000)

exampleAccountBalanceIntervals :: AccountBalanceIntervals era
exampleAccountBalanceIntervals =
  AccountBalanceIntervals $
    Map.fromList
      [
        ( AccountAddress Mainnet (AccountId $ KeyHashObj $ mkKeyHash 400)
        , AccountBalanceLowerBound (Inclusive $ Coin 500)
        )
      ,
        ( AccountAddress Mainnet (AccountId $ KeyHashObj $ mkKeyHash 401)
        , AccountBalanceUpperBound (Exclusive $ Coin 10_000)
        )
      ,
        ( AccountAddress Mainnet (AccountId $ ScriptHashObj $ mkScriptHash 402)
        , AccountBalanceBothBounds (Inclusive $ Coin 100) (Exclusive $ Coin 5000)
        )
      ]

exampleDijkstraOnwardsEraPParams :: (DijkstraEraPParams era, ConwayEraPParams era) => PParams era
exampleDijkstraOnwardsEraPParams =
  exampleConwayOnwardsEraPParams
    & ppMaxRefScriptSizePerBlockL .~ 1024 * 1024
    & ppMaxRefScriptSizePerTxL .~ 200 * 1024
    & ppRefScriptCostStrideL .~ knownNonZeroBounded @25_600
    & ppRefScriptCostMultiplierL .~ 12 %! 10
    & ppLeiosAnnouncementPeriodLengthL .~ Milliseconds32 1_000
    & ppLeiosVotePeriodLengthL .~ Milliseconds32 4_000
    & ppLeiosDiffusionPeriodLengthL .~ Milliseconds32 7_000
    & ppLeiosCommitteeSizeL .~ 900
    & ppLeiosQuorumStakeThresholdL .~ 3 %! 4
    & ppMaxEndorserBlockReferencesSizeL .~ 512 * 1024
    & ppMaxEndorserBlockTxsSizeL .~ 12 * 1024 * 1024
    & ppMaxEndorserBlockExUnitsL .~ OrdExUnits (ExUnits 7_000_000_000 2_000_000_000_000)
    & ppMaxRefScriptSizePerEndorserBlockL .~ 12 * 1024 * 1024

exampleDijkstraOnwardsEraPParamsUpdate ::
  (DijkstraEraPParams era, ConwayEraPParams era) => PParamsUpdate era
exampleDijkstraOnwardsEraPParamsUpdate =
  exampleConwayOnwardsEraPParamsUpdate
    & ppuMaxRefScriptSizePerBlockL .~ SJust (1024 * 1024)
    & ppuMaxRefScriptSizePerTxL .~ SJust (200 * 1024)
    & ppuRefScriptCostStrideL .~ SJust (knownNonZeroBounded @25_600)
    & ppuRefScriptCostMultiplierL .~ SJust (12 %! 10)
    & ppuLeiosAnnouncementPeriodLengthL .~ SJust (Milliseconds32 1_000)
    & ppuLeiosVotePeriodLengthL .~ SJust (Milliseconds32 4_000)
    & ppuLeiosDiffusionPeriodLengthL .~ SJust (Milliseconds32 7_000)
    & ppuLeiosCommitteeSizeL .~ SJust 900
    & ppuLeiosQuorumStakeThresholdL .~ SJust (3 %! 4)
    & ppuMaxEndorserBlockReferencesSizeL .~ SJust (512 * 1024)
    & ppuMaxEndorserBlockTxsSizeL .~ SJust (12 * 1024 * 1024)
    & ppuMaxEndorserBlockExUnitsL .~ SJust (OrdExUnits (ExUnits 7_000_000_000 2_000_000_000_000))
    & ppuMaxRefScriptSizePerEndorserBlockL .~ SJust (12 * 1024 * 1024)

exampleBlsKey :: BlsKey
exampleBlsKey =
  BlsKey
    { blsPubKey = vk
    , blsPossessionProof = createPossessionProofDSIGN minSigPoPDST sk
    }
  where
    seed = mkSeedFromBytes $ Strict.replicate (fromIntegral $ seedSizeDSIGN (Proxy @BLS12381MinSigDSIGN)) 42
    sk = genKeyDSIGNWithContext @BLS12381MinSigDSIGN Nothing seed
    vk = deriveVerKeyDSIGN sk
