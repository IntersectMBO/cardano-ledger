{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Alonzo.Examples (
  ledgerExamples,
  mkAlonzoBasedLedgerExamples,
  exampleAlonzoBasedTx,
  exampleAlonzoToBabbageTx,
  exampleAlonzoToConwayTx,
  exampleDatum,
) where

import Cardano.Ledger.Alonzo (AlonzoEra, ApplyTxError (AlonzoApplyTxError))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (
  ExUnits (..),
  Prices (..),
 )
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..), plutusBinary)
import Cardano.Ledger.Shelley.API (
  Credential (..),
  Network (..),
  NewEpochState (..),
  ProposedPPUpdates (..),
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyDelegsPredFailure (DelplFailure),
  ShelleyDelplPredFailure (DelegFailure),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import Data.Default (def)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, unsafeBoundRational)
import Test.Cardano.Ledger.Mary.Examples (
  exampleMaryBasedTx,
  exampleMaryToBabbageTx,
  exampleMaryToConwayTx,
  exampleMultiAssetValue,
 )
import Test.Cardano.Ledger.Plutus (alwaysFailsPlutus, zeroTestingCostModelV1)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  exampleNewEpochState,
  exampleNonMyopicRewards,
  examplePayKey,
  examplePoolDistr,
  exampleStakeKey,
  exampleTxIns,
  mkKeyHash,
  mkScriptHash,
  testShelleyGenesis,
 )

ledgerExamples :: LedgerExamples AlonzoEra
ledgerExamples =
  mkAlonzoBasedLedgerExamples
    ( AlonzoApplyTxError $
        pure $
          DelegsFailure $
            DelplFailure $
              DelegFailure $
                DelegateeNotRegisteredDELEG @AlonzoEra (mkKeyHash 1)
    )
    exampleAlonzoNewEpochState
    exampleAlonzoToBabbageTx
    exampleAlonzoGenesis

mkAlonzoBasedLedgerExamples ::
  forall era.
  AlonzoEraPParams era =>
  ApplyTxError era ->
  NewEpochState era ->
  Tx TopTx era ->
  TranslationContext era ->
  LedgerExamples era
mkAlonzoBasedLedgerExamples
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

exampleAlonzoNewEpochState :: NewEpochState AlonzoEra
exampleAlonzoNewEpochState =
  exampleNewEpochState
    (exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOWordL .~ CoinPerWord (Coin 1))

-- Complete transaction which is compatible with any era starting with Alonzo.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleAlonzoBasedTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  , Value era ~ MaryValue
  ) =>
  Tx TopTx era
exampleAlonzoBasedTx =
  addAlonzoBasedTxFeatures exampleMaryBasedTx

-- Complete Alonzo transaction that is compatible until Babbage era
-- ('ConwayEra' is not an instance of 'ShelleyEraTxBody').
exampleAlonzoToBabbageTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , ShelleyEraTxBody era
  ) =>
  Tx TopTx era
exampleAlonzoToBabbageTx =
  addAlonzoToConwayTxFeatures exampleMaryToBabbageTx

-- Complete Alonzo transaction that is compatible until Conway era
-- ('DijkstraEra' is not an instance of 'ShelleyEraTxCert').
exampleAlonzoToConwayTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , ShelleyEraTxCert era
  ) =>
  Tx TopTx era
exampleAlonzoToConwayTx =
  addAlonzoToConwayTxFeatures exampleMaryToConwayTx

addAlonzoToConwayTxFeatures ::
  forall era.
  ( AlonzoEraTxBody era
  , AtMostEra "Conway" era
  , EraTx era
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addAlonzoToConwayTxFeatures tx =
  tx & bodyTxL . reqSignerHashesTxBodyL .~ Set.singleton (mkKeyHash 212)

addAlonzoBasedTxFeatures ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , Value era ~ MaryValue
  , EraPlutusTxInfo 'PlutusV1 era
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addAlonzoBasedTxFeatures tx =
  tx
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL
                .~ Map.singleton
                  (hashScript @era $ alwaysSucceeds @'PlutusV1 3)
                  (alwaysSucceeds @'PlutusV1 3)
              & datsTxWitsL .~ TxDats (Map.singleton (hashData (exampleDatum @era)) exampleDatum)
              & rdmrsTxWitsL .~ redeemers
          )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                <>~ Map.singleton PlutusV1 (NE.singleton (plutusBinary (alwaysFailsPlutus @'PlutusV1 2)))
        )
    & isValidTxL .~ IsValid True
    & bodyTxL . collateralInputsTxBodyL .~ exampleTxIns
    & bodyTxL . outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 3)
            & dataHashTxOutL .~ SJust (mkDummySafeHash 1)
        ]
    & bodyTxL . scriptIntegrityHashTxBodyL .~ SJust (mkDummySafeHash 42)
    & bodyTxL . networkIdTxBodyL .~ SJust Mainnet
  where
    redeemers =
      Redeemers $
        Map.fromList
          [ (SpendingPurpose $ AsIx 0, (redeemerData, ExUnits 5000 5000))
          , (MintingPurpose $ AsIx 1, (redeemerData, ExUnits 5000 5000))
          , (CertifyingPurpose $ AsIx 2, (redeemerData, ExUnits 5000 5000))
          , (RewardingPurpose $ AsIx 3, (redeemerData, ExUnits 5000 5000))
          ]
    redeemerData = Data (P.Constr 1 [P.List [P.I 1], P.Map [(P.I 2, P.B "2")]])

exampleDatum :: Era era => Data era
exampleDatum = Data (P.Constr 0 [P.List [P.I 0], P.Map [(P.I 1, P.B "1")]])

exampleAlonzoGenesis :: AlonzoGenesis
exampleAlonzoGenesis =
  AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 1
    , agPlutusV1CostModel = zeroTestingCostModelV1
    , agPrices = Prices (unsafeBoundRational 90) (unsafeBoundRational 91)
    , agMaxTxExUnits = ExUnits 123 123
    , agMaxBlockExUnits = ExUnits 223 223
    , agMaxValSize = 1234
    , agCollateralPercentage = 20
    , agMaxCollateralInputs = 30
    , agExtraConfig = Nothing
    }
