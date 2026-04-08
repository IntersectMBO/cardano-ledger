{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Examples (
  ledgerExamples,
  mkAlonzoBasedLedgerExamples,
  mkAlonzoBasedExampleTx,
  exampleAlonzoBasedShelleyTxBody,
  exampleAlonzoBasedTxBody,
  exampleDatum,
) where

import Cardano.Ledger.Alonzo (AlonzoEra, ApplyTxError (AlonzoApplyTxError))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
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
import Test.Cardano.Ledger.Allegra.Examples (mkAllegraBasedExampleTx)
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, unsafeBoundRational)
import Test.Cardano.Ledger.Mary.Examples (
  exampleMaryBasedShelleyTxBody,
  exampleMaryBasedTxBody,
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
    exampleTxAlonzo
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

exampleTxAlonzo :: Tx TopTx AlonzoEra
exampleTxAlonzo =
  mkAlonzoBasedExampleTx
    exampleAlonzoBasedShelleyTxBody
    (AlonzoSpending $ AsIx 0)

mkAlonzoBasedExampleTx ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , EraPlutusTxInfo 'PlutusV1 era
  ) =>
  TxBody TopTx era ->
  PlutusPurpose AsIx era ->
  Tx TopTx era
mkAlonzoBasedExampleTx txBody scriptPurpose =
  mkAllegraBasedExampleTx
    txBody
    & witsTxL
      <>~ ( mkBasicTxWits
              & scriptTxWitsL
                .~ Map.singleton
                  (hashScript @era $ alwaysSucceeds @'PlutusV1 3)
                  (alwaysSucceeds @'PlutusV1 3)
              & datsTxWitsL .~ TxDats (Map.singleton (hashData (exampleDatum @era)) exampleDatum)
              & rdmrsTxWitsL
                .~ Redeemers (Map.singleton scriptPurpose (exampleRedeemer, ExUnits 5000 5000))
          )
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData
              & plutusScriptsTxAuxDataL
                <>~ Map.singleton PlutusV1 (NE.singleton (plutusBinary (alwaysFailsPlutus @'PlutusV1 2)))
        )
    & isValidTxL .~ IsValid True

exampleAlonzoBasedTxBody ::
  forall era.
  ( AlonzoEraTxBody era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleAlonzoBasedTxBody =
  mkAlonzoBasedExampleTxBody exampleMaryBasedTxBody

exampleAlonzoBasedShelleyTxBody ::
  forall era.
  ( AlonzoEraTxBody era
  , ShelleyEraTxBody era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era
exampleAlonzoBasedShelleyTxBody =
  mkAlonzoBasedPreConwayExampleTxBody exampleMaryBasedShelleyTxBody

mkAlonzoBasedPreConwayExampleTxBody ::
  forall era.
  ( AlonzoEraTxBody era
  , Value era ~ MaryValue
  , AtMostEra "Conway" era
  ) =>
  TxBody TopTx era ->
  TxBody TopTx era
mkAlonzoBasedPreConwayExampleTxBody txBody =
  mkAlonzoBasedExampleTxBody txBody
    & reqSignerHashesTxBodyL .~ Set.singleton (mkKeyHash 212)

mkAlonzoBasedExampleTxBody ::
  forall era.
  ( AlonzoEraTxBody era
  , Value era ~ MaryValue
  ) =>
  TxBody TopTx era ->
  TxBody TopTx era
mkAlonzoBasedExampleTxBody txBody =
  txBody
    & collateralInputsTxBodyL .~ exampleTxIns
    & outputsTxBodyL
      <>~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 3)
            & dataHashTxOutL .~ SJust (mkDummySafeHash 1)
        ]
    & scriptIntegrityHashTxBodyL .~ SJust (mkDummySafeHash 42)
    & networkIdTxBodyL .~ SJust Mainnet

exampleDatum :: Era era => Data era
exampleDatum = Data (P.I 191)

exampleRedeemer :: Era era => Data era
exampleRedeemer = Data (P.I 919)

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
