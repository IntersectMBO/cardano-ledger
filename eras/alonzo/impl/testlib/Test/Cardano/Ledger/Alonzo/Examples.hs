{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Examples (
  ledgerExamples,
  mkLedgerExamples,
  exampleTx,
  exampleDatum,
  exampleAlonzoGenesis,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoExtraConfig (..), AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  ExUnits (..),
  Prices (..),
 )
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData, mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), TxBody (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.CostModels (mkCostModels)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  ApplyTxError (..),
  Credential (..),
  Network (..),
  NewEpochState (..),
  ProposedPPUpdates (..),
  RewardAccount (..),
  TxId (..),
  Update (..),
 )
import Cardano.Ledger.Shelley.Rules (ShelleyDelegsPredFailure (..), ShelleyLedgerPredFailure (..))
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, unsafeBoundRational)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Plutus (zeroTestingCostModelV1)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  exampleAuxDataMap,
  exampleCerts,
  exampleNewEpochState,
  exampleNonMyopicRewards,
  examplePayKey,
  examplePoolDistr,
  exampleStakeKey,
  keyToCredential,
  mkKeyHash,
  mkScriptHash,
  testShelleyGenesis,
 )

ledgerExamples :: LedgerExamples AlonzoEra
ledgerExamples =
  mkLedgerExamples
    ( ApplyTxError $
        pure $
          DelegsFailure $
            DelegateeNotRegisteredDELEG @AlonzoEra (mkKeyHash 1)
    )
    exampleAlonzoNewEpochState
    exampleTxAlonzo
    exampleAlonzoGenesis

mkLedgerExamples ::
  forall era.
  AlonzoEraPParams era =>
  ApplyTxError era ->
  NewEpochState era ->
  Tx era ->
  TranslationContext era ->
  LedgerExamples era
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

exampleAlonzoNewEpochState :: NewEpochState AlonzoEra
exampleAlonzoNewEpochState =
  exampleNewEpochState
    (exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOWordL .~ CoinPerWord (Coin 1))

exampleTxAlonzo :: Tx AlonzoEra
exampleTxAlonzo =
  exampleTx
    exampleTxBodyAlonzo
    (AlonzoSpending $ AsIx 0)
    (RequireAllOf @AlonzoEra mempty)

exampleTx ::
  forall era.
  ( AlonzoEraTx era
  , EraPlutusTxInfo 'PlutusV1 era
  , TxAuxData era ~ AlonzoTxAuxData era
  , Script era ~ AlonzoScript era
  ) =>
  TxBody era -> PlutusPurpose AsIx era -> NativeScript era -> Tx era
exampleTx txBody scriptPurpose nativeScript =
  mkBasicTx @era txBody
    & witsTxL
      .~ ( mkBasicTxWits
             & addrTxWitsL .~ mkWitnessesVKey (hashAnnotated txBody) [asWitness examplePayKey]
             & bootAddrTxWitsL .~ mempty
             & scriptTxWitsL
               .~ Map.singleton
                 (hashScript @era $ alwaysSucceeds @'PlutusV1 3)
                 (alwaysSucceeds @'PlutusV1 3)
             & datsTxWitsL .~ TxDats (Map.singleton (hashData (exampleDatum @era)) exampleDatum)
             & rdmrsTxWitsL
               .~ Redeemers (Map.singleton scriptPurpose (exampleRedeemer, ExUnits 5000 5000))
         )
    & isValidTxL .~ IsValid True
    & auxDataTxL
      .~ SJust
        ( mkAlonzoTxAuxData
            exampleAuxDataMap
            [alwaysFails @'PlutusV1 2, NativeScript nativeScript]
        )

exampleTxBodyAlonzo :: TxBody AlonzoEra
exampleTxBodyAlonzo =
  AlonzoTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral
    ( StrictSeq.fromList
        [ AlonzoTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            (SJust $ mkDummySafeHash 1) -- outputs
        ]
    )
    exampleCerts -- txcerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (keyToCredential exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    ( SJust $
        Update
          ( ProposedPPUpdates $
              Map.singleton
                (mkKeyHash 1)
                (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)
          )
          (EpochNo 0)
    ) -- txUpdates
    (Set.singleton $ mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleDatum :: Era era => Data era
exampleDatum = Data (P.I 191)

exampleRedeemer :: Era era => Data era
exampleRedeemer = Data (P.I 919)

exampleAlonzoGenesis :: AlonzoGenesis
exampleAlonzoGenesis =
  AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 1
    , agCostModels = mkCostModels (Map.fromList [(PlutusV1, zeroTestingCostModelV1)])
    , agPrices = Prices (unsafeBoundRational 90) (unsafeBoundRational 91)
    , agMaxTxExUnits = ExUnits 123 123
    , agMaxBlockExUnits = ExUnits 223 223
    , agMaxValSize = 1234
    , agCollateralPercentage = 20
    , agMaxCollateralInputs = 30
    , agExtraConfig = AlonzoExtraConfig Nothing
    }
