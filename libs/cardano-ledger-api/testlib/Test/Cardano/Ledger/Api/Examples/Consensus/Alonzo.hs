{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Alonzo (
  ledgerExamplesAlonzo,
  exampleTxBodyAlonzo,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleAlonzoNewEpochState,
  exampleAlonzoGenesis,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  ExUnits (..),
  Prices (..),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), TxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (NonNegativeInterval, StrictMaybe (..), boundRational)
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
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Mary as SLE
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Plutus (zeroTestingCostModelV1)

-- | ShelleyLedgerExamples for Alonzo era
ledgerExamplesAlonzo :: SLE.ShelleyLedgerExamples AlonzoEra
ledgerExamplesAlonzo =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @AlonzoEra (SLE.mkKeyHash 1)
    , SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (SLE.mkScriptHash 1))
          , Right (KeyHashObj (SLE.mkKeyHash 2))
          ]
    , SLE.sleResultExamples = resultExamples
    , SLE.sleNewEpochState = exampleAlonzoNewEpochState
    , SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    , SLE.sleTranslationContext = exampleAlonzoGenesis
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

exampleTxBodyAlonzo :: TxBody AlonzoEra
exampleTxBodyAlonzo =
  AlonzoTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral
    ( StrictSeq.fromList
        [ AlonzoTxOut
            (mkAddr SLE.examplePayKey SLE.exampleStakeKey)
            (SLE.exampleMultiAssetValue 2)
            (SJust $ mkDummySafeHash 1) -- outputs
        ]
    )
    SLE.exampleCerts -- txcerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    ( SJust $
        Update
          ( ProposedPPUpdates $
              Map.singleton
                (SLE.mkKeyHash 1)
                (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)
          )
          (EpochNo 0)
    ) -- txUpdates
    (Set.singleton $ SLE.mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
  where
    MaryValue _ exampleMultiAsset = SLE.exampleMultiAssetValue 3

datumExample :: Data AlonzoEra
datumExample = Data (P.I 191)

redeemerExample :: Data AlonzoEra
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx AlonzoEra
exampleTx =
  ShelleyTx
    exampleTxBodyAlonzo
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyAlonzo) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @AlonzoEra $ alwaysSucceeds @'PlutusV1 3)
            (alwaysSucceeds @'PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (AlonzoSpending $ AsIx 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        mkAlonzoTxAuxData
          SLE.exampleAuxDataMap -- auxiliary data
          [alwaysFails @'PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
    )

exampleTransactionInBlock :: AlonzoTx AlonzoEra
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a
  where
    ShelleyTx b w a = exampleTx

exampleAlonzoNewEpochState :: NewEpochState AlonzoEra
exampleAlonzoNewEpochState =
  SLE.exampleNewEpochState
    (SLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOWordL .~ CoinPerWord (Coin 1))

exampleAlonzoGenesis :: AlonzoGenesis
exampleAlonzoGenesis =
  AlonzoGenesis
    { agCoinsPerUTxOWord = CoinPerWord $ Coin 1
    , agCostModels = mkCostModels (Map.fromList [(PlutusV1, zeroTestingCostModelV1)])
    , agPrices = Prices (boundRational' 90) (boundRational' 91)
    , agMaxTxExUnits = ExUnits 123 123
    , agMaxBlockExUnits = ExUnits 223 223
    , agMaxValSize = 1234
    , agCollateralPercentage = 20
    , agMaxCollateralInputs = 30
    }
  where
    boundRational' :: HasCallStack => Rational -> NonNegativeInterval
    boundRational' x = case boundRational x of
      Nothing -> error $ "Expected non-negative value but got: " <> show x
      Just x' -> x'
