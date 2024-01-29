{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples.Consensus where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
  ExUnits (..),
  Prices (..),
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash (..), mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes (NonNegativeInterval, StrictMaybe (..), boundRational)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.CostModels (mkCostModels)
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
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
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as SLE
import Test.Cardano.Ledger.Plutus (zeroTestingCostModelV1)
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE

-- | ShelleyLedgerExamples for Alonzo era
ledgerExamplesAlonzo :: SLE.ShelleyLedgerExamples Alonzo
ledgerExamplesAlonzo =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @Alonzo)
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @Alonzo (SLE.mkKeyHash 1)
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

exampleTxBodyAlonzo :: AlonzoTxBody Alonzo
exampleTxBodyAlonzo =
  AlonzoTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 0]) -- inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 2)) 1]) -- collateral
    ( StrictSeq.fromList
        [ AlonzoTxOut
            (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
            (SLE.exampleMultiAssetValue 2)
            (SJust $ mkDummySafeHash Proxy 1) -- outputs
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
    (SJust $ mkDummySafeHash (Proxy @StandardCrypto) 42) -- scriptIntegrityHash
    (SJust . AuxiliaryDataHash $ mkDummySafeHash (Proxy @StandardCrypto) 42) -- adHash
    (SJust Mainnet) -- txnetworkid
  where
    MaryValue _ exampleMultiAsset = SLE.exampleMultiAssetValue 3

datumExample :: Data Alonzo
datumExample = Data (P.I 191)

redeemerExample :: Data Alonzo
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx Alonzo
exampleTx =
  ShelleyTx
    exampleTxBodyAlonzo
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyAlonzo) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @Alonzo $ alwaysSucceeds @'PlutusV1 3)
            (alwaysSucceeds @'PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (AlonzoSpending $ AsIndex 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        mkAlonzoTxAuxData
          SLE.exampleAuxDataMap -- auxiliary data
          [alwaysFails @'PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
    )

exampleTransactionInBlock :: AlonzoTx Alonzo
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a
  where
    ShelleyTx b w a = exampleTx

exampleAlonzoNewEpochState :: NewEpochState Alonzo
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
