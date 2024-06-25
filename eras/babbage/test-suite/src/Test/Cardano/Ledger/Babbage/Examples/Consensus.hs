{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Examples.Consensus where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AuxiliaryDataHash (..),
  mkAlonzoTxAuxData,
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
  hashData,
 )
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
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import qualified Test.Cardano.Ledger.Core.Utils as SLE
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as MarySLE
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE

-- | ShelleyLedgerExamples for Babbage era
ledgerExamplesBabbage :: SLE.ShelleyLedgerExamples Babbage
ledgerExamplesBabbage =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @Babbage)
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @Babbage (SLE.mkKeyHash 1)
    , SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (SLE.mkScriptHash 1))
          , Right (KeyHashObj (SLE.mkKeyHash 2))
          ]
    , SLE.sleResultExamples = resultExamples
    , SLE.sleNewEpochState = exampleBabbageNewEpochState
    , SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    , SLE.sleTranslationContext = ()
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

collateralOutput :: BabbageTxOut Babbage
collateralOutput =
  BabbageTxOut
    (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
    (MaryValue (Coin 8675309) mempty)
    NoDatum
    SNothing

exampleTxBodyBabbage :: TxBody Babbage
exampleTxBodyBabbage =
  BabbageTxBody
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @Babbage) $
            BabbageTxOut
              (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
              (MarySLE.exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData datumExample) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @Babbage) collateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
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
    (SJust $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) -- scriptIntegrityHash
    (SJust . AuxiliaryDataHash $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) -- adHash
    (SJust Mainnet) -- txnetworkid
  where
    MaryValue _ exampleMultiAsset = MarySLE.exampleMultiAssetValue 3

datumExample :: Data Babbage
datumExample = Data (P.I 191)

redeemerExample :: Data Babbage
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx Babbage
exampleTx =
  ShelleyTx
    exampleTxBodyBabbage
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyBabbage) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @Babbage $ alwaysSucceeds @'PlutusV1 3)
            (alwaysSucceeds @'PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (AlonzoSpending $ AsIx 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        mkAlonzoTxAuxData
          SLE.exampleAuxDataMap -- metadata
          [alwaysFails @'PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
    )

exampleTransactionInBlock :: AlonzoTx Babbage
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a -- mempty
  where
    ShelleyTx b w a = exampleTx

exampleBabbageNewEpochState :: NewEpochState Babbage
exampleBabbageNewEpochState =
  SLE.exampleNewEpochState
    (MarySLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1))
