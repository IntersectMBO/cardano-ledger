{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Examples.Consensus where

import Cardano.Ledger.Alonzo.Data
  ( AlonzoTxAuxData (..),
    AuxiliaryDataHash (..),
    Data (..),
    dataToBinaryData,
    hashData,
  )
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (RdmrPtr (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxBody (Datum (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.PParams (BabbagePParamsHKD (..), emptyPParams, emptyPParamsUpdate)
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (BabbageTxBody (..), BabbageTxOut (..))
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Core (EraScript (hashScript), TxBody, eraProtVerHigh)
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (GenDelegs (..), asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( ApplyTxError (..),
    Network (..),
    NewEpochState (..),
    ProposedPPUpdates (..),
    RewardAcnt (..),
    TxId (..),
    Update (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.Rules
  ( ShelleyDelegsPredFailure (..),
    ShelleyLedgerPredFailure (DelegsFailure),
  )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Ledger.UTxO (makeWitnessesVKey)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as MarySLE
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils (mkAddr)

-- ==============================================================

-- | ShelleyLedgerExamples for Conway era
ledgerExamplesConway :: SLE.ShelleyLedgerExamples Conway
ledgerExamplesConway =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock,
      SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @Conway),
      SLE.sleTx = exampleTransactionInBlock,
      SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @Conway (SLE.mkKeyHash 1),
      SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (SLE.mkScriptHash 1)),
            Right (KeyHashObj (SLE.mkKeyHash 2))
          ],
      SLE.sleResultExamples = resultExamples,
      SLE.sleNewEpochState = exampleConwayNewEpochState,
      SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1,
      SLE.sleTranslationContext = exampleConwayGenesis
    }
  where
    resultExamples =
      SLE.ShelleyResultExamples
        { SLE.srePParams = def,
          SLE.sreProposedPPUpdates = examplePPPU,
          SLE.srePoolDistr = SLE.examplePoolDistr,
          SLE.sreNonMyopicRewards = SLE.exampleNonMyopicRewards,
          SLE.sreShelleyGenesis = SLE.testShelleyGenesis
        }
    examplePPPU =
      ProposedPPUpdates $
        Map.singleton
          (SLE.mkKeyHash 0)
          (emptyPParamsUpdate {_collateralPercentage = SJust 150})

collateralOutput :: BabbageTxOut Conway
collateralOutput =
  BabbageTxOut
    (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
    (MaryValue 8675309 mempty)
    NoDatum
    SNothing

exampleTxBodyConway :: TxBody Conway
exampleTxBodyConway =
  BabbageTxBody
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @Conway) $
            BabbageTxOut
              (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
              (MarySLE.exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData datumExample) -- inline datum
              (SJust $ alwaysSucceeds PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @Conway) collateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    SLE.exampleCerts -- txcerts
    ( Wdrl $
        Map.singleton
          (RewardAcnt Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    ( SJust $
        Update
          ( ProposedPPUpdates $
              Map.singleton
                (SLE.mkKeyHash 1)
                (emptyPParamsUpdate {_maxBHSize = SJust 4000})
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

datumExample :: Data Conway
datumExample = Data (Plutus.I 191)

redeemerExample :: Data Conway
redeemerExample = Data (Plutus.I 919)

exampleTx :: ShelleyTx Conway
exampleTx =
  ShelleyTx
    exampleTxBodyConway
    ( AlonzoTxWits
        (makeWitnessesVKey (hashAnnotated exampleTxBodyConway) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @Conway $ alwaysSucceeds PlutusV1 3)
            (alwaysSucceeds PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        AlonzoTxAuxData
          SLE.exampleAuxDataMap -- metadata
          ( StrictSeq.fromList
              [alwaysFails PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
          )
    )

exampleTransactionInBlock :: AlonzoTx Conway
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a
  where
    (ShelleyTx b w a) = exampleTx

exampleConwayNewEpochState :: NewEpochState Conway
exampleConwayNewEpochState =
  SLE.exampleNewEpochState
    (MarySLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams {_coinsPerUTxOByte = Coin 1})

exampleConwayGenesis :: ConwayGenesis c
exampleConwayGenesis =
  ConwayGenesis (GenDelegs Map.empty)
