{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Conway (
  ledgerExamplesConway,
  collateralOutput,
  exampleConwayCerts,
  exampleTxBodyConway,
  datumExample,
  redeemerExample,
  exampleTx,
  exampleTransactionInBlock,
  exampleConwayNewEpochState,
  exampleConwayGenesis,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.TxAuxData (mkAlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.Tx (AlonzoTx (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
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
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Mary as MarySLE
import qualified Test.Cardano.Ledger.Api.Examples.Consensus.Shelley as SLE
import Test.Cardano.Ledger.Conway.Genesis (expectedConwayGenesis)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)

-- ==============================================================

-- | ShelleyLedgerExamples for Conway era
ledgerExamplesConway ::
  SLE.ShelleyLedgerExamples ConwayEra
ledgerExamplesConway =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            wrapFailed @(ConwayDELEG ConwayEra) @(ConwayLEDGER ConwayEra) $
              DelegateeStakePoolNotRegisteredDELEG @ConwayEra (SLE.mkKeyHash 1)
    , SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (SLE.mkScriptHash 1))
          , Right (KeyHashObj (SLE.mkKeyHash 2))
          ]
    , SLE.sleResultExamples = resultExamples
    , SLE.sleNewEpochState = exampleConwayNewEpochState
    , SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    , SLE.sleTranslationContext = exampleConwayGenesis
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

collateralOutput :: BabbageTxOut ConwayEra
collateralOutput =
  BabbageTxOut
    (mkAddr SLE.examplePayKey SLE.exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
    NoDatum
    SNothing

exampleConwayCerts :: OSet.OSet (ConwayTxCert era)
exampleConwayCerts =
  OSet.fromList -- TODO should I add the new certs here?
    [ ConwayTxCertPool (RegPool SLE.examplePoolParams)
    ]

exampleTxBodyConway :: TxBody ConwayEra
exampleTxBodyConway =
  ConwayTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @ConwayEra) $
            BabbageTxOut
              (mkAddr SLE.examplePayKey SLE.exampleStakeKey)
              (MarySLE.exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData datumExample) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @ConwayEra) collateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    exampleConwayCerts -- txcerts
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

datumExample :: Data ConwayEra
datumExample = Data (P.I 191)

redeemerExample :: Data ConwayEra
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx ConwayEra
exampleTx =
  ShelleyTx
    exampleTxBodyConway
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyConway) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @ConwayEra $ alwaysSucceeds @'PlutusV1 3)
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

exampleTransactionInBlock :: Tx ConwayEra
exampleTransactionInBlock = MkConwayTx $ AlonzoTx b w (IsValid True) a
  where
    ShelleyTx b w a = exampleTx

exampleConwayNewEpochState :: NewEpochState ConwayEra
exampleConwayNewEpochState =
  SLE.exampleNewEpochState
    (MarySLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1))

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = expectedConwayGenesis
