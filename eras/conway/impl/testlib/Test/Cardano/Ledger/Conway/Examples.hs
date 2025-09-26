{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Examples (
  ledgerExamples,
) where

import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxBody (TxBody (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.API (
  ApplyTxError (..),
  RewardAccount (..),
  TxId (..),
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleDatum,
  exampleTx,
  mkLedgerExamples,
 )
import Test.Cardano.Ledger.Babbage.Examples (exampleBabbageNewEpochState, exampleCollateralOutput)
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Conway.Genesis (expectedConwayGenesis)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  examplePayKey,
  examplePoolParams,
  exampleStakeKey,
  keyToCredential,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples ConwayEra
ledgerExamples =
  mkLedgerExamples
    ( ApplyTxError $
        pure $
          wrapFailed @(ConwayDELEG ConwayEra) @(ConwayLEDGER ConwayEra) $
            DelegateeStakePoolNotRegisteredDELEG @ConwayEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxConway
    exampleConwayGenesis

exampleTxConway :: Tx ConwayEra
exampleTxConway =
  exampleTx
    exampleTxBodyConway
    (ConwaySpending $ AsIx 0)
    (RequireAllOf @ConwayEra mempty)

exampleTxBodyConway :: TxBody ConwayEra
exampleTxBodyConway =
  ConwayTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @ConwayEra) $
            BabbageTxOut
              (mkAddr examplePayKey exampleStakeKey)
              (exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData exampleDatum) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @ConwayEra) exampleCollateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    exampleConwayCerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (keyToCredential exampleStakeKey))
          (Coin 100)
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    (Set.singleton $ mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash 42) -- scriptIntegrityHash
    (SJust . TxAuxDataHash $ mkDummySafeHash 42) -- adHash
    (SJust Mainnet) -- txnetworkid
    (VotingProcedures mempty)
    mempty
    (SJust $ Coin 867530900000) -- current treasury value
    mempty
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleConwayCerts :: OSet.OSet (ConwayTxCert era)
exampleConwayCerts =
  OSet.fromList -- TODO add all possible certificates
    [ConwayTxCertPool (RegPool examplePoolParams)]

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = expectedConwayGenesis
