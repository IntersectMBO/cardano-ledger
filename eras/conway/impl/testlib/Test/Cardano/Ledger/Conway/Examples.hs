{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Examples (
  ledgerExamples,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ApplyTxError (ConwayApplyTxError), ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Rules (ConwayDELEG, ConwayDelegPredFailure (..), ConwayLEDGER)
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
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
  exampleStakeKey,
  exampleStakePoolParams,
  keyToCredential,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples ConwayEra
ledgerExamples =
  mkLedgerExamples
    ( ConwayApplyTxError $
        pure $
          wrapFailed @(ConwayDELEG ConwayEra) @(ConwayLEDGER ConwayEra) $
            DelegateeStakePoolNotRegisteredDELEG @ConwayEra (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxConway
    exampleConwayGenesis

exampleTxConway :: Tx TopTx ConwayEra
exampleTxConway =
  exampleTx
    exampleTxBodyConway
    (ConwaySpending $ AsIx 0)
    (RequireAllOf @ConwayEra mempty)

exampleTxBodyConway :: TxBody TopTx ConwayEra
exampleTxBodyConway =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]
    & collateralInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]
    & referenceInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]
    & outputsTxBodyL
      .~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum)
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV2 3)
        ]
    & collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & totalCollateralTxBodyL .~ SJust (Coin 8675309)
    & certsTxBodyL .~ exampleConwayCerts
    & withdrawalsTxBodyL
      .~ Withdrawals
        ( Map.singleton
            (AccountAddress Testnet (AccountId (keyToCredential exampleStakeKey)))
            (Coin 100)
        )
    & feeTxBodyL .~ Coin 999
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
    & reqSignerHashesTxBodyL .~ Set.singleton (mkKeyHash 212)
    & mintTxBodyL .~ exampleMultiAsset
    & scriptIntegrityHashTxBodyL .~ SJust (mkDummySafeHash 42)
    & auxDataHashTxBodyL .~ SJust (TxAuxDataHash $ mkDummySafeHash 42)
    & networkIdTxBodyL .~ SJust Mainnet
    & votingProceduresTxBodyL .~ VotingProcedures mempty
    & proposalProceduresTxBodyL .~ mempty
    & currentTreasuryValueTxBodyL .~ SJust (Coin 867530900000)
    & treasuryDonationTxBodyL .~ mempty
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleConwayCerts :: StrictSeq.StrictSeq (ConwayTxCert era)
exampleConwayCerts =
  -- TODO add all possible certificates
  StrictSeq.fromList
    [ ConwayTxCertPool (RegPool exampleStakePoolParams)
    ]

exampleConwayGenesis :: ConwayGenesis
exampleConwayGenesis = expectedConwayGenesis
