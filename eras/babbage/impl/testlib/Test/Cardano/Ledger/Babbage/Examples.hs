{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.Examples (
  ledgerExamples,
  exampleBabbageNewEpochState,
  exampleCollateralOutput,
) where

import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Babbage (ApplyTxError (BabbageApplyTxError), BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Binary ()
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Datum (..),
  dataToBinaryData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState (..))
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (..),
  Update (..),
 )
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyDelegsPredFailure (DelplFailure),
  ShelleyDelplPredFailure (DelegFailure),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Examples (exampleDatum, exampleTx, mkLedgerExamples)
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Mary.Examples (exampleMultiAssetValue)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples (..),
  exampleCerts,
  exampleNewEpochState,
  examplePayKey,
  exampleStakeKey,
  keyToCredential,
  mkKeyHash,
 )

ledgerExamples :: LedgerExamples BabbageEra
ledgerExamples =
  mkLedgerExamples
    ( BabbageApplyTxError $
        pure $
          DelegsFailure $
            DelplFailure $
              DelegFailure $
                DelegateeNotRegisteredDELEG (mkKeyHash 1)
    )
    exampleBabbageNewEpochState
    exampleTxBabbage
    NoGenesis

exampleBabbageNewEpochState ::
  ( BabbageEraTest era
  , Value era ~ MaryValue
  ) =>
  NewEpochState era
exampleBabbageNewEpochState =
  exampleNewEpochState
    (exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (CompactCoin 1))

exampleTxBabbage :: Tx TopTx BabbageEra
exampleTxBabbage =
  exampleTx
    exampleTxBodyBabbage
    (AlonzoSpending $ AsIx 0)
    (RequireAllOf mempty)

exampleTxBodyBabbage :: TxBody TopTx BabbageEra
exampleTxBodyBabbage =
  mkBasicTxBody
    & inputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 0]
    & collateralInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 2)) 1]
    & referenceInputsTxBodyL .~ Set.fromList [mkTxInPartial (TxId (mkDummySafeHash 1)) 3]
    & outputsTxBodyL
      .~ StrictSeq.fromList
        [ mkBasicTxOut
            (mkAddr examplePayKey exampleStakeKey)
            (exampleMultiAssetValue 2)
            & datumTxOutL .~ Datum (dataToBinaryData exampleDatum) -- inline datum
            & referenceScriptTxOutL .~ SJust (alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    & collateralReturnTxBodyL .~ SJust exampleCollateralOutput
    & totalCollateralTxBodyL .~ SJust (Coin 8675309)
    & certsTxBodyL .~ exampleCerts
    & withdrawalsTxBodyL
      .~ Withdrawals
        ( Map.singleton
            (AccountAddress Testnet (AccountId (keyToCredential exampleStakeKey)))
            (Coin 100)
        )
    & feeTxBodyL .~ Coin 999
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
    & updateTxBodyL
      .~ SJust
        ( Update
            ( ProposedPPUpdates $
                Map.singleton
                  (mkKeyHash 1)
                  (emptyPParamsUpdate & ppuMaxBHSizeL .~ SJust 4000)
            )
            (EpochNo 0)
        )
    & reqSignerHashesTxBodyL .~ Set.singleton (mkKeyHash 212)
    & mintTxBodyL .~ exampleMultiAsset
    & scriptIntegrityHashTxBodyL .~ SJust (mkDummySafeHash 42)
    & auxDataHashTxBodyL .~ SJust (TxAuxDataHash $ mkDummySafeHash 42)
    & networkIdTxBodyL .~ SJust Mainnet
  where
    MaryValue _ exampleMultiAsset = exampleMultiAssetValue 3

exampleCollateralOutput ::
  ( BabbageEraTxOut era
  , Value era ~ MaryValue
  ) =>
  TxOut era
exampleCollateralOutput =
  mkBasicTxOut
    (mkAddr examplePayKey exampleStakeKey)
    (MaryValue (Coin 8675309) mempty)
