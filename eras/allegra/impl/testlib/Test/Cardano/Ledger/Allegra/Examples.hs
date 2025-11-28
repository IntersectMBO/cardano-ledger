{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Allegra.Examples (
  ledgerExamples,
  exampleAllegraTxBody,
  exampleAllegraTxAuxData,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Coin
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegsPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Slotting.Slot
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  exampleAuxDataMap,
  exampleCerts,
  exampleCoin,
  examplePayKey,
  exampleProposedPPUpdates,
  exampleStakeKey,
  exampleTxIns,
  exampleWithdrawals,
  mkKeyHash,
  mkLedgerExamples,
  mkWitnessesPreAlonzo,
 )

ledgerExamples :: LedgerExamples AllegraEra
ledgerExamples =
  mkLedgerExamples
    ( AllegraApplyTxError . pure . DelegsFailure $
        DelegateeNotRegisteredDELEG @AllegraEra (mkKeyHash 1)
    )
    (mkWitnessesPreAlonzo (Proxy @AllegraEra))
    exampleCoin
    (exampleAllegraTxBody exampleCoin)
    exampleAllegraTxAuxData
    NoGenesis

exampleAllegraTxBody ::
  forall era.
  ( AllegraEraTxBody era
  , ShelleyEraTxBody era
  ) =>
  Value era ->
  TxBody TopTx era
exampleAllegraTxBody value =
  mkBasicTxBody
    & inputsTxBodyL .~ exampleTxIns
    & outputsTxBodyL
      .~ StrictSeq.singleton (mkBasicTxOut (mkAddr examplePayKey exampleStakeKey) value)
    & certsTxBodyL .~ exampleCerts
    & withdrawalsTxBodyL .~ exampleWithdrawals
    & feeTxBodyL .~ Coin 3
    & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
    & updateTxBodyL .~ SJust (Update exampleProposedPPUpdates (EpochNo 0))
    & auxDataHashTxBodyL .~ SJust auxiliaryDataHash
  where
    -- Dummy hash to decouple from the auxiliary data in 'exampleTx'.
    auxiliaryDataHash :: TxAuxDataHash
    auxiliaryDataHash =
      TxAuxDataHash $ mkDummySafeHash 30

exampleTimelock :: AllegraEraScript era => NativeScript era
exampleTimelock =
  RequireMOf 2 $
    StrictSeq.fromList
      [ RequireAllOf $
          StrictSeq.fromList
            [ RequireTimeStart (SlotNo 0)
            , RequireTimeExpire (SlotNo 9)
            ]
      , RequireAnyOf $
          StrictSeq.fromList
            [ RequireSignature (mkKeyHash 0)
            , RequireSignature (mkKeyHash 1)
            ]
      , RequireSignature (mkKeyHash 100)
      ]

exampleAllegraTxAuxData ::
  (AllegraEraScript era, NativeScript era ~ Timelock era) => AllegraTxAuxData era
exampleAllegraTxAuxData = AllegraTxAuxData exampleAuxDataMap (StrictSeq.fromList [exampleTimelock])
