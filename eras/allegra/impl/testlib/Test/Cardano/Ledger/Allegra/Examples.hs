{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The example transactions in this module are not valid transactions. We
-- don't care, we are only interested in serialisation, not validation.
module Test.Cardano.Ledger.Allegra.Examples (
  ledgerExamples,
  exampleAllegraBasedTx,
  exampleAllegraToBabbageTx,
  exampleAllegraToConwayTx,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure (DelegateeNotRegisteredDELEG),
  ShelleyDelegsPredFailure (DelplFailure),
  ShelleyDelplPredFailure (DelegFailure),
  ShelleyLedgerPredFailure (DelegsFailure),
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Slotting.Slot
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  exampleCoin,
  exampleShelleyBasedTx,
  exampleShelleyToBabbageTx,
  exampleShelleyToConwayTx,
  mkKeyHash,
  mkShelleyBasedLedgerExamples,
 )

ledgerExamples :: LedgerExamples AllegraEra
ledgerExamples =
  mkShelleyBasedLedgerExamples
    ( AllegraApplyTxError . pure . DelegsFailure . DelplFailure . DelegFailure $
        DelegateeNotRegisteredDELEG @AllegraEra (mkKeyHash 1)
    )
    exampleCoin
    exampleAllegraToBabbageTx
    NoGenesis

-- Complete transaction which is compatible with any era starting with Allegra.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleAllegraBasedTx ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  ) =>
  Tx TopTx era
exampleAllegraBasedTx =
  addAllegraBasedTxFeatures exampleShelleyBasedTx

-- Complete Allegra transaction that is compatible until Babbage era
-- ('ConwayEra' is not an instance of 'ShelleyEraTxBody').
exampleAllegraToBabbageTx ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , ShelleyEraTxBody era
  ) =>
  Tx TopTx era
exampleAllegraToBabbageTx =
  addAllegraBasedTxFeatures exampleShelleyToBabbageTx

-- Complete Allegra transaction that is compatible until Conway era
-- ('DijkstraEra' is not an instance of 'ShelleyEraTxCert').
exampleAllegraToConwayTx ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , ShelleyEraTxCert era
  ) =>
  Tx TopTx era
exampleAllegraToConwayTx =
  addAllegraBasedTxFeatures exampleShelleyToConwayTx

addAllegraBasedTxFeatures ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  ) =>
  Tx TopTx era ->
  Tx TopTx era
addAllegraBasedTxFeatures tx =
  tx
    & witsTxL . scriptTxWitsL <>~ Map.singleton (hashScript script) script
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData & nativeScriptsTxAuxDataL <>~ StrictSeq.singleton exampleTimelock
        )
    & bodyTxL . vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))
  where
    script = fromNativeScript exampleTimelock

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
