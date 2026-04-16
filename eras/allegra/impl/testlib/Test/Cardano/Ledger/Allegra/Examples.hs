{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Examples (
  ledgerExamples,
  mkAllegraBasedExampleTx,
  exampleAllegraBasedTxBody,
  exampleAllegraBasedShelleyTxBody,
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
  exampleShelleyBasedShelleyTxBody,
  exampleShelleyBasedTxBody,
  mkKeyHash,
  mkShelleyBasedExampleTx,
  mkShelleyBasedLedgerExamples,
 )

ledgerExamples :: LedgerExamples AllegraEra
ledgerExamples =
  mkShelleyBasedLedgerExamples
    ( AllegraApplyTxError . pure . DelegsFailure . DelplFailure . DelegFailure $
        DelegateeNotRegisteredDELEG @AllegraEra (mkKeyHash 1)
    )
    exampleCoin
    (mkAllegraBasedExampleTx $ exampleAllegraBasedShelleyTxBody exampleCoin)
    NoGenesis

mkAllegraBasedExampleTx ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  ) =>
  TxBody TopTx era ->
  Tx TopTx era
mkAllegraBasedExampleTx txBody =
  mkShelleyBasedExampleTx @era txBody
    & witsTxL . scriptTxWitsL <>~ Map.singleton (hashScript script) script
    & auxDataTxL
      %~ fmap
        ( \auxData ->
            auxData & nativeScriptsTxAuxDataL <>~ StrictSeq.singleton exampleTimelock
        )
  where
    script = fromNativeScript exampleTimelock

exampleAllegraBasedShelleyTxBody ::
  forall era.
  ( AllegraEraTxBody era
  , ShelleyEraTxBody era
  ) =>
  Value era ->
  TxBody TopTx era
exampleAllegraBasedShelleyTxBody value =
  mkAllegraBasedExampleTxBody (exampleShelleyBasedShelleyTxBody value)

exampleAllegraBasedTxBody ::
  forall era.
  AllegraEraTxBody era =>
  Value era ->
  TxBody TopTx era
exampleAllegraBasedTxBody value =
  mkAllegraBasedExampleTxBody (exampleShelleyBasedTxBody value)

mkAllegraBasedExampleTxBody ::
  forall era.
  AllegraEraTxBody era =>
  TxBody TopTx era ->
  TxBody TopTx era
mkAllegraBasedExampleTxBody txBody =
  txBody & vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))

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
