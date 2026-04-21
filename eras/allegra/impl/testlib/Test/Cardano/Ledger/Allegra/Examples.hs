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
import qualified Data.MapExtras as Map
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Examples (
  LedgerExamples,
  addShelleyBasedTopTxExampleFee,
  addShelleyToBabbageExampleProposedPUpdates,
  addShelleyToBabbageTxCerts,
  addShelleyToConwayTxCerts,
  exampleCoin,
  exampleShelleyBasedTx,
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
    exampleAllegraTx
    NoGenesis

-- Complete Allegra transaction that is compatible until Babbage era
-- ('ConwayEra' is not an instance of 'ShelleyEraTxBody').
exampleAllegraTx ::
  forall era.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , ShelleyEraTxBody era
  ) =>
  Tx TopTx era
exampleAllegraTx =
  exampleAllegraBasedTx
    & addShelleyBasedTopTxExampleFee
    & addShelleyToBabbageExampleProposedPUpdates
    & addShelleyToBabbageTxCerts
    & addShelleyToConwayTxCerts

-- Complete transaction which is compatible with any era starting with Allegra.
-- This transaction forms the basis on which future era transactions will be
-- at the very least based on.
exampleAllegraBasedTx ::
  forall era l.
  ( EraTx era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , Typeable l
  ) =>
  Tx l era
exampleAllegraBasedTx =
  exampleShelleyBasedTx
    & witsTxL . scriptTxWitsL <>~ Map.fromElems hashScript [fromNativeScript exampleTimelock]
    & modifyTxAuxData
      (nativeScriptsTxAuxDataL <>~ StrictSeq.singleton exampleTimelock)
    & bodyTxL . vldtTxBodyL .~ ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))

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
