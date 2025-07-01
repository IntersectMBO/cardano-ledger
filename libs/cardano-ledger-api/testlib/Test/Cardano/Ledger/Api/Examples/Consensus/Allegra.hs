{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Api.Examples.Consensus.Allegra (
  ledgerExamplesAllegra,
  exampleAllegraTxBody,
  exampleTimelock,
  exampleAllegraTxAuxData,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Test.Cardano.Ledger.Api.Examples.Consensus.Shelley

-- | ShelleyLedgerExamples for Allegra era
ledgerExamplesAllegra :: ShelleyLedgerExamples AllegraEra
ledgerExamplesAllegra = undefined

exampleAllegraTxBody ::
  forall era.
  ( AllegraEraTxBody era
  , ShelleyEraTxBody era
  ) =>
  Value era ->
  TxBody era
exampleAllegraTxBody = undefined

exampleTimelock :: AllegraEraScript era => NativeScript era
exampleTimelock = undefined

exampleAllegraTxAuxData ::
  (AllegraEraScript era, NativeScript era ~ Timelock era) => AllegraTxAuxData era
exampleAllegraTxAuxData = undefined
