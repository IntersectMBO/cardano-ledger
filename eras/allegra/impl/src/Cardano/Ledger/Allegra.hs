{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra (
  AllegraEra,
  Tx (..),
  ApplyTxError (..),
) where

import Cardano.Ledger.Allegra.BlockBody ()
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Forecast ()
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Rules ()
import Cardano.Ledger.Allegra.Scripts ()
import Cardano.Ledger.Allegra.State ()
import Cardano.Ledger.Allegra.Transition ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Allegra.Tx (Tx (..))
import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Block (EraBlockHeader)
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ledgerPpL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (utxoG)
import Control.State.Transition.Extended (ValidationPolicy (..))
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

instance ApplyTx AllegraEra where
  newtype ApplyTxError AllegraEra
    = AllegraApplyTxError (NonEmpty (Shelley.ShelleyLedgerPredFailure AllegraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx _ _ _ _ = id

  internalApplyTxWithValidation validationPolicy globals env state tx =
    let stAnnTx =
          mkStAnnTx
            (epochInfo globals)
            (systemStart globals)
            (env ^. ledgerPpL)
            (state ^. utxoG)
            tx
     in first AllegraApplyTxError $
          ruleApplyTxValidation @"LEDGER" validationPolicy globals env state stAnnTx

  internalReapplyValidatedTx globals env state vtx =
    fst
      <$> first
        AllegraApplyTxError
        ( ruleApplyTxValidation @"LEDGER"
            (ValidateSuchThat (notElem lblStatic))
            globals
            env
            state
            (vtStAnnTx vtx)
        )

instance ApplyTick AllegraEra

instance EraBlockHeader h AllegraEra => ApplyBlock h AllegraEra
