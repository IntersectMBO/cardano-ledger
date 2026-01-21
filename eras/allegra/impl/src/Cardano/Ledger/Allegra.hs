{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Rules ()
import Cardano.Ledger.Allegra.Scripts ()
import Cardano.Ledger.Allegra.State ()
import Cardano.Ledger.Allegra.Transition ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Allegra.Tx (Tx (..))
import Cardano.Ledger.Allegra.UTxO ()
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)

--------------------------------------------------------------------------------
-- Mempool instances
--------------------------------------------------------------------------------

instance ApplyTx AllegraEra where
  newtype ApplyTxError AllegraEra = AllegraApplyTxError (NonEmpty (ShelleyLedgerPredFailure AllegraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup)
  applyTxValidation validationPolicy globals env state tx =
    first AllegraApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyBlock AllegraEra
