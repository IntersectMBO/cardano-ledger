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
import Cardano.Ledger.Core (witsTxL)
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Core (scriptTxWitsL)
import Cardano.Ledger.Shelley.Rules (ShelleyLedgerPredFailure)
import Cardano.Ledger.StAnnTx (EraStAnnTx (..))
import Cardano.Ledger.State (ScriptsProvided (..))
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

instance EraStAnnTx AllegraEra where
  type StAnnTx l AllegraEra = Tx l AllegraEra

  txStAnnTxG = id

  mkStAnnTx _ _ _ _ = id

  scriptsProvidedStAnnTx tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

instance ApplyTx AllegraEra where
  newtype ApplyTxError AllegraEra = AllegraApplyTxError (NonEmpty (ShelleyLedgerPredFailure AllegraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  applyTxValidation validationPolicy globals env state tx =
    first AllegraApplyTxError $
      ruleApplyTxValidation @"LEDGER" validationPolicy globals env state tx

instance ApplyTick AllegraEra

instance EraBlockHeader h AllegraEra => ApplyBlock h AllegraEra
