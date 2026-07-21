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
import Cardano.Ledger.Shelley.API
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

instance ApplyTx AllegraEra where
  newtype ApplyTxError AllegraEra
    = AllegraApplyTxError (NonEmpty (Shelley.ShelleyLedgerPredFailure AllegraEra))
    deriving (Eq, Show)
    deriving newtype (EncCBOR, DecCBOR, Semigroup, Generic)

  mkStAnnTx _ _ _ _ _ = id

  internalApplyTxWithValidation = defaultApplyTxWithValidation @"LEDGER" AllegraApplyTxError

  internalReapplyValidatedTx = defaultReapplyValidatedTx @"LEDGER" AllegraApplyTxError

instance ApplyTick AllegraEra

instance EraBlockHeader h AllegraEra => ApplyBlock h AllegraEra
