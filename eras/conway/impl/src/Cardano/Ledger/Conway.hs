{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway (
  Conway,
  ConwayEra,
)
where

import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Conway.API.Genesis (CanStartFromGenesis (..))
import Cardano.Ledger.Conway.API.Mempool (
  ApplyTx (reapplyTx),
  ApplyTxError (ApplyTxError),
  extractTx,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance (RunConwayRatify (..))
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.Transition ()
import Cardano.Ledger.Conway.Translation ()
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Conway.UTxO ()
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Control.Arrow (left)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition (TRC (TRC))

type Conway = ConwayEra StandardCrypto

-- =====================================================

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  ) =>
  ApplyTx (ConwayEra c)
  where
  reapplyTx globals env state vtx =
    let res =
          flip runReader globals
            . applySTSNonStatic
              @(EraRule "LEDGER" (ConwayEra c))
            $ TRC (env, state, extractTx vtx)
     in liftEither . left ApplyTxError $ res

instance Crypto c => CanStartFromGenesis (ConwayEra c) where
  type AdditionalGenesisConfig (ConwayEra c) = ConwayGenesis c
  fromShelleyPParams =
    error "Unimplemented: Current interface is too limited and needs replacement for Conway to work"

instance Crypto c => RunConwayRatify (ConwayEra c)
