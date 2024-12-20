{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo (
  Alonzo,
  AlonzoEra,
  AlonzoTxOut,
  MaryValue,
  AlonzoTxBody,
  AlonzoScript,
  AlonzoTxAuxData,
  reapplyAlonzoTx,
)
where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Plutus.TxInfo ()
import Cardano.Ledger.Alonzo.Rules ()
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Transition ()
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.Tx ()
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody, AlonzoTxOut)
import Cardano.Ledger.Alonzo.TxWits ()
import Cardano.Ledger.Alonzo.UTxO ()
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import Cardano.Ledger.Shelley.API
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))

type Alonzo = AlonzoEra

{-# DEPRECATED Alonzo "In favor of `AlonzoEra`" #-}

-- =====================================================

reapplyAlonzoTx ::
  forall era m.
  (ApplyTx era, MonadError (ApplyTxError era) m) =>
  Globals ->
  MempoolEnv era ->
  MempoolState era ->
  Validated (Tx era) ->
  m (MempoolState era)
reapplyAlonzoTx globals env state vtx =
  let res =
        flip runReader globals
          . applySTSNonStatic
            @(EraRule "LEDGER" era)
          $ TRC (env, state, extractTx vtx)
   in liftEither . left ApplyTxError $ res

instance ApplyTx AlonzoEra where
  reapplyTx = reapplyAlonzoTx

instance ApplyBlock AlonzoEra
