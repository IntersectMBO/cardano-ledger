{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- CanStartFromGenesis
{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Cardano.Ledger.Alonzo.Genesis
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
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Plutus.Data ()
import Cardano.Ledger.Rules.ValidationMode (applySTSNonStatic)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.API.Mempool
import Control.Arrow (left)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Reader (runReader)
import Control.State.Transition.Extended (TRC (TRC))

type Alonzo = AlonzoEra StandardCrypto

-- =====================================================

reapplyAlonzoTx ::
  forall era m.
  (API.ApplyTx era, MonadError (ApplyTxError era) m) =>
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
          $ TRC (env, state, API.extractTx vtx)
   in liftEither . left API.ApplyTxError $ res

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyTx (AlonzoEra c) where
  reapplyTx = reapplyAlonzoTx

instance (Crypto c, DSignable c (Hash c EraIndependentTxBody)) => API.ApplyBlock (AlonzoEra c)

instance Crypto c => API.CanStartFromGenesis (AlonzoEra c) where
  type AdditionalGenesisConfig (AlonzoEra c) = AlonzoGenesis
  fromShelleyPParams ag = translateEra' ag . API.fromShelleyPParams ()
