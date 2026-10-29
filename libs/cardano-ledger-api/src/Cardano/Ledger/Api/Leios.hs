{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Api.Leios (
  validateLeiosCert,
  LeiosCertEnv (..),
  DijkstraLeiosCertPredFailure (..),
  LeiosCert,
) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraLeiosCertPredFailure (..),
  LEIOSCERT,
  LeiosCertEnv (..),
 )
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (TRC (..), applySTS)
import Data.List.NonEmpty (NonEmpty)

validateLeiosCert ::
  forall era.
  Era era =>
  Globals ->
  LeiosCertEnv ->
  LeiosCert ->
  Either (NonEmpty (DijkstraLeiosCertPredFailure era)) ()
validateLeiosCert globals env cert =
  runReader (applySTS @(LEIOSCERT era) (TRC (env, (), cert))) globals
