{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.Rules (utxoEnvCertStateL)
import Control.State.Transition.Extended (TRC (..))
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runFromAgdaFunction,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Generic.Instances ()

instance ExecSpecRule "UTXO" ConwayEra where
  type ExecContext "UTXO" ConwayEra = UtxoExecContext ConwayEra

  translateInputs ctx (TRC (env, st, sig)) = do
    agdaEnv <- runSpecTransM () $ toSpecRep @ConwayEra env
    agdaSt <- runSpecTransM (uecUtxoEnv ctx ^. utxoEnvCertStateL) $ toSpecRep @ConwayEra st
    agdaSig <- runSpecTransM () $ toSpecRep @ConwayEra sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  translateOutput ctx _ st =
    runSpecTransM (uecUtxoEnv ctx ^. utxoEnvCertStateL) $ toSpecRep @ConwayEra st

  runAgdaRule = runFromAgdaFunction (Agda.utxoStep externalFunctions)
