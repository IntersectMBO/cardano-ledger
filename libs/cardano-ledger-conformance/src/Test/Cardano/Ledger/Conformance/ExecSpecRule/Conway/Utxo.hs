{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo () where

import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.Rules (utxoEnvCertStateL)
import Control.State.Transition.Extended (TRC (..))
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTRC (..),
  SpecTranslate (..),
  runFromAgdaFunction,
  withCtxSpecTransM,
  withSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Generic.Instances ()

instance ExecSpecRule "UTXO" ConwayEra where
  type ExecContext "UTXO" ConwayEra = UtxoExecContext ConwayEra

  translateInputs (TRC (env, st, sig)) = do
    agdaEnv <- withCtxSpecTransM () $ toSpecRep env
    agdaSt <- withSpecTransM (view utxoEnvCertStateL . uecUtxoEnv) $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  translateOutput _ =
    withSpecTransM (view utxoEnvCertStateL . uecUtxoEnv) . toSpecRep

  runAgdaRule = runFromAgdaFunction (Agda.utxoStep externalFunctions)

instance EncCBOR (Tx l era) => EncCBOR (AlonzoStAnnTx l era) where
  encCBOR AlonzoStAnnTx {asatTx} = encCBOR asatTx
