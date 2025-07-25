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

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo (
  genUtxoExecContext,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraPParams (..), ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Lens.Micro ((&), (.~), (^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen)
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..), runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (
  UtxoExecContext (..),
 )
import Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
  GenSize (..),
  GenState (..),
  initialLedgerState,
  runGenRS,
 )
import qualified Test.Cardano.Ledger.Generic.GenState as GenSize
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)

genUtxoExecContext :: Gen (UtxoExecContext ConwayEra)
genUtxoExecContext = do
  ueSlot <- arbitrary
  let
    genSize =
      GenSize.small
        { invalidScriptFreq = 0 -- TODO make the test work with invalid scripts
        , regCertFreq = 0
        , delegCertFreq = 0
        }
  ((uecUTxO, uecTx), gs) <- runGenRS genSize $ genAlonzoTx ueSlot
  let
    txSize = uecTx ^. sizeTxF
    lState = initialLedgerState gs
    ueCertState = lsCertState lState
    uePParams =
      gePParams (gsGenEnv gs)
        & ppMaxTxSizeL .~ fromIntegral txSize
        & ppProtocolVersionL .~ ProtVer (natVersion @10) 0
    uecUtxoEnv = UtxoEnv {..}
  pure UtxoExecContext {..}

instance ExecSpecRule "UTXO" ConwayEra where
  type ExecContext "UTXO" ConwayEra = UtxoExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction (Agda.utxoStep externalFunctions)
