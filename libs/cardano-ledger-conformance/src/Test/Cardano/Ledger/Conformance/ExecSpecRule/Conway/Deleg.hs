{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg () where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core (KeyRole (..))
import Cardano.Ledger.Credential (Credential)
import Data.Bifunctor (second)
import Data.Set (Set)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()

instance ExecSpecRule "DELEG" ConwayEra where
  -- The context is the set of all DRep delegatees in the transaction
  type ExecContext "DELEG" ConwayEra = Set (Credential 'DRepRole)

  runAgdaRule (SpecTRC env (Agda.MkCertState dState pState vState) sig) =
    second
      (\dState' -> Agda.MkCertState dState' pState vState)
      . unComputationResult
      $ Agda.delegStep env dState sig
