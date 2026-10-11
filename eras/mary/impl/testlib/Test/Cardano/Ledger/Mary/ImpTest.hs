{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.ImpTest (
  MaryEraImp,
  module Test.Cardano.Ledger.Allegra.ImpTest,
) where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.Era
import Test.Cardano.Ledger.Mary.TreeDiff ()

instance ShelleyEraImp MaryEra where
  impSatisfyNativeScript = impAllegraSatisfyNativeScript
  fixupTx = shelleyFixupTx
  expectTxSuccess = impShelleyExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer
  genRegTxCert = shelleyGenRegTxCert
  genUnRegTxCert = shelleyGenUnRegTxCert
  delegStakeTxCert = shelleyDelegStakeTxCert

class
  ( AllegraEraImp era
  , MaryEraTest era
  , Value era ~ MaryValue
  ) =>
  MaryEraImp era

instance AllegraEraImp MaryEra

instance MaryEraImp MaryEra
