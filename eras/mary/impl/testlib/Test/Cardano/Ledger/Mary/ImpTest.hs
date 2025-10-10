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
  mkTokenMintingTx,
) where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
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
  ( ShelleyEraImp era
  , MaryEraTest era
  , Value era ~ MaryValue
  ) =>
  MaryEraImp era

instance MaryEraImp MaryEra

mkTokenMintingTx :: MaryEraImp era => ScriptHash -> ImpTestM era (Tx era)
mkTokenMintingTx sh = do
  name <- arbitrary
  count <- choose (1, 10)
  let policyId = PolicyID sh
  let ma = multiAssetFromList [(policyId, name, count)]
  addr <- freshKeyAddr_
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL . mintTxBodyL .~ ma
      & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr (MaryValue mempty ma)]
