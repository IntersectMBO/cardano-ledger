{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.PoolSpec (conwayEraSpecificSpec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

conwayEraSpecificSpec ::
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
conwayEraSpecificSpec =
  it "withdraw rewards and deregister in same transaction" $ do
    poolKh <- freshKeyHash
    registerPool poolKh
    stakeCred <- KeyHashObj <$> freshKeyHash
    _ <- registerStakeCredential stakeCred
    delegateStake stakeCred poolKh
    -- Conway requires DRep delegation for withdrawals (post-bootstrap)
    void $ delegateToDRep stakeCred (Coin 0) DRepAlwaysAbstain
    registerAndRetirePoolToMakeReward stakeCred
    balance <- getBalance stakeCred
    balance `shouldSatisfy` (> Coin 0)
    accountAddress <- getAccountAddressFor stakeCred
    unRegTxCert <- genUnRegTxCert stakeCred
    submitTx_ $
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL .~ [unRegTxCert]
        & bodyTxL . withdrawalsTxBodyL
          .~ Withdrawals (Map.singleton accountAddress balance)
    expectStakeCredNotRegistered stakeCred
    expectNotDelegatedToAnyPool stakeCred
