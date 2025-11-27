{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Inject (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core (
  BabbageEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  InjectRuleFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxoPredFailure (..))
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  LedgerSpec,
  freshKeyHash,
  getsPParams,
  submitFailingTx,
 )
import Test.Cardano.Ledger.Imp.Common (SpecWith, arbitrary, describe, it)

spec ::
  forall era.
  ( DijkstraEraImp era
  , InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec =
  describe "Collaterals" $ do
    it "Fails to submit a transaction containing a Ptr in collateral return" $ do
      cred <- KeyHashObj <$> freshKeyHash
      ptr <- arbitrary
      pp <- getsPParams id
      let
        ptrAddr = Addr Testnet cred (StakeRefPtr ptr)
        ptrOutput = ensureMinCoinTxOut pp $ mkBasicTxOut ptrAddr . inject $ Coin 100
        tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . collateralReturnTxBodyL .~ SJust ptrOutput
      submitFailingTx tx [injectFailure $ PtrPresentInCollateral ptrOutput]
