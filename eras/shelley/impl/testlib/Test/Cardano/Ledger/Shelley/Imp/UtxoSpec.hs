{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Address (protectedAddressesSupported)
import Cardano.Ledger.BaseTypes (Mismatch (..), Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Val (inject)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set.NonEmpty as NES
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec :: forall era. ShelleyEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  unless (protectedAddressesSupported (eraProtVerHigh @era)) $ do
    it "rejects protected ordinary outputs submitted directly in an unsupported era" $ do
      payment <- KeyHashObj <$> freshKeyHash
      let ordinary = Addr Testnet payment StakeRefNull
          protected = AddrProtected Testnet payment StakeRefNull
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut ordinary (inject (Coin 2000000))]
          protectFirst :: Tx TopTx era -> Tx TopTx era
          protectFirst txToProtect =
            txToProtect
              & bodyTxL . outputsTxBodyL %~ \case
                Empty -> Empty
                firstOut :<| rest -> (firstOut & addrTxOutL @era .~ protected) :<| rest
      withPostFixup (rederiveAddrTxWits . protectFirst) $
        submitFailingTx tx [injectFailure $ UnsupportedOutputAddresses (NES.singleton 0)]
  describe "ShelleyUtxoPredFailure" $ do
    it "ValueNotConservedUTxO" $ do
      addr1 <- freshKeyAddr_
      let txAmount = Coin 2000000
      txIn <- sendCoinTo addr1 txAmount
      addr2 <- freshKeyAddr_
      (_, rootTxOut) <- getImpRootTxOut
      let extra = Coin 3
          rootTxOutValue = rootTxOut ^. valueTxOutL
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ [txIn]
              & outputsTxBodyL .~ [mkBasicTxOut addr2 mempty]
          adjustTxOut = \case
            Empty -> error "Unexpected empty sequence of outputs"
            txOut :<| outs -> (txOut & coinTxOutL %~ (<> extra)) :<| outs
          adjustFirstTxOut tx =
            tx
              & bodyTxL . outputsTxBodyL %~ adjustTxOut
              & witsTxL .~ mkBasicTxWits
      withPostFixup (updateAddrTxWits . adjustFirstTxOut) $
        submitFailingTx
          (mkBasicTx txBody)
          [ injectFailure $
              ValueNotConservedUTxO $
                Mismatch
                  (rootTxOutValue <> inject txAmount)
                  (rootTxOutValue <> inject (txAmount <> extra))
          ]
