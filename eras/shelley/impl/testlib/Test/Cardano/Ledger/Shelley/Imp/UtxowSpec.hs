{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxowSpec (spec) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.SafeHash (extractHash, hashAnnotated)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  describe "Bootstrap Witness" $ do
    it "Valid Witnesses" $ do
      modifyPParams (ppMaxTxSizeL .~ 1000)
      aliceBootAddr <- freshBootstapAddress
      txIn <- sendCoinTo (AddrBootstrap aliceBootAddr) (Coin 1000000)
      let txBody = mkBasicTxBody & inputsTxBodyL .~ [txIn]
      submitTx_ (mkBasicTx txBody)
    it "InvalidWitnessesUTXOW" $ do
      modifyPParams (ppMaxTxSizeL .~ 1000)
      aliceBootAddr@(BootstrapAddress aliceByronAddr) <- freshBootstapAddress
      aliceByronKeyPair <- lookupByronKeyPair aliceBootAddr
      txIn <- sendCoinTo (AddrBootstrap aliceBootAddr) (Coin 1000000)
      let (aliceVKey, _) = unpackByronVKey (bkpVerificationKey aliceByronKeyPair)
          txBody = mkBasicTxBody & inputsTxBodyL .~ [txIn]

          -- below we use TxBody that was not fixed up, which means the hash for signing
          -- will be different thus resulting in a wrong signature
          aliceBadWitness =
            makeBootstrapWitness
              (extractHash (hashAnnotated txBody))
              (bkpSigningKey aliceByronKeyPair)
              (Byron.addrAttributes aliceByronAddr)

          txBad =
            mkBasicTx txBody
              & (witsTxL %~ (bootAddrTxWitsL .~ [aliceBadWitness]))
      submitFailingTx txBad [injectFailure $ InvalidWitnessesUTXOW [aliceVKey]]
