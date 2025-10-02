{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.TxInfoSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (ContextError),
  toPlutusTxCert,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxInfo (transValidityInterval)
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Slot
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Proxy (Proxy (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Genesis ()

spec :: Spec
spec = do
  describe "TxInfo" $ do
    let trans pv cert = either (error . show) id (toPlutusTxCert @'PlutusV3 @ConwayEra Proxy pv cert)
        transV9 = trans (ProtVer (natVersion @9) 0)
        transV10 = trans (ProtVer (natVersion @10) 0)

    prop "Deposit in registration certs" $ \(cred :: StakeCredential) (coin :: Coin) -> do
      expectNoDeposit $ transV9 $ ConwayTxCertDeleg $ ConwayRegCert cred (SJust coin)
      expectNoDeposit $ transV9 $ RegDepositTxCert cred coin
      expectNoDeposit $ transV9 $ ConwayTxCertDeleg $ ConwayRegCert cred SNothing

      expectDeposit coin $ transV10 $ ConwayTxCertDeleg $ ConwayRegCert cred (SJust coin)
      expectDeposit coin $ transV10 $ RegDepositTxCert cred coin
      expectNoDeposit $ transV10 $ ConwayTxCertDeleg $ ConwayRegCert cred SNothing

    prop "Deposit in unregistration certs" $ \(cred :: StakeCredential) (coin :: Coin) -> do
      expectNoDeposit $ transV9 $ ConwayTxCertDeleg $ ConwayUnRegCert cred (SJust coin)
      expectNoDeposit $ transV9 $ UnRegDepositTxCert cred coin
      expectNoDeposit $ transV9 $ ConwayTxCertDeleg $ ConwayUnRegCert cred SNothing

      expectDeposit coin $ transV10 $ ConwayTxCertDeleg $ ConwayUnRegCert cred (SJust coin)
      expectDeposit coin $ transV10 $ UnRegDepositTxCert cred coin
      expectNoDeposit $ transV10 $ ConwayTxCertDeleg $ ConwayUnRegCert cred SNothing

    it "validity interval's upper bound is open when protocol >= 9" $
      transVITimeUpperBoundIsOpen
  where
    expectDeposit :: Coin -> PV3.TxCert -> IO ()
    expectDeposit (Coin c) =
      \case
        PV3.TxCertRegStaking _ (Just d) -> PV2.Lovelace c `shouldBe` d
        PV3.TxCertUnRegStaking _ (Just d) -> PV2.Lovelace c `shouldBe` d
        txcert ->
          expectationFailure $
            "Deposit: " <> show (Coin c) <> " expected in: " <> show txcert <> ", but not found"
    expectNoDeposit :: PV3.TxCert -> IO ()
    expectNoDeposit =
      \case
        PV3.TxCertRegStaking _ Nothing -> pure ()
        PV3.TxCertUnRegStaking _ Nothing -> pure ()
        txcert ->
          expectationFailure $
            "Deposit not expected, but found in: " <> show txcert

-- | The test checks that since protocol version 9 'transVITime' works correctly,
-- by returning open upper bound of the validaty interval.
transVITimeUpperBoundIsOpen :: Expectation
transVITimeUpperBoundIsOpen = do
  let interval = ValidityInterval SNothing (SJust (SlotNo 40))
  case transValidityInterval (Proxy @ConwayEra) ei ss interval of
    Left (e :: ContextError ConwayEra) ->
      expectationFailure $ "no translation error was expected, but got: " <> show e
    Right t ->
      t
        `shouldBe` PV1.Interval
          (PV1.LowerBound PV1.NegInf True)
          (PV1.UpperBound (PV1.Finite (PV1.POSIXTime 40000)) False)

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0
