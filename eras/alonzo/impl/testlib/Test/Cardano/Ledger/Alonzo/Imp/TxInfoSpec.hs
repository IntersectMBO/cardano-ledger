{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.TxInfoSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo (..), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.BaseTypes (EpochSize (..), Inject (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Either (isRight)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = withImpInit @(LedgerSpec AlonzoEra) $ describe "TxInfoSpec" $ do
  describe "PlutusV1" $ do
    it "toPlutusTxInfo does not fail when Byron scripts are present in TxOuts" $ do
      pv <- getProtVer
      (_, shelleyAddr) <- freshKeyAddr
      byronAddr <- AddrBootstrap <$> freshBootstapAddress
      shelleyTxIn <- arbitrary
      let
        byronTxOut = mkBasicTxOut byronAddr . inject $ Coin 1
        tx =
          mkBasicTx @AlonzoEra mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton shelleyTxIn
            & bodyTxL . outputsTxBodyL .~ SSeq.singleton byronTxOut
        lti =
          LedgerTxInfo
            { ltiProtVer = pv
            , ltiEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
            , ltiSystemStart = SystemStart $ posixSecondsToUTCTime 0
            , ltiUTxO = UTxO . Map.singleton shelleyTxIn $ AlonzoTxOut shelleyAddr (inject $ Coin 2) SNothing
            , ltiTx = tx
            }
      toPlutusTxInfo SPlutusV1 lti `shouldSatisfy` isRight
    it "toPlutusTxInfo does not fail when Byron scripts are present in TxIns" $ do
      pv <- getProtVer
      (_, shelleyAddr) <- freshKeyAddr
      byronAddr <- AddrBootstrap <$> freshBootstapAddress
      byronTxIn <- arbitrary
      let
        shelleyTxOut = mkBasicTxOut shelleyAddr . inject $ Coin 1
        tx =
          mkBasicTx @AlonzoEra mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton byronTxIn
            & bodyTxL . outputsTxBodyL .~ SSeq.singleton shelleyTxOut
        lti =
          LedgerTxInfo
            { ltiProtVer = pv
            , ltiEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
            , ltiSystemStart = SystemStart $ posixSecondsToUTCTime 0
            , ltiUTxO =
                UTxO . Map.singleton byronTxIn $
                  AlonzoTxOut byronAddr (inject $ Coin 2) SNothing
            , ltiTx = tx
            }
      toPlutusTxInfo SPlutusV1 lti `shouldSatisfy` isRight
