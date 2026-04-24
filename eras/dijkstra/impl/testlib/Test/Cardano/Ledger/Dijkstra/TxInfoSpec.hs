{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
  SupportedLanguage (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.BaseTypes (Globals (..), Inject (..), Network (..), ProtVer (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Plutus (Language (..), SLanguage (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec ::
  forall era.
  ( EraPlutusTxInfo PlutusV1 era
  , EraPlutusTxInfo PlutusV2 era
  , EraPlutusTxInfo PlutusV3 era
  , EraPlutusTxInfo PlutusV4 era
  , Inject (DijkstraContextError era) (ContextError era)
  , ConwayEraTxBody era
  , EraTx era
  , Arbitrary (Value era)
  ) =>
  Spec
spec = describe "TxInfo" $ do
  describe "PlutusV4" $ do
    prop "Fails translation when Ptr present in outputs" $ do
      paymentCred <- arbitrary
      ptr <- arbitrary
      val <- arbitrary
      let
        txOut = mkBasicTxOut (Addr Testnet paymentCred (StakeRefPtr ptr)) val
      txIn <- arbitrary
      paymentCred2 <- arbitrary
      stakeRef <- arbitrary
      let
        utxo =
          UTxO
            [ (txIn, mkBasicTxOut (Addr Testnet paymentCred2 stakeRef) val)
            ]
        tx =
          mkBasicTx @era @TopTx $
            mkBasicTxBody
              & outputsTxBodyL .~ [txOut]
              & inputsTxBodyL .~ [txIn]
        ledgerTxInfo =
          LedgerTxInfo
            { ltiProtVer = ProtVer (eraProtVerLow @era) 0
            , ltiEpochInfo = epochInfo testGlobals
            , ltiSystemStart = systemStart testGlobals
            , ltiUTxO = utxo
            , ltiTx = tx
            , ltiMemoizedSubTransactions = mempty
            }
      pure $
        (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
          `shouldBeLeft` inject (PointerPresentInOutput @era [txOut])
  describe "PlutusV1-V3" $ do
    let plutusV1toV3 :: [SupportedLanguage era]
        plutusV1toV3 =
          [ SupportedLanguage SPlutusV1
          , SupportedLanguage SPlutusV2
          , SupportedLanguage SPlutusV3
          ]
    forM_ plutusV1toV3 $ \(SupportedLanguage slang) -> do
      it "SubTxIsNotSupported" $ do
        let
          tx = mkBasicTx @era @SubTx mkBasicTxBody
          ledgerTxInfo =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = epochInfo testGlobals
              , ltiSystemStart = systemStart testGlobals
              , ltiUTxO = mempty
              , ltiTx = tx
              , ltiMemoizedSubTransactions = mempty
              }
          txInfoResult =
            ($ SpendingPurpose AsPurpose)
              <$> unPlutusTxInfoResult (toPlutusTxInfo slang ledgerTxInfo)
        txInfoResult `shouldBeLeft` inject (SubTxIsNotSupported @era (txIdTx tx))
