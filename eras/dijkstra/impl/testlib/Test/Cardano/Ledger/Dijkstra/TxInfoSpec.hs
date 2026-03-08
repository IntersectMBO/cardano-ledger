{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.TxInfoSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfoResult (..),
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..), pattern SpendingPurpose)
import Cardano.Ledger.BaseTypes (Globals (..), Inject (..), Network (..), ProtVer (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Dijkstra.Core (
  ConwayEraTxBody,
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  Value,
  eraProtVerLow,
 )
import Cardano.Ledger.Dijkstra.State (UTxO (..))
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError (..))
import Cardano.Ledger.Plutus (Language (..), SLanguage (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common (Arbitrary (..), Spec, describe, prop, shouldBeLeft)
import Test.Cardano.Ledger.Core.Utils (testGlobals)
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

spec ::
  forall era.
  ( EraPlutusTxInfo PlutusV4 era
  , Inject (DijkstraContextError era) (ContextError era)
  , ConwayEraTxBody era
  , EraTx era
  , Arbitrary (Value era)
  ) =>
  Spec
spec = describe "TxInfo" $ do
  describe "PlutusV4" $ do
    prop "Fails translation when Ptr present in outputs" $
      do
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
            mkBasicTx @era $
              mkBasicTxBody
                & outputsTxBodyL .~ [txOut]
                & inputsTxBodyL .~ [txIn]
          ledgerTxInfo =
            LedgerTxInfo @era
              (ProtVer (eraProtVerLow @era) 0)
              (epochInfo testGlobals)
              (systemStart testGlobals)
              utxo
              tx
        pure $
          (($ SpendingPurpose AsPurpose) <$> unPlutusTxInfoResult (toPlutusTxInfo SPlutusV4 ledgerTxInfo))
            `shouldBeLeft` inject (PointerPresentInOutput @era [txOut])
