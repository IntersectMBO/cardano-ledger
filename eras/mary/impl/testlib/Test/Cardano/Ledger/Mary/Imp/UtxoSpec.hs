{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  ( ShelleyEraImp era
  , MaryEraTxBody era
  , NativeScript era ~ Timelock era
  , Value era ~ MaryValue (EraCrypto era)
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXO" $ do
  it "Mint a Token" $ do
    (_, addr) <- freshKeyAddr
    (keyHash, _) <- freshKeyPair
    scriptHash <- impAddNativeScript $ RequireSignature keyHash
    let txCoin = Coin 1000000
        txAsset = MultiAsset $ Map.singleton (PolicyID scriptHash) $ Map.singleton (AssetName "testAsset") 100
        txValue = MaryValue txCoin txAsset
        txBody =
          mkBasicTxBody
            & outputsTxBodyL .~ [mkBasicTxOut addr txValue]
            & mintTxBodyL .~ txAsset
    submitTx_ $ mkBasicTx txBody
