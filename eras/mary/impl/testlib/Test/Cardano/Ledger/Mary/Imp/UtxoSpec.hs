{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import Lens.Micro
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.ImpTest

mintBasicToken ::
  (HasCallStack, AllegraEraScript era, MaryEraImp era) => ImpTestM era (Tx era)
mintBasicToken = do
  addr <- freshKeyAddr_
  keyHash <- freshKeyHash
  scriptHash <- impAddNativeScript $ RequireSignature keyHash
  Positive amount <- arbitrary
  let txAsset = MultiAsset $ Map.singleton (PolicyID scriptHash) $ Map.singleton (AssetName "testAsset") amount
      txValue :: MaryValue
      txValue = MaryValue mempty txAsset
      txBody =
        mkBasicTxBody
          & outputsTxBodyL .~ [mkBasicTxOut addr txValue]
          & mintTxBodyL .~ txAsset
  submitTx $ mkBasicTx txBody

spec ::
  ( HasCallStack
  , MaryEraImp era
  , AllegraEraScript era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  it "Mint a Token" $ void mintBasicToken
  describe "ShelleyUtxoPredFailure" $ do
    it "ValueNotConservedUTxO" $ do
      -- Burn too much
      Positive tooMuch <- arbitrary
      txMinted <- mintBasicToken
      let MaryValue c (MultiAsset mintedMultiAsset) =
            case txMinted ^. bodyTxL . outputsTxBodyL of
              Empty -> error "Empty outputs was unexpected"
              txOut :<| _ -> txOut ^. valueTxOutL
          burnTooMuchMultiAsset@(MultiAsset burnTooMuch) =
            MultiAsset (Map.map (Map.map (subtract tooMuch . negate)) mintedMultiAsset)
          -- Produced should contain positive value that was atttempted to be burned
          burnTooMuchProducedMultiAsset = MultiAsset (Map.map (Map.map negate) burnTooMuch)
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ [txInAt (0 :: Int) txMinted]
              & mintTxBodyL .~ burnTooMuchMultiAsset
      (_, rootTxOut) <- lookupImpRootTxOut
      let rootTxOutValue = rootTxOut ^. valueTxOutL
      submitFailingTx
        (mkBasicTx txBody)
        [ injectFailure $
            ValueNotConservedUTxO $
              Mismatch
                (rootTxOutValue <> MaryValue c (MultiAsset mintedMultiAsset))
                (rootTxOutValue <> MaryValue c burnTooMuchProducedMultiAsset)
        ]
