{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Imp.UtxoSpec (spec, maryOnlySpec) where

import Cardano.Ledger.Allegra.Rules
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Value
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Tools
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import Lens.Micro
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Mary.ImpTest

mintBasicToken ::
  (HasCallStack, MaryEraImp era) => ImpTestM era (Tx TopTx era)
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
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1279
  -- TODO: Re-enable after issue is resolved, by removing this override
  disableInConformanceIt "Mint a Token" $ void mintBasicToken
  describe "ShelleyUtxoPredFailure" $ do
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/1279
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "ValueNotConservedUTxO" $ do
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
              & inputsTxBodyL .~ [txInAt 0 txMinted]
              & mintTxBodyL .~ burnTooMuchMultiAsset
      (_, rootTxOut) <- getImpRootTxOut
      let rootTxOutValue = rootTxOut ^. valueTxOutL
      submitFailingTx
        (mkBasicTx txBody)
        [ injectFailure $
            Shelley.ValueNotConservedUTxO $
              Mismatch
                (rootTxOutValue <> MaryValue c (MultiAsset mintedMultiAsset))
                (rootTxOutValue <> MaryValue c burnTooMuchProducedMultiAsset)
        ]

maryOnlySpec :: SpecWith (ImpInit (LedgerSpec MaryEra))
maryOnlySpec = describe "UTXO" $
  describe "AllegraUtxoPredFailure" $
    it "OutputTooBigUTxO" $ do
      pp <- getsPParams id
      policyId <- PolicyID <$> (impAddNativeScript . RequireSignature =<< freshKeyHash)
      -- Serialized, 125 asset names of 32 bytes exceed the 4000 bytes allowed for a `Value`
      assetNames <- replicateM 125 $ AssetName <$> uniformShortByteStringM 32
      addr <- freshKeyAddr_
      let multiAsset = multiAssetFromList [(policyId, assetName, 1) | assetName <- assetNames]
          txOut = setMinCoinTxOut pp . mkBasicTxOut addr $ MaryValue mempty multiAsset
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & mintTxBodyL .~ multiAsset
              & outputsTxBodyL .~ [txOut]
        )
        [injectFailure $ OutputTooBigUTxO [txOut]]
