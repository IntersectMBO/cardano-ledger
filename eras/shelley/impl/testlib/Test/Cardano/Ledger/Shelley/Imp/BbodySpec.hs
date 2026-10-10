{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.BbodySpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Block (
  Block (..),
  blockBodyHashBlockHeaderL,
  blockBodySizeBlockHeaderL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure (..),
  ShelleyUtxoPredFailure (..),
 )
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((%~), (.~), (^.))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ShelleyEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "BBODY" $ do
  it "WrongBlockBodySizeBBODY" $ do
    protVer <- getProtVer
    withTxsInModifiedFailingBlockM
      (blockBodySizeBlockHeaderL %~ (+ 1))
      (submitTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              WrongBlockBodySizeBBODY
                Mismatch
                  { mismatchSupplied = blockBodySize protVer $ blockBody block
                  , mismatchExpected = fromIntegral $ block ^. blockBodySizeBlockHeaderL
                  }
          ]

  it "InvalidBodyHashBBODY" $ do
    invalidBodyHash <- arbitrary
    withTxsInModifiedFailingBlockM
      (blockBodyHashBlockHeaderL .~ invalidBodyHash)
      (submitTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              InvalidBodyHashBBODY
                Mismatch
                  { mismatchSupplied = hashBlockBody $ blockBody block
                  , mismatchExpected = block ^. blockBodyHashBlockHeaderL
                  }
          ]

  it "LedgersFailure" $ do
    protVer <- getProtVer
    withTxsInModifiedFailingSubsetBlockM
      (modifyBlockBody protVer $ txSeqBlockBodyL %~ \txs -> txs <> txs)
      (submitTx_ $ mkBasicTx mkBasicTxBody)
      $ \block -> do
        Just badInputs <-
          pure $
            NES.fromSet $
              foldMap (^. bodyTxL . inputsTxBodyL) $
                blockBody block ^. txSeqBlockBodyL
        pure [injectFailure $ BadInputsUTxO badInputs]
