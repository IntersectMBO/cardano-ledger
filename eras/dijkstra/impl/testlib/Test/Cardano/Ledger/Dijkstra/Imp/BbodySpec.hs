{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.BbodySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
  BlockHeaderVersionInfo (..),
  prevNonceBlockHeaderL,
  versionInfoBlockHeaderL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraEraBlockBody (..))
import Cardano.Ledger.Dijkstra.Rules (DijkstraBbodyPredFailure (..))
import Lens.Micro ((%~), (.~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "BBODY" $ do
  it "PerasCertValidationFailed" $ do
    protVer <- getProtVer
    perasCert <- arbitrary
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ perasCertBlockBodyL .~ SJust perasCert)
      (submitTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              PerasCertValidationFailed perasCert $
                block ^. prevNonceBlockHeaderL
          ]

  it "HeaderProtVerTooLow" $ do
    ProtVer curMajor _ <- getProtVer
    withTxsInModifiedFailingBlockM
      ( versionInfoBlockHeaderL
          %~ \versionInfo ->
            versionInfo
              { bhviHighestSupportedMajorVersion =
                  pred $ bhviHighestSupportedMajorVersion versionInfo
              }
      )
      (submitTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              HeaderProtVerTooLow
                Mismatch
                  { mismatchSupplied =
                      bhviHighestSupportedMajorVersion $ block ^. versionInfoBlockHeaderL
                  , mismatchExpected = getVersion32 curMajor
                  }
          ]
